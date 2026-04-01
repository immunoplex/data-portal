# I-SPI Migration SQL Queries
# Direct insertion into madi_dat schema
# All functions support transaction testing (commit=FALSE for testing)

# Helper function to generate next accession number
generate_next_accession <- function(conn, table, column, prefix) {
  query <- sprintf(
    "SELECT CONCAT('%s', COALESCE(MAX(CAST(SUBSTRING(%s, %d) AS INTEGER)), 0) + 1) AS next_accession 
     FROM madi_dat.%s 
     WHERE %s ~ '^%s[0-9]+$'",
    prefix,
    column,
    nchar(prefix) + 1,
    table,
    column,
    prefix
  )
  
  result <- DBI::dbGetQuery(conn, query)
  return(result$next_accession)
}

# =====================================================
# 0. VALIDATE OR CREATE EXPERIMENT
# =====================================================
validate_or_create_experiment <- function(conn, experiment_accession, study_accession, 
                                          workspace_id, create_new = FALSE, 
                                          name = NULL, description = NULL, measurement_technique = NULL,
                                          commit = FALSE, manage_transaction = TRUE) {
  
  cat("[INFO] Validating experiment\n")
  
  # Check if experiment exists
  existing <- DBI::dbGetQuery(conn,
    "SELECT experiment_accession, name, measurement_technique 
     FROM madi_dat.experiment 
     WHERE experiment_accession = $1 AND workspace_id = $2 LIMIT 1",
    params = list(as.character(experiment_accession)[1], as.integer(workspace_id)[1])
  )
  
  if(nrow(existing) > 0) {
    if(create_new) {
       cat("  [WARN] Experiment", experiment_accession, "already exists. Reusing it instead of creating new.\n")
    }
    cat("  [OK] Experiment", experiment_accession, "exists:", existing$name[1], "\n")
    return(list(success = TRUE, created = FALSE, experiment_accession = existing$experiment_accession[1], experiment_name = existing$name[1]))
  }
  
  if(!create_new) {
    cat("  [ERROR] Experiment", experiment_accession, "NOT FOUND!\n")
    return(list(success = FALSE, created = FALSE, error = paste("Experiment", experiment_accession, "does not exist")))
  }
  
  cat("  🔨 Creating new experiment:", experiment_accession, "\n")
  
  # Set defaults if missing
  exp_name <- if(is.null(name) || name == "") paste("ISPI", experiment_accession) else name
  exp_desc <- if(is.null(description) || description == "") paste("I-SPI Migration", Sys.Date()) else description
  exp_tech <- if(is.null(measurement_technique) || measurement_technique == "") "Multiplex Bead Array Assay" else measurement_technique
  
  # Start transaction
  if(manage_transaction && !commit) DBI::dbExecute(conn, "BEGIN")
  
  tryCatch({
    insert_reference_to_madi_dat.experiment(
      connection = conn, experiment_accession = experiment_accession,
      description = exp_desc,
      measurement_technique = exp_tech,
      experiment_name = exp_name,
      study_accession = study_accession, workspace_id = workspace_id
    )
    
    if(manage_transaction) {
      if(!commit) {
        DBI::dbExecute(conn, "ROLLBACK")
        cat("  [ROLLBACK] ROLLED BACK: Would create experiment\n")
      }  else {
        cat("  [OK] Created experiment\n")
        # In commit mode, we rely on auto-commit or explicit commit if we started transaction?
        # DBI usually auto-commits if no transaction started.
        # But if we didn't start one (because commit=TRUE?), wait.
        # Logic: if commit=TRUE, we usually Don't do BEGIN/COMMIT explicitly unless multi-step.
        # But here we didn't start BEGIN if commit=TRUE.
        # So we don't need to COMMIT either.
      }
    } else {
        cat("  [OK] Created experiment (in transaction)\n")
    }
    
    return(list(success = TRUE, created = TRUE, experiment_accession = experiment_accession, experiment_name = paste("ISPI", experiment_accession)))
    
  }, error = function(e) {
    if(manage_transaction && !commit) DBI::dbExecute(conn, "ROLLBACK")
    cat("  [ERROR] Failed:", e$message, "\n")
    return(list(success = FALSE, created = FALSE, error = e$message))
  })
}


# =====================================================
# UTILITY: Get List of Source Studies (filtered by project_id)
# =====================================================
get_source_studies_list <- function(conn, schema = "madi_results", project_id = NULL) {
  tryCatch({
    if(!is.null(project_id)) {
      # Filter studies by project_id from xmap_header
      query <- paste0("SELECT DISTINCT study_accession FROM ", schema, 
                      ".xmap_header WHERE project_id = $1 ORDER BY study_accession")
      res <- DBI::dbGetQuery(conn, query, params = list(as.integer(project_id)))
    } else {
      # Fallback: get all studies from xmap_header
      query <- paste0("SELECT DISTINCT study_accession FROM ", schema, 
                      ".xmap_header ORDER BY study_accession")
      res <- DBI::dbGetQuery(conn, query)
    }
    return(res$study_accession)
  }, error = function(e) {
    return(character(0))
  })
}

# =====================================================
# UTILITY: Get List of Source Experiments (filtered by project_id + study)
# =====================================================
get_source_experiments_list <- function(conn, schema = "madi_results", project_id = NULL, study_accession = NULL) {
  tryCatch({
    conditions <- c()
    params <- list()
    param_idx <- 1
    
    if(!is.null(project_id)) {
      conditions <- c(conditions, paste0("project_id = $", param_idx))
      params[[param_idx]] <- as.integer(project_id)
      param_idx <- param_idx + 1
    }
    if(!is.null(study_accession) && study_accession != "") {
      conditions <- c(conditions, paste0("study_accession = $", param_idx))
      params[[param_idx]] <- as.character(study_accession)
      param_idx <- param_idx + 1
    }
    
    where_clause <- if(length(conditions) > 0) paste0(" WHERE ", paste(conditions, collapse = " AND ")) else ""
    query <- paste0("SELECT DISTINCT experiment_accession FROM ", schema, ".xmap_header", 
                    where_clause, " ORDER BY experiment_accession")
    res <- DBI::dbGetQuery(conn, query, params = params)
    return(res$experiment_accession)
  }, error = function(e) {
    return(character(0))
  })
}


# =====================================================
# 1. VALIDATE EXISTING SUBJECTS
# =====================================================
# Validates that subjects from xmap_subjects exist in the target database
# Does NOT create new subjects - they should already exist
validate_existing_subjects <- function(conn, source_data, workspace_id, commit = FALSE, mapping_override = NULL) {
  
  # Get unique patients and their mapped subject_accession from source data
  # Note: source_data should include subject_accession from xmap_subjects join
  unique_patients <- source_data %>%
    select(patientid, subject_accession) %>%
    filter(!is.na(patientid), !is.na(subject_accession)) %>%
    distinct(patientid, .keep_all = TRUE)
  
  # Also include patients mapped via CSV/Excel upload (mapping_override)
  if(!is.null(mapping_override) && nrow(mapping_override) > 0) {
    override_patients <- mapping_override %>%
      rename(subject_accession = subject_accession) %>%
      filter(!is.na(patientid), !is.na(subject_accession)) %>%
      distinct(patientid, .keep_all = TRUE)
    # Merge — mapping_override wins for any patient_id that appears in both
    combined <- bind_rows(
      override_patients,
      unique_patients %>% filter(!patientid %in% override_patients$patientid)
    )
    unique_patients <- combined %>% distinct(patientid, .keep_all = TRUE)
  }
  
  cat("[INFO] Found", nrow(unique_patients), "unique patients with subject mappings\n")
  
  if(nrow(unique_patients) == 0) {
    cat("[WARN] No patients with subject_accession mappings\n")
    return(list(success = FALSE, error = "No subject mappings found in source data", validated = 0, missing = 0))
  }
  
  validated_count <- 0
  missing_subjects <- list()
  
  for(i in 1:nrow(unique_patients)) {
    patient <- unique_patients[i, ]
    subject_acc <- as.character(patient$subject_accession)[1]
    
    # Check if subject exists in target database
    existing <- DBI::dbGetQuery(conn,
      "SELECT COUNT(*) as count FROM madi_dat.subject 
       WHERE subject_accession = $1 AND workspace_id = $2",
      params = list(subject_acc, as.integer(workspace_id)[1])
    )
    
    if(existing$count > 0) {
      validated_count <- validated_count + 1
      if(validated_count <= 5) {
        cat("  [OK] Subject", subject_acc, "exists (patient", patient$patientid, ")\n")
      } else if(validated_count == 6) {
        cat("  ... (showing first 5)\n")
      }
    } else {
      cat("  [ERROR] Subject", subject_acc, "NOT FOUND in workspace", workspace_id, "(patient", patient$patientid, ")\n")
      missing_subjects[[length(missing_subjects) + 1]] <- list(
        patientid = patient$patientid,
        subject_accession = subject_acc
      )
    }
  }
  
  cat("\n[OK] Validation complete:", validated_count, "subjects found,", length(missing_subjects), "missing\n")
  
  if(length(missing_subjects) > 0) {
    return(list(
      success = FALSE, 
      error = paste(length(missing_subjects), "subjects not found in target database"),
      validated = validated_count,
      missing = length(missing_subjects),
      missing_list = missing_subjects
    ))
  }
  
  return(list(
    success = TRUE, 
    validated = validated_count,
    missing = 0
  ))
}

# =====================================================
# 1B. INSERT BIOSAMPLES (with hybrid check-create logic)
# =====================================================
# Checks if biosamples exist, creates if missing, reuses if exists
insert_biosamples <- function(conn, source_data, workspace_id, study_accession, 
                              timeperiod_mapping, config, mapping_override = NULL) {
  
  cat("[INFO] Processing biosamples\n")
  
  # Get unique samples with all biosample data
  unique_samples <- source_data %>%
    select(patientid, subject_accession, timeperiod, sampleid, 
           biosample_type, actual_visit_day) %>%
    filter(!is.na(patientid), !is.na(timeperiod)) %>%
    distinct(sampleid, patientid, timeperiod, .keep_all = TRUE)
  
  if(nrow(unique_samples) == 0) {
    cat("[WARN] No samples to process\n")
    return(list(success = FALSE, error = "No samples found", inserted = 0, existing = 0, failed = list(), biosample_map = list()))
  }
  
  # NOTE: No transaction management - uses parent migration transaction
  
  inserted_count <- 0
  existing_count <- 0
  failed_biosamples <- list()
  biosample_map <- list()  # Track which biosample each sample uses
  
  for(i in 1:nrow(unique_samples)) {
    row <- unique_samples[i, ]
    
    # Get planned_visit from timeperiod mapping
    planned_visit_acc <- timeperiod_mapping[[row$timeperiod]]
    if(is.null(planned_visit_acc) || planned_visit_acc == "NEW") {
      next  # Skip samples with unmapped timeperiods
    }
    
    # Resolve subject_accession: from JOIN (xmap_subjects) or from mapping_override
    subject_acc <- as.character(row$subject_accession)[[1]]
    if(is.null(subject_acc) || length(subject_acc) == 0 || is.na(subject_acc) || !nzchar(subject_acc)) {
      # Try mapping_override (Excel/CSV upload)
      if(!is.null(mapping_override)) {
        match_idx <- which(as.character(mapping_override$patientid) == as.character(row$patientid))
        if(length(match_idx) > 0) {
          subject_acc <- as.character(mapping_override$subject_accession[match_idx[1]])
        }
      }
    }
    
    if(is.null(subject_acc) || length(subject_acc) == 0 || is.na(subject_acc) || !nzchar(subject_acc)) {
      cat("  [SKIP] No subject mapping for patient", row$patientid, "\n")
      next
    }
    
    sp_name <- paste0("bio_sp_", i)
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))

      # Check if biosample already exists for this subject+planned_visit
      existing <- DBI::dbGetQuery(conn,
        "SELECT biosample_accession FROM madi_dat.biosample 
         WHERE subject_accession = $1 
           AND planned_visit_accession = $2 
           AND workspace_id = $3
         LIMIT 1",
        params = list(
          subject_acc,
          as.character(planned_visit_acc)[1],
          as.integer(workspace_id)[1]
        )
      )
      
      if(nrow(existing) > 0) {
        # Biosample exists - reuse it
        biosample_acc <- existing$biosample_accession[1]
        existing_count <- existing_count + 1
        
        if(existing_count <= 3) {
          cat("  [OK] Reusing biosample:", biosample_acc, "\n")
        } else if(existing_count == 4) {
          cat("  ... (showing first 3 reused)\n")
        }
      } else {
        # Biosample doesn't exist - create new
        subject_num <- gsub("SUB", "", subject_acc)
        timeperiod_short <- substr(row$timeperiod, 1, 3)
        
        # Safely constrain to 15 characters: "BS[sub][tpd]_[i]"
        bs_suffix <- paste0("_", i)
        bs_max_len <- 15 - nchar(bs_suffix)
        bs_base <- substr(sprintf("BS%s%s", subject_num, timeperiod_short), 1, bs_max_len)
        biosample_acc <- paste0(bs_base, bs_suffix)
        
        biosample_name <- sprintf("%s_%s", subject_acc, row$timeperiod)
        
        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.biosample (
             biosample_accession, name, planned_visit_accession, 
             study_accession, study_time_collected, 
             study_time_collected_unit, study_time_t0_event,
             subject_accession, type, workspace_id
           ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          params = list(
            biosample_acc,
            biosample_name,
            as.character(planned_visit_acc)[[1]],
            as.character(study_accession)[[1]],
            if(is.null(row$actual_visit_day) || length(row$actual_visit_day) == 0 || is.na(row$actual_visit_day[[1]])) NA_real_ else as.numeric(row$actual_visit_day[[1]]),
            as.character(config$time_unit %||% "Days")[[1]],
            as.character(config$t0_event %||% "Time of enrollment")[[1]],
            subject_acc,
            (function(bt) { res <- as.character(if(length(bt)==0 || is.na(bt[[1]])) "Whole blood" else bt[[1]]); if(tolower(res)=="blood") "Whole blood" else res })(row$biosample_type),
            as.integer(workspace_id)[[1]]
          )
        )
        
        inserted_count <- inserted_count + 1
        if(inserted_count <= 3) {
          cat("  [OK] Created biosample:", biosample_acc, "\n")
        } else if(inserted_count == 4) {
          cat("  ... (showing first 3 created)\n")
        }
      }
      
      # Track biosample for this sample — key is sampleid+patientid to avoid collision
      map_key <- paste0(row$sampleid, "_", row$patientid)
      biosample_map[[map_key]] <- biosample_acc

      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      
    }, error = function(e) {
      tryCatch(
        suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))),
        error = function(e2) {}
      )
      cat("  [ERROR] Failed biosample for sample", row$sampleid, ":", e$message, "\n")
      
      # Determine safe param 5 value for debugging (don't log it if it crashed during eval, but here we know it evaled)
      p5_eval <- if(is.null(row$actual_visit_day) || length(row$actual_visit_day) == 0 || is.na(row$actual_visit_day[[1]])) NA_real_ else as.numeric(row$actual_visit_day[[1]])
      
      cat("    [DEBUG] Param 1 length:", length(biosample_acc), "type:", typeof(biosample_acc), "\n")
      cat("    [DEBUG] Param 2 length:", length(biosample_name), "type:", typeof(biosample_name), "\n")
      cat("    [DEBUG] Param 3 length:", length(as.character(planned_visit_acc)[[1]]), "type:", typeof(as.character(planned_visit_acc)[[1]]), "\n")
      cat("    [DEBUG] Param 4 length:", length(as.character(study_accession)[[1]]), "type:", typeof(as.character(study_accession)[[1]]), "\n")
      cat("    [DEBUG] Param 5 length:", length(p5_eval), "type:", typeof(p5_eval), "value:", p5_eval, "\n")
      cat("    [DEBUG] Param 8 length:", length(subject_acc), "type:", typeof(subject_acc), "\n")
      failed_biosamples[[length(failed_biosamples) + 1]] <<- list(
        sampleid = row$sampleid,
        error = e$message
      )
    })
  }
  
  cat("[OK] Processed", inserted_count, "new biosamples,", existing_count, "reused\n")
  
  return(list(
    success = length(failed_biosamples) == 0,
    inserted = inserted_count,
    existing = existing_count,
    failed = failed_biosamples,
    biosample_map = biosample_map  # Return mapping for linking step
  ))
}

# =====================================================
# 1C. LINK EXPSAMPLE TO BIOSAMPLE (Internal helper)
# =====================================================
# Called from within insert_experiment_samples transaction
link_expsample_biosample_internal <- function(conn, biosample_map, sample_prefix = "ES_") {
  
  if(length(biosample_map) == 0) {
    return(list(success = TRUE, linked = 0, failed = list()))
  }
  
  linked_count <- 0
  failed_links <- list()
  
  # For each sample, link its expsample to biosample
  for(map_key in names(biosample_map)) {
    biosample_acc <- biosample_map[[map_key]]
    # map_key is "sampleid_patientid" — extract just the sampleid for the expsample accession
    sampleid <- strsplit(map_key, "_")[[1]][1]
    expsample_acc <- paste0(sample_prefix, sampleid)
    
    sp_name <- paste0("link_sp_", gsub("[^[:alnum:]]", "_", sampleid))
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))

      DBI::dbExecute(conn,
        "INSERT INTO madi_dat.expsample_2_biosample (
           expsample_accession, biosample_accession
         ) VALUES ($1, $2)
         ON CONFLICT DO NOTHING",
        params = list(expsample_acc, biosample_acc)
      )
      
      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      linked_count <- linked_count + 1
      if(linked_count <= 3) {
        cat("  [OK] Linked", expsample_acc, "→", biosample_acc, "\n")
      } else if(linked_count == 4) {
        cat("  ... (showing first 3 links)\n")
      }
      
    }, error = function(e) {
      tryCatch(
        suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))),
        error = function(e2) {}
      )
      cat("  [ERROR] Failed to link", expsample_acc, ":", e$message, "\n")
      failed_links[[length(failed_links) + 1]] <<- list(
        sampleid = sampleid,
        expsample_acc = expsample_acc,
        biosample_acc = biosample_acc,
        error = e$message
      )
    })
  }
  
  cat("[OK] Linked", linked_count, "expsamples to biosamples\n")
  
  return(list(
    success = length(failed_links) == 0,
    linked = linked_count,
    failed = failed_links
  ))
}

# Inserts subjects from xmap_subjects into madi_dat.subject
insert_subjects <- function(conn, source_data, workspace_id, subject_prefix = "SUB_", default_gender = "Not Specified", default_species = "Human", commit = FALSE) {
  
  # Get unique patients 
  # Note: source_data now includes columns from xmap_subjects via the join in ispi_migration.R
  # We select what we have, but rely on defaults for missing core fields
  unique_patients <- source_data %>%
    select(patientid, any_of(c("ethnicity", "race", "race_specify", "strain", "strain_characteristics", "subject_description"))) %>%
    filter(!is.na(patientid)) %>%
    distinct(patientid, .keep_all = TRUE)
  
  cat("[INFO] Found", nrow(unique_patients), "unique patients to migrate\n")
  cat("[INFO] Using defaults - Gender:", default_gender, "| Species:", default_species, "\n")
  
  if(nrow(unique_patients) == 0) {
    cat("[WARN] No patients to insert\n")
    return(list(success = TRUE, inserted = 0, existing = 0, failed = list()))
  }
  
  # NOTE: No transaction management - always called inside global execute_migration transaction
  
  tryCatch({
    inserted_count <- 0
    existing_count <- 0
    failed_items <- list()
    
    for(i in 1:nrow(unique_patients)) {
      patient <- unique_patients[i, , drop = FALSE]
      subject_acc <- paste0(subject_prefix, patient$patientid)
      
      # Create savepoint for this insert
      savepoint_name <- paste0("sp_subject_", gsub("[^0-9]", "", patient$patientid))
      
      tryCatch({
        # Set savepoint
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", savepoint_name)))
        
        # Check if subject already exists
        existing <- DBI::dbGetQuery(conn,
          "SELECT COUNT(*) as count FROM madi_dat.subject WHERE subject_accession = $1",
          params = list(subject_acc)
        )
        
        if(existing$count > 0) {
          cat("  [INFO] Subject", subject_acc, "already exists - skipping\n")
          existing_count <- existing_count + 1
          suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name)))
          next
        }
        
        # Insert new subject with defaults for gender/species
        # Extract all values as scalars to avoid vector length issues
        species_val <- as.character(if(is.null(default_species)) "Homo sapiens" else default_species)[1]
        
        # Helper to extract scalar from dataframe column - returns scalar string or NULL
        extract_scalar <- function(df, col_name) {
          if(!col_name %in% names(df)) return(NULL)
          val <- df[[col_name]][1]
          if(is.null(val)) return(NULL)
          if(length(val) == 0) return(NULL)
          if(is.na(val)) return(NULL)
          val_char <- as.character(val)
          if(length(val_char) == 0) return(NULL)
          if(nchar(val_char[1]) == 0) return(NULL)
          return(val_char[1])  # Force to single element
        }
        
        ethnicity_val <- extract_scalar(patient, "ethnicity")
        race_val <- extract_scalar(patient, "race")
        race_spec_val <- extract_scalar(patient, "race_specify")
        strain_val <- extract_scalar(patient, "strain")
        strain_char_val <- extract_scalar(patient, "strain_characteristics")
        desc_val <- extract_scalar(patient, "subject_description")
        
        # Debug output
        cat(sprintf("  DEBUG: Subject %s - ethnicity: %s (len=%d), race: %s (len=%d)\n",
                    subject_acc,
                    if(is.null(ethnicity_val)) "NULL" else ethnicity_val,
                    if(is.null(ethnicity_val)) 0 else length(ethnicity_val),
                    if(is.null(race_val)) "NULL" else race_val,
                    if(is.null(race_val)) 0 else length(race_val)))

        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.subject 
           (subject_accession, workspace_id, gender, species, ethnicity, race, 
            race_specify, strain, strain_characteristics, description) 
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          params = list(
            subject_acc, 
            as.integer(workspace_id)[1],
            as.character(default_gender)[1],
            species_val,
            if(is.null(ethnicity_val)) NA_character_ else ethnicity_val,
            if(is.null(race_val)) NA_character_ else race_val,
            if(is.null(race_spec_val)) NA_character_ else race_spec_val,
            if(is.null(strain_val)) NA_character_ else strain_val,
            if(is.null(strain_char_val)) NA_character_ else strain_char_val,
            if(is.null(desc_val)) NA_character_ else desc_val
          )
        )
        
        cat("  [OK] Inserted subject:", subject_acc, "\n")
        inserted_count <- inserted_count + 1
        
        # Release savepoint on success
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name)))
        
      }, error = function(e) {
        # Rollback to savepoint on error
        tryCatch({
          suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", savepoint_name)))
        }, error = function(rollback_err) {
          # Ignore rollback errors
        })
        
        cat("  [ERROR] Failed to insert subject", subject_acc, ":", e$message, "\n")
        failed_items[[length(failed_items) + 1]] <<- list(
          type = "subject",
          identifier = patient$patientid,
          accession = subject_acc,
          error = e$message
        )
      })
    }
    
    cat("[OK] Subjects processed (in global transaction):", inserted_count, "inserted,", existing_count, "existing,", length(failed_items), "failed\n")
    
    return(list(
      success = TRUE, 
      inserted = inserted_count, 
      existing = existing_count,
      failed = failed_items
    ))
    
  }, error = function(e) {
    cat("[ERROR] ERROR in insert_subjects:", e$message, "\n")
    return(list(success = FALSE, error = e$message, failed = list()))
  })
}

# =====================================================
# 2. CREATE PLANNED VISITS
# =====================================================
# Creates new planned visits for unmapped timeperiods
create_planned_visits <- function(conn, timeperiod_values, workspace_id, study_accession, commit = FALSE, manage_transaction = TRUE) {
  
  cat("[INFO] Creating", length(timeperiod_values), "new planned visits\n")
  
  if(length(timeperiod_values) == 0) {
    return(list(success = TRUE, created = 0, mapping = list()))
  }
  
  # Start transaction
  if(manage_transaction && !commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("[INFO] Transaction started (TEST MODE - will rollback)\n")
  }
  
  tryCatch({
    created_count <- 0
    mapping <- list()
    
    # Get current max order number for this study
    max_order <- DBI::dbGetQuery(conn,
      "SELECT COALESCE(MAX(order_number), 0) as max_order 
       FROM madi_dat.planned_visit 
       WHERE study_accession = $1",
      params = list(study_accession)
    )
    
    next_order <- max_order$max_order + 1
    
    for(tp_value in timeperiod_values) {
      # Generate next accession
      pv_accession <- generate_next_accession(conn, "planned_visit", "planned_visit_accession", "PV")
      
      # Insert new planned visit
      DBI::dbExecute(conn,
        "INSERT INTO madi_dat.planned_visit 
         (planned_visit_accession, name, order_number, study_accession, workspace_id) 
         VALUES ($1, $2, $3, $4, $5)",
        params = list(pv_accession, tp_value, next_order, study_accession, workspace_id)
      )
      
      cat("  [OK] Created planned visit:", pv_accession, "- Name:", tp_value, "Order:", next_order, "\n")
      
      # Store mapping
      mapping[[tp_value]] <- pv_accession
      created_count <- created_count + 1
      next_order <- next_order + 1
    }
    
    if(manage_transaction) {
      if(commit) {
        DBI::dbExecute(conn, "COMMIT")
        cat("[OK] COMMITTED: Created", created_count, "planned visits\n")
      } else {
        DBI::dbExecute(conn, "ROLLBACK")
        cat("[ROLLBACK] ROLLED BACK: Would have created", created_count, "planned visits\n")
      }
    } else {
      cat("[OK] Created", created_count, "planned visits (in transaction)\n")
    }
    
    return(list(success = TRUE, created = created_count, mapping = mapping))
    
  }, error = function(e) {
    if(manage_transaction) {
      DBI::dbExecute(conn, "ROLLBACK")
      cat("[ROLLBACK] Transaction rolled back\n")
    }
    cat("[ERROR] ERROR:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =====================================================
# 3. CREATE ARMS/COHORTS
# =====================================================
# Creates new arms/cohorts for unmapped agroups
create_arms <- function(conn, agroup_values, workspace_id, study_accession, arm_type = "Observational", commit = FALSE, manage_transaction = TRUE) {
  
  cat("[INFO] Creating", length(agroup_values), "new arms/cohorts\n")
  
  if(length(agroup_values) == 0) {
    return(list(success = TRUE, created = 0, mapping = list()))
  }
  
  # Start transaction
  if(manage_transaction && !commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("[INFO] Transaction started (TEST MODE - will rollback)\n")
  }
  
  tryCatch({
    created_count <- 0
    mapping <- list()
    
    for(ag_value in agroup_values) {
      # Generate next accession
      arm_accession <- generate_next_accession(conn, "arm_or_cohort", "arm_accession", "ARM")
      
      # Insert new arm/cohort
      DBI::dbExecute(conn,
        "INSERT INTO madi_dat.arm_or_cohort 
         (arm_accession, name, type_reported, study_accession, workspace_id, description) 
         VALUES ($1, $2, $3, $4, $5, $6)",
        params = list(
          arm_accession, 
          ag_value, 
          arm_type, 
          study_accession, 
          workspace_id,
          paste0("Migrated from I-SPI agroup: ", ag_value)
        )
      )
      
      cat("  [OK] Created arm/cohort:", arm_accession, "- Name:", ag_value, "Type:", arm_type, "\n")
      
      # Store mapping
      mapping[[ag_value]] <- arm_accession
      created_count <- created_count + 1
    }
    
    if(manage_transaction) {
      if(commit) {
        DBI::dbExecute(conn, "COMMIT")
        cat("[OK] COMMITTED: Created", created_count, "arms/cohorts\n")
      } else {
        DBI::dbExecute(conn, "ROLLBACK")
        cat("[ROLLBACK] ROLLED BACK: Would have created", created_count, "arms/cohorts\n")
      }
    } else {
      cat("[OK] Created", created_count, "arms/cohorts (in transaction)\n")
    }
    
    return(list(success = TRUE, created = created_count, mapping = mapping))
    
  }, error = function(e) {
    if(manage_transaction) {
      DBI::dbExecute(conn, "ROLLBACK")
      cat("[ROLLBACK] Transaction rolled back\n")
    }
    cat("[ERROR] ERROR:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =====================================================
# 4. INSERT ARM_2_SUBJECT ASSOCIATIONS
# =====================================================
# Links subjects to their arms based on arm_accession from xmap_subjects
insert_arm_subject_associations <- function(conn, source_data, arm_mapping, workspace_id, study_accession, commit = FALSE, manage_transaction = TRUE) {
  
  cat("[INFO] Creating arm-subject associations\n")
  
  # Start transaction
  if(manage_transaction && !commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("[INFO] Transaction started (TEST MODE - will rollback)\n")
  }
  
  tryCatch({
    inserted_count <- 0
    existing_count <- 0
    failed_items <- list()
    
    # Get unique patient combos; bring agroup so we can fall back to arm_mapping
    avail_cols <- intersect(c("patientid", "subject_accession", "arm_accession", "agroup"), names(source_data))
    unique_combos <- unique(source_data[, avail_cols, drop = FALSE])
    unique_combos <- unique_combos[!is.na(unique_combos$patientid), ]
    
    for(i in 1:nrow(unique_combos)) {
      subject_acc <- unique_combos$subject_accession[i]
      arm_acc     <- unique_combos$arm_accession[i]
      patient_id  <- unique_combos$patientid[i]
      
      # Validate subject_accession
      if(is.null(subject_acc) || is.na(subject_acc) || nchar(as.character(subject_acc)) == 0) {
        cat("  [WARN] Patient", patient_id, "has no subject_accession - skipping\n")
        failed_items[[length(failed_items) + 1]] <- list(
          type = "arm_2_subject",
          patient_id = patient_id,
          error = "No subject_accession in source data"
        )
        next
      }
      
      # If arm_accession is missing from xmap_subjects JOIN, fall back to agroup_mapping
      if(is.null(arm_acc) || is.na(arm_acc) || nchar(as.character(arm_acc)) == 0) {
        agroup_val <- if("agroup" %in% names(unique_combos)) as.character(unique_combos$agroup[i]) else NULL
        if(!is.null(agroup_val) && !is.na(agroup_val) && nchar(agroup_val) > 0) {
          arm_acc <- arm_mapping[[agroup_val]]
        }
      }
      
      if(is.null(arm_acc) || is.na(arm_acc) || nchar(as.character(arm_acc)) == 0) {
        cat("  [WARN] Patient", patient_id, "has no arm_accession (checked xmap_subjects + agroup_mapping) - skipping\n")
        failed_items[[length(failed_items) + 1]] <- list(
          type = "arm_2_subject",
          patient_id = patient_id,
          subject_accession = as.character(subject_acc),
          error = "No arm_accession from xmap_subjects or agroup_mapping"
        )
        next
      }
      
      # Check if association already exists
      existing <- DBI::dbGetQuery(conn,
        "SELECT COUNT(*) as count FROM madi_dat.arm_2_subject 
         WHERE arm_accession = $1 AND subject_accession = $2",
        params = list(arm_acc, subject_acc)
      )
      
      if(existing$count > 0) {
        existing_count <- existing_count + 1
        next
      }
      
      # Insert association
      DBI::dbExecute(conn,
        "INSERT INTO madi_dat.arm_2_subject 
         (arm_accession, subject_accession, workspace_id, study_accession) 
         VALUES ($1, $2, $3, $4)",
        params = list(arm_acc, subject_acc, workspace_id, study_accession)
      )
      
      cat("  [OK] Linked:", subject_acc, "→", arm_acc, "\n")
      inserted_count <- inserted_count + 1
    }
    
    pasted_message_created <- paste("[OK] COMMITTED: Created", inserted_count, "associations,", existing_count, "already existed,", length(failed_items), "failed\n")
    pasted_message_rolled <- paste("[ROLLBACK] ROLLED BACK: Would have created", inserted_count, "associations,", length(failed_items), "would fail\n")
    pasted_message_trans <- paste("[OK] Created", inserted_count, "associations (in transaction),", existing_count, "already existed\n")

    if(manage_transaction) {
      if(commit) {
        DBI::dbExecute(conn, "COMMIT")
        cat(pasted_message_created)
      } else {
        DBI::dbExecute(conn, "ROLLBACK")
        cat(pasted_message_rolled)
      }
    } else {
        cat(pasted_message_trans)
    }
    
    return(list(success = TRUE, inserted = inserted_count, existing = existing_count, failed = failed_items))
    
  }, error = function(e) {
    if(manage_transaction) DBI::dbExecute(conn, "ROLLBACK")
    cat("[ERROR] ERROR:", e$message, "\n")
    if(manage_transaction) cat("[ROLLBACK] Transaction rolled back\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =====================================================
# 5. INSERT EXPERIMENT SAMPLES
# =====================================================
# Creates expsample records linking samples to subjects and visits
insert_experiment_samples <- function(conn, source_data, timeperiod_mapping, workspace_id, study_accession, experiment_accession, sample_prefix = "ES_", result_schema = "MBAA", biosample_map = NULL, commit = FALSE) {
  
  cat("[INFO] Creating experiment samples\n")
  
  # NOTE: No transaction management - uses parent migration transaction
  
  tryCatch({
    inserted_count <- 0
    existing_count <- 0
    failed_items <- list()
    skipped_unmapped <- list()
    sample_mapping <- list() # Store mapping for Result Loading
    
    # Process each row
    for(i in 1:nrow(source_data)) {
      row <- source_data[i, ]
      
      # Create savepoint for this insert
      savepoint_name <- paste0("sp_sample_", gsub("[^0-9]", "", row$sampleid))
      
      tryCatch({
        # Set savepoint
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", savepoint_name)))
        
        expsample_acc <- paste0(sample_prefix, row$sampleid)
        pv_acc <- timeperiod_mapping[[as.character(row$timeperiod)]]
        
        if(is.null(pv_acc)) {
          cat("  [WARN] No planned visit mapping for timeperiod:", row$timeperiod, "- skipping sample", row$sampleid, "\n")
          skipped_unmapped[[length(skipped_unmapped) + 1]] <<- list(
            type = "unmapped_timeperiod",
            sample_id = row$sampleid,
            timeperiod = row$timeperiod,
            reason = "No mapping provided"
          )
          suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name)))
          next
        }
        
        # Determine biosample accession
        # If biosample_map is provided (from Step 2B), use composite sampleid_patientid key
        biosample_acc <- NULL
        if(!is.null(biosample_map)) {
          composite_key <- paste0(row$sampleid, "_", row$patientid)
          if(composite_key %in% names(biosample_map)) {
            biosample_acc <- biosample_map[[composite_key]]
          }
        }

        # Check if expsample already exists
        existing <- DBI::dbGetQuery(conn,
          "SELECT COUNT(*) as count FROM madi_dat.expsample 
           WHERE expsample_accession = $1",
          params = list(expsample_acc)
        )
        
        if(existing$count > 0) {
          existing_count <- existing_count + 1
          sample_mapping[[ as.character(i) ]] <- list(
             expsample_accession = expsample_acc,
             biosample_accession = biosample_acc,
             patientid = as.character(row$patientid)[[1]],
             plate = if("plate" %in% names(row)) as.character(row$plate)[[1]] else NA,
             well = if("well" %in% names(row)) as.character(row$well)[[1]] else NA,
             timeperiod = if("timeperiod" %in% names(row)) as.character(row$timeperiod)[[1]] else NA,
             subject_accession = if("subject_accession" %in% names(row) && !is.null(row$subject_accession) && !is.na(row$subject_accession)) as.character(row$subject_accession) else NULL,
             arm_accession = if("arm_accession" %in% names(row) && !is.null(row$arm_accession) && !is.na(row$arm_accession)) as.character(row$arm_accession) else NULL
          )
          suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name)))
          next
        }
        
        # Insert experiment sample
        # Note: expsample table does NOT have planned_visit_accession
        # That relationship is through biosample (which we may need to create separately)
        # result_schema comes from user selection in UI
        
        # Debug output
        cat(sprintf("  DEBUG: Inserting expsample %s - workspace_id: %s (len=%d), result_schema: %s, exp_acc: %s\n",
                    expsample_acc,
                    if(is.null(workspace_id)) "NULL" else workspace_id,
                    if(is.null(workspace_id)) 0 else length(workspace_id),
                    result_schema,
                    experiment_accession))
        
        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.expsample 
           (expsample_accession, experiment_accession, name, result_schema, workspace_id) 
           VALUES ($1, $2, $3, $4, $5)",
          params = list(
            expsample_acc, 
            experiment_accession, 
            as.character(row$sampleid)[[1]], 
            result_schema,  # Use parameter from function
            as.integer(workspace_id)[[1]]
          )
        )
        
        if(inserted_count < 5) {  # Show first 5
          cat("  [OK] Created sample:", expsample_acc, "→ Visit:", pv_acc, "\n")
        } else if(inserted_count == 5) {
          cat("  ... (showing first 5)\n")
        }
        
        inserted_count <- inserted_count + 1
        
        # Add to mapping for Result Loading
        sample_mapping[[ as.character(i) ]] <- list(
             expsample_accession = expsample_acc,
             biosample_accession = biosample_acc,
             patientid = as.character(row$patientid)[[1]],
             plate = if("plate" %in% names(row)) as.character(row$plate)[[1]] else NA,
             well = if("well" %in% names(row)) as.character(row$well)[[1]] else NA,
             timeperiod = if("timeperiod" %in% names(row)) as.character(row$timeperiod)[[1]] else NA,
             subject_accession = if("subject_accession" %in% names(row) && !is.null(row$subject_accession) && !is.na(row$subject_accession)) as.character(row$subject_accession) else NULL,
             arm_accession = if("arm_accession" %in% names(row) && !is.null(row$arm_accession) && !is.na(row$arm_accession)) as.character(row$arm_accession) else NULL
        )
        
        # Release savepoint on success
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", savepoint_name)))
        
      }, error = function(e) {
        # Rollback to savepoint on error
        tryCatch({
          suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", savepoint_name)))
        }, error = function(rollback_err) {
          # Ignore rollback errors
        })
        
        cat("  [ERROR] Failed to insert sample", row$sampleid, ":", e$message, "\n")
        failed_items[[length(failed_items) + 1]] <<- list(
          type = "expsample",
          sample_id = row$sampleid,
          patient_id = row$patientid,
          timeperiod = row$timeperiod,
          error = e$message
        )
      })
    }
    
    cat("[OK] Expsamples processed (in global transaction):", inserted_count, "inserted,", existing_count, "existing,",
        length(failed_items), "failed,", length(skipped_unmapped), "skipped (unmapped)\n")
    
    # BEFORE rollback/commit: do biosample linking (if biosample_map provided)
    biosample_link_result <- list(success = TRUE, linked = 0, failed = list())
    if(!is.null(biosample_map) && length(biosample_map) > 0) {
      cat("\n[INFO] Linking expsamples to biosamples (within same transaction)...\n")
      biosample_link_result <- link_expsample_biosample_internal(conn, biosample_map, sample_prefix)
    }
    
    # NOTE: Parent migration handles rollback/commit
   cat("[OK] Expsample creation complete\n")
    
    return(list(
      success = TRUE, 
      inserted = inserted_count, 
      existing = existing_count,
      failed = failed_items,
      skipped = skipped_unmapped,
      biosample_link_result = biosample_link_result,
      sample_mapping = sample_mapping
    ))
    
  }, error = function(e) {
    cat("[ERROR] ERROR:", e$message, "\n")
    return(list(success = FALSE, error = e$message, failed = list(), skipped = list(), biosample_link_result = list(success = FALSE, linked = 0, failed = list())))
  })
}

# =====================================================
# RE-RUN SAFETY: Check existing data for an experiment
# =====================================================
# Returns counts of already-migrated data so the UI can warn users before re-running.
# rerun_mode in config controls what execute_migration does when data already exists:
#   "first_run"    (default) — normal insert, no checks; will duplicate non-keyed tables
#   "skip_existing"          — skip insert steps that already have data for this experiment
#   "clean_slate"            — delete existing result data first, then insert fresh
check_existing_migration_data <- function(conn, experiment_accession) {
  safe_count <- function(q) {
    tryCatch(as.integer(DBI::dbGetQuery(conn, q)[[1]]), error = function(e) NA_integer_)
  }
  exp <- experiment_accession
  list(
    expsamples        = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.expsample WHERE experiment_accession='", exp, "'")),
    mbaa_expsample    = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", exp, "' AND source_type='EXPSAMPLE'")),
    mbaa_control      = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", exp, "' AND source_type='CONTROL SAMPLE'")),
    control_samples   = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.control_sample WHERE experiment_accession='", exp, "'")),
    standard_curves   = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "'")),
    model_qc          = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.model_qc_data WHERE standard_curve_accession IN (SELECT standard_curve_accession FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "')")),
    sample_qc         = safe_count(paste0("SELECT COUNT(*) FROM madi_dat.sample_qc_data WHERE expsample_accession IN (SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession='", exp, "')"))
  )
}

# Deletes all non-keyed result data for an experiment so a clean re-run can proceed.
# Does NOT delete expsamples, biosamples, subjects, arms — only result-level data.
# Called inside the global migration transaction so it rolls back if migration fails.
clear_experiment_results <- function(conn, experiment_accession) {
  exp <- experiment_accession
  cat("[INFO] CLEAN SLATE: Clearing existing result data for", exp, "\n")

  # model_qc_data links via standard_curve, must go first
  n_mqc <- DBI::dbGetQuery(conn, paste0(
    "SELECT COUNT(*) FROM madi_dat.model_qc_data WHERE standard_curve_accession IN ",
    "(SELECT standard_curve_accession FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "')"
  ))[[1]]
  DBI::dbExecute(conn, paste0(
    "DELETE FROM madi_dat.model_qc_data WHERE standard_curve_accession IN ",
    "(SELECT standard_curve_accession FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "')"
  ))
  cat("  [OK] Deleted", n_mqc, "model_qc_data rows\n")

  n_sc <- DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "'"))[[1]]
  DBI::dbExecute(conn, paste0("DELETE FROM madi_dat.standard_curve WHERE experiment_accession='", exp, "'"))
  cat("  [OK] Deleted", n_sc, "standard_curve rows\n")

  n_sqc <- DBI::dbGetQuery(conn, paste0(
    "SELECT COUNT(*) FROM madi_dat.sample_qc_data WHERE expsample_accession IN ",
    "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession='", exp, "')"
  ))[[1]]
  DBI::dbExecute(conn, paste0(
    "DELETE FROM madi_dat.sample_qc_data WHERE expsample_accession IN ",
    "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession='", exp, "')"
  ))
  cat("  [OK] Deleted", n_sqc, "sample_qc_data rows\n")

  n_mbaa <- DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", exp, "'"))[[1]]
  DBI::dbExecute(conn, paste0("DELETE FROM madi_dat.mbaa_result WHERE experiment_accession='", exp, "'"))
  cat("  [OK] Deleted", n_mbaa, "mbaa_result rows\n")

  n_detail <- DBI::dbGetQuery(conn, paste0(
    "SELECT COUNT(*) FROM madi_dat.expsample_mbaa_detail WHERE assay_group_id='", exp, "'"
  ))[[1]]
  DBI::dbExecute(conn, paste0(
    "DELETE FROM madi_dat.expsample_mbaa_detail WHERE assay_group_id='", exp, "'"
  ))
  cat("  [OK] Deleted", n_detail, "expsample_mbaa_detail rows\n")

  cat("[OK] Clean slate complete for", exp, "\n")
}

# =====================================================
# MASTER FUNCTION: Execute Full Migration
# =====================================================
# Orchestrates all migration steps in correct order
# config$rerun_mode: "first_run" (default) | "skip_existing" | "clean_slate"
execute_migration <- function(conn, source_data, config, commit = FALSE, source_conn = NULL, progress_cb = NULL) {
  
  cat("═══════════════════════════════════════════════════\n")
  cat("[INFO] STARTING I-SPI MIGRATION\n")
  cat("Mode:", ifelse(commit, "COMMIT (LIVE)", "TEST (ROLLBACK)"), "\n")
  cat("═══════════════════════════════════════════════════\n\n")
  
  results <- list()
  
  # Helper: call progress callback safely
  pcb <- function(message, detail = "") {
    if(!is.null(progress_cb)) tryCatch(progress_cb(message, detail), error = function(e) {})
  }

  # Resolve rerun mode (default: first_run)
  rerun_mode <- config$rerun_mode %||% "first_run"
  skip_existing <- (rerun_mode == "skip_existing")
  cat("[INFO] Re-run mode:", rerun_mode, "\n")

  # Start Global Transaction for ALL modes
  # Test mode: ROLLBACK at end. Live mode: COMMIT at end.
  cat("\n[INFO] GLOBAL MIGRATION TRANSACTION STARTED\n")
  pcb("Starting migration...", "Opening database transaction")
  DBI::dbExecute(conn, "BEGIN")

  tryCatch({

  # If clean_slate: delete all existing result data inside transaction (rolls back if migration fails)
  if(rerun_mode == "clean_slate") {
    pcb("Clearing existing data...", paste("Deleting previous results for", config$experiment_accession))
    clear_experiment_results(conn, config$experiment_accession)
  }

  # Step 0: Validate or Create Experiment
  cat("\n--- STEP 0: VALIDATE/CREATE EXPERIMENT ---\n")
  pcb("Step 1/9: Validating Experiment", paste("Checking", config$experiment_accession))
  results$experiment <- validate_or_create_experiment(
    conn, config$experiment_accession, config$study_accession,
    config$workspace_id, config$create_new_experiment %||% FALSE, 
    config$new_experiment_name, config$new_experiment_description, config$new_experiment_tech,
    commit, manage_transaction = FALSE
  )
  
  if(!results$experiment$success) {
    cat("\n[ERROR] Migration failed at experiment validation\n")
    return(results)
  }
  
  # Step 1: Validate Subjects Exist in Target Database
  cat("\n--- STEP 1: VALIDATE SUBJECTS EXIST ---\n")
  pcb("Step 2/9: Validating Subjects", paste("Checking", length(unique(source_data$patientid)), "patients"))
  results$subjects <- validate_existing_subjects(
    conn, source_data, 
    config$workspace_id,
    commit,
    mapping_override = config$mapping_override
  )
  
  if(!results$subjects$success) {
    cat("\n[ERROR] Migration failed at subject validation\n")
    return(results)
  }
  
  # Step 2: Create Planned Visits (if needed)
  cat("\n--- STEP 2: CREATE PLANNED VISITS ---\n")
  pcb("Step 3/9: Planned Visits", "Creating any new planned visits")
  new_pv_values <- names(config$timeperiod_mapping)[config$timeperiod_mapping == "NEW"]
  if(length(new_pv_values) > 0) {
    results$planned_visits <- create_planned_visits(
      conn, new_pv_values, 
      config$workspace_id, config$study_accession, 
      commit, manage_transaction = FALSE
    )
    # Merge new mappings with existing
    if(results$planned_visits$success) {
      config$timeperiod_mapping <- c(
        config$timeperiod_mapping[config$timeperiod_mapping != "NEW"],
        results$planned_visits$mapping
      )
    }
  }
  
  #==========================================================================
  # STEPS 2B-5: Single Transaction (Biosamples + Expsamples + Links)
  #==========================================================================
  cat("\n--- STEPS 2B-5: CREATE BIOSAMPLES, EXPSAMPLES & LINKS (single transaction) ---\n")
  
  # Start one big transaction for all three related operations
  cat("[INFO] (Using Global Transaction for Steps 2B-5)\n")
  
  # Step 2B: Insert Biosamples
  cat("\nStep 2B: Processing biosamples...\n")
  pcb("Step 4/9: Biosamples", paste("Processing", nrow(source_data), "source rows"))
  results$biosamples <- insert_biosamples(
    conn, source_data,
    config$workspace_id, config$study_accession,
    config$timeperiod_mapping, config,
    mapping_override = config$mapping_override
  )
  
  if(!results$biosamples$success) {
    if(!commit) DBI::dbExecute(conn, "ROLLBACK")
    cat("\n[ERROR] Migration failed at biosample creation\n")
    return(results)
  }
  
  # Step 5: Insert Experiment Samples (and link to biosamples)
  cat("\nStep 5: Creating experiment samples & linking to biosamples...\n")
  pcb("Step 5/9: Experiment Samples", paste("Inserting", nrow(source_data), "samples"))
  results$expsamples <- insert_experiment_samples(
    conn, source_data, config$timeperiod_mapping,
    config$workspace_id, config$study_accession, config$experiment_accession,
    config$sample_prefix, config$result_schema_type, 
    results$biosamples$biosample_map,  # Pass biosample map for linking
    commit = FALSE  # Don't let it manage transaction
  )
  
  # Extract biosample link results from expsample results
  results$biosample_links <- results$expsamples$biosample_link_result
  
  # Steps 2B-5 complete (managed by global transaction)
  cat("[OK] Step 2B-5 Complete\n")
  #==========================================================================
  
  # Step 6: Insert MBAA Results (if applicable)
  # This uses the mapping from Step 5 to link results to created experiment samples
  cat("\n--- STEP 6: INSERT RESULTS ---\n")
  pcb("Step 6/9: MBAA Results", paste("Inserting assay results (this is the longest step)"))
  if(!is.null(config$result_schema_type) && config$result_schema_type == "MBAA") {
    results$mbaa_results <- insert_mbaa_results(
      conn, source_data, results$expsamples$sample_mapping,
      config$experiment_accession, config$study_accession,
      config$workspace_id, commit, manage_transaction = FALSE,
      skip_if_exists = skip_existing
    )
    # Capture preview from Step 6 (will be overwritten by Step 7-9 if luminex also runs)
    if(!is.null(results$mbaa_results$preview) && length(results$mbaa_results$preview) > 0) {
      results$preview_data <- results$mbaa_results$preview
    }
  } else {
    cat("Result schema is", config$result_schema, "- skipping MBAA result insertion\n")
  }
  
  # Step 3: Create Arms (if needed)
  cat("\n--- STEP 3: CREATE ARMS/COHORTS ---\n")
  pcb("Step 7/9: Arms & Cohorts", "Creating arm/cohort associations")
  new_arm_values <- names(config$agroup_mapping)[config$agroup_mapping == "NEW"]
  if(length(new_arm_values) > 0) {
    results$arms <- create_arms(
      conn, new_arm_values, 
      config$workspace_id, config$study_accession,
      config$default_arm_type, commit, manage_transaction = FALSE
    )
    # Merge new mappings with existing
    if(results$arms$success) {
      config$agroup_mapping <- c(
        config$agroup_mapping[config$agroup_mapping != "NEW"],
        results$arms$mapping
      )
    }
  } else {
    cat("No new arms/cohorts to create\n")
    results$arms <- list(success = TRUE, created = 0)
  }
  
  # Step 4: Insert Arm-Subject Associations
  cat("\n--- STEP 4: INSERT ARM-SUBJECT ASSOCIATIONS ---\n")
  pcb("Step 8/9: Arm—Subject Links", "Linking subjects to their arms")
  results$arm_associations <- insert_arm_subject_associations(
    conn, source_data, config$agroup_mapping,
    config$workspace_id, config$study_accession,
    commit, manage_transaction = FALSE
  )

  # Backfill arm_accession on any mbaa_result EXPSAMPLE rows that are still NULL.
  # agroup_mapping may miss some rows if agroup values differ across source experiments,
  # so we do a definitive UPDATE here using arm_2_subject (just populated above).
  if(!is.null(config$experiment_accession) && !is.null(config$study_accession)) {
    tryCatch({
      backfill_n <- DBI::dbExecute(conn, paste0(
        "UPDATE madi_dat.mbaa_result r ",
        "SET arm_accession = a2s.arm_accession ",
        "FROM madi_dat.arm_2_subject a2s ",
        "JOIN madi_dat.arm_or_cohort aoc ON aoc.arm_accession = a2s.arm_accession ",
        "  AND aoc.study_accession = '", config$study_accession, "' ",
        "WHERE r.subject_accession = a2s.subject_accession ",
        "  AND r.experiment_accession = '", config$experiment_accession, "' ",
        "  AND r.source_type = 'EXPSAMPLE' ",
        "  AND r.arm_accession IS NULL"
      ))
      if(backfill_n > 0) cat(paste0("[INFO] Backfilled arm_accession on ", backfill_n, " mbaa_result rows via arm_2_subject\n"))
    }, error = function(e) {
      cat(paste0("[WARN] arm_accession backfill failed: ", e$message, "\n"))
    })
  }

  # ==========================================================================
  # Step 7-9: Controls and Standards (MBAA Only)
  # ==========================================================================
  if(!is.null(config$result_schema_type) && config$result_schema_type == "MBAA") {
      cat("\n--- STEPS 7-9: CONTROLS & STANDARDS ---\n")
      
      # Determine which connection to use for SOURCE data fetching
      fetch_conn <- if(!is.null(source_conn) && DBI::dbIsValid(source_conn)) source_conn else conn
      if(identical(fetch_conn, conn)) {
        cat("[WARN] No separate source connection provided - using target DB for source queries\n")
      } else {
        cat("[OK] Using separate source DB connection for xmap_* queries\n")
      }
      
      # 1. Fetch Data from SOURCE DB
      source_schema <- config$source_schema %||% "madi_results"
      
      # Use source_study logic:
      # If source_study is provided in config, use it for FETCHING.
      # Otherwise default to target study_accession.
      study_acc_target <- config$study_accession
      study_acc_source <- if(!is.null(config$source_study) && config$source_study != "") config$source_study else study_acc_target
      
      cat(paste0("[INFO] Fetching auxiliary data from ", source_schema, " for Study '", study_acc_source, "'...\n"))
      
      # xmap_control (FETCH from source)
      ctrl_q <- paste0("SELECT * FROM ", source_schema, ".xmap_control WHERE study_accession = '", study_acc_source, "'")
      ctrl_data <- tryCatch(DBI::dbGetQuery(fetch_conn, ctrl_q), error=function(e){ cat("  Warning: xmap_control fetch failed:", e$message, "\n"); data.frame() })
      
      # xmap_buffer (FETCH from source)
      buff_q <- paste0("SELECT * FROM ", source_schema, ".xmap_buffer WHERE study_accession = '", study_acc_source, "'")
      buff_data <- tryCatch(DBI::dbGetQuery(fetch_conn, buff_q), error=function(e){ cat("  Warning: xmap_buffer fetch failed:", e$message, "\n"); data.frame() })

      # xmap_standard (FETCH from source)
      std_q <- paste0("SELECT * FROM ", source_schema, ".xmap_standard WHERE study_accession = '", study_acc_source, "'")
      std_data <- tryCatch(DBI::dbGetQuery(fetch_conn, std_q), error=function(e){ cat("  Warning: xmap_standard fetch failed:", e$message, "\n"); data.frame() })


      
      # best_glance_all (FETCH from source) for Standard Curves
      # Source Experiment Name comes from source_data (e.g. IgG1) NOT config (EXPxxxx)
      source_exp_name_detected <- if(!is.null(source_data$experiment_accession)) unique(source_data$experiment_accession)[[1]] else config$experiment_accession
      
      sc_q <- paste0("SELECT * FROM ", source_schema, ".best_glance_all WHERE study_accession = '", study_acc_source, "' AND experiment_accession = '", source_exp_name_detected, "'")
      
      cat(paste0("  [INFO] Fetching Standard Curves Query: ", sc_q, "\n"))
      cat(paste0("  [INFO] Detected Source Exp Name: '", source_exp_name_detected, "' (from source data)\n"))
      cat(paste0("  [INFO] Config Exp Acc (Target): '", config$experiment_accession, "'\n"))
      
      # Debug: Check available experiments in source for this study
      try({
        avail_exps <- DBI::dbGetQuery(fetch_conn, paste0("SELECT DISTINCT experiment_accession FROM ", source_schema, ".best_glance_all WHERE study_accession = '", study_acc_source, "'"))
        cat(paste0("  [INFO] Available Experiments in best_glance_all: ", paste(avail_exps$experiment_accession, collapse=", "), "\n"))
      }, silent=TRUE)

      sc_data <- tryCatch(DBI::dbGetQuery(fetch_conn, sc_q), error=function(e){ cat("  Warning: best_glance_all fetch failed:", e$message, "\n"); data.frame() })
      
      if(nrow(sc_data) == 0) {
        cat("  [WARN] WARNING: No standard curves found for source experiment '", source_exp_name_detected, "'!\n")
      }
      
      # Fetch QC data from source using stored functions (if project_id is provided)
      sample_qc_data <- data.frame()
      model_qc_data <- data.frame()
      ispi_project_id <- config$ispi_project_id
      
      if(!is.null(ispi_project_id) && !is.na(ispi_project_id)) {
        cat(paste0("  [INFO] Fetching QC data from source (project_id: ", ispi_project_id, ")...\n"))
        
        # Fetch sample QC data
        sample_qc_data <- tryCatch({
          DBI::dbGetQuery(fetch_conn, paste0(
            "SELECT * FROM madi_results.get_sample_qc_data('", 
            study_acc_source, "', '", source_exp_name_detected, "', ", ispi_project_id, ")"
          ))
        }, error = function(e) { 
          cat("  [WARN] get_sample_qc_data fetch failed:", e$message, "\n")
          data.frame() 
        })
        
        # Fetch model QC data
        model_qc_data <- tryCatch({
          DBI::dbGetQuery(fetch_conn, paste0(
            "SELECT * FROM madi_results.get_model_qc_data('", 
            study_acc_source, "', '", source_exp_name_detected, "', ", ispi_project_id, ")"
          ))
        }, error = function(e) { 
          cat("  [WARN] get_model_qc_data fetch failed:", e$message, "\n")
          data.frame() 
        })
        
        cat(paste0("  [INFO] Fetched QC data: ", nrow(sample_qc_data), " sample QC rows, ", 
                   nrow(model_qc_data), " model QC rows\n"))
      } else {
        cat("  [INFO] No I-SPI project ID provided - skipping QC data fetch\n")
      }
      
      cat(paste0("  [INFO] Fetched: ", nrow(ctrl_data), " controls, ", nrow(buff_data), " buffers, ", 
                 nrow(std_data), " standards, ", nrow(sc_data), " standard curves\n"))
      
      # 2. Insert Control Samples from all 3 tables (INSERT to TARGET via conn)
      pcb("Step 9/9: Controls & Standards", paste("Inserting", nrow(ctrl_data), "controls,", nrow(buff_data), "blanks,", nrow(std_data), "standards"))
      results$control_samples <- insert_control_samples(
        conn, ctrl_data, buff_data, std_data,
        config$workspace_id, study_acc_target, config$experiment_accession,
        config$result_schema, commit
      )
      
      # 3. Insert MBAA Results for Controls/Buffers/Standards (INSERT to TARGET)
      results$control_results <- insert_control_results(
         conn, ctrl_data, buff_data, std_data,
         config$workspace_id, study_acc_target, config$experiment_accession,
         config$result_schema, commit, skip_if_exists = skip_existing
      )

      # 10. Insert Standard Curves (using insert_standard_curves from ispi_migration_controls.R)
      results$standard_curves <- insert_standard_curves(
         conn, sc_data,
         config$workspace_id, study_acc_target, config$experiment_accession,
         config$result_schema, commit, skip_if_exists = skip_existing
      )

      # 11. Insert Model QC Data (linked to standard curves)
      if(nrow(model_qc_data) > 0 && !is.null(results$standard_curves$sc_accession_map)) {
        cat("\n--- STEP 11: MODEL QC DATA ---\n")
        results$model_qc <- insert_model_qc_data(
          conn, model_qc_data, results$standard_curves$sc_accession_map,
          config$workspace_id, config$experiment_accession, commit,
          skip_if_exists = skip_existing
        )
      } else {
        results$model_qc <- list(success = TRUE, inserted = 0, failed = 0)
      }

      # 5. Insert Sample Results (Luminex/MBAA Specific - FETCH from source, INSERT to target)
      tech <- config$measurement_technique %||% "Luminex"
      if(tech %in% c("Luminex", "MBAA")) {
        results$luminex_results <- insert_mbaa_results_luminex(
          conn, source_schema, study_acc_source,
          config$workspace_id, study_acc_target, config$experiment_accession,
          results$expsamples$sample_mapping,
          results$biosamples$biosample_map,
          config$result_schema, commit,
          source_conn = fetch_conn,
          skip_if_exists = skip_existing,
          agroup_mapping = config$agroup_mapping
        )
        if(!is.null(results$luminex_results$preview)) {
          results$preview_data <- results$luminex_results$preview
        }
      }

      # 12. Insert Sample QC Data (linked to expsamples)
      if(nrow(sample_qc_data) > 0 && !is.null(results$expsamples$sample_mapping)) {
        cat("\n--- STEP 12: SAMPLE QC DATA ---\n")
        results$sample_qc <- insert_sample_qc_data(
          conn, sample_qc_data, results$expsamples$sample_mapping,
          config$workspace_id, config$experiment_accession, commit,
          skip_if_exists = skip_existing
        )
      } else {
        results$sample_qc <- list(success = TRUE, inserted = 0, failed = 0)
      }
  }
  
  
  # Summary
  cat("\n═══════════════════════════════════════════════════\n")
  cat("[INFO] MIGRATION SUMMARY\n")
  cat("═══════════════════════════════════════════════════\n")
  exp_status <- if(results$experiment$created) "(NEW)" else "(existing)"
  cat("Experiment:", results$experiment$experiment_accession, exp_status, "\n")
  cat("Subjects:", results$subjects$validated %||% 0, "validated,", results$subjects$missing %||% 0, "missing\n")
  cat("Planned Visits:", results$planned_visits$created, "created\n")
  cat("Biosamples:", results$biosamples$inserted %||% 0, "inserted,", results$biosamples$existing %||% 0, "existing\n")
  cat("Arms/Cohorts:", results$arms$created, "created\n")
  cat("Arm Associations:", results$arm_associations$inserted,  "inserted\n")
  cat("Experiment Samples:", results$expsamples$inserted, "inserted,", length(results$expsamples$failed), "failed,", length(results$expsamples$skipped), "skipped\n")
  cat("Biosample Links:", results$biosample_links$linked %||% 0, "linked (", length(results$biosample_links$failed), "failed)\n")
  
  if(!is.null(results$mbaa_results)) {
     cat("MBAA Results:", results$mbaa_results$inserted_result %||% 0, "inserted\n")
  }
  if(!is.null(results$control_samples)) {
     cat("Control Samples:", results$control_samples$inserted %||% 0, "inserted,", length(results$control_samples$failed), "failed\n")
  }
  if(!is.null(results$control_results)) {
     cat("Control MBAA Results:", results$control_results$inserted %||% 0, "inserted,", results$control_results$failed %||% 0, "failed\n")
  }
  if(!is.null(results$standard_curves)) {
     cat("Standard Curves:", results$standard_curves$inserted %||% 0, "inserted,", results$standard_curves$failed %||% 0, "failed\n")
  }
  if(!is.null(results$model_qc)) {
     cat("Model QC Data:", results$model_qc$inserted %||% 0, "inserted,", results$model_qc$failed %||% 0, "failed\n")
  }
  if(!is.null(results$sample_qc)) {
     cat("Sample QC Data:", results$sample_qc$inserted %||% 0, "inserted,", results$sample_qc$failed %||% 0, "failed\n")
  }
  
  # Detect any failures
  has_failures <- (
    length(results$subjects$missing_list %||% list()) > 0 ||
    length(results$biosamples$failed) > 0 ||
    length(results$expsamples$failed) > 0 ||
    length(results$biosample_links$failed) > 0 ||
    (!is.null(results$mbaa_results) && !results$mbaa_results$success) ||
    (!is.null(results$control_samples) && length(results$control_samples$failed) > 0) ||
    (!is.null(results$control_results) && (results$control_results$failed %||% 0) > 0) ||
    (!is.null(results$standard_curves) && (results$standard_curves$failed %||% 0) > 0)
  )
  
  results$overall_success <- !has_failures
  
  # Collect all failures and issues
  all_failures <- c(
    results$subjects$failed,
    results$expsamples$failed
  )
  
  all_skipped <- results$expsamples$skipped
  
  if(length(all_failures) > 0 || length(all_skipped) > 0) {
    cat("\n[WARN] ITEMS NEEDING ATTENTION:\n")
    cat("───────────────────────────────────────────────────\n")
    
    if(length(all_failures) > 0) {
      cat("\n[ERROR] FAILED ITEMS (", length(all_failures), "):\n", sep="")
      for(i in 1:min(10, length(all_failures))) {
        item <- all_failures[[i]]
        cat("  ", i, ". [", item$type, "] ", sep="")
        if(!is.null(item$identifier)) cat("ID:", item$identifier, " ")
        if(!is.null(item$sample_id)) cat("Sample:", item$sample_id, " ")
        if(!is.null(item$patient_id)) cat("Patient:", item$patient_id, " ")
        cat("\n     Error:", item$error, "\n")
      }
      if(length(all_failures) > 10) {
        cat("  ... and", length(all_failures) - 10, "more failures\n")
      }
    }
    
    if(length(all_skipped) > 0) {
      cat("\n⏭️  SKIPPED ITEMS (", length(all_skipped), "):\n", sep="")
      for(i in 1:min(10, length(all_skipped))) {
        item <- all_skipped[[i]]
        cat("  ", i, ". [", item$type, "] ", sep="")
        if(!is.null(item$sample_id)) cat("Sample:", item$sample_id, " ")
        if(!is.null(item$timeperiod)) cat("Timeperiod:", item$timeperiod, " ")
        cat("\n     Reason:", item$reason, "\n")
      }
      if(length(all_skipped) > 10) {
        cat("  ... and", length(all_skipped) - 10, "more skipped items\n")
      }
    }
  }
  
  cat("═══════════════════════════════════════════════════\n")
  
  if(commit) {
    DBI::dbExecute(conn, "COMMIT")
    cat("[OK] ALL CHANGES COMMITTED TO DATABASE\n")
  } else {
    DBI::dbExecute(conn, "ROLLBACK")
    cat("[ROLLBACK] ALL CHANGES ROLLED BACK (TEST MODE)\n")
  }
  
  cat("═══════════════════════════════════════════════════\n")
  
  # Add failures and skipped to results
  results$all_failures <- all_failures
  results$all_skipped <- all_skipped
  
  return(results)
  
  }, error = function(e) {
    try(DBI::dbExecute(conn, "ROLLBACK"), silent=TRUE)
    cat("\n[ERROR] CRITICAL ERROR IN MIGRATION:", e$message, "\n")
    cat("[ROLLBACK] All changes rolled back due to error.\n")
    return(list(success = FALSE, overall_success = FALSE, error = e$message))
  })
}

# =====================================================
# 6. INSERT MBAA RESULTS
# =====================================================
# Inserts result data into expsample_mbaa_detail and mbaa_result
insert_mbaa_results <- function(conn, source_data, sample_mapping, experiment_accession, study_accession, workspace_id, commit = FALSE, manage_transaction = TRUE, skip_if_exists = FALSE) {

  cat("[INFO] Creating MBAA Results (Detail + Result Table)\n")

  if(length(sample_mapping) == 0) {
    cat("[WARN] No sample mapping provided, skipping result insertion\n")
    return(list(success = TRUE, inserted = 0))
  }

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", experiment_accession, "' AND source_type='EXPSAMPLE'"))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] MBAA results already exist (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = 0, inserted_result = 0, skipped_existing = existing_n))
    }
  }

  # Start transaction
  if(manage_transaction && !commit) {
    DBI::dbExecute(conn, "BEGIN")
    cat("[INFO] Transaction started (TEST MODE - will rollback)\n")
  }
  
  tryCatch({
    inserted_detail <- 0
    inserted_result <- 0
    
    # Progress bar
    total_rows <- nrow(source_data)
    cat("  Processing", total_rows, "rows...\n")
    
    # Collect unique analytes for bulk creation in lk_analyte
    unique_analytes <- c()
    
    for(i in 1:total_rows) {
      idx_str <- as.character(i)
      if(!idx_str %in% names(sample_mapping)) next
      
      row <- source_data[i, ]
      antigen <- if(!is.null(row$antigen) && !is.na(row$antigen)) as.character(row$antigen) else ""
      feature <- if(!is.null(row$feature) && !is.na(row$feature)) as.character(row$feature) else ""
      analyte_str <- if(nchar(feature) > 0) paste0(antigen, "|", feature) else antigen
      unique_analytes <- c(unique_analytes, analyte_str)
    }
    unique_analytes <- unique(unique_analytes)
    
    # Create analyte entries in lk_analyte if they don't exist
    if(length(unique_analytes) > 0) {
      ensure_analytes_exist(conn, unique_analytes, workspace_id)
    }
    
    # Preview storage
    preview_rows <- list()
    
    for(i in 1:total_rows) {
      idx_str <- as.character(i)
      if(!idx_str %in% names(sample_mapping)) next
      
      map <- sample_mapping[[idx_str]]
      row <- source_data[i, ]
      
      expsample_acc <- map$expsample_accession
      biosample_acc <- map$biosample_accession
      subject_acc <- map$subject_accession  # Get subject from mapping
      
      if(is.null(expsample_acc)) next
      
      # Use SAVEPOINT for each row to prevent transaction cascade
      sp_name <- paste0("mbaa_sp_", i)
      tryCatch({
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
        
        # Build analyte string: antigen|feature
        antigen <- if(!is.null(row$antigen) && !is.na(row$antigen)) as.character(row$antigen) else ""
        feature <- if(!is.null(row$feature) && !is.na(row$feature)) as.character(row$feature) else ""
        analyte_str <- if(nchar(feature) > 0) paste0(antigen, "|", feature) else antigen
        
        # Build assay_id: plate_number|nominal_sample_dilution
        plate_id <- if(!is.null(row$plate_id) && !is.na(row$plate_id)) as.character(row$plate_id) else ""
        dilution <- if(!is.null(row$dilution) && !is.na(row$dilution)) as.character(row$dilution) else ""
        nominal_dilution <- if("nominal_sample_dilution" %in% names(row) && !is.null(row$nominal_sample_dilution) && !is.na(row$nominal_sample_dilution)) {
          as.character(row$nominal_sample_dilution)
        } else {
          dilution
        }
        assay_id <- paste0(plate_id, "|", nominal_dilution)
        
        # Concentration value (antibody_au)
        antibody_au <- if(!is.null(row$antibody_au) && !is.na(row$antibody_au[[1]])) as.character(row$antibody_au[[1]]) else NA_character_
        
        # MFI value
        antibody_mfi <- if(!is.null(row$antibody_mfi) && !is.na(row$antibody_mfi[[1]])) as.character(row$antibody_mfi[[1]]) else NA_character_
        
        # Arm Accession from mapping or source data
        arm_acc <- if(!is.null(map$arm_accession) && !is.na(map$arm_accession)) {
          map$arm_accession
        } else if("arm_accession" %in% names(row) && length(row$arm_accession) > 0 && !is.na(row$arm_accession[[1]])) {
          as.character(row$arm_accession[[1]])
        } else {
          NA_character_
        }
        
        # 1. Insert Expsample Detail
        existing_detail <- suppressWarnings(DBI::dbGetQuery(conn, 
            "SELECT 1 FROM madi_dat.expsample_mbaa_detail WHERE expsample_accession = $1", 
            params = list(expsample_acc)))
        
        if(nrow(existing_detail) == 0) {
          DBI::dbExecute(conn,
            "INSERT INTO madi_dat.expsample_mbaa_detail 
             (expsample_accession, assay_group_id, assay_id, dilution_factor) 
             VALUES ($1, $2, $3, $4)",
            params = list(expsample_acc, experiment_accession, assay_id, dilution)
          )
          inserted_detail <- inserted_detail + 1
        }
        
        # 2. Insert MBAA Result (all fields per spec)
        DBI::dbExecute(conn,
          "INSERT INTO madi_dat.mbaa_result 
           (experiment_accession, study_accession, workspace_id,
            subject_accession, arm_accession, biosample_accession,
            source_accession, source_type,
            analyte_accession, analyte_reported,
            assay_group_id, assay_id,
            concentration_unit_reported, concentration_value_reported,
            mfi) 
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)",
          params = list(
            experiment_accession,           # $1
            study_accession,                # $2
            as.integer(workspace_id),       # $3
            subject_acc,                    # $4 subject_accession
            arm_acc,                        # $5 arm_accession
            biosample_acc,                  # $6 biosample_accession
            expsample_acc,                  # $7 source_accession
            "EXPSAMPLE",                    # $8 source_type
            analyte_str,                    # $9 analyte_accession (from lk_analyte)
            analyte_str,                    # $10 analyte_reported
            experiment_accession,           # $11 assay_group_id
            assay_id,                       # $12 assay_id
            "AU",                           # $13 concentration_unit_reported
            antibody_au,                    # $14 concentration_value_reported
            antibody_mfi                    # $15 mfi
          )
        )
        
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
        inserted_result <- inserted_result + 1
        
        # Capture preview (first 10)
        if(inserted_result <= 10) {
          preview_rows[[length(preview_rows) + 1]] <- list(
            SourceSample = if(!is.null(row$sampleid)) as.character(row$sampleid) else idx_str,
            ExpSample = expsample_acc,
            Biosample = biosample_acc,
            Subject = if(!is.null(subject_acc)) subject_acc else "",
            Analyte = analyte_str,
            Conc = antibody_au,
            MFI = antibody_mfi
          )
        }
        
      }, error = function(e) {
        tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
        if(i <= 5) {
          cat("  [ERROR] MBAA row", i, "failed:", e$message, "\n")
        } else if(i == 6) {
          cat("  ... (suppressing further row errors)\n")
        }
      })
      
      if(i %% 50 == 0) cat(".")
    }
    cat("\n")
    
    # Commit/Rollback Logic
    if(manage_transaction) {
      if(commit) {
        DBI::dbExecute(conn, "COMMIT")
        cat("[OK] COMMITTED: Created", inserted_result, "MBAA results\n")
      } else {
        DBI::dbExecute(conn, "ROLLBACK")
        cat("[ROLLBACK] ROLLED BACK: Would have created", inserted_result, "MBAA results\n")
      }
    } else {
      cat("[OK] Created", inserted_result, "MBAA results (in transaction)\n")
    }
    
    # Print preview table
    if(length(preview_rows) > 0) {
      cat("\n[INFO] MBAA Result Preview (first", min(length(preview_rows), 10), "rows):\n")
      cat(sprintf("  %-8s %-10s %-12s %-12s %-25s %-18s %-10s\n", "Sample", "ExpSample", "Biosample", "Subject", "Analyte", "Conc", "MFI"))
      cat(paste0("  ", paste(rep("-", 100), collapse=""), "\n"))
      for(pr in preview_rows) {
        # Show just the basename of the assay_id (file path) for readability
        cat(sprintf("  %-8s %-10s %-12s %-12s %-25s %-18s %-10s\n",
          substr(as.character(pr$SourceSample %||% ""), 1, 8),
          substr(as.character(pr$ExpSample %||% ""), 1, 10),
          substr(as.character(pr$Biosample %||% ""), 1, 12),
          substr(as.character(pr$Subject %||% ""), 1, 12),
          substr(as.character(pr$Analyte %||% ""), 1, 25),
          substr(as.character(pr$Conc %||% ""), 1, 18),
          substr(as.character(pr$MFI %||% ""), 1, 10)
        ))
      }
      cat("\n")
    }
    
    return(list(success = TRUE, inserted_detail = inserted_detail, inserted_result = inserted_result, preview = preview_rows))
    
  }, error = function(e) {
    if(manage_transaction) try(DBI::dbExecute(conn, "ROLLBACK"), silent=TRUE)
    cat("[ERROR] ERROR in Result Load:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =====================================================
# POST-MIGRATION DATA QUALITY AUDIT
# =====================================================
# Runs comprehensive checks after migration and returns a plain-English report
generate_dq_audit_report <- function(conn, experiment_accession) {
  
  lines <- c()
  add <- function(...) lines <<- c(lines, paste0(...))
  warnings_count <- 0
  criticals_count <- 0
  checks_passed <- 0
  
  # Helper: safely extract integer, treating NULL, NA, and empty as default
  safe_int <- function(x, default = 0L) {
    if(is.null(x) || length(x) == 0 || is.na(x[[1]])) return(as.integer(default))
    as.integer(x[[1]])
  }
  
  add("════════════════════════════════════════════════")
  add("  DATA QUALITY AUDIT REPORT")
  add("  Experiment: ", experiment_accession)
  add("  Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  add("════════════════════════════════════════════════")
  add("")
  
  # --- ROW COUNTS ---
  add("─── SECTION 1: ROW COUNTS ───")
  add("")
  
  count_queries <- list(
    list("experiment", paste0("SELECT count(*) AS n FROM madi_dat.experiment WHERE experiment_accession = '", experiment_accession, "'")),
    list("expsample", paste0("SELECT count(*) AS n FROM madi_dat.expsample WHERE experiment_accession = '", experiment_accession, "'")),
    list("expsample_2_biosample", paste0(
      "SELECT count(*) AS n FROM madi_dat.expsample_2_biosample WHERE expsample_accession IN ",
      "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession = '", experiment_accession, "')")),
    list("expsample_mbaa_detail", paste0(
      "SELECT count(*) AS n FROM madi_dat.expsample_mbaa_detail WHERE expsample_accession IN ",
      "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession = '", experiment_accession, "')")),
    list("mbaa_result (experimental)", paste0(
      "SELECT count(*) AS n FROM madi_dat.mbaa_result WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'EXPSAMPLE'")),
    list("mbaa_result (control)", paste0(
      "SELECT count(*) AS n FROM madi_dat.mbaa_result WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'CONTROL SAMPLE'")),
    list("control_sample", paste0("SELECT count(*) AS n FROM madi_dat.control_sample WHERE experiment_accession = '", experiment_accession, "'")),
    list("standard_curve", paste0("SELECT count(*) AS n FROM madi_dat.standard_curve WHERE experiment_accession = '", experiment_accession, "'")),
    list("model_qc_data", paste0(
      "SELECT count(*) AS n FROM madi_dat.model_qc_data WHERE standard_curve_accession IN ",
      "(SELECT standard_curve_accession FROM madi_dat.standard_curve WHERE experiment_accession = '", experiment_accession, "')")),
    list("sample_qc_data", paste0(
      "SELECT count(*) AS n FROM madi_dat.sample_qc_data WHERE expsample_accession IN ",
      "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession = '", experiment_accession, "')"))
  )
  
  counts <- list()
  for(q in count_queries) {
    tryCatch({
      val <- safe_int(DBI::dbGetQuery(conn, q[[2]])$n)
      counts[[q[[1]]]] <- val
      status <- if(val > 0L) "OK" else "EMPTY"
      add(sprintf("  %-30s %8d   [%s]", q[[1]], val, status))
    }, error = function(e) {
      counts[[q[[1]]]] <<- 0L
      add(sprintf("  %-30s %8s   [ERROR: %s]", q[[1]], "?", e$message))
    })
  }
  add("")
  
  # --- REFERENTIAL INTEGRITY ---
  add("─── SECTION 2: DATA INTEGRITY CHECKS ───")
  add("")
  
  # Check 1: Biosample linkage
  n_expsamples <- safe_int(counts[["expsample"]])
  n_e2b <- safe_int(counts[["expsample_2_biosample"]])
  if(isTRUE(n_e2b == 0L) && isTRUE(n_expsamples > 0L)) {
    add("  🔴 CRITICAL: Biosample Linkage MISSING")
    add(sprintf("     %d expsamples exist but 0 are linked to biosamples.", n_expsamples))
    add("     Cause: The expsample_2_biosample table was not populated.")
    add("     Impact: Downstream views cannot traverse expsample→biosample→subject chain.")
    add("     Action: Check the linking step in the migration code.")
    criticals_count <- criticals_count + 1
  } else if(isTRUE(n_e2b < n_expsamples) && isTRUE(n_expsamples > 0L)) {
    add(sprintf("  🟡 WARNING: Partial Biosample Linkage — %d/%d linked", n_e2b, n_expsamples))
    add(sprintf("     %d expsamples are missing biosample links.", n_expsamples - n_e2b))
    warnings_count <- warnings_count + 1
  } else if(isTRUE(n_e2b == n_expsamples) && isTRUE(n_expsamples > 0L)) {
    add(sprintf("  ✅ Biosample Linkage: %d/%d expsamples linked (100%%)", n_e2b, n_expsamples))
    checks_passed <- checks_passed + 1
  }
  add("")
  
  # Check 2: Orphan results
  tryCatch({
    orphans <- safe_int(DBI::dbGetQuery(conn, paste0(
      "SELECT count(*) AS n FROM madi_dat.mbaa_result r ",
      "WHERE r.experiment_accession = '", experiment_accession, "' ",
      "AND r.source_type = 'EXPSAMPLE' ",
      "AND r.source_accession NOT IN (SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession = '", experiment_accession, "')"
    ))$n)
    if(isTRUE(orphans == 0L)) {
      add("  ✅ Orphan Results: 0 — every result references a valid expsample")
      checks_passed <- checks_passed + 1
    } else {
      add(sprintf("  🔴 CRITICAL: %d orphan results found (source_accession not in expsample)", orphans))
      add("     Action: Some results reference expsamples that don't exist.")
      criticals_count <- criticals_count + 1
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check orphan results: ", e$message))
  })
  add("")
  
  # Check 3: Control results → control_sample match
  tryCatch({
    ctrl_count <- safe_int(counts[["mbaa_result (control)"]])
    if(isTRUE(ctrl_count > 0L)) {
      ctrl_matched <- safe_int(DBI::dbGetQuery(conn, paste0(
        "SELECT count(*) AS n FROM madi_dat.mbaa_result r ",
        "JOIN madi_dat.control_sample cs ON cs.control_sample_accession = r.source_accession ",
        "WHERE r.experiment_accession = '", experiment_accession, "' AND r.source_type = 'CONTROL SAMPLE'"
      ))$n)
      pct <- round(ctrl_matched / ctrl_count * 100, 1)
      if(isTRUE(ctrl_matched == ctrl_count)) {
        add(sprintf("  ✅ Control Result Linkage: %d/%d matched (100%%)", ctrl_matched, ctrl_count))
        checks_passed <- checks_passed + 1
      } else {
        add(sprintf("  🟡 WARNING: Control Result Linkage: %d/%d matched (%.1f%%)", ctrl_matched, ctrl_count, pct))
        add(sprintf("     %d control results have no matching control_sample entry.", ctrl_count - ctrl_matched))
        warnings_count <- warnings_count + 1
      }
    } else {
      add("  ℹ No control results to check")
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check control linkage: ", e$message))
  })
  add("")
  
  # Check 4: MFI/AU null rates for experimental results
  tryCatch({
    mfi_check <- DBI::dbGetQuery(conn, paste0(
      "SELECT ",
      "count(*) FILTER (WHERE mfi IS NULL) AS null_mfi, ",
      "count(*) FILTER (WHERE mfi IS NOT NULL) AS has_mfi, ",
      "count(*) FILTER (WHERE concentration_value_reported IS NULL) AS null_au, ",
      "count(*) FILTER (WHERE concentration_value_reported IS NOT NULL) AS has_au ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'EXPSAMPLE'"
    ))
    null_mfi <- safe_int(mfi_check$null_mfi)
    has_mfi <- safe_int(mfi_check$has_mfi)
    null_au <- safe_int(mfi_check$null_au)
    has_au <- safe_int(mfi_check$has_au)
    total <- null_mfi + has_mfi
    if(isTRUE(total > 0L)) {
      mfi_pct <- round(has_mfi / total * 100, 1)
      
      if(isTRUE(null_mfi == 0L)) {
        add(sprintf("  ✅ Experimental MFI: %d/%d populated (100%%)", has_mfi, total))
        checks_passed <- checks_passed + 1
      } else {
        add(sprintf("  🟡 WARNING: Experimental MFI: %d/%d null (%.1f%% missing)", null_mfi, total, 100 - mfi_pct))
        warnings_count <- warnings_count + 1
      }
      
      if(isTRUE(null_au == 0L)) {
        au_pct <- round(has_au / total * 100, 1)
        add(sprintf("  ✅ Experimental AU: %d/%d populated (100%%)", has_au, total))
        checks_passed <- checks_passed + 1
      } else {
        au_pct <- round(has_au / total * 100, 1)
        add(sprintf("  🟡 WARNING: Experimental AU: %d/%d null (%.1f%% missing)", null_au, total, 100 - au_pct))
        warnings_count <- warnings_count + 1
      }
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check MFI/AU: ", e$message))
  })
  add("")
  
  # Check 4b: subject / biosample / arm coverage on EXPSAMPLE rows
  tryCatch({
    cov <- DBI::dbGetQuery(conn, paste0(
      "SELECT ",
      "count(*) AS total, ",
      "count(subject_accession) AS has_subject, ",
      "count(biosample_accession) AS has_biosample, ",
      "count(arm_accession) AS has_arm ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'EXPSAMPLE'"
    ))
    total_es <- safe_int(cov$total)
    if(isTRUE(total_es > 0L)) {
      for(field in c("has_subject", "has_biosample", "has_arm")) {
        n_has  <- safe_int(cov[[field]])
        label  <- sub("has_", "", field)
        label  <- paste0(toupper(substring(label, 1, 1)), substring(label, 2), " coverage")
        if(isTRUE(n_has == total_es)) {
          add(sprintf("  ✅ EXPSAMPLE %-25s %d/%d (100%%)", label, n_has, total_es))
          checks_passed <- checks_passed + 1
        } else {
          n_miss <- total_es - n_has
          pct    <- round(n_has / total_es * 100, 1)
          add(sprintf("  🔴 CRITICAL: EXPSAMPLE %-20s %d/%d populated (%.1f%%) — %d rows missing",
                      label, n_has, total_es, pct, n_miss))
          add(paste0("     Action: Run backfill UPDATE joining expsample_2_biosample → biosample → arm_2_subject."))
          criticals_count <- criticals_count + 1
        }
      }
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check EXPSAMPLE coverage: ", e$message))
  })
  add("")

  # Check 4c: concentration_value_reported on EXPSAMPLE rows only
  # Controls do NOT have concentration (raw MFI only) — this check is EXPSAMPLE-specific
  tryCatch({
    conc_check <- DBI::dbGetQuery(conn, paste0(
      "SELECT count(*) AS total, ",
      "count(concentration_value_reported) AS has_conc ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'EXPSAMPLE'"
    ))
    total_es  <- safe_int(conc_check$total)
    has_conc  <- safe_int(conc_check$has_conc)
    null_conc <- total_es - has_conc
    if(isTRUE(total_es > 0L)) {
      pct <- round(has_conc / total_es * 100, 1)
      if(isTRUE(null_conc == 0L)) {
        add(sprintf("  ✅ EXPSAMPLE Concentration (AU): %d/%d populated (100%%)", has_conc, total_es))
        checks_passed <- checks_passed + 1
      } else {
        add(sprintf("  🟡 WARNING: EXPSAMPLE Concentration (AU): %d/%d null (%.1f%% missing)",
                    null_conc, total_es, 100 - pct))
        add("     Note: Controls are expected to have null concentration (MFI only).")
        add("     For EXPSAMPLE rows, null AU means antibody_au was NA in xmap_sample source.")
        warnings_count <- warnings_count + 1
      }
    }
    # Controls — explicitly note concentration is expected null
    ctrl_conc <- safe_int(DBI::dbGetQuery(conn, paste0(
      "SELECT count(concentration_value_reported) AS n FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'CONTROL SAMPLE'"
    ))$n)
    ctrl_total <- safe_int(DBI::dbGetQuery(conn, paste0(
      "SELECT count(*) AS n FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'CONTROL SAMPLE'"
    ))$n)
    if(isTRUE(ctrl_total > 0L)) {
      add(sprintf("  ℹ CONTROL concentration: null by design (raw MFI only) — %d rows", ctrl_total))
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check concentration coverage: ", e$message))
  })
  add("")

  # Check 5: Control MFI nulls
  tryCatch({
    ctrl_mfi <- DBI::dbGetQuery(conn, paste0(
      "SELECT ",
      "count(*) FILTER (WHERE mfi IS NULL) AS null_mfi, ",
      "count(*) FILTER (WHERE mfi IS NOT NULL) AS has_mfi ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND source_type = 'CONTROL SAMPLE'"
    ))
    c_null <- safe_int(ctrl_mfi$null_mfi)
    c_has <- safe_int(ctrl_mfi$has_mfi)
    ctrl_total <- c_null + c_has
    if(isTRUE(ctrl_total > 0L)) {
      if(isTRUE(c_null == 0L)) {
        add(sprintf("  ✅ Control MFI: %d/%d populated (100%%)", c_has, ctrl_total))
        checks_passed <- checks_passed + 1
      } else {
        pct_null <- round(c_null / ctrl_total * 100, 1)
        add(sprintf("  🟡 WARNING: Control MFI: %d/%d null (%.1f%%)", c_null, ctrl_total, pct_null))
        add("     These are likely blanks or failed wells in the source data.")
        warnings_count <- warnings_count + 1
      }
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check control MFI: ", e$message))
  })
  add("")
  
  # Check 6: Analyte trailing pipe
  tryCatch({
    trailing <- safe_int(DBI::dbGetQuery(conn, paste0(
      "SELECT count(DISTINCT analyte_reported) AS n ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' AND analyte_reported LIKE '%|'"
    ))$n)
    if(isTRUE(trailing == 0L)) {
      add("  ✅ Analyte Format: No trailing '|' characters found")
      checks_passed <- checks_passed + 1
    } else {
      add(sprintf("  🟡 WARNING: %d analytes have trailing '|' — possible format issue", trailing))
      warnings_count <- warnings_count + 1
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check analyte format: ", e$message))
  })
  add("")
  
  # Check 7: Distinct analytes
  tryCatch({
    n_analytes <- DBI::dbGetQuery(conn, paste0(
      "SELECT count(DISTINCT analyte_reported) AS n ",
      "FROM madi_dat.mbaa_result WHERE experiment_accession = '", experiment_accession, "'"
    ))$n[1]
    add(sprintf("  ℹ Distinct analytes: %d", n_analytes))
  }, error = function(e) {})
  add("")

  # Check 8: Sample QC — ratio of inserted vs expected (expsamples × analytes from source)
  tryCatch({
    sqc_count  <- safe_int(counts[["sample_qc_data"]])
    es_count   <- safe_int(counts[["expsample"]])
    if(isTRUE(sqc_count == 0L) && isTRUE(es_count > 0L)) {
      add("  🔴 CRITICAL: Sample QC Data is empty (0 rows)")
      add("     Cause: patient_lookup may have failed — check if patientid+well composite key matches QC source subject_id+plate_well.")
      add("     Action: Verify get_sample_qc_data returns rows for this experiment in I-SPI source.")
      criticals_count <- criticals_count + 1
    } else if(isTRUE(sqc_count > 0L)) {
      # Expected: at least 1 QC row per expsample
      if(isTRUE(sqc_count >= es_count)) {
        add(sprintf("  ✅ Sample QC Data: %d rows for %d expsamples (%.1fx per sample)",
                    sqc_count, es_count, sqc_count / max(es_count, 1)))
        checks_passed <- checks_passed + 1
      } else {
        add(sprintf("  🟡 WARNING: Sample QC Data: only %d rows for %d expsamples — possible skips",
                    sqc_count, es_count))
        add("     Cause: Some patientid+well combos in QC source may not match any expsample.")
        warnings_count <- warnings_count + 1
      }
    } else {
      add("  ℹ Sample QC: no expsamples to check against")
    }
  }, error = function(e) {
    add(paste0("  ⚠ Could not check sample QC: ", e$message))
  })
  add("")
  
  # --- ASSAY GROUP BREAKDOWN ---
  add("─── SECTION 3: ASSAY GROUP BREAKDOWN ───")
  add("")
  tryCatch({
    groups <- DBI::dbGetQuery(conn, paste0(
      "SELECT assay_group_id, source_type, count(*) AS n ",
      "FROM madi_dat.mbaa_result ",
      "WHERE experiment_accession = '", experiment_accession, "' ",
      "GROUP BY assay_group_id, source_type ORDER BY assay_group_id"
    ))
    if(nrow(groups) > 0) {
      add(sprintf("  %-25s %-18s %8s", "assay_group_id", "source_type", "count"))
      add(paste0("  ", paste(rep("-", 55), collapse = "")))
      for(r in 1:nrow(groups)) {
        add(sprintf("  %-25s %-18s %8s", as.character(groups$assay_group_id[r]), as.character(groups$source_type[r]), as.character(groups$n[r])))
      }
    }
  }, error = function(e) {
    add(paste0("  Could not fetch assay groups: ", e$message))
  })
  add("")
  
  # --- SCORECARD ---
  add("════════════════════════════════════════════════")
  add("  SCORECARD")
  add("════════════════════════════════════════════════")
  add("")
  add(sprintf("  ✅ Checks Passed:   %d", checks_passed))
  add(sprintf("  🟡 Warnings:        %d", warnings_count))
  add(sprintf("  🔴 Critical Issues: %d", criticals_count))
  add("")
  
  if(criticals_count > 0) {
    add("  OVERALL: 🔴 ACTION REQUIRED — Critical issues found that need investigation.")
  } else if(warnings_count > 0) {
    add("  OVERALL: 🟡 MOSTLY GOOD — Some minor warnings to review.")
  } else {
    add("  OVERALL: ✅ ALL CLEAR — Data looks clean!")
  }
  add("")
  add("════════════════════════════════════════════════")
  
  return(paste(lines, collapse = "\n"))
}
