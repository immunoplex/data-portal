
# =====================================================
# ISPI MIGRATION: CONTROLS & STANDARDS
# =====================================================

# -----------------------------------------------------
# 7. INSERT CONTROL SAMPLES
# -----------------------------------------------------
# Creates control_sample records from xmap_control, xmap_buffer, and xmap_standard
insert_control_samples <- function(conn, control_data, buffer_data, standard_data,
                                    workspace_id, study_accession, experiment_accession,
                                    result_schema = "MBAA", commit = FALSE) {
  
  cat(paste0("[INFO] Processing Control Samples (", nrow(control_data), " controls, ",
             nrow(buffer_data), " buffers, ", nrow(standard_data), " standards)...\n"))
  
  inserted_count <- 0
  existing_count <- 0
  failed_items <- list()
  
  # Helper to process a single control/buffer/standard row
  # type: "control", "blank", or "standard"
  process_control_row <- function(row, type) {
    # Generate accession safely under 15 characters for MADI PROD limits
    # xmap id is globally unique across ISPI schema.
    prefix <- switch(type,
      "control" = "cs_",
      "blank"   = "cb_",
      "standard" = "st_"
    )
    id_col <- switch(type,
      "control" = "xmap_control_id",
      "blank"   = "xmap_buffer_id",
      "standard" = "xmap_standard_id"
    )
    source_id <- as.character(row[[id_col]])[[1]]
    
    # "cs_123456" = 9 chars (safely under 15)
    acc <- paste0(prefix, source_id)
    
    # Check if exists
    exists_query <- "SELECT control_sample_accession FROM madi_dat.control_sample WHERE control_sample_accession = $1"
    existing <- suppressWarnings(DBI::dbGetQuery(conn, exists_query, params = list(acc)))
    
    if(nrow(existing) > 0) {
      return(list(status = "existing", acc = acc))
    }
    
    # Build assay_group_id: {experiment_accession}_{type}
    assay_group_id <- paste0(experiment_accession, "_", type)
    
    # Extract fields safely
    plate_id <- as.character(row$plate_id)[1]
    
    dilution <- as.character(row$dilution)[1]
    
    # source column exists in xmap_control and xmap_standard but NOT in xmap_buffer
    source_val <- if("source" %in% names(row) && !is.null(row$source) && length(row$source) > 0 && !is.na(row$source[1])) {
      as.character(row$source)[1]
    } else {
      NA_character_
    }
    
    insert_query <- "
      INSERT INTO madi_dat.control_sample (
        control_sample_accession, experiment_accession, workspace_id, result_schema,
        assay_group_id, assay_id, dilution_factor, source, upload_result_status
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    "
    
    suppressWarnings(DBI::dbExecute(conn, insert_query, params = list(
      acc, experiment_accession, workspace_id, result_schema,
      assay_group_id, plate_id, dilution, source_val, "new"
    )))
    
    return(list(status = "inserted", acc = acc))
  }
  
  # --- Process Controls ---
  if(nrow(control_data) > 0) {
    for(i in 1:nrow(control_data)) {
      sp_name <- paste0("ctrl_sp_", i)
      tryCatch({
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
        res <- process_control_row(control_data[i, ], "control")
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
        if(res$status == "inserted") inserted_count <- inserted_count + 1
        else existing_count <- existing_count + 1
      }, error = function(e) {
        tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
        acc <- paste0("cs_", control_data[i, "xmap_control_id"])
        err_msg <- sprintf("%s | ExpAccLen:%d", e$message, nchar(experiment_accession))
        failed_items[[length(failed_items) + 1]] <<- list(id = acc, error = err_msg)
        if(length(failed_items) <= 10) cat("  [ERROR] Control", acc, "failed:", err_msg, "\n")
      })
      if(i %% 200 == 0) cat(sprintf("  [INFO] Processed %d / %d controls (Failed: %d)\n", i, nrow(control_data), length(failed_items)))
    }
  }
  
  # --- Process Buffers ---
  if(nrow(buffer_data) > 0) {
    for(i in 1:nrow(buffer_data)) {
      sp_name <- paste0("buff_sp_", i)
      tryCatch({
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
        res <- process_control_row(buffer_data[i, ], "blank")
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
        if(res$status == "inserted") inserted_count <- inserted_count + 1
        else existing_count <- existing_count + 1
      }, error = function(e) {
        tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
        acc <- paste0("cb_", buffer_data[i, "xmap_buffer_id"])
        err_msg <- sprintf("%s | ExpAccLen:%d", e$message, nchar(experiment_accession))
        failed_items[[length(failed_items) + 1]] <<- list(id = acc, error = err_msg)
        if(length(failed_items) <= 10) cat("  [ERROR] Buffer", acc, "failed:", err_msg, "\n")
      })
      if(i %% 200 == 0) cat(sprintf("  [INFO] Processed %d / %d buffers (Failed: %d)\n", i, nrow(buffer_data), length(failed_items)))
    }
  }
  
  # --- Process Standards ---
  if(nrow(standard_data) > 0) {
    for(i in 1:nrow(standard_data)) {
      sp_name <- paste0("std_sp_", i)
      tryCatch({
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
        res <- process_control_row(standard_data[i, ], "standard")
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
        if(res$status == "inserted") inserted_count <- inserted_count + 1
        else existing_count <- existing_count + 1
      }, error = function(e) {
        tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
        acc <- paste0("st_", standard_data[i, "xmap_standard_id"])
        err_msg <- sprintf("%s | ExpAccLen:%d", e$message, nchar(experiment_accession))
        failed_items[[length(failed_items) + 1]] <<- list(id = acc, error = err_msg)
        if(length(failed_items) <= 10) cat("  [ERROR] Standard", acc, "failed:", err_msg, "\n")
      })
      if(i %% 200 == 0) cat(sprintf("  [INFO] Processed %d / %d standards (Failed: %d)\n", i, nrow(standard_data), length(failed_items)))
    }
  }
  
  if(commit) {
    cat("[OK] COMMITTED: Created", inserted_count, "control samples,", existing_count, "already existed\n")
  } else {
    cat("[ROLLBACK] Would rollback:", inserted_count, "control samples\n")
  }
  
  if(length(failed_items) > 0) {
    cat("[ERROR] Failed items:", length(failed_items), "\n")
  }
  
  return(list(success = TRUE, inserted = inserted_count, existing = existing_count, failed = failed_items))
}


# -----------------------------------------------------
# 9. INSERT CONTROL/STANDARD MBAA RESULTS
# -----------------------------------------------------
# Creates mbaa_result records for Controls, Buffers, and Standards
# Only fills MFI (no AU/concentration)
insert_control_results <- function(conn, control_data, buffer_data, standard_data,
                                    workspace_id, study_accession, experiment_accession,
                                    result_schema = "MBAA", commit = FALSE, skip_if_exists = FALSE) {

  total <- nrow(control_data) + nrow(buffer_data) + nrow(standard_data)
  cat(paste0("[INFO] Processing Control/Standard MBAA Results (", total, " rows)...\n"))

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", experiment_accession, "' AND source_type='CONTROL SAMPLE'"))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] Control MBAA results already exist (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = existing_n, failed = 0))
    }
  }
  
  inserted_count <- 0
  failed_count <- 0
  missing_mfi_count <- 0
  failed_items <- list()
  preview_rows <- list()
  
  # 1. Collect unique analytes from all 3 tables and ensure they exist
  all_analytes <- character(0)
  for(df in list(control_data, buffer_data, standard_data)) {
    if(nrow(df) > 0 && "antigen" %in% names(df) && "feature" %in% names(df)) {
      antigen_vals <- as.character(df$antigen)
      feature_vals <- as.character(df$feature)
      analytes <- ifelse(nchar(feature_vals) > 0, paste0(antigen_vals, "|", feature_vals), antigen_vals)
      all_analytes <- c(all_analytes, analytes)
    }
  }
  unique_analytes <- unique(all_analytes)
  if(length(unique_analytes) > 0) {
    cat(paste0("  [INFO] Ensuring ", length(unique_analytes), " unique analytes exist...\n"))
    ensure_analytes_exist(conn, unique_analytes, workspace_id)
  }
  
  # 2. Helper to insert a single MBAA result row
  insert_one_result <- function(row, type) {
    # Build accession using the SAME format as process_control_row (cs_, cb_, st_)
    prefix <- switch(type,
      "control" = "cs_",
      "blank"   = "cb_",
      "standard" = "st_"
    )
    id_col <- switch(type,
      "control" = "xmap_control_id",
      "blank"   = "xmap_buffer_id",
      "standard" = "xmap_standard_id"
    )
    source_id <- as.character(row[[id_col]])[1]
    source_acc <- paste0(prefix, source_id)
    
    # Build analyte string: antigen|feature (same pattern as expsample MBAA results)
    antigen <- if(!is.null(row$antigen) && !is.na(row$antigen)) as.character(row$antigen)[1] else ""
    feature <- if(!is.null(row$feature) && !is.na(row$feature)) as.character(row$feature)[1] else ""
    analyte_str <- if(nchar(feature) > 0) paste0(antigen, "|", feature) else antigen
    
    # Build assay_group_id and assay_id
    assay_group_id <- paste0(experiment_accession, "_", type)
    plate_id <- if(!is.null(row$plate_id) && !is.na(row$plate_id)) as.character(row$plate_id)[1] else ""
    dilution <- if(!is.null(row$dilution) && !is.na(row$dilution)) as.character(row$dilution)[1] else ""
    nominal_dilution <- if("nominal_sample_dilution" %in% names(row) && !is.null(row$nominal_sample_dilution) && !is.na(row$nominal_sample_dilution)) {
      as.character(row$nominal_sample_dilution)[1]
    } else {
      dilution
    }
    assay_id <- paste0(plate_id, "|", nominal_dilution)
    
    # MFI value (only field we fill — no AU/concentration)
    # Use NA_character_ instead of NULL because NULL has length 0 and causes DBI error "Parameter 10 does not have length 1"
    mfi_val <- if(!is.null(row$antibody_mfi) && !is.na(row$antibody_mfi)) {
      as.character(row$antibody_mfi)[1] 
    } else {
      missing_mfi_count <<- missing_mfi_count + 1
      NA_character_
    }
    
    # Insert MBAA result
    query <- "
      INSERT INTO madi_dat.mbaa_result (
        experiment_accession, study_accession, workspace_id,
        source_type, source_accession,
        analyte_accession, analyte_reported,
        assay_group_id, assay_id,
        mfi
      ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
    "
    
    suppressWarnings(DBI::dbExecute(conn, query, params = list(
      experiment_accession,  # $1
      study_accession,       # $2
      workspace_id,          # $3
      "CONTROL SAMPLE",      # $4 source_type (from lk_source_type)
      source_acc,            # $5 source_accession
      analyte_str,           # $6 analyte_accession
      analyte_str,           # $7 analyte_reported
      assay_group_id,        # $8 assay_group_id
      assay_id,              # $9 assay_id
      mfi_val                # $10 mfi
    )))
    
    return(list(source_acc = source_acc, analyte = analyte_str, mfi = mfi_val, type = type))
  }
  
  # 3. Process all rows with SAVEPOINT protection
  process_rows <- function(data, type, label) {
    if(nrow(data) == 0) return()
    cat(paste0("  Processing ", nrow(data), " ", label, " results...\n"))
    
    for(i in 1:nrow(data)) {
      sp_name <- paste0("cr_", substr(type, 1, 3), "_", i)
      tryCatch({
        suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
        res <- insert_one_result(data[i, ], type)
        suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
        inserted_count <<- inserted_count + 1
        
        # Capture preview (first 10 total)
        if(inserted_count <= 10) {
          preview_rows[[length(preview_rows) + 1]] <<- res
        }
      }, error = function(e) {
        tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
        failed_count <<- failed_count + 1
        if(failed_count <= 10) message("  [ERROR] ", label, " MBAA result ", i, " failed: ", e$message)
      })
      if(i %% 200 == 0) message(sprintf("  [INFO] Processed %d / %d %s MBAA results (Failed: %d)", i, nrow(data), label, failed_count))
    }
  }
  
  process_rows(control_data, "control", "control")
  process_rows(buffer_data, "blank", "buffer")
  process_rows(standard_data, "standard", "standard")
  
  # 4. Print preview table
  if(length(preview_rows) > 0) {
    cat(paste0("\n[INFO] Control MBAA Result Preview (first ", min(length(preview_rows), 10), " rows):\n"))
    cat(sprintf("  %-8s %-12s %-25s %-10s\n", "Type", "SourceAcc", "Analyte", "MFI"))
    cat(paste0("  ", paste(rep("-", 60), collapse=""), "\n"))
    for(pr in preview_rows) {
      cat(sprintf("  %-8s %-12s %-25s %-10s\n",
        substr(as.character(pr$type %||% ""), 1, 8),
        substr(as.character(pr$source_acc %||% ""), 1, 12),
        substr(as.character(pr$analyte %||% ""), 1, 25),
        substr(as.character(pr$mfi %||% ""), 1, 10)
      ))
    }
    cat("\n")
  }
  
  # 5. Summary
  if(commit) {
    cat("[OK] COMMITTED: Created", inserted_count, "control MBAA results\n")
  } else {
    cat("[ROLLBACK] Would rollback:", inserted_count, "control MBAA results\n")
  }
  if(failed_count > 0) {
    cat("[ERROR] Failed:", failed_count, "control MBAA results\n")
  }
  
  if(missing_mfi_count > 0) {
    cat(paste0("[WARN] Warning: ", missing_mfi_count, " results have missing MFI values (inserted as NULL)\n"))
  }
  
  return(list(success = TRUE, inserted = inserted_count, failed = failed_count))
}

# -----------------------------------------------------
# 10. INSERT STANDARD CURVES
# -----------------------------------------------------
# Migrates standard curve data from best_glance_all to standard_curve
insert_standard_curves <- function(conn, fit_data, workspace_id, study_accession,
                                   experiment_accession, result_schema = "MBAA", commit = FALSE, skip_if_exists = FALSE) {

  if(nrow(fit_data) == 0) {
    cat("[INFO] No standard curve fit data to insert.\n")
    return(list(success = TRUE, inserted = 0, failed = 0, sc_accession_map = list()))
  }

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) FROM madi_dat.standard_curve WHERE experiment_accession='", experiment_accession, "'"))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] Standard curves already exist (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = 0, failed = 0, sc_accession_map = list()))
    }
  }

  cat(paste0("[INFO] Processing Standard Curves (", nrow(fit_data), " rows)...\n"))
  
  inserted_count <- 0
  failed_count <- 0
  sc_accession_map <- list()  # Maps antigen|experiment|plate -> SC accession
  
  # 1. Ensure all analytes exist in lk_analyte (FK constraint fk_standard_curve_4)
  # NOTE: best_glance_all has NO 'feature' column. The feature (IgG1, ADCD, etc.)
  # is stored in the 'experiment_accession' column of the source table.
  if("antigen" %in% names(fit_data) && "experiment_accession" %in% names(fit_data)) {
    sc_analytes <- unique(paste0(as.character(fit_data$antigen), "|", as.character(fit_data$experiment_accession)))
    cat(paste0("[INFO] Checking/Creating ", length(sc_analytes), " analytes for standard curves...\n"))
    ensure_analytes_exist(conn, sc_analytes, workspace_id)
  }
  
  # 2. Determine starting Standard Curve Accession ID (SCxxxx)
  sc_query <- "SELECT MAX(CAST(SUBSTRING(standard_curve_accession FROM 3) AS INTEGER)) FROM madi_dat.standard_curve WHERE standard_curve_accession LIKE 'SC%'"
  max_sc <- tryCatch({
    res <- DBI::dbGetQuery(conn, sc_query)
    if(nrow(res) > 0 && !is.na(res[[1]])) res[[1]] else 0
  }, error = function(e) 0)
  
  current_sc_id <- max_sc
  
  # 3. Prepare Statement
  insert_query <- "
    INSERT INTO madi_dat.standard_curve (
      standard_curve_accession, result_schema, workspace_id,
      experiment_accession, assay_group_id, assay_id,
      analyte_accession, analyte_reported, analyte_preferred,
      formula,
      lower_limit, lower_limit_unit,
      upper_limit, upper_limit_unit,
      upload_result_status
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
  "
  
  # 4. Loop and Insert (with SAVEPOINT protection per row)
  for(i in 1:nrow(fit_data)) {
    row <- fit_data[i, ]
    sp_name <- paste0("sc_sp_", i)
    
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
      
      # Generate ID
      current_sc_id <- current_sc_id + 1
      sc_acc <- paste0("SC", current_sc_id)
      
      # Construct mappings
      antigen <- if(!is.null(row$antigen) && !is.na(row$antigen)) as.character(row$antigen)[1] else ""
      # Feature comes from experiment_accession in best_glance_all (e.g. IgG1, ADCD)
      feature <- if(!is.null(row$experiment_accession) && !is.na(row$experiment_accession)) as.character(row$experiment_accession)[1] else ""
      analyte_str <- if(nchar(feature) > 0) paste0(antigen, "|", feature) else antigen
      
      plate <- if(!is.null(row$plate) && !is.na(row$plate)) as.character(row$plate)[[1]] else ""
      dilution <- if(!is.null(row$nominal_sample_dilution) && !is.na(row$nominal_sample_dilution)) as.character(row$nominal_sample_dilution)[[1]] else ""
      assay_id <- paste0(plate, "|", dilution)
      
      assay_group_id <- paste0(experiment_accession, "_standard")
      
      # Formula string
      formula_body <- if(!is.null(row$formula) && !is.na(row$formula)) as.character(row$formula)[[1]] else "NA"
      params <- c(
        paste0("a=", if(!is.null(row$a)) row$a else "NA"),
        paste0("b=", if(!is.null(row$b)) row$b else "NA"),
        paste0("c=", if(!is.null(row$c)) row$c else "NA"),
        paste0("d=", if(!is.null(row$d)) row$d else "NA"),
        paste0("g=", if(!is.null(row$g)) row$g else "NA")
      )
      formula_str <- paste0("Formula: ", formula_body, "; Params: ", paste(params, collapse=", "))
      if(nchar(formula_str) > 1000) formula_str <- substr(formula_str, 1, 1000)
      
      llod <- if(!is.null(row$llod) && !is.na(row$llod)) as.character(row$llod)[[1]] else NA_character_
      ulod <- if(!is.null(row$ulod) && !is.na(row$ulod)) as.character(row$ulod)[[1]] else NA_character_
      
      # Execute Insert
      DBI::dbExecute(conn, insert_query, params = list(
        sc_acc,                # $1 standard_curve_accession
        result_schema,         # $2 result_schema
        workspace_id,          # $3 workspace_id
        experiment_accession,  # $4 experiment_accession
        assay_group_id,        # $5 assay_group_id
        assay_id,              # $6 assay_id
        analyte_str,           # $7 analyte_accession
        analyte_str,           # $8 analyte_reported
        analyte_str,           # $9 analyte_preferred
        formula_str,           # $10 formula
        llod,                  # $11 lower_limit
        "UI/ML",               # $12 lower_limit_unit
        ulod,                  # $13 upper_limit
        "UI/ML",               # $14 upper_limit_unit
        "not parsed"           # $15 upload_result_status
      ))
      
      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      inserted_count <- inserted_count + 1
      
      # Record mapping for model QC data linking
      map_key <- paste0(antigen, "|", feature, "|", plate)
      sc_accession_map[[map_key]] <- sc_acc
      # Also store without plate for fallback matching
      map_key_no_plate <- paste0(antigen, "|", feature)
      if(is.null(sc_accession_map[[map_key_no_plate]])) sc_accession_map[[map_key_no_plate]] <- sc_acc
    }, error = function(e) {
      tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
      if(failed_count < 5) cat("[ERROR] Standard fit failed row", i, ":", e$message, "\n")
      failed_count <<- failed_count + 1
    })
  }
  
  if(failed_count > 0) {
    cat("[ERROR] Failed:", failed_count, "standard curve inserts\n")
  }
  cat("[OK] Inserted:", inserted_count, "standard curves\n")
  
  return(list(success = TRUE, inserted = inserted_count, failed = failed_count,
              sc_accession_map = sc_accession_map))
}

# =============================================================================
# Insert Sample QC Data (from get_sample_qc_data function)
# Links each row to an expsample_accession via plate+well+experiment+patientid
# =============================================================================
insert_sample_qc_data <- function(conn, sample_qc_df, sample_mapping, workspace_id,
                                   experiment_accession, commit = FALSE, skip_if_exists = FALSE) {

  if(is.null(sample_qc_df) || nrow(sample_qc_df) == 0) {
    cat("[INFO] No sample QC data to insert.\n")
    return(list(success = TRUE, inserted = 0, failed = 0))
  }

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0(
        "SELECT COUNT(*) FROM madi_dat.sample_qc_data WHERE expsample_accession IN ",
        "(SELECT expsample_accession FROM madi_dat.expsample WHERE experiment_accession='", experiment_accession, "')"
      ))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] Sample QC data already exists (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = 0, failed = 0))
    }
  }

  cat(paste0("[INFO] Processing Sample QC Data (", nrow(sample_qc_df), " rows)...\n"))
  
  # Build a patientid → expsample_accession lookup table from sample_mapping
  # sample_mapping is keyed by row index, each entry has patientid, expsample_accession, etc.
  patient_lookup <- list()
  if(!is.null(sample_mapping)) {
    for(key in names(sample_mapping)) {
      entry <- sample_mapping[[key]]
      pid <- as.character(entry$patientid)
      if(!is.null(pid) && !is.na(pid) && pid != "") {
        # Store first expsample per patient (they share the same expsample if same plate/well)
        if(is.null(patient_lookup[[pid]])) {
          patient_lookup[[pid]] <- entry$expsample_accession
        }
      }
    }
  }
  
  cat(paste0("[INFO] Built patient lookup: ", length(patient_lookup), " unique patients mapped to expsamples\n"))
  
  if(length(patient_lookup) == 0) {
    cat("[WARN] No patient-to-expsample mappings found. Cannot insert sample QC data.\n")
    return(list(success = TRUE, inserted = 0, failed = 0))
  }
  
  inserted_count <- 0
  failed_count <- 0
  skipped_count <- 0
  
  insert_query <- "
    INSERT INTO madi_dat.sample_qc_data (
      expsample_accession, workspace_id, plate_number, plate_well,
      nominal_sample_dilution, analyte_name, reagent_name,
      timeperiod, subject_id, sample_dilution, pctaggbeads,
      samplingerrors, bead_count, mfi, predicted_concentration,
      se_concentration, gate_class_loq, gate_class_lod
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18)
  "
  
  for(i in 1:nrow(sample_qc_df)) {
    row <- sample_qc_df[i, ]
    sp_name <- paste0("sqc_sp_", i)
    
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
      
      # Look up expsample by patientid (subject_id in QC data)
      patient_id <- as.character(row$subject_id)[[1]]
      matched_expsample <- patient_lookup[[patient_id]]
      
      if(is.null(matched_expsample)) {
        suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name)))
        if(skipped_count < 3) cat("[WARN] No matching expsample for patient:", patient_id, "\n")
        skipped_count <- skipped_count + 1
        next
      }
      
      DBI::dbExecute(conn, insert_query, params = list(
        matched_expsample,                                                          # $1
        workspace_id,                                                               # $2
        if(!is.null(row$plate_number)) as.character(row$plate_number)[[1]] else NA,    # $3
        if(!is.null(row$plate_well)) as.character(row$plate_well)[[1]] else NA,        # $4
        if(!is.null(row$nominal_sample_dilution)) as.character(row$nominal_sample_dilution)[[1]] else NA, # $5
        if(!is.null(row$analyte_name)) as.character(row$analyte_name)[[1]] else NA,    # $6
        if(!is.null(row$reagent_name)) as.character(row$reagent_name)[[1]] else NA,    # $7
        if(!is.null(row$timeperiod)) as.character(row$timeperiod)[[1]] else NA,        # $8
        if(!is.null(row$subject_id)) as.character(row$subject_id)[[1]] else NA,        # $9
        if(!is.null(row$sample_dilution) && !is.na(row$sample_dilution)) as.numeric(row$sample_dilution)[[1]] else NA, # $10
        if(!is.null(row$pctaggbeads) && !is.na(row$pctaggbeads)) as.numeric(row$pctaggbeads)[[1]] else NA, # $11
        if(!is.null(row$samplingerrors)) as.character(row$samplingerrors)[[1]] else NA, # $12
        if(!is.null(row$bead_count) && !is.na(row$bead_count)) as.integer(row$bead_count)[[1]] else NA_integer_, # $13
        if(!is.null(row$mfi) && !is.na(row$mfi)) as.numeric(row$mfi)[[1]] else NA,    # $14
        if(!is.null(row$predicted_concentration) && !is.na(row$predicted_concentration)) as.numeric(row$predicted_concentration)[[1]] else NA, # $15
        if(!is.null(row$se_concentration) && !is.na(row$se_concentration)) as.numeric(row$se_concentration)[[1]] else NA, # $16
        if(!is.null(row$gate_class_loq)) as.character(row$gate_class_loq)[[1]] else NA, # $17
        if(!is.null(row$gate_class_lod)) as.character(row$gate_class_lod)[[1]] else NA  # $18
      ))
      
      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      inserted_count <- inserted_count + 1
      
      if(inserted_count %% 5000 == 0) {
        cat(paste0("    ... processed ", inserted_count, "/", nrow(sample_qc_df), " rows\n"))
      }
    }, error = function(e) {
      tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
      if(failed_count < 5) cat("[ERROR] Sample QC row", i, "failed:", e$message, "\n")
      failed_count <<- failed_count + 1
    })
  }
  
  cat("[OK] Inserted:", inserted_count, "sample QC rows,", skipped_count, "skipped (no match),", failed_count, "failed\n")
  return(list(success = TRUE, inserted = inserted_count, failed = failed_count))
}


# =============================================================================
# Insert Model QC Data (from get_model_qc_data function)
# Links each row to a standard_curve_accession via plate+antigen+experiment
# =============================================================================
insert_model_qc_data <- function(conn, model_qc_df, sc_accession_map, workspace_id,
                                  experiment_accession, commit = FALSE, skip_if_exists = FALSE) {

  if(is.null(model_qc_df) || nrow(model_qc_df) == 0) {
    cat("[INFO] No model QC data to insert.\n")
    return(list(success = TRUE, inserted = 0, failed = 0))
  }

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0(
        "SELECT COUNT(*) FROM madi_dat.model_qc_data WHERE standard_curve_accession IN ",
        "(SELECT standard_curve_accession FROM madi_dat.standard_curve WHERE experiment_accession='", experiment_accession, "')"
      ))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] Model QC data already exists (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = 0, failed = 0))
    }
  }

  cat(paste0("[INFO] Processing Model QC Data (", nrow(model_qc_df), " rows)...\n"))
  
  inserted_count <- 0
  failed_count <- 0
  
  insert_query <- "
    INSERT INTO madi_dat.model_qc_data (
      standard_curve_accession, workspace_id, plate_number,
      nominal_sample_dilution, source_standard, analyte_name, reagent_name,
      formula_name, formula,
      a, b, c, d, g,
      bkg_method, is_log_response, is_log_x, apply_prozone,
      dfresidual, nobs,
      lloq, uloq, lloq_y, uloq_y, llod, ulod,
      mindc, minrdl, maxdc, maxrdl,
      inflect_x, inflect_y, dydx_inflect,
      aic, bic, loglik, mse
    ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$30,$31,$32,$33,$34,$35,$36,$37)
  "
  
  for(i in 1:nrow(model_qc_df)) {
    row <- model_qc_df[i, ]
    sp_name <- paste0("mqc_sp_", i)
    
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))
      
      # Find matching standard_curve_accession from the map
      # sc_accession_map is keyed by "antigen|experiment_accession|plate"
      antigen <- if(!is.null(row$reagent_name)) as.character(row$reagent_name)[[1]] else ""
      plate <- if(!is.null(row$plate_number)) as.character(row$plate_number)[[1]] else ""
      experiment <- if(!is.null(row$experiment_name)) as.character(row$experiment_name)[[1]] else ""
      
      map_key <- paste0(antigen, "|", experiment, "|", plate)
      matched_sc <- if(!is.null(sc_accession_map[[map_key]])) sc_accession_map[[map_key]] else NULL
      
      # Also try without plate if no match
      if(is.null(matched_sc)) {
        map_key_no_plate <- paste0(antigen, "|", experiment)
        matched_sc <- if(!is.null(sc_accession_map[[map_key_no_plate]])) sc_accession_map[[map_key_no_plate]] else NULL
      }
      
      if(is.null(matched_sc)) {
        suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name)))
        if(failed_count < 3) cat("[WARN] No matching standard curve for:", map_key, "\n")
        failed_count <- failed_count + 1
        next
      }
      
      safe_num <- function(x) if(!is.null(x) && !is.na(x[[1]])) as.numeric(x)[[1]] else NA_real_
      safe_chr <- function(x) if(!is.null(x) && !is.na(x[[1]])) as.character(x)[[1]] else NA_character_
      safe_bool <- function(x) if(!is.null(x) && !is.na(x[[1]])) as.logical(x)[[1]] else NA
      safe_dbl <- function(x) if(!is.null(x) && !is.na(x[[1]])) as.double(x)[[1]] else NA_real_
      
      DBI::dbExecute(conn, insert_query, params = list(
        matched_sc,                                  # $1 standard_curve_accession
        workspace_id,                                # $2
        safe_chr(row$plate_number),                  # $3
        safe_chr(row$nominal_sample_dilution),       # $4
        safe_chr(row$source_standard),               # $5
        safe_chr(row$analyte_name),                  # $6
        safe_chr(row$reagent_name),                  # $7
        safe_chr(row$formula_name),                  # $8
        safe_chr(row$formula),                       # $9
        safe_num(row$a),                             # $10
        safe_num(row$b),                             # $11
        safe_num(row$c),                             # $12
        safe_num(row$d),                             # $13
        safe_num(row$g),                             # $14
        safe_chr(row$bkg_method),                    # $15
        safe_bool(row$is_log_response),              # $16
        safe_bool(row$is_log_x),                     # $17
        safe_bool(row$apply_prozone),                # $18
        safe_num(row$dfresidual),                    # $19
        safe_num(row$nobs),                          # $20
        safe_num(row$lloq),                          # $21
        safe_num(row$uloq),                          # $22
        safe_num(row$lloq_y),                        # $23
        safe_num(row$uloq_y),                        # $24
        safe_num(row$llod),                          # $25
        safe_num(row$ulod),                          # $26
        safe_num(row$mindc),                         # $27
        safe_num(row$minrdl),                        # $28
        safe_num(row$maxdc),                         # $29
        safe_num(row$maxrdl),                        # $30
        safe_num(row$inflect_x),                     # $31
        safe_num(row$inflect_y),                     # $32
        safe_num(row$dydx_inflect),                  # $33
        safe_num(row$aic),                           # $34
        safe_num(row$bic),                           # $35
        safe_num(row$loglik),                        # $36
        safe_dbl(row$mse)                            # $37
      ))
      
      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      inserted_count <- inserted_count + 1
    }, error = function(e) {
      tryCatch(suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))), error = function(e2) {})
      if(failed_count < 5) cat("[ERROR] Model QC row", i, "failed:", e$message, "\n")
      failed_count <<- failed_count + 1
    })
  }
  
  cat("[OK] Inserted:", inserted_count, "model QC rows,", failed_count, "failed\n")
  return(list(success = TRUE, inserted = inserted_count, failed = failed_count))
}
