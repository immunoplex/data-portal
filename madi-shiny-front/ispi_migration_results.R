
# =====================================================
# ISPI MIGRATION: RESULTS (Luminex/MBAA)
# =====================================================

# -----------------------------------------------------
# UTILITY: Ensure Analytes Exist
# -----------------------------------------------------
ensure_analytes_exist <- function(conn, analytes, workspace_id) {
  if(length(analytes) == 0) return(TRUE)
  
  cat(paste0("[INFO] Checking/Creating ", length(analytes), " analytes...\n"))
  
  # Format: antigen|feature
  count_created <- 0
  count_failed <- 0
  
  for(analyte_str in analytes) {
    tryCatch({
      # Helper to parse antigen|feature
      parts <- strsplit(analyte_str, "\\|")[[1]]
      antigen <- parts[1]
      feature <- if(length(parts) > 1) parts[2] else ""
      
      # Check if exists
      exists_query <- "SELECT analyte_accession FROM madi_dat.lk_analyte WHERE analyte_accession = $1"
      existing <- suppressWarnings(DBI::dbGetQuery(conn, exists_query, params = list(analyte_str)))
      
      if(nrow(existing) == 0) {
        # Use a FIXED savepoint name ("analyte_sp") to avoid accumulating lock entries
        # in PostgreSQL's shared memory when looping over many analytes.
        # Using a unique name per analyte causes "out of shared memory" errors.
        suppressWarnings(DBI::dbExecute(conn, "SAVEPOINT analyte_sp"))
        
        tryCatch({
          insert_query <- "
            INSERT INTO madi_dat.lk_analyte (
              analyte_accession, immunology_symbol, official_gene_name,
              link, gene_symbol
            ) VALUES ($1, $2, $3, $4, $5)
          "
          
          DBI::dbExecute(conn, insert_query, params = list(
            analyte_str,          # analyte_accession
            analyte_str,          # immunology_symbol
            antigen,              # official_gene_name
            "Migrated from I-SPI", # link
            antigen               # gene_symbol
          ))
          
          suppressWarnings(DBI::dbExecute(conn, "RELEASE SAVEPOINT analyte_sp"))
          count_created <- count_created + 1
        }, error = function(e) {
          # Rollback to savepoint so transaction remains usable
          tryCatch(
            suppressWarnings(DBI::dbExecute(conn, "ROLLBACK TO SAVEPOINT analyte_sp")),
            error = function(e2) {}
          )
          cat(paste0("  [WARN] Failed to insert analyte: ", analyte_str, " - ", e$message, "\n"))
          count_failed <<- count_failed + 1
        })
      }
    }, error = function(e) {
      cat(paste0("  [WARN] Failed to check analyte: ", analyte_str, " - ", e$message, "\n"))
      count_failed <<- count_failed + 1
    })
  }
  
  cat(paste0("  [OK] Created ", count_created, " new analytes (", count_failed, " failed)\n"))
  return(TRUE)
}

# -----------------------------------------------------
# INSERT MBAA RESULTS (Luminex Specific)
# -----------------------------------------------------
insert_mbaa_results_luminex <- function(conn, source_schema, study_acc_source,
                                       workspace_id, study_accession, experiment_accession,
                                       expsample_map, biosample_map,
                                       result_schema = "MBAA", commit = FALSE,
                                       source_conn = NULL, skip_if_exists = FALSE,
                                       agroup_mapping = NULL) {

  cat("[INFO] Processing MBAA/Luminex Results (Specific Logic)...\n")

  if(skip_if_exists) {
    existing_n <- tryCatch(
      as.integer(DBI::dbGetQuery(conn, paste0(
        "SELECT COUNT(*) FROM madi_dat.mbaa_result WHERE experiment_accession='", experiment_accession,
        "' AND source_type='EXPSAMPLE'"
      ))[[1]]),
      error = function(e) 0L
    )
    if(existing_n > 0) {
      cat("[SKIP] Luminex MBAA results already exist (", existing_n, "rows) — skipping insert (skip_existing mode)\n")
      return(list(success = TRUE, inserted = 0, failed = 0, preview = list()))
    }
  }
  
  # Use source_conn for fetching, fall back to conn
  fetch_conn <- if(!is.null(source_conn) && DBI::dbIsValid(source_conn)) source_conn else conn
  
  # 1. Fetch Source Data with JOINs
  # We need: plate_id, dilution, antibody_mfi, antibody_au
  # Source: xmap_sample
  # Note: 'plate_id' and 'dilution' are columns in xmap_sample.
  
  cat(paste0("  [INFO] Fetching result data from ", source_schema, ".xmap_sample..."))
  if(!identical(fetch_conn, conn)) cat(" (using source DB connection)")
  cat("\n")
  
  query <- paste0("
    SELECT
      xmap_sample_id, sampleid, patientid, antigen, feature,
      dilution, plate_id, antibody_mfi, antibody_au,
      nominal_sample_dilution, agroup, well
    FROM ", source_schema, ".xmap_sample
    WHERE study_accession = $1
  ")
  
  # Fetch from SOURCE DB
  results_data <- tryCatch(
    DBI::dbGetQuery(fetch_conn, query, params = list(study_acc_source)),
    error = function(e) {
      cat("  [ERROR] Result fetch failed:", e$message, "\n")
      return(data.frame())
    }
  )
  
  cat(paste0("  [INFO] Fetched ", nrow(results_data), " rows from xmap_sample\n"))
  
  if(nrow(results_data) == 0) {
    cat("  [WARN] No result data found in xmap_sample.\n")
    return(list(success = TRUE, inserted = 0))
  }
  
  # 2. Prepare Analytes
  # Create list of unique 'antigen|feature'
  results_data$analyte_acc <- paste0(results_data$antigen, "|", coalesce_str(results_data$feature))
  unique_analytes <- unique(results_data$analyte_acc)
  
  ensure_analytes_exist(conn, unique_analytes, workspace_id)
  
  # 3. Insert Results
  inserted_count <- 0
  failed_count <- 0
  skipped_count <- 0
  
  # Preview storage
  preview_rows <- list()
  
  cat("  [INFO] Inserting results...\n")
  
  # Pre-calc assay_group_id
  assay_group_id <- experiment_accession

  # Build biosample_acc -> expsample_acc / subject_accession / arm_accession lookups
  biosample_to_expsample  <- list()
  biosample_to_subject    <- list()
  biosample_to_arm        <- list()
  for(key in names(expsample_map)) {
    entry <- expsample_map[[key]]
    if(!is.null(entry$biosample_accession) && !is.null(entry$expsample_accession)) {
      bs <- entry$biosample_accession
      biosample_to_expsample[[ bs ]] <- entry$expsample_accession
      if(!is.null(entry$subject_accession)) biosample_to_subject[[ bs ]] <- entry$subject_accession
      if(!is.null(entry$arm_accession))     biosample_to_arm[[     bs ]] <- entry$arm_accession
    }
  }

  # Loop
  total_rows <- nrow(results_data)
  prog_step <- max(1, floor(total_rows / 10))
  
  for(i in 1:total_rows) {
    if(i %% prog_step == 0) cat(paste0("    ... processed ", i, "/", total_rows, " rows\n"))
    
    row <- results_data[i, ]
    
    # Map Source Sample -> Target Expsample
    # biosample_map is keyed by sampleid_patientid (composite key from insert_biosamples)
    patient_id_str <- if(!is.null(row$patientid) && length(row$patientid) > 0 && !is.na(row$patientid[[1]])) as.character(row$patientid[[1]]) else ""
    sample_id_str <- as.character(row$sampleid[[1]])
    composite_key <- paste0(sample_id_str, "_", patient_id_str)
    biosample_acc <- biosample_map[[composite_key]]
    
    if(is.null(biosample_acc)) {
      skipped_count <- skipped_count + 1
      next 
    }
    
    expsample_acc <- biosample_to_expsample[[biosample_acc]]
    
    if(is.null(expsample_acc)) {
      skipped_count <- skipped_count + 1
      next 
    }
    
    sp_name <- paste0("mbaa_row_", i)
    tryCatch({
      suppressWarnings(DBI::dbExecute(conn, paste0("SAVEPOINT ", sp_name)))

      # Per-row value computations
      dilution_val <- if(length(row$dilution) > 0 && !is.na(row$dilution[[1]])) as.character(row$dilution[[1]]) else "1"
      plate_val <- if(!is.null(row$plate_id) && length(row$plate_id) > 0 && !is.na(row$plate_id[[1]])) as.character(row$plate_id[[1]]) else "UnknownPlate"
      nominal_dilution <- if("nominal_sample_dilution" %in% names(row) && length(row$nominal_sample_dilution) > 0 && !is.na(row$nominal_sample_dilution[[1]])) as.character(row$nominal_sample_dilution[[1]]) else dilution_val
      assay_id_val <- paste0(plate_val, "|", nominal_dilution)
      conc_val <- if(length(row$antibody_au) > 0 && !is.na(row$antibody_au[[1]])) as.character(row$antibody_au[[1]]) else NA_character_
      mfi_val <- if(length(row$antibody_mfi) > 0 && !is.na(row$antibody_mfi[[1]])) as.character(row$antibody_mfi[[1]]) else NA_character_
      well_val <- if("well" %in% names(row) && length(row$well) > 0 && !is.na(row$well[[1]])) as.character(row$well[[1]]) else NA_character_
      analyte_val <- row$analyte_acc[[1]]

      subject_acc_val  <- biosample_to_subject[[ biosample_acc ]]
      # Resolve arm via agroup_mapping (agroup col from xmap_sample) — biosample_to_arm is empty
      # because xmap_sample has `agroup`, not `arm_accession`
      agroup_val <- if("agroup" %in% names(row) && !is.null(row$agroup[[1]]) && !is.na(row$agroup[[1]])) as.character(row$agroup[[1]]) else NULL
      arm_acc_val <- if(!is.null(agroup_val) && !is.null(agroup_mapping)) agroup_mapping[[ agroup_val ]] else biosample_to_arm[[ biosample_acc ]]

      insert_q <- "
        INSERT INTO madi_dat.mbaa_result (
          experiment_accession, study_accession, workspace_id,
          source_type, source_accession,
          biosample_accession, subject_accession, arm_accession,
          analyte_accession, analyte_reported,
          assay_group_id, assay_id,
          concentration_unit_reported, concentration_value_reported,
          mfi, mfi_coordinate
        ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
      "

      DBI::dbExecute(conn, insert_q, params = list(
        experiment_accession, study_accession, workspace_id,
        "EXPSAMPLE", expsample_acc,
        biosample_acc,
        if(is.null(subject_acc_val)) NA_character_ else subject_acc_val,
        if(is.null(arm_acc_val))     NA_character_ else arm_acc_val,
        analyte_val, analyte_val,
        assay_group_id, assay_id_val,
        "AU", conc_val,
        mfi_val, well_val
      ))

      suppressWarnings(DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", sp_name)))
      inserted_count <- inserted_count + 1

      # Capture Preview (first 10)
      if(inserted_count <= 10) {
        preview_rows[[length(preview_rows) + 1]] <- list(
          SourceSample = sample_id_str,
          ExpSample = expsample_acc,
          Analyte = analyte_val,
          AssayID = assay_id_val,
          Conc = conc_val,
          MFI = mfi_val
        )
      }

    }, error = function(e) {
      tryCatch(
        suppressWarnings(DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", sp_name))),
        error = function(e2) {}
      )
      failed_count <<- failed_count + 1
    })
  }
  
  if(commit) {
    cat(paste0("  [OK] COMMITTED: ", inserted_count, " results inserted (", skipped_count, " skipped, ", failed_count, " failed)\n"))
  } else {
    cat(paste0("  [ROLLBACK] PROCESSED: ", inserted_count, " results (", skipped_count, " skipped, ", failed_count, " failed) - Test Mode\n"))
  }
  
  # Print Preview Table to Console/Log
  if(length(preview_rows) > 0) {
    cat("\n  👀 RESULT PREVIEW (First 10 Rows):\n")
    cat(sprintf("  %-15s %-20s %-30s %-20s %-10s %-10s\n", "SourceSample", "ExpSample", "Analyte", "AssayID", "Conc", "MFI"))
    cat("  ------------------------------------------------------------------------------------------------------------------\n")
    for(r in preview_rows) {
      cat(sprintf("  %-15s %-20s %-30.30s %-20s %-10s %-10s\n", 
                  r$SourceSample, r$ExpSample, r$Analyte, r$AssayID, 
                  coalesce_str(r$Conc), coalesce_str(r$MFI)))
    }
    cat("\n")
  }
  
  return(list(success = TRUE, inserted = inserted_count, failed = failed_count, preview = preview_rows))
}

# Helper
coalesce_str <- function(x) {
  ifelse(is.na(x) | x == "", "NA", x)
}
