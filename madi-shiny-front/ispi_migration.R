# Simple I-SPI Data Migration Module
# Starting small - just data preview and basic connection

# UI for Migration Module
output$migration_ui <- renderUI({
  fluidPage(
    style = "padding: 20px;",
    
    # Header
    div(
      style = "background: linear-gradient(135deg, #8e44ad, #9b59b6); padding: 20px; border-radius: 10px; margin-bottom: 20px; color: white;",
      div(style = "display: flex; justify-content: space-between; align-items: flex-start;",
        div(
          h2("I-SPI Data Migration", style = "margin: 0; color: white;"),
          p("Connect to I-SPI database and preview data before migration",
            style = "margin: 10px 0 0 0; color: #f8f9fa;")
        ),
        div(
          style = "text-align: right; font-size: 11px; color: rgba(255,255,255,0.75); font-family: monospace;",
          p(paste0("loaded: ", format(Sys.time(), "%Y-%m-%d %H:%M")), style = "margin: 0;")
        )
      )
    ),
    
    # Workspace Context Panel
    fluidRow(
      column(12,
        div(
          style = "background-color: #e8f5e9; border-left: 4px solid #4caf50; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
          div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            tags$i(class = "fa fa-folder-open", style = "font-size: 24px; color: #4caf50; margin-right: 15px;"),
            div(
              h4("Current Target Workspace", style = "margin: 0; color: #2e7d32;"),
              uiOutput("migration_workspace_display")
            )
          ),
          div(
            style = "background-color: #f1f8e9; padding: 10px; border-radius: 3px; margin-top: 10px;",
            p(
              tags$strong("Important:"), 
              " All migrated data will be associated with this workspace. Studies, subjects, and samples will only be visible to users who have access to this workspace.",
              style = "margin: 0; color: #33691e; font-size: 0.9em;"
            )
          )
        )
      )
    ),
    
    # I-SPI Authentication
    fluidRow(
      column(6,
        wellPanel(
          style = "background-color: #ffffff; border: 2px solid #8e44ad;",
          h4("I-SPI Authentication", style = "color: #8e44ad; margin-bottom: 15px;"),
          
          p("Enter your workspace credentials to access I-SPI data. Contact your administrator if you do not have access.",
            style = "color: #666; font-size: 0.85em; margin-bottom: 15px;"),
          
          numericInput("ispi_project_id", "Project / Workspace ID:", 
                      value = NULL, min = 1,
                      width = "100%"),
          
          textInput("ispi_access_key", "Access Key:",
                   placeholder = "Enter your workspace access key (UUID)",
                   width = "100%"),
          
          br(),
          actionButton("validate_access", "Validate Access",
                      class = "btn-primary", 
                      style = "background-color: #8e44ad; border-color: #8e44ad; width: 100%;"),
          
          br(), br(),
          uiOutput("access_status_display")
        )
      ),
      
      column(6,
        wellPanel(
          style = "background-color: #ffffff; border: 2px solid #16a085;",
          h4("Data Source Configuration", style = "color: #16a085; margin-bottom: 15px;"),
          
          selectInput("study_accession_filter", "Source Study (Filter):", 
                     choices = NULL, 
                     selectize = TRUE),
          
          selectInput("experiment_accession_filter", "Source Experiment:", 
                     choices = NULL,
                     selectize = TRUE),
          
          selectInput("preview_table", "Table to Preview:",
                     choices = list(
                       "Sample Data" = "xmap_sample",
                       "Subjects" = "xmap_subjects", 
                       "Controls" = "xmap_control",
                       "Standards" = "xmap_standard",
                       "Planned Visits" = "xmap_planned_visit",
                       "Standard Fits" = "xmap_standard_fits"
                     ),
                     selected = "xmap_sample"),
          
          # Optional row limit (default: load ALL rows for full experiment scope)
          checkboxInput("limit_rows", "Limit number of rows (for preview only)", value = FALSE),
          conditionalPanel(
            condition = "input.limit_rows",
            numericInput("preview_limit", "Max rows:", value = 200, min = 1, max = 100000)
          ),
          
          br(),
          actionButton("load_data_preview", "Load Data from Source",
                      class = "btn-success", 
                      style = "background-color: #16a085; border-color: #16a085; width: 100%;",
                      disabled = TRUE),
          

      )
    )
  ),
    
    # Connection Status
    fluidRow(
      column(12,
        wellPanel(
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
          h5("Connection Status", style = "color: #495057; margin-bottom: 10px;"),
          uiOutput("access_status_display_main")
        )
      )
    ),
    
    # Destination Mapping Section
    conditionalPanel(
      condition = "output.show_data_preview",
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #fff3cd; border: 2px solid #ffc107;",
            h4("Destination Mapping", style = "color: #856404; margin-bottom: 15px;"),
            p("Map I-SPI data to ImmunoPlex Data Port structure:", style = "color: #856404;"),
            
            fluidRow(
              column(6,
                h5("Experiment Settings", style = "color: #856404; margin-bottom: 10px;"),
                
                # Target Study Selection (Always Visible)
                uiOutput("target_study_ui"),
                
                # Measurement Technique - only Luminex supported currently
                selectInput("measurement_technique", "Measurement Technique:",
                           choices = c("Luminex" = "Luminex",
                                       "MBAA (Coming Soon)" = "__disabled_MBAA",
                                       "Flow Cytometry (Coming Soon)" = "__disabled_FC",
                                       "ELISA (Coming Soon)" = "__disabled_ELISA",
                                       "Other (Coming Soon)" = "__disabled_Other"),
                           selected = "Luminex"),
                tags$script(HTML("
                  $(document).on('shiny:inputchanged', function(e) {
                    if (e.name === 'measurement_technique' && e.value.startsWith('__disabled_')) {
                      Shiny.setInputValue('measurement_technique', 'Luminex');
                      alert('Only Luminex is currently supported. Other techniques coming soon.');
                    }
                  });
                ")),
                
                checkboxInput("create_new_experiment", "Create new experiment?", value = FALSE),
                
                conditionalPanel(
                  condition = "input.create_new_experiment == true",
                  textInput("new_experiment_accession", "New Accession:", placeholder = "EXP_..."),
                  textInput("new_experiment_name", "Name:", placeholder = "Experiment Name"),
                  textAreaInput("new_experiment_description", "Description:", placeholder = "Description...", rows = 2),
                  selectInput("new_experiment_tech", "Technique (Metadata):", choices = NULL)
                ),
                conditionalPanel(
                  condition = "input.create_new_experiment == false",
                  # uiOutput("target_study_ui") moved to top
                  selectInput("target_experiment", "Select Existing Experiment:",
                             choices = list("Select study first" = ""),
                             selected = NULL)
                )
              ),
              
              column(6,
                h5(icon("exchange-alt"), " Field Mapping", style = "color: #856404; margin-bottom: 10px;"),
                div(
                  style = "background-color: #fff8e1; padding: 12px; border-radius: 5px; border: 1px solid #ffe082;",
                  p(strong("I-SPI"), icon("arrow-right"), strong("ImmunoPlex Data Port"), 
                    style = "color: #856404; margin-bottom: 8px;"),
                  tags$table(
                    style = "width: 100%; font-size: 0.9em;",
                    tags$tr(
                      tags$td(tags$code("sampleid"), style = "padding: 4px;"),
                      tags$td(icon("long-arrow-alt-right"), style = "padding: 4px; text-align: center; color: #856404;"),
                      tags$td(tags$code("expsample_accession"), style = "padding: 4px;"),
                      tags$td(tags$span("Auto", class = "badge", style = "background-color: #28a745; color: white; font-size: 0.75em;"), style = "padding: 4px;")
                    ),
                    tags$tr(
                      tags$td(tags$code("patientid"), style = "padding: 4px;"),
                      tags$td(icon("long-arrow-alt-right"), style = "padding: 4px; text-align: center; color: #856404;"),
                      tags$td(tags$code("subject_accession"), style = "padding: 4px;"),
                      tags$td(tags$span("Mapped", class = "badge", style = "background-color: #17a2b8; color: white; font-size: 0.75em;"), style = "padding: 4px;")
                    ),
                    tags$tr(
                      tags$td(tags$code("timeperiod"), style = "padding: 4px;"),
                      tags$td(icon("long-arrow-alt-right"), style = "padding: 4px; text-align: center; color: #856404;"),
                      tags$td(tags$code("planned_visit_accession"), style = "padding: 4px;"),
                      tags$td(tags$span("Manual", class = "badge", style = "background-color: #ffc107; color: #333; font-size: 0.75em;"), style = "padding: 4px;")
                    ),
                    tags$tr(
                      tags$td(tags$code("agroup"), style = "padding: 4px;"),
                      tags$td(icon("long-arrow-alt-right"), style = "padding: 4px; text-align: center; color: #856404;"),
                      tags$td(tags$code("arm_accession"), style = "padding: 4px;"),
                      tags$td(tags$span("Manual", class = "badge", style = "background-color: #ffc107; color: #333; font-size: 0.75em;"), style = "padding: 4px;")
                    )
                  )
                ),
                br(),
                h6("Lookup Table Assignments", style = "color: #856404;"),
                p("These help with creating new records:", style = "font-size: 12px; color: #6c757d;"),
                uiOutput("default_arm_type_ui"),
                uiOutput("default_time_unit_ui")
              )
            ),
            
            # Advice Panel
            fluidRow(
              column(12,
                tags$details(
                  style = "margin-top: 15px; background-color: #fff; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                  tags$summary(strong("Read Me: Handling Multiple Antigens/Assays", style = "color: #0056b3; cursor: pointer;")),
                  tags$div(
                    style = "margin-top: 10px; font-size: 0.9em; color: #495057;",
                    tags$ul(style = "padding-left: 20px;",
                      tags$li(strong("Source IDs are Reused:"), " Sample IDs (e.g., '1') are reused across different experiments (IgG1, ADCD) in the source."),
                      tags$li(strong("Avoid Merging:"), " If you keep the default 'Sample Prefix' (ES_), these will merge into a single sample record, sharing metadata (dilution)."),
                      tags$li(strong("Recommendation:"), " Change the Sample Prefix (e.g., ES_IgG1_, ES_ADCD_) for each run to ensure unique samples."),
                      tags$li("Biosamples (Subjects) are correctly reused automatically.")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # Data Assignment & Manual Overrides
    conditionalPanel(
      condition = "output.show_data_preview",
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #d1ecf1; border: 1px solid #bee5eb;",
            h5("Data Assignment & Manual Overrides", style = "color: #0c5460; margin-bottom: 10px;"),
            p("Fine-tune assignments for specific data:", style = "color: #0c5460;"),
            
            fluidRow(
              column(3,
                textInput("workspace_id_override", "Workspace ID Override:", 
                         placeholder = "Leave blank to use current")
              ),
              column(4,
                textInput("sample_prefix", "Sample Accession Prefix:", 
                          placeholder = "e.g., ES_",
                          value = paste0("ES_", format(Sys.Date(), "%y%m%d"), "_")),
                tags$p(HTML("Note: Change prefix per antigen/assay to avoid merging samples.<br/><strong>⚠ Prefix + sample number must be ≤ 15 characters total</strong> (e.g. prefix <code>ES_260220_</code> + number <code>12</code> = 12 chars ✓)."),
                       style = "color: #721c24; font-size: 0.85em; margin-top: -10px; font-style: italic;")
              ),
              column(4,
                selectInput("default_species", "Default Subject Species:",
                           choices = c("Homo sapiens", "Chicken", "Chimpanzee", "dog", "domestic ferret", "domestic pig", "Fruit Fly", "Green monkey", "Macaca fascicularis", "Mallard duck"),
                           selected = "Homo sapiens")
              ),
              column(3,
                uiOutput("result_schema_ui")
              )
            ),
            
            fluidRow(
              column(3,
                textInput("biosample_time_unit", "Biosample Collection Time Unit:", 
                         value = "Days",
                         placeholder = "e.g., Days, Weeks")
              ),
              column(3,
                textInput("biosample_t0_event", "Biosample T0 Event:", 
                         value = "Time of enrollment",
                         placeholder = "e.g., Time of enrollment")
              )
            ),
            
            # Timeperiod to Planned Visit Mapping
            hr(),
            h5("Time Period → Planned Visit Mapping", style = "color: #0c5460; margin-bottom: 10px;"),
            p("Map source time period values to target planned visits:", style = "color: #6c757d; font-size: 0.9em;"),
            
            fluidRow(
              column(12,
                div(
                  style = "background-color: #ffffff; padding: 15px; border-radius: 5px; border: 1px solid #17a2b8;",
                  uiOutput("timeperiod_mapping_ui")
                )
              )
            ),
            
            # Agroup to Arm/Cohort Mapping
            hr(),
            h5("Agroup / Arm/Cohort Mapping", style = "color: #0c5460; margin-bottom: 10px;"),
            p("Map source agroup values to target arms/cohorts:", style = "color: #6c757d; font-size: 0.9em;"),
            
            fluidRow(
              column(12,
                div(
                  style = "background-color: #ffffff; padding: 15px; border-radius: 5px; border: 1px solid #17a2b8;",
                  uiOutput("agroup_mapping_ui")
                )
              )
            ),
            
            # Patient to Subject Mapping
            hr(),
            h5("Patient ID → Subject Accession Mapping", style = "color: #0c5460; margin-bottom: 10px;"),
            p("Map I-SPI patient IDs to existing subjects in the Data Portal:", style = "color: #6c757d; font-size: 0.9em;"),
            div(
              style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
              p(
                icon("info-circle"), 
                strong(" Auto-mapping: "), 
                "If xmap_subjects table has subject_accession values, they will be used automatically. ",
                "Otherwise, manually select subjects below.",
                style = "margin: 0; color: #0c5460; font-size: 0.9em;"
              )
            ),
            
            fluidRow(
              column(12,
                div(
                  style = "background-color: #ffffff; padding: 15px; border-radius: 5px; border: 1px solid #17a2b8;",
                  uiOutput("patient_mapping_ui")
                )
              )
            )
          )
        )
      )
    ),
    
    # Data Preview Section
    fluidRow(
      column(12,
        conditionalPanel(
          condition = "output.show_data_preview",
          wellPanel(
            style = "background-color: #ffffff; border: 2px solid #17a2b8;",
            h4("Data Preview", style = "color: #17a2b8; margin-bottom: 15px;"),
            
            div(
              style = "margin-bottom: 15px;",
              tags$div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                tags$span(textOutput("preview_info"), style = "color: #6c757d;"),
                actionButton("save_to_target", "Save Selected Data to Target",
                            class = "btn-warning", 
                            style = "background-color: #f39c12; border-color: #f39c12;",
                            disabled = TRUE)
              )
            ),
            
            # Toggle for data preview table
            checkboxInput("show_preview_table", "Show data preview table", value = FALSE),
            conditionalPanel(
              condition = "input.show_preview_table",
              DT::dataTableOutput("data_preview_table")
            )
          )
        )
      )
    )
  )
})

# Server logic for migration
# Connection status reactive value
migration_values <- reactiveValues(
  source_connected = FALSE,
  connection_message = "Not connected",
  preview_data = NULL
)

# Reactive value for migration log download
migration_log_text <- reactiveVal(NULL)

# Reactive value for DQ audit report download
migration_dq_report <- reactiveVal(NULL)

# Workspace Display for Migration Page
output$migration_workspace_display <- renderUI({
  ws_id <- current_workspace()
  
  if(is.null(ws_id)) {
    return(p("No workspace selected", style = "color: #d32f2f; font-weight: bold; margin: 5px 0 0 0;"))
  }
  
  # Get workspace details from database
  tryCatch({
    workspace_info <- DBI::dbGetQuery(conn,
      "SELECT workspace_id, workspace_name, category 
       FROM madi_dat.workspace 
       WHERE workspace_id = $1",
      params = list(ws_id)
    )
    
    if(nrow(workspace_info) > 0) {
      div(
        p(
          tags$strong(workspace_info$workspace_name), 
          tags$span(
            paste0(" (ID: ", workspace_info$workspace_id, ")"),
            style = "color: #666; font-size: 0.9em;"
          ),
          style = "margin: 5px 0; font-size: 1.1em; color: #1b5e20;"
        ),
        p(
          tags$span("Category: ", style = "color: #666;"),
          workspace_info$category,
          style = "margin: 0; font-size: 0.9em; color: #2e7d32;"
        )
      )
    } else {
      p(paste0("Workspace ID: ", ws_id), style = "color: #1b5e20; font-weight: bold; margin: 5px 0 0 0;")
    }
  }, error = function(e) {
    p(paste0("Workspace ID: ", ws_id), style = "color: #1b5e20; font-weight: bold; margin: 5px 0 0 0;")
  })
})


# Populate target database dropdowns
# Populate target database dropdowns
# Using renderUI ensures they are updated whenever the workspace changes
# AND when the UI element is actually rendered (fixing the 'Loading...' issue)

# 1. Default Arm Type - Render UI
output$default_arm_type_ui <- renderUI({
  # Get current user's workspace ID from reactive value
  ws_id <- current_workspace()
  
  # Default fallback UI
  fallback <- selectInput("default_arm_type", "Default Arm Type (for new arms):",
                         choices = list("Loading..." = ""),
                         selected = NULL)
  
  if (is.null(ws_id)) return(fallback)
  
  # Check connection
  if(!exists("conn") || !DBI::dbIsValid(conn)) return(fallback)
  
  tryCatch({
    arm_types <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_arm_type ORDER BY name")
    
    if(nrow(arm_types) > 0) {
      selectInput("default_arm_type", "Default Arm Type (for new arms):",
                 choices = arm_types$name, 
                 selected = "Observational")
    } else {
      selectInput("default_arm_type", "Default Arm Type (for new arms):",
                 choices = list("No arm types found" = ""),
                 selected = NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to load arm types:", e$message))
    fallback
  })
})

# 1b. Result Schema Type - Render UI
output$result_schema_ui <- renderUI({
  # Get current user's workspace ID from reactive value
  ws_id <- current_workspace()
  
  # Default fallback UI
  fallback <- selectInput("result_schema_type", "Expsample Result Schema:",
                         choices = list("Loading..." = ""),
                         selected = NULL)
  
  if (is.null(ws_id)) return(fallback)
  
  # Check connection
  if(!exists("conn") || !DBI::dbIsValid(conn)) return(fallback)
  
  tryCatch({
    result_schemas <- DBI::dbGetQuery(conn, "SELECT result_schema FROM madi_dat.lk_result_schema ORDER BY result_schema")
    
    if(nrow(result_schemas) > 0) {
      # Build choices: MBAA is available, others are tagged as unavailable
      schema_choices <- sapply(result_schemas$result_schema, function(s) {
        if(toupper(s) == "MBAA") s
        else paste0("__disabled_", s)
      })
      names(schema_choices) <- sapply(result_schemas$result_schema, function(s) {
        if(toupper(s) == "MBAA") s
        else paste0(s, " (Unavailable)")
      })
      
      tagList(
        selectInput("result_schema_type", "Expsample Result Schema:",
                   choices = schema_choices, 
                   selected = "MBAA"),
        tags$script(HTML("
          $(document).on('shiny:inputchanged', function(e) {
            if (e.name === 'result_schema_type' && e.value.startsWith('__disabled_')) {
              Shiny.setInputValue('result_schema_type', 'MBAA');
              alert('Only MBAA is currently supported for I-SPI migration.');
            }
          });
        "))
      )
    } else {
      selectInput("result_schema_type", "Expsample Result Schema:",
                 choices = list("No schemas found" = ""),
                 selected = NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to load result schemas:", e$message))
    fallback
  })
})


# 2. Default Time Unit - Render UI
output$default_time_unit_ui <- renderUI({
  # Get current user's workspace ID from reactive value
  ws_id <- current_workspace()
  
  # Default fallback UI
  fallback <- selectInput("default_time_unit", "Default Time Unit (for planned visits):",
                         choices = list("Loading..." = ""),
                         selected = NULL)
  
  if (is.null(ws_id)) return(fallback)
  
  # Check connection
  if(!exists("conn") || !DBI::dbIsValid(conn)) return(fallback)
  
  tryCatch({
    time_units <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit ORDER BY name")
    
    if(nrow(time_units) > 0) {
      selectInput("default_time_unit", "Default Time Unit (for planned visits):",
                 choices = time_units$name, 
                 selected = "Days")
    } else {
      selectInput("default_time_unit", "Default Time Unit (for planned visits):",
                 choices = list("No time units found" = ""),
                 selected = NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to load time units:", e$message))
    fallback
  })
})

# 3. Target Study - Render UI (Fixing the "Loading..." issue)
output$target_study_ui <- renderUI({
  # Get current user's workspace ID from reactive value
  ws_id <- current_workspace()
  
  # Default fallback UI
  fallback <- selectInput("target_study", "Select Target Study:",
                         choices = list("Loading studies..." = ""),
                         selected = NULL)
  
  print(paste("DEBUG: target_study_ui rendering for workspace:", if(is.null(ws_id)) "NULL" else ws_id))
  
  if (is.null(ws_id)) return(fallback)
  
  # Check connection
  if(!exists("conn") || !DBI::dbIsValid(conn)) return(fallback)
  
  tryCatch({
    # Populate target studies
    target_studies <- DBI::dbGetQuery(conn, 
      "SELECT study_accession, brief_title FROM madi_dat.study WHERE workspace_id = $1 ORDER BY study_accession", 
      params = list(ws_id))
      
    print(paste("DEBUG: Found", nrow(target_studies), "studies for dropdown"))
      
    if(nrow(target_studies) > 0) {
      study_choices <- setNames(target_studies$study_accession, 
                               paste(target_studies$study_accession, "-", substring(target_studies$brief_title, 1, 50)))
      selectInput("target_study", "Select Target Study:",
                 choices = study_choices)
    } else {
      selectInput("target_study", "Select Target Study:",
                 choices = list("No studies in workspace" = ""),
                 selected = NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to load studies:", e$message))
    fallback
  })
})

# Update experiments when target study changes
observeEvent(input$target_study, {
  req(input$target_study)
  
  tryCatch({
    if(exists("conn") && DBI::dbIsValid(conn)) {
      print(paste("DEBUG: Loading experiments for study:", input$target_study))
      
      experiments <- DBI::dbGetQuery(conn, 
        "SELECT experiment_accession, name FROM madi_dat.experiment WHERE study_accession = $1 ORDER BY experiment_accession", 
        params = list(input$target_study))
      
      print(paste("DEBUG: Found", nrow(experiments), "experiments for study", input$target_study))
      
      if(nrow(experiments) > 0) {
        exp_choices <- setNames(experiments$experiment_accession, 
                               paste(experiments$experiment_accession, "-", substring(experiments$name, 1, 40)))
        updateSelectInput(session, "target_experiment", choices = exp_choices)
        print("DEBUG: Updated target_experiment dropdown")
      } else {
        updateSelectInput(session, "target_experiment", choices = list("No experiments found" = ""))
        print("DEBUG: No experiments found for study")
      }
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to load experiments:", e$message))
  })
})

# Auto-populate New Experiment fields when checkbox is checked
observeEvent(input$create_new_experiment, {
  req(input$create_new_experiment)
  if(!exists("conn") || !DBI::dbIsValid(conn)) return()
  
  print("DEBUG: Create New Experiment checked - generating defaults...")
  
  # 1. Populate Measurement Techniques
  tryCatch({
    meas_techs <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_exp_measurement_tech ORDER BY name")
    print(paste("DEBUG: Found", nrow(meas_techs), "measurement techniques"))
    
    if(nrow(meas_techs) > 0) {
      default_tech <- "Multiplex Bead Array Assay"
      if(!default_tech %in% meas_techs$name) default_tech <- meas_techs$name[1]
      
      updateSelectInput(session, "new_experiment_tech", choices = meas_techs$name, selected = default_tech)
    }
  }, error = function(e) {
    print(paste("ERROR loading measurement techniques:", e$message))
  })
  
  # 2. Auto-generate Experiment ID
  tryCatch({
    # Only generate if current value is empty or default
    current_val <- input$new_experiment_accession
    if(is.null(current_val) || current_val == "") {
      
      max_exp <- DBI::dbGetQuery(conn, "SELECT MAX(experiment_accession) as max_id FROM madi_dat.experiment WHERE experiment_accession LIKE 'EXP%'")
      next_id <- "EXP00001"
      
      if(nrow(max_exp) > 0 && !is.na(max_exp$max_id)) {
         num_part <- sub("^EXP", "", max_exp$max_id)
         if(grepl("^[0-9]+$", num_part)) {
           current_num <- as.numeric(num_part)
           next_id <- sprintf("EXP%05d", current_num + 1)
         }
      }
      
      updateTextInput(session, "new_experiment_accession", value = next_id)
      print(paste("DEBUG: Auto-generated ID:", next_id))
    }
  }, error = function(e) {
    print(paste("ERROR generating ID:", e$message))
  })
})

# Load planned visits when target study changes and generate timeperiod mapping UI
output$timeperiod_mapping_ui <- renderUI({
  print(paste("DEBUG MAPPING: timeperiod_mapping_ui triggered. target_study =", 
              if(is.null(input$target_study)) "NULL" else input$target_study,
              ", preview_data rows =", 
              if(is.null(migration_values$preview_data)) "NULL" else nrow(migration_values$preview_data)))
  req(input$target_study)
  req(migration_values$preview_data)
  print(paste("DEBUG MAPPING: timeperiod_mapping_ui passed req() checks"))
  
  tryCatch({
    if(exists("conn") && DBI::dbIsValid(conn)) {
      print(paste("DEBUG: Loading planned visits for study:", input$target_study))
      
      # Get planned visits for the selected study
      planned_visits <- DBI::dbGetQuery(conn, 
        "SELECT planned_visit_accession, name, min_start_day, max_start_day, order_number 
         FROM madi_dat.planned_visit 
         WHERE study_accession = $1 
         ORDER BY order_number", 
        params = list(input$target_study))
      
      print(paste("DEBUG: Found", nrow(planned_visits), "planned visits"))
      
      if(nrow(planned_visits) == 0) {
        return(div(
          style = "padding: 15px; background-color: #fff3cd; border-radius: 5px;",
          p("No planned visits found for this study. Please create planned visits in the target study first.", 
            style = "color: #856404; margin: 0;")
        ))
      }
      
      # Get unique timeperiod values from preview data
      unique_timeperiods <- unique(migration_values$preview_data$timeperiod)
      unique_timeperiods <- unique_timeperiods[!is.na(unique_timeperiods)]
      
      if(length(unique_timeperiods) == 0) {
        return(div(
          style = "padding: 15px; background-color: #d1ecf1; border-radius: 5px;",
          p("No timeperiod values found in preview data.", 
            style = "color: #0c5460; margin: 0;")
        ))
      }
      
      # Create mapping dropdowns for each unique timeperiod
      # Format planned visit choices to show helpful info
      pv_choices <- setNames(
        planned_visits$planned_visit_accession,
        paste0("#", planned_visits$order_number, ": ",
               planned_visits$planned_visit_accession, " - ", 
               planned_visits$name, 
               " (Days: ", planned_visits$min_start_day, "-", planned_visits$max_start_day, ")")
      )
      
      # Add option to create new planned visit
      pv_choices <- c("Create New Planned Visit" = "NEW", pv_choices)
      
      # Generate UI for each timeperiod
      mapping_uis <- lapply(unique_timeperiods, function(tp) {
        fluidRow(
          column(4,
            div(
              style = "padding: 10px; background-color: #f8f9fa; border-radius: 3px; margin-bottom: 10px;",
              strong("Source: ", style = "color: #495057;"),
              tags$code(tp, style = "background-color: #e9ecef; padding: 2px 6px; border-radius: 3px;")
            )
          ),
          column(1,
            div(
              style = "padding: 10px; text-align: center;",
              tags$i(class = "fa fa-arrow-right", style = "color: #17a2b8; font-size: 1.2em;")
            )
          ),
          column(7,
            selectInput(
              inputId = paste0("timeperiod_map_", gsub("[^[:alnum:]]", "_", tp)),
              label = NULL,
              choices = pv_choices,
              selected = NULL,
              width = "100%"
            )
          )
        )
      })
      
      return(tagList(
        div(
          style = "margin-bottom: 10px;",
          p(strong("Map ", length(unique_timeperiods), " unique timeperiod values:"), 
            style = "color: #0c5460; margin: 0;")
        ),
        mapping_uis,
        hr(),
        div(
          style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(strong("Tip:"), " Choose existing planned visits based on the time range, or select 'Create New Planned Visit' to create them during migration.", 
            style = "color: #155724; margin: 0; font-size: 0.9em;")
        )
      ))
      
    } else {
      return(NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to generate timeperiod mapping UI:", e$message))
    return(div(
      style = "padding: 15px; background-color: #f8d7da; border-radius: 5px;",
      p(paste("Error loading planned visits:", e$message), 
        style = "color: #721c24; margin: 0;")
    ))
  })
})

# Load arms/cohorts when target study changes and generate agroup mapping UI
output$agroup_mapping_ui <- renderUI({
  print(paste("DEBUG MAPPING: agroup_mapping_ui triggered. target_study =", 
              if(is.null(input$target_study)) "NULL" else input$target_study,
              ", preview_data rows =", 
              if(is.null(migration_values$preview_data)) "NULL" else nrow(migration_values$preview_data)))
  req(input$target_study)
  req(migration_values$preview_data)
  print(paste("DEBUG MAPPING: agroup_mapping_ui passed req() checks"))
  
  tryCatch({
    if(exists("conn") && DBI::dbIsValid(conn)) {
      print(paste("DEBUG: Loading arms/cohorts for study:", input$target_study))
      
      # Get arms/cohorts for the selected study
      arms <- DBI::dbGetQuery(conn, 
        "SELECT arm_accession, name, description, type_reported, type_preferred 
         FROM madi_dat.arm_or_cohort 
         WHERE study_accession = $1 
         ORDER BY arm_accession", 
        params = list(input$target_study))
      
      print(paste("DEBUG: Found", nrow(arms), "arms/cohorts"))
      
      if(nrow(arms) == 0) {
        return(div(
          style = "padding: 15px; background-color: #fff3cd; border-radius: 5px;",
          p("No arms/cohorts found for this study. Please create arms/cohorts in the target study first.", 
            style = "color: #856404; margin: 0;")
        ))
      }
      
      # Get unique agroup values from preview data
      unique_agroups <- unique(migration_values$preview_data$agroup)
      unique_agroups <- unique_agroups[!is.na(unique_agroups)]
      
      if(length(unique_agroups) == 0) {
        return(div(
          style = "padding: 15px; background-color: #d1ecf1; border-radius: 5px;",
          p("No agroup values found in preview data.", 
            style = "color: #0c5460; margin: 0;")
        ))
      }
      
      # Create mapping dropdowns for each unique agroup
      # Format arm choices to show helpful info
      arm_choices <- setNames(
        arms$arm_accession,
        paste0(arms$arm_accession, " - ", 
               arms$name, 
               " (Type: ", ifelse(is.na(arms$type_preferred), arms$type_reported, arms$type_preferred), ")",
               ifelse(!is.na(arms$description) & nzchar(arms$description), 
                      paste0(" - ", substr(arms$description, 1, 50)), 
                      ""))
      )
      
      # Add option to create new arm/cohort
      arm_choices <- c("Create New Arm/Cohort" = "NEW", arm_choices)
      
      # Generate UI for each agroup
      mapping_uis <- lapply(unique_agroups, function(ag) {
        fluidRow(
          column(4,
            div(
              style = "padding: 10px; background-color: #f8f9fa; border-radius: 3px; margin-bottom: 10px;",
              strong("Source: ", style = "color: #495057;"),
              tags$code(ag, style = "background-color: #e9ecef; padding: 2px 6px; border-radius: 3px;")
            )
          ),
          column(1,
            div(
              style = "padding: 10px; text-align: center;",
              tags$i(class = "fa fa-arrow-right", style = "color: #17a2b8; font-size: 1.2em;")
            )
          ),
          column(7,
            selectInput(
              inputId = paste0("agroup_map_", gsub("[^[:alnum:]]", "_", ag)),
              label = NULL,
              choices = arm_choices,
              selected = NULL,
              width = "100%"
            )
          )
        )
      })
      
      return(tagList(
        div(
          style = "margin-bottom: 10px;",
          p(strong("Map ", length(unique_agroups), " unique agroup values:"), 
            style = "color: #0c5460; margin: 0;")
        ),
        mapping_uis,
        hr(),
        div(
          style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(strong("Tip:"), " Choose existing arms/cohorts based on type and description, or select 'Create New Arm/Cohort' to create them during migration.", 
            style = "color: #155724; margin: 0; font-size: 0.9em;")
        )
      ))
      
    } else {
      return(NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to generate agroup mapping UI:", e$message))
    return(div(
      style = "padding: 15px; background-color: #f8d7da; border-radius: 5px;",
      p(paste("Error loading arms/cohorts:", e$message), 
        style = "color: #721c24; margin: 0;")
    ))
  })
})

# Load subjects when target study changes and generate patient mapping UI
output$patient_mapping_ui <- renderUI({
  print(paste("DEBUG MAPPING: patient_mapping_ui triggered. target_study =", 
              if(is.null(input$target_study)) "NULL" else input$target_study,
              ", preview_data rows =", 
              if(is.null(migration_values$preview_data)) "NULL" else nrow(migration_values$preview_data)))
  req(input$target_study)
  req(migration_values$preview_data)
  print(paste("DEBUG MAPPING: patient_mapping_ui passed req() checks"))
  
  tryCatch({
    if(exists("conn") && DBI::dbIsValid(conn)) {
      print(paste("DEBUG: Loading subjects for study:", input$target_study))
      
      # Get subjects for the selected study (via arm_2_subject junction table)
      subjects <- DBI::dbGetQuery(conn, 
        "SELECT DISTINCT s.subject_accession, s.gender, s.species, s.description
         FROM madi_dat.subject s
         JOIN madi_dat.arm_2_subject a2s ON s.subject_accession = a2s.subject_accession
         JOIN madi_dat.arm_or_cohort ac ON a2s.arm_accession = ac.arm_accession
         WHERE ac.study_accession = $1
         ORDER BY s.subject_accession", 
        params = list(input$target_study))
      
      print(paste("DEBUG: Found", nrow(subjects), "subjects for study"))
      
      # Get unique patient IDs from preview data
      unique_patients <- unique(migration_values$preview_data$patientid)
      unique_patients <- unique_patients[!is.na(unique_patients)]
      
      if(length(unique_patients) == 0) {
        return(div(
          style = "padding: 15px; background-color: #d1ecf1; border-radius: 5px;",
          p("No patient IDs found in preview data.", 
            style = "color: #0c5460; margin: 0;")
        ))
      }
      
      # EARLY EXIT: If no subjects exist in study, show clear message
      if(nrow(subjects) == 0) {
        return(div(
          style = "padding: 15px; background-color: #f8d7da; border-radius: 5px;",
          div(
            p(
              icon("exclamation-triangle"),
              strong(" No Subjects Found"),
              style = "color: #721c24; font-size: 1.1em; margin: 0 0 10px 0;"
            ),
            p(
              "Study ", tags$code(input$target_study), " has no subjects yet.",
              style = "color: #721c24; margin: 5px 0;"
            ),
            p(
              strong("Required action: "),
              "Create subjects for this study in the Data Portal before migrating I-SPI data.",
              style = "color: #721c24; margin: 5px 0;"
            ),
            hr(style = "border-color: #f5c6cb;"),
            p(
              icon("lightbulb"),
              strong(" Tip: "),
              "Subjects must be created and linked to study arms before migration. ",
              "Use the Subject management section in the Data Portal to create them.",
              style = "color: #856404; margin: 5px 0; font-size: 0.9em;"
            )
          )
        ))
      }
      
      # Create a set of valid subject_accessions from the target study for quick lookup
      valid_subjects <- tryCatch({
        # Use an environment for O(1) lookup and avoiding subscript errors
        env <- new.env(hash = TRUE)
        if(nrow(subjects) > 0) {
          for(s in subjects$subject_accession) {
            assign(s, TRUE, envir = env)
          }
        }
        env
      }, error = function(e) {
        new.env()
      })
      
      # VALIDATE mappings from xmap_subjects against target study
      patients_valid_mapping <- list()      # Has mapping AND exists in study
      patients_invalid_mapping <- list()   # Has mapping BUT doesn't exist in study
      patients_no_mapping <- c()            # No mapping in xmap_subjects
      
      for(pid in unique_patients) {
        tryCatch({
          patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
          subject_acc <- patient_rows$subject_accession[1]
          
          if(!is.null(subject_acc) && !is.na(subject_acc) && nchar(subject_acc) > 0) {
            # Safe lookup using environment
            if(exists(subject_acc, envir = valid_subjects, inherits = FALSE)) {
              # Valid - subject exists in target study
              patients_valid_mapping[[as.character(pid)]] <- subject_acc
            } else {
              # Invalid - subject doesn't exist in target study (or wrong study)
              patients_invalid_mapping[[as.character(pid)]] <- subject_acc
            }
          } else {
            # No mapping in xmap_subjects
            patients_no_mapping <- c(patients_no_mapping, pid)
          }
        }, error = function(e) {
          # If validation fails for this patient, treat as no mapping
          print(paste("WARN: Failed to validate patient", pid, ":", e$message))
          patients_no_mapping <<- c(patients_no_mapping, pid)
        })
      }
      
      # Build status UI with validation results
      status_uis <- list()
      
      if(length(patients_valid_mapping) > 0) {
        status_uis[[length(status_uis) + 1]] <- div(
          style = "padding: 10px; background-color: #d4edda; border-radius: 5px; margin-bottom: 10px;",
          p(
            icon("check-circle"), 
            strong(" Valid: "), 
            length(patients_valid_mapping), " patients have valid subject mappings in this study.",
            style = "color: #155724; margin: 0;"
          ),
          div(
            style = "margin-top: 5px;",
            actionButton("show_valid_mappings", "View Valid Mappings", 
                        class = "btn-sm btn-success", 
                        style = "margin-right: 5px;"),
            actionButton("edit_all_mappings", "Edit All Mappings", 
                        class = "btn-sm btn-warning")
          )
        )
      }
      
      if(length(patients_invalid_mapping) > 0) {
        invalid_list <- paste(
          sapply(names(patients_invalid_mapping), function(pid) {
            paste0("Patient ", pid, " → ", patients_invalid_mapping[[pid]])
          }),
          collapse = ", "
        )
        status_uis[[length(status_uis) + 1]] <- div(
          style = "padding: 10px; background-color: #f8d7da; border-radius: 5px; margin-bottom: 10px;",
          p(
            icon("exclamation-triangle"), 
            strong(" Invalid: "), 
            length(patients_invalid_mapping), " patients have mappings to subjects NOT in study ", 
            tags$code(input$target_study),
            style = "color: #721c24; margin: 0;"
          ),
          p(invalid_list, style = "color: #721c24; font-size: 0.85em; margin: 5px 0 0 0;")
        )
      }
      
      if(length(patients_no_mapping) > 0) {
        status_uis[[length(status_uis) + 1]] <- div(
          style = "padding: 10px; background-color: #fff3cd; border-radius: 5px; margin-bottom: 10px;",
          p(
            icon("info-circle"), 
            strong(" Missing: "), 
            length(patients_no_mapping), " patients have no subject_accession in xmap_subjects.",
            style = "color: #856404; margin: 0;"
          )
        )
      }
      
      # Patients needing manual mapping = invalid + no mapping
      patients_needing_mapping <- c(names(patients_invalid_mapping), patients_no_mapping)
      
      # If user clicked "Edit All Mappings", show dropdowns for ALL patients
      if(!is.null(migration_values$edit_all_patient_mappings) && migration_values$edit_all_patient_mappings) {
        patients_needing_mapping <- unique_patients  # Override to show ALL patients
        
        # Add note about edit mode
        status_uis[[length(status_uis) + 1]] <- div(
          style = "padding: 10px; background-color: #d1ecf1; border-radius: 5px; margin-bottom: 10px;",
          p(
            icon("edit"), 
            strong(" Edit Mode: "), 
            "Manual mapping enabled for ALL ", length(unique_patients), " patients.",
            style = "color: #0c5460; margin: 0;"
          ),
          actionButton("cancel_edit_all", "Cancel Edit Mode", 
                      class = "btn-sm btn-secondary", 
                      style = "margin-top: 5px;")
        )
      }
      
      # If all patients have VALID mappings AND not in edit mode, show summary only
      if(length(patients_needing_mapping) == 0) {
        return(tagList(
          status_uis,
          div(
            style = "padding: 15px; background-color: #d1ecf1; border-radius: 5px;",
            p(strong("All patients validated!"), " All mappings are correct for this study.", 
              style = "color: #0c5460; margin: 0;")
          )
        ))
      }
      
      # Format subject choices to show helpful info (we already checked subjects exist above)
      subject_choices <- setNames(
        subjects$subject_accession,
        paste0(subjects$subject_accession, " - ", 
               ifelse(!is.na(subjects$gender), paste0(subjects$gender, ", "), ""),
               ifelse(!is.na(subjects$species), subjects$species, "Unknown"),
               ifelse(!is.na(subjects$description) & nzchar(subjects$description), 
                      paste0(" - ", substr(subjects$description, 1, 40)), 
                      ""))
      )
      
      # Add placeholder option
      subject_choices <- c("Select subject..." = "", subject_choices)
      
      # Generate UI for each patient needing mapping (invalid + no mapping OR all if edit mode)
      mapping_uis <- lapply(patients_needing_mapping, function(pid) {
        # Get current mapping from xmap_subjects
        patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
        current_subject <- patient_rows$subject_accession[1]
        has_current <- !is.null(current_subject) && !is.na(current_subject) && nchar(current_subject) > 0
        
        # Check if this patient has invalid mapping
        invalid_subject <- patients_invalid_mapping[[as.character(pid)]]
        has_invalid <- !is.null(invalid_subject)
        
        # Check if this patient has valid mapping
        valid_subject <- patients_valid_mapping[[as.character(pid)]]
        has_valid <- !is.null(valid_subject)
        
        # Set background color and label based on status
        if(has_valid) {
          bg_color <- "#d4edda"
          text_color <- "#155724"
          label_text <- paste0("Patient ID ", pid, " (Current: ", valid_subject, ")")
          pre_selected <- valid_subject
        } else if(has_invalid) {
          bg_color <- "#f8d7da"
          text_color <- "#721c24"
          label_text <- paste0("Patient ID ", pid, " (Invalid: ", invalid_subject, ")")
          pre_selected <- ""
        } else {
          bg_color <- "#fff3cd"
          text_color <- "#856404"
          label_text <- paste0("Patient ID ", pid)
          pre_selected <- ""
        }
        
        fluidRow(
          column(4,
            div(
              style = paste0("padding: 10px; background-color: ", bg_color, "; border-radius: 3px; margin-bottom: 10px;"),
              strong(label_text, style = paste0("color: ", text_color, ";"))
            )
          ),
          column(1,
            div(
              style = "padding: 10px; text-align: center;",
              tags$i(class = "fa fa-arrow-right", style = "color: #17a2b8; font-size: 1.2em;")
            )
          ),
          column(7,
            selectInput(
              inputId = paste0("patient_map_", gsub("[^[:alnum:]]", "_", pid)),
              label = NULL,
              choices = subject_choices,
              selected = pre_selected,
              width = "100%"
            )
          )
        )
      })
      
      return(tagList(
        status_uis,
        div(
          style = "margin-bottom: 10px; margin-top: 15px;",
          
          # Add Download/Upload Section Here
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #dee2e6;",
            h5("Bulk Mapping Tools", style = "color: #495057; margin-top: 0;"),
            fluidRow(
              column(6,
                downloadButton("download_mapping_template", "Download Mapping Template",
                              style = "width: 100%; background-color: #6c757d; border-color: #6c757d; color: white;"),
                p("Download pre-filled template with patient IDs.", style = "color: #6c757d; font-size: 0.85em; margin-top: 5px;")
              ),
              column(6,
                fileInput("mapping_file_upload", NULL,
                         accept = c(".csv", ".xlsx", ".xls"),
                         placeholder = "Upload filled template"),
                p("Upload CSV/Excel to apply mappings.", style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;")
              )
            )
          ),
          
          # Add Available Subjects Panel
          tags$details(
            style = "margin-bottom: 15px; border: 1px solid #dee2e6; border-radius: 5px; padding: 10px;",
            tags$summary(strong("Show Available Subject Accessions for this Study"), style = "cursor: pointer; color: #007bff;"),
            div(
              style = "margin-top: 10px; max-height: 200px; overflow-y: auto;",
              if(nrow(subjects) > 0) {
                tags$table(
                  class = "table table-sm table-striped",
                  style = "font-size: 0.9em; width: 100%;",
                  tags$thead(
                    tags$tr(
                      tags$th("Subject Accession"),
                      tags$th("Gender"),
                      tags$th("Species"),
                      tags$th("Description")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(subjects), function(i) {
                      tags$tr(
                        tags$td(subjects$subject_accession[i], style="font-family: monospace;"),
                        tags$td(if(!is.na(subjects$gender[i])) subjects$gender[i] else ""),
                        tags$td(if(!is.na(subjects$species[i])) subjects$species[i] else ""),
                        tags$td(if(!is.na(subjects$description[i])) subjects$description[i] else "")
                      )
                    })
                  )
                )
              } else {
                p("No subjects found.", style = "color: #6c757d; font-style: italic;")
              }
            )
          ),
          
          p(strong("Map ", length(patients_needing_mapping), " patients to subjects in this study:"), 
            style = "color: #721c24; margin: 0;")
        ),
        mapping_uis,
        hr(),
        div(
          style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin-top: 10px;",
          p(strong("Tip:"), " Invalid mappings mean the subject from xmap_subjects doesn't exist in study ", 
            tags$code(input$target_study), ". Select the correct subject from the dropdown.", 
            style = "color: #0c5460; margin: 0; font-size: 0.9em;")
        )
      ))
      
    } else {
      return(NULL)
    }
  }, error = function(e) {
    print(paste("ERROR: Failed to generate patient mapping UI:", e$message))
    return(div(
      style = "padding: 15px; background-color: #f8d7da; border-radius: 5px;",
      p(paste("Error loading subjects:", e$message), 
        style = "color: #721c24; margin: 0;")
    ))
  })
})

# Helper: Create source DB connection from environment variables
get_source_conn <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("ISPI_SOURCE_HOST", "localhost"),
    port = as.integer(Sys.getenv("ISPI_SOURCE_PORT", "5432")),
    dbname = Sys.getenv("ISPI_SOURCE_DB", "local_madi_ispi"),
    user = Sys.getenv("ISPI_SOURCE_USER", Sys.getenv("USER")),
    password = Sys.getenv("ISPI_SOURCE_PASSWORD", "")
  )
}

# Validate I-SPI access key
observeEvent(input$validate_access, {
  req(input$ispi_project_id, input$ispi_access_key)
  
  showNotification("Validating access key...", type = "message", duration = 3)
  
  tryCatch({
    conn_auth <- get_source_conn()
    on.exit(DBI::dbDisconnect(conn_auth))
    
    # Check access key against project_access_keys table
    result <- DBI::dbGetQuery(conn_auth,
      "SELECT project_id FROM madi_lumi_users.project_access_keys 
       WHERE project_id = $1 AND access_key = $2::uuid",
      params = list(as.integer(input$ispi_project_id), input$ispi_access_key)
    )
    
    if (nrow(result) > 0) {
      migration_values$source_connected <- TRUE
      migration_values$access_validated <- TRUE
      migration_values$connection_message <- paste("Access validated for workspace", input$ispi_project_id)
      
      # Enable data preview button
      shinyjs::enable("load_data_preview")
      
      showNotification("Access validated! You can now load data.", type = "message")
    } else {
      migration_values$source_connected <- FALSE
      migration_values$access_validated <- FALSE
      migration_values$connection_message <- "Invalid access key for this project. Contact your administrator."
      shinyjs::disable("load_data_preview")
      showNotification("Invalid access key. Please check your credentials.", type = "error", duration = 10)
    }
    
  }, error = function(e) {
    migration_values$source_connected <- FALSE
    migration_values$access_validated <- FALSE
    migration_values$connection_message <- paste("Authentication failed:", e$message)
    shinyjs::disable("load_data_preview")
    showNotification(paste("Authentication failed:", e$message), type = "error", duration = 10)
  })
})

# Display access validation status
output$access_status_display <- renderUI({
  msg <- migration_values$connection_message
  if (is.null(msg) || msg == "") return(NULL)
  
  is_valid <- isTRUE(migration_values$access_validated)
  style <- if(is_valid) {
    "color: #2e7d32; font-weight: bold; padding: 8px; background-color: #e8f5e9; border-radius: 4px; border: 1px solid #a5d6a7;"
  } else {
    "color: #c62828; font-weight: bold; padding: 8px; background-color: #ffebee; border-radius: 4px; border: 1px solid #ef9a9a;"
  }
  
  p(msg, style = style)
})


# Load data preview
observeEvent(input$load_data_preview, {
  req(migration_values$source_connected, input$preview_table)
  
  showNotification("Loading data preview...", type = "message", duration = 3)
  
  tryCatch({
    # Create connection using env vars
    conn_preview <- get_source_conn()
    
    # Build query with optional filters
    # For xmap_sample, join with xmap_subjects to get existing subject/arm mappings
    # Also join with xmap_planned_visit and xmap_sample_timing for biosample data
    if (input$preview_table == "xmap_sample") {
      base_query <- paste0(
        "SELECT xs.*, ",
        "subj.subject_accession, ",
        "subj.arm_accession, ",
        "visit.type as biosample_type, ",
        "visit.planned_visit_accession, ",
        "timing.actual_visit_day ",
        "FROM ", Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"), ".xmap_sample xs ",
        "LEFT JOIN ", Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"), ".xmap_subjects subj ",
        "  ON xs.patientid = subj.xmap_patientid AND xs.study_accession = subj.study_accession ",
        "LEFT JOIN ", Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"), ".xmap_planned_visit visit ",
        "  ON xs.timeperiod = visit.timepoint_name AND xs.study_accession = visit.study_accession ",
        "LEFT JOIN ", Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"), ".xmap_sample_timing timing ",
        "  ON xs.patientid = timing.patientid AND xs.timeperiod = timing.timeperiod"
      )
    } else {
      base_query <- paste0("SELECT * FROM ", Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"), ".", input$preview_table)
    }
    
    where_conditions <- c()
    params <- list()
    
    
    if (!is.null(input$study_accession_filter) && input$study_accession_filter != "") {
      # Qualify column with xs. prefix for joined query to avoid ambiguity
      if (input$preview_table == "xmap_sample") {
        where_conditions <- c(where_conditions, "xs.study_accession = $1")
      } else {
        where_conditions <- c(where_conditions, "study_accession = $1")
      }
      params <- c(params, list(input$study_accession_filter))
    }
    
    if (!is.null(input$experiment_accession_filter) && input$experiment_accession_filter != "") {
      param_num <- length(params) + 1
      # Qualify column with xs. prefix for joined query to avoid ambiguity
      if (input$preview_table == "xmap_sample") {
        where_conditions <- c(where_conditions, paste0("xs.experiment_accession = $", param_num))
      } else {
        where_conditions <- c(where_conditions, paste0("experiment_accession = $", param_num))
      }
      params <- c(params, list(input$experiment_accession_filter))
    }
    
    if (length(where_conditions) > 0) {
      base_query <- paste(base_query, "WHERE", paste(where_conditions, collapse = " AND "))
    }
    
    # Only apply row limit if checkbox is checked (default: load all rows)
    if(isTRUE(input$limit_rows) && !is.null(input$preview_limit)) {
      query <- paste(base_query, "LIMIT", input$preview_limit)
    } else {
      query <- base_query
    }
    
    # Execute query
    if (length(params) > 0) {
      preview_data <- DBI::dbGetQuery(conn_preview, query, params = params)
    } else {
      preview_data <- DBI::dbGetQuery(conn_preview, query)
    }
    
    migration_values$preview_data <- preview_data
    
    DBI::dbDisconnect(conn_preview)
    
    showNotification(paste("Loaded", nrow(preview_data), "rows successfully!"), type = "message")
    
    # Now that I-SPI data is loaded, populate destination mapping dropdowns
    current_workspace_id <- session$userData$user_workspace_id
    if(!is.null(current_workspace_id) && exists("conn") && DBI::dbIsValid(conn)) {
      
      tryCatch({
        print(paste("DEBUG: I-SPI data loaded - populating destination dropdowns for workspace:", current_workspace_id))
        
        # Populate target studies for current workspace
        target_studies <- DBI::dbGetQuery(conn, 
          "SELECT study_accession, brief_title FROM madi_dat.study WHERE workspace_id = $1 ORDER BY study_accession", 
          params = list(current_workspace_id))
        
        if(nrow(target_studies) > 0) {
          study_choices <- setNames(target_studies$study_accession, 
                                   paste(target_studies$study_accession, "-", substring(target_studies$brief_title, 1, 50)))
          updateSelectInput(session, "target_study", choices = study_choices)
          print(paste("DEBUG: Updated target_study dropdown with", nrow(target_studies), "studies"))
        }
        
        # Populate lookup tables
        arm_types <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_arm_type ORDER BY name")
        if(nrow(arm_types) > 0) {
          updateSelectInput(session, "default_arm_type", choices = arm_types$name, selected = "Observational")
          print(paste("DEBUG: Updated arm_type dropdown with", nrow(arm_types), "types"))
        }
        
        time_units <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_time_unit ORDER BY name")
        if(nrow(time_units) > 0) {
          updateSelectInput(session, "default_time_unit", choices = time_units$name, selected = "Days")
          print(paste("DEBUG: Updated time_unit dropdown with", nrow(time_units), "units"))
        }

        # Populate measurement techniques
        meas_techs <- DBI::dbGetQuery(conn, "SELECT name FROM madi_dat.lk_exp_measurement_tech ORDER BY name")
        print(paste("DEBUG: Raw measurement techniques found:", nrow(meas_techs)))
        print(head(meas_techs))
        
        if(nrow(meas_techs) > 0) {
          # Try to select Multiplex Bead Array Assay as default if it exists
          default_tech <- "Multiplex Bead Array Assay"
          if(!default_tech %in% meas_techs$name) default_tech <- meas_techs$name[1]
          
          updateSelectInput(session, "new_experiment_tech", choices = meas_techs$name, selected = default_tech)
          print(paste("DEBUG: Updated measurement_tech dropdown with", nrow(meas_techs), "techniques"))
        }

        # Auto-generate next Experiment ID
        tryCatch({
          max_exp <- DBI::dbGetQuery(conn, "SELECT MAX(experiment_accession) as max_id FROM madi_dat.experiment WHERE experiment_accession LIKE 'EXP%'")
          next_id <- "EXP00001" # Default start
          
          if(nrow(max_exp) > 0 && !is.na(max_exp$max_id)) {
             # Extract numbers using regex
             num_part <- sub("^EXP", "", max_exp$max_id)
             if(grepl("^[0-9]+$", num_part)) {
               current_num <- as.numeric(num_part)
               next_id <- sprintf("EXP%05d", current_num + 1)
             }
          }
          
          updateTextInput(session, "new_experiment_accession", value = next_id)
          print(paste("DEBUG: Auto-generated next experiment ID:", next_id))
          
        }, error = function(e) {
          print(paste("WARNING: Could not generate next experiment ID:", e$message))
        })
        
        showNotification("Destination mapping options loaded!", type = "message", duration = 3)
        
      }, error = function(e) {
        print(paste("ERROR: Failed to populate destination dropdowns:", e$message))
      })
    }
    
    # Enable save button if data exists
    if (nrow(preview_data) > 0) {
      shinyjs::enable("save_to_target")
    }
    
  }, error = function(e) {
    migration_values$preview_data <- NULL
    shinyjs::disable("save_to_target")
    showNotification(paste("Failed to load data:", e$message), type = "error", duration = 10)
  })
})

# Display access status in the Connection Status panel 
output$access_status_display_main <- renderUI({
  msg <- migration_values$connection_message
  if (is.null(msg) || msg == "") return(p("Not connected", style = "color: #999;"))
  
  is_valid <- isTRUE(migration_values$access_validated)
  style <- if(is_valid) {
    "color: #2e7d32; font-weight: bold; padding: 8px; background-color: #e8f5e9; border-radius: 4px;"
  } else {
    "color: #c62828; font-weight: bold; padding: 8px; background-color: #ffebee; border-radius: 4px;"
  }
  
  p(msg, style = style)
})

# Download mapping template with pre-filled patient IDs
output$download_mapping_template <- downloadHandler(
  filename = function() {
    paste0("patient_mapping_template_", format(Sys.time(), "%Y%m%d"), ".csv")
  },
  content = function(file) {
    preview_data <- migration_values$preview_data
    
    if (is.null(preview_data) || nrow(preview_data) == 0) {
      template <- data.frame(
        patientid = character(0),
        subject_accession = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # Get unique patient IDs from preview data
      patient_ids <- unique(as.character(preview_data$patientid))
      patient_ids <- patient_ids[!is.na(patient_ids) & patient_ids != ""]
      
      template <- data.frame(
        patientid = patient_ids,
        subject_accession = "",
        stringsAsFactors = FALSE
      )
      
      # 1. Try to pre-fill from I-SPI xmap_subjects (source DB mapping)
      tryCatch({
        source_schema <- Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results")
        conn_source <- get_source_conn()
        on.exit(DBI::dbDisconnect(conn_source), add = TRUE)
        
        ispi_mappings <- DBI::dbGetQuery(conn_source,
          paste0("SELECT DISTINCT xmap_patientid, subject_accession 
                  FROM ", source_schema, ".xmap_subjects 
                  WHERE subject_accession IS NOT NULL AND subject_accession != ''")
        )
        
        if(nrow(ispi_mappings) > 0) {
          for (i in seq_len(nrow(ispi_mappings))) {
            idx <- which(template$patientid == as.character(ispi_mappings$xmap_patientid[i]))
            if (length(idx) > 0 && template$subject_accession[idx[1]] == "") {
              template$subject_accession[idx[1]] <- as.character(ispi_mappings$subject_accession[i])
            }
          }
          filled <- sum(template$subject_accession != "")
          cat("[INFO] Pre-filled", filled, "mappings from I-SPI xmap_subjects\n")
        }
      }, error = function(e) {
        cat("[WARN] Could not check I-SPI xmap_subjects:", e$message, "\n")
      })
      
      # 2. Override with preview data mappings (if available from loaded data)
      if ("subject_accession" %in% names(preview_data)) {
        existing <- preview_data[!is.na(preview_data$subject_accession) & preview_data$subject_accession != "", 
                                c("patientid", "subject_accession")]
        existing <- existing[!duplicated(existing$patientid), ]
        for (i in seq_len(nrow(existing))) {
          idx <- which(template$patientid == as.character(existing$patientid[i]))
          if (length(idx) > 0) {
            template$subject_accession[idx[1]] <- as.character(existing$subject_accession[i])
          }
        }
      }
    }
    
    write.csv(template, file, row.names = FALSE)
  }
)


# Handle CSV/Excel file upload for patient-subject mapping override
observeEvent(input$mapping_file_upload, {
  req(input$mapping_file_upload)
  
  file_path <- input$mapping_file_upload$datapath
  file_name <- input$mapping_file_upload$name
  
  tryCatch({
    # Read file based on extension
    ext <- tolower(tools::file_ext(file_name))
    
    if(ext == "csv") {
      mapping_data <- read.csv(file_path, stringsAsFactors = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      if(!requireNamespace("readxl", quietly = TRUE)) {
        showNotification("The 'readxl' package is required for Excel files. Install it with: install.packages('readxl')", 
                        type = "error", duration = 10)
        return()
      }
      mapping_data <- readxl::read_excel(file_path)
      mapping_data <- as.data.frame(mapping_data)
    } else {
      showNotification(paste("Unsupported file format:", ext, ". Use .csv, .xlsx, or .xls"), type = "error")
      return()
    }
    
    # Validate required columns
    col_names <- tolower(names(mapping_data))
    has_patient <- any(col_names %in% c("patientid", "patient_id", "patient"))
    has_subject <- any(col_names %in% c("subject_accession", "subject", "subject_id"))
    
    if(!has_patient || !has_subject) {
      showNotification(
        paste("File must contain 'patientid' (or 'patient_id') and 'subject_accession' (or 'subject') columns.",
              "Found columns:", paste(names(mapping_data), collapse = ", ")),
        type = "error", duration = 15
      )
      return()
    }
    
    # Normalize column names
    patient_col <- names(mapping_data)[which(col_names %in% c("patientid", "patient_id", "patient"))[1]]
    subject_col <- names(mapping_data)[which(col_names %in% c("subject_accession", "subject", "subject_id"))[1]]
    
    # Store mapping override
    migration_values$mapping_override <- data.frame(
      patientid = as.character(mapping_data[[patient_col]]),
      subject_accession = as.character(mapping_data[[subject_col]]),
      stringsAsFactors = FALSE
    )
    
    n_mappings <- nrow(migration_values$mapping_override)
    showNotification(
      paste("Loaded", n_mappings, "patient-subject mappings from", file_name), 
      type = "message", duration = 5
    )
    
    cat(paste0("[INFO] Loaded mapping file: ", file_name, " (", n_mappings, " mappings)\n"))
    
    # Update UI dropdowns to reflect the uploaded mappings
    for(j in 1:n_mappings) {
      pid <- migration_values$mapping_override$patientid[j]
      subj <- migration_values$mapping_override$subject_accession[j]
      if(!is.na(pid) && !is.na(subj) && nzchar(subj)) {
        input_id <- paste0("patient_map_", gsub("[^[:alnum:]]", "_", pid))
        updateSelectInput(session, input_id, selected = subj)
      }
    }
    
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
  })
})

# Show/hide data preview section
output$show_data_preview <- reactive({
  !is.null(migration_values$preview_data) && nrow(migration_values$preview_data) > 0
})
outputOptions(output, "show_data_preview", suspendWhenHidden = FALSE)

# Preview info
output$preview_info <- renderText({
  if (!is.null(migration_values$preview_data)) {
    paste("Showing", nrow(migration_values$preview_data), "rows from", input$preview_table)
  } else {
    ""
  }
})

# Data preview table
output$data_preview_table <- DT::renderDataTable({
  if (!is.null(migration_values$preview_data)) {
    DT::datatable(
      migration_values$preview_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      ),
      class = "compact stripe"
    )
  }
})

# Save to target - Execute Migration
observeEvent(input$save_to_target, {
  cat("[SAVE] Save to Target button clicked\n")
  
  if(is.null(migration_values$preview_data) || nrow(migration_values$preview_data) == 0) {
    showNotification("No data loaded. Please load data first.", type = "error", duration = 5)
    return()
  }
  
  if(is.null(input$target_study) || !nzchar(input$target_study)) {
    showNotification("Please select a target study.", type = "error", duration = 5)
    return()
  }
  
  # Check experiment: either new or existing must be specified
  experiment_acc <- if(isTRUE(input$create_new_experiment)) {
    input$new_experiment_accession
  } else {
    input$target_experiment
  }
  
  if(is.null(experiment_acc) || !nzchar(experiment_acc)) {
    showNotification("Please select or create an experiment.", type = "error", duration = 5)
    return()
  }
  
  cat(paste0("[SAVE] Study: ", input$target_study, ", Experiment: ", experiment_acc, "\n"))
  
  # Collect timeperiod mappings from user inputs
  timeperiod_mapping <- list()
  unique_timeperiods <- unique(migration_values$preview_data$timeperiod)
  unique_timeperiods <- unique_timeperiods[!is.na(unique_timeperiods)]
  
  for(tp in unique_timeperiods) {
    input_id <- paste0("timeperiod_map_", gsub("[^[:alnum:]]", "_", tp))
    mapped_value <- input[[input_id]]
    if(!is.null(mapped_value) && nzchar(mapped_value)) {
      timeperiod_mapping[[as.character(tp)]] <- mapped_value
    }
  }
  
  # Collect agroup mappings from user inputs
  agroup_mapping <- list()
  unique_agroups <- unique(migration_values$preview_data$agroup)
  unique_agroups <- unique_agroups[!is.na(unique_agroups)]
  
  for(ag in unique_agroups) {
    input_id <- paste0("agroup_map_", gsub("[^[:alnum:]]", "_", ag))
    mapped_value <- input[[input_id]]
    if(!is.null(mapped_value) && nzchar(mapped_value)) {
      agroup_mapping[[as.character(ag)]] <- mapped_value
    }
  }
  
  # Collect patient mappings from user inputs (for patients without xmap_subjects mapping)
  patient_mapping <- list()
  unique_patients <- unique(migration_values$preview_data$patientid)
  unique_patients <- unique_patients[!is.na(unique_patients)]
  
  patients_needing_mapping <- c()
  for(pid in unique_patients) {
    # Check if patient already has subject_accession from xmap_subjects
    patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
    subject_acc <- patient_rows$subject_accession[1]
    
    if(is.null(subject_acc) || is.na(subject_acc) || nchar(subject_acc) == 0) {
      # Patient needs manual mapping
      patients_needing_mapping <- c(patients_needing_mapping, pid)
      input_id <- paste0("patient_map_", gsub("[^[:alnum:]]", "_", pid))
      mapped_value <- input[[input_id]]
      if(!is.null(mapped_value) && nzchar(mapped_value)) {
        patient_mapping[[as.character(pid)]] <- mapped_value
      }
    }
  }
  
  # Also include patients mapped via CSV/Excel upload (mapping_override)
  if(!is.null(migration_values$mapping_override)) {
    file_map <- migration_values$mapping_override
    for(pid in patients_needing_mapping) {
      pid_str <- as.character(pid)
      # Only fill if not already mapped via dropdown
      if(is.null(patient_mapping[[pid_str]]) || !nzchar(patient_mapping[[pid_str]])) {
        match_idx <- which(file_map$patientid == pid_str)
        if(length(match_idx) > 0 && !is.na(file_map$subject_accession[match_idx[1]]) && nzchar(file_map$subject_accession[match_idx[1]])) {
          patient_mapping[[pid_str]] <- file_map$subject_accession[match_idx[1]]
        }
      }
    }
  }
  
  # Validate mappings
  cat(paste0("[SAVE] Timeperiods: ", length(timeperiod_mapping), "/", length(unique_timeperiods),
             ", Agroups: ", length(agroup_mapping), "/", length(unique_agroups),
             ", Patients: ", length(patient_mapping), "/", length(patients_needing_mapping), "\n"))
  
  if(length(timeperiod_mapping) != length(unique_timeperiods)) {
    showNotification(
      paste0("Please map all timeperiod values before saving (", length(timeperiod_mapping), "/", length(unique_timeperiods), " mapped)"), 
      type = "warning", duration = 8)
    return()
  }
  
  if(length(agroup_mapping) != length(unique_agroups)) {
    showNotification(
      paste0("Please map all agroup values before saving (", length(agroup_mapping), "/", length(unique_agroups), " mapped)"), 
      type = "warning", duration = 8)
    return()
  }
  
  if(length(patient_mapping) != length(patients_needing_mapping)) {
    showNotification(
      paste0("Please map all ", length(patients_needing_mapping), " unmapped patient IDs to subjects before saving (",
             length(patient_mapping), "/", length(patients_needing_mapping), " mapped)"),
      type = "warning", duration = 8
    )
    return()
  }
  
  # Apply manual patient mappings to preview data (override NULL subject_accession values)
  preview_data_with_mappings <- migration_values$preview_data
  if(length(patient_mapping) > 0) {
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- preview_data_with_mappings$patientid[i]
      if(!is.na(pid) && (is.null(preview_data_with_mappings$subject_accession[i]) || 
                         is.na(preview_data_with_mappings$subject_accession[i]) || 
                         nchar(preview_data_with_mappings$subject_accession[i]) == 0)) {
        # Override with manual mapping
        mapped_subject <- patient_mapping[[as.character(pid)]]
        if(!is.null(mapped_subject)) {
          preview_data_with_mappings$subject_accession[i] <- mapped_subject
        }
      }
    }
  }
  
  # Apply file-based mapping override (from CSV/Excel upload)
  if(!is.null(migration_values$mapping_override)) {
    file_map <- migration_values$mapping_override
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- as.character(preview_data_with_mappings$patientid[i])
      match_idx <- which(file_map$patientid == pid)
      if(length(match_idx) > 0) {
        preview_data_with_mappings$subject_accession[i] <- file_map$subject_accession[match_idx[1]]
      }
    }
  }
  
  # Build configuration
  config <- list(
    workspace_id = current_workspace(),
    study_accession = input$target_study,
    experiment_accession = if(input$create_new_experiment) input$new_experiment_accession else input$target_experiment,
    create_new_experiment = input$create_new_experiment %||% FALSE,
    new_experiment_name = input$new_experiment_name,
    new_experiment_description = input$new_experiment_description,
    new_experiment_tech = input$new_experiment_tech,
    sample_prefix = ifelse(is.null(input$sample_prefix) || input$sample_prefix == "", "ES_", input$sample_prefix),
    default_arm_type = input$default_arm_type %||% "Observational",

    default_species = input$default_species,
    result_schema_type = input$result_schema_type %||% "MBAA",
    time_unit = input$biosample_time_unit %||% "Days",
    t0_event = input$biosample_t0_event %||% "Time of enrollment",
    timeperiod_mapping = timeperiod_mapping,
    agroup_mapping = agroup_mapping,
    ispi_project_id = input$ispi_project_id,
    mapping_override = migration_values$mapping_override
  )
  
  # Debug config values
  cat(sprintf("\n DEBUG CONFIG: workspace_id=%s, result_schema_type='%s', study=%s\n", 
              if(is.null(config$workspace_id)) "NULL" else config$workspace_id,
              if(is.null(config$result_schema_type)) "NULL" else config$result_schema_type,
              config$study_accession))
  
  # Show confirmation dialog with test option
  showModal(modalDialog(
    title = "Confirm I-SPI Data Migration",
    size = "l",
    div(
      style = "padding: 15px;",
      h4("Migration Summary", style = "color: #0c5460;"),
      tags$ul(
        tags$li(strong("Source Records:"), nrow(migration_values$preview_data), "rows"),
        tags$li(strong("Target Study:"), config$study_accession),
        tags$li(strong("Target Experiment:"), config$experiment_accession),
        tags$li(strong("Workspace:"), config$workspace_id),
        tags$li(strong("Unique Patients:"), length(unique(migration_values$preview_data$patientid))),
        tags$li(strong("Unique Timeperiods:"), length(unique_timeperiods), "→", length(timeperiod_mapping), "mapped"),
        tags$li(strong("Unique Agroups:"), length(unique_agroups), "→", length(agroup_mapping), "mapped")
      ),
      hr(),
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        p(strong("Important:"), " Test mode will show what would happen without making changes. Use this first!", 
          style = "color: #856404; margin: 0;")
      ),
      div(
        style = "background-color: #f8d7da; padding: 10px; border-radius: 5px;",
        p(strong("Production Database:"), " Live mode will commit changes to the production database. Only use after testing!", 
          style = "color: #721c24; margin: 0;")
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("execute_test_migration", "Test Migration (Rollback)", 
                  class = "btn-warning",
                  style = "background-color: #f39c12; border-color: #f39c12;"),
      actionButton("execute_live_migration", "Execute Live Migration", 
                  class = "btn-danger")
    )
  ))
})

# Show valid patient mappings modal
observeEvent(input$show_valid_mappings, {
  req(migration_values$preview_data)
  req(input$target_study)
  
  # Get subjects for the selected study
  subjects <- DBI::dbGetQuery(conn, 
    "SELECT DISTINCT s.subject_accession, s.gender, s.species
     FROM madi_dat.subject s
     JOIN madi_dat.arm_2_subject a2s ON s.subject_accession = a2s.subject_accession
     JOIN madi_dat.arm_or_cohort ac ON a2s.arm_accession = ac.arm_accession
     WHERE ac.study_accession = $1
     ORDER BY s.subject_accession", 
    params = list(input$target_study))
  
  valid_subjects <- setNames(rep(TRUE, nrow(subjects)), subjects$subject_accession)
  
  # Get valid mappings
  unique_patients <- unique(migration_values$preview_data$patientid)
  unique_patients <- unique_patients[!is.na(unique_patients)]
  
  valid_mappings <- data.frame(
    patient_id = character(),
    subject_accession = character(),
    gender = character(),
    species = character(),
    stringsAsFactors = FALSE
  )
  
  for(pid in unique_patients) {
    patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
    subject_acc <- patient_rows$subject_accession[1]
    
    if(!is.null(subject_acc) && !is.na(subject_acc) && nchar(subject_acc) > 0) {
      if(!is.null(valid_subjects[[subject_acc]])) {
        # Valid mapping
        subject_info <- subjects[subjects$subject_accession == subject_acc, ]
        valid_mappings <- rbind(valid_mappings, data.frame(
          patient_id = pid,
          subject_accession = subject_acc,
          gender = ifelse(nrow(subject_info) > 0 && !is.na(subject_info$gender[1]), subject_info$gender[1], "Unknown"),
          species = ifelse(nrow(subject_info) > 0 && !is.na(subject_info$species[1]), subject_info$species[1], "Unknown"),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  showModal(modalDialog(
    title = "Valid Patient → Subject Mappings",
    size = "l",
    div(
      style = "padding: 15px;",
      p(paste("Found", nrow(valid_mappings), "valid mappings from xmap_subjects for study", input$target_study)),
      hr(),
      DT::dataTableOutput("valid_mappings_table")
    ),
    footer = modalButton("Close"),
    easyClose = TRUE
  ))
  
  output$valid_mappings_table <- DT::renderDataTable({
    DT::datatable(
      valid_mappings,
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      ),
      colnames = c("Patient ID", "Subject Accession", "Gender", "Species"),
      class = "compact stripe"
    )
  })
})

# Cancel edit all mappings mode
observeEvent(input$cancel_edit_all, {
  migration_values$edit_all_patient_mappings <- FALSE
  showNotification("Returned to normal mode - showing only unmapped/invalid patients", type = "message", duration = 3)
})

# Edit all mappings - show dropdowns for ALL patients (including valid ones)
observeEvent(input$edit_all_mappings, {
  # Store that user wants to edit all mappings
  migration_values$edit_all_patient_mappings <- TRUE
  
  showNotification("Showing manual mapping controls for ALL patients", type = "message", duration = 3)
})

# Execute TEST migration (rollback)
observeEvent(input$execute_test_migration, {
  removeModal()
  
  # Rebuild config (same as above)
  unique_timeperiods <- unique(migration_values$preview_data$timeperiod[!is.na(migration_values$preview_data$timeperiod)])
  unique_agroups <- unique(migration_values$preview_data$agroup[!is.na(migration_values$preview_data$agroup)])
  
  timeperiod_mapping <- list()
  for(tp in unique_timeperiods) {
    input_id <- paste0("timeperiod_map_", gsub("[^[:alnum:]]", "_", tp))
    timeperiod_mapping[[as.character(tp)]] <- input[[input_id]]
  }
  
  agroup_mapping <- list()
  for(ag in unique_agroups) {
    input_id <- paste0("agroup_map_", gsub("[^[:alnum:]]", "_", ag))
    agroup_mapping[[as.character(ag)]] <- input[[input_id]]
  }
  
  # Collect patient mappings for unmapped patients
  patient_mapping <- list()
  unique_patients <- unique(migration_values$preview_data$patientid)
  unique_patients <- unique_patients[!is.na(unique_patients)]
  
  patients_needing_mapping <- c()
  for(pid in unique_patients) {
    patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
    subject_acc <- patient_rows$subject_accession[1]
    
    if(is.null(subject_acc) || is.na(subject_acc) || nchar(subject_acc) == 0) {
      patients_needing_mapping <- c(patients_needing_mapping, pid)
      input_id <- paste0("patient_map_", gsub("[^[:alnum:]]", "_", pid))
      mapped_value <- input[[input_id]]
      if(!is.null(mapped_value) && nzchar(mapped_value)) {
        patient_mapping[[as.character(pid)]] <- mapped_value
      }
    }
  }
  
  # Validate patient mappings
  if(length(patient_mapping) != length(patients_needing_mapping)) {
    showNotification(
      paste0("Please map all ", length(patients_needing_mapping), " unmapped patient IDs before testing"),
      type = "warning", duration = 5
    )
    return()
  }
  
  # Apply manual patient mappings to preview data
  preview_data_with_mappings <- migration_values$preview_data
  if(length(patient_mapping) > 0) {
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- preview_data_with_mappings$patientid[i]
      if(!is.na(pid) && (is.null(preview_data_with_mappings$subject_accession[i]) || 
                         is.na(preview_data_with_mappings$subject_accession[i]) || 
                         nchar(preview_data_with_mappings$subject_accession[i]) == 0)) {
        mapped_subject <- patient_mapping[[as.character(pid)]]
        if(!is.null(mapped_subject)) {
          preview_data_with_mappings$subject_accession[i] <- mapped_subject
        }
      }
    }
  }
  
  # Apply file-based mapping override (from CSV/Excel upload)
  if(!is.null(migration_values$mapping_override)) {
    file_map <- migration_values$mapping_override
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- as.character(preview_data_with_mappings$patientid[i])
      match_idx <- which(file_map$patientid == pid)
      if(length(match_idx) > 0) {
        preview_data_with_mappings$subject_accession[i] <- file_map$subject_accession[match_idx[1]]
      }
    }
  }
  
  config <- list(
    workspace_id = current_workspace(),
    study_accession = input$target_study,
    experiment_accession = if(input$create_new_experiment) input$new_experiment_accession else input$target_experiment,
    create_new_experiment = input$create_new_experiment %||% FALSE,
    new_experiment_name = input$new_experiment_name,
    new_experiment_description = input$new_experiment_description,
    new_experiment_tech = input$new_experiment_tech,
    sample_prefix = ifelse(is.null(input$sample_prefix) || input$sample_prefix == "", "ES_", input$sample_prefix),
    default_arm_type = input$default_arm_type %||% "Observational",

    default_species = input$default_species,
    result_schema_type = input$result_schema_type %||% "MBAA",
    result_schema = input$result_schema_type %||% "MBAA", # redundant alias for queries
    source_schema = Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"),
    source_study = input$study_accession_filter,
    timeperiod_mapping = timeperiod_mapping,
    agroup_mapping = agroup_mapping,
    ispi_project_id = input$ispi_project_id
  )
  

  # Show processing modal
  showModal(modalDialog(
    title = "Migration In Progress",
    div(
      style = "text-align: center; padding: 30px;",
      tags$div(class = "fa fa-spinner fa-spin", style = "font-size: 48px; color: #8e44ad; margin-bottom: 20px;"),
      h4("Running TEST Migration...", style = "color: #8e44ad;"),
      h5(id = "test_progress_msg", "Initializing...", style = "color: #333; font-weight: bold; margin-top: 15px;"),
      p(id = "test_progress_detail", "Preparing database transaction.", style = "color: #666; font-size: 0.95em;"),
      p("This may take several minutes for large datasets. Please do not close this window.",
        style = "color: #999; font-size: 0.85em; font-style: italic;")
    ),
    footer = NULL,
    easyClose = FALSE,
    size = "m"
  ))

  # In-memory log capture for download (no disk file)
  log_con <- textConnection("test_log_lines", "w", local = FALSE)
  
  # Start logging to memory
  cat(paste0("[INFO] Starting Test Migration Logging\n"))
  sink(log_con, split = TRUE)
  on.exit({
    try(sink(), silent = TRUE)
    try(close(log_con), silent = TRUE)
  }, add = TRUE)

  source_conn <- NULL
  tryCatch({
    # Create source DB connection using env vars
    source_conn <- tryCatch({
      get_source_conn()
    }, error = function(e) {
      cat("[WARN] Could not create source DB connection:", e$message, "\n")
      NULL
    })
    
    progress_cb <- function(msg, detail = "") {
      shinyjs::html(id = "test_progress_msg", html = msg)
      shinyjs::html(id = "test_progress_detail", html = detail)
    }
    
    results <<- execute_migration(
      conn, preview_data_with_mappings, config,
      commit = FALSE, source_conn = source_conn,
      progress_cb = progress_cb
    )
    
    # Store log for download
    migration_log_text(paste(test_log_lines, collapse = "\n"))
    
    removeModal()
    
    # Show results modal
    showModal(modalDialog(
      title = "Test Migration Results (ROLLED BACK)",
      size = "l",
      div(
        style = "padding: 15px;",
        div(
          style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          h4("Test Completed Successfully", style = "color: #155724; margin-top: 0;"),
          p("All changes were rolled back. No data was saved to the database.", 
            style = "color: #155724; margin: 0;")
        ),
        h5("Migration Summary:", style = "color: #0c5460;"),
        tags$ul(
          tags$li(strong("Experiment:"), results$experiment$experiment_accession, 
                  ifelse(results$experiment$created, "(NEW)", "(existing)")),
          tags$li(strong("Subjects:"), results$subjects$validated %||% 0, "validated,", 
                  results$subjects$missing %||% 0, "missing"),
          tags$li(strong("Planned Visits:"), results$planned_visits$created, "would be created"),
          tags$li(strong("Biosamples:"), results$biosamples$inserted %||% 0, "would be inserted,",
                  results$biosamples$existing %||% 0, "already exist"),
          tags$li(strong("Arms/Cohorts:"), results$arms$created, "would be created"),
          tags$li(strong("Arm Associations:"), results$arm_associations$inserted, "would be created"),
          tags$li(strong("Experiment Samples:"), results$expsamples$inserted, "would be created,",
                  ifelse(length(results$expsamples$failed) > 0,
                         paste(length(results$expsamples$failed), "would fail,"),
                         "0 failures,"),
                  ifelse(length(results$expsamples$skipped) > 0,
                         paste(length(results$expsamples$skipped), "would skip"),
                         "0 skipped")),
          tags$li(strong("Biosample Links:"), results$biosample_links$linked %||% 0, "linked (",
                  length(results$biosample_links$failed) %||% 0, "failed)"),
          if(!is.null(results$mbaa_results)) {
            tags$li(strong("MBAA Results:"), results$mbaa_results$inserted_result %||% 0, "would be inserted")
          },
          if(!is.null(results$control_samples)) {
            tags$li(strong("Control Samples:"), results$control_samples$inserted %||% 0, "would be inserted,",
                    length(results$control_samples$failed), "failed")
          },
          if(!is.null(results$control_results)) {
            tags$li(strong("Control MBAA Results:"), results$control_results$inserted %||% 0, "would be inserted,",
                    results$control_results$failed %||% 0, "failed")
          },
          if(!is.null(results$standard_curves)) {
            tags$li(strong("Standard Curves:"), results$standard_curves$inserted %||% 0, "would be inserted,",
                    results$standard_curves$failed %||% 0, "failed")
          },
          if(!is.null(results$model_qc) && results$model_qc$inserted > 0) {
            tags$li(strong("Model QC Data:"), results$model_qc$inserted %||% 0, "would be inserted,",
                    results$model_qc$failed %||% 0, "failed")
          },
          if(!is.null(results$sample_qc) && results$sample_qc$inserted > 0) {
            tags$li(strong("Sample QC Data:"), results$sample_qc$inserted %||% 0, "would be inserted,",
                    results$sample_qc$failed %||% 0, "failed")
          }
        ),
        
        # Download button for migration log
        div(
          style = "margin-top: 15px; text-align: center;",
          downloadButton("download_migration_log", "Download Migration Log", 
                        class = "btn-info")
        ),
        
        # Show failures/warnings if any
        if(length(results$all_failures) > 0 || length(results$all_skipped) > 0) {
          tagList(
            hr(),
            div(
              style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              h5("Items Needing Attention", style = "color: #856404; margin-top: 0;"),
              
              if(length(results$all_failures) > 0) {
                tagList(
                  p(strong("Failed Items (", length(results$all_failures), "):"), 
                    style = "color: #721c24; margin-bottom: 5px;"),
                  tags$div(
                    style = "max-height: 200px; overflow-y: auto; background-color: #f8d7da; padding: 10px; border-radius: 3px;",
                    tags$table(
                      style = "width: 100%; font-size: 0.9em;",
                      lapply(1:min(10, length(results$all_failures)), function(i) {
                        item <- results$all_failures[[i]]
                        tags$tr(
                          tags$td(paste0(i, "."), style = "padding: 3px; width: 30px;"),
                          tags$td(
                            strong(item$type), ": ",
                            if(!is.null(item$identifier)) paste("ID:", item$identifier),
                            if(!is.null(item$sample_id)) paste("Sample:", item$sample_id),
                            if(!is.null(item$patient_id)) paste("Patient:", item$patient_id),
                            tags$br(),
                            tags$small(item$error, style = "color: #721c24;"),
                            style = "padding: 3px;"
                          )
                        )
                      }),
                      if(length(results$all_failures) > 10) {
                        tags$tr(tags$td(colspan = 2, 
                          paste("... and", length(results$all_failures) - 10, "more"),
                          style = "padding: 3px; font-style: italic;"))
                      }
                    )
                  )
                )
              },
              
              if(length(results$all_skipped) > 0) {
                tagList(
                  tags$br(),
                  p(strong("Skipped Items (", length(results$all_skipped), "):"), 
                    style = "color: #856404; margin-bottom: 5px;"),
                  tags$div(
                    style = "max-height: 150px; overflow-y: auto; background-color: #fff3cd; padding: 10px; border-radius: 3px;",
                    tags$table(
                      style = "width: 100%; font-size: 0.9em;",
                      lapply(1:min(10, length(results$all_skipped)), function(i) {
                        item <- results$all_skipped[[i]]
                        tags$tr(
                          tags$td(paste0(i, "."), style = "padding: 3px; width: 30px;"),
                          tags$td(
                            if(!is.null(item$sample_id)) paste("Sample:", item$sample_id),
                            if(!is.null(item$timeperiod)) paste("- Timeperiod:", item$timeperiod),
                            tags$br(),
                            tags$small(item$reason, style = "color: #856404;"),
                            style = "padding: 3px;"
                          )
                        )
                      }),
                      if(length(results$all_skipped) > 10) {
                        tags$tr(tags$td(colspan = 2, 
                          paste("... and", length(results$all_skipped) - 10, "more"),
                          style = "padding: 3px; font-style: italic;"))
                      }
                    )
                  )
                )
              }
            )
          )
        },
        
        hr(),
        div(
          style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px;",
          p(strong("Next Step:"), 
            if(length(results$all_failures) > 0) {
              "Review failed items and fix issues before executing live migration."
            } else {
              "If these numbers look correct, you can execute the live migration to save the data."
            },
            style = "color: #0c5460; margin: 0;")
        )
      ),
      footer = tagList(
        modalButton("Close"),
        if(length(results$all_failures) == 0) {
          actionButton("execute_live_migration_after_test", "Execute Live Migration Now", 
                      class = "btn-success")
        }
      )
    ))
    
  }, error = function(e) {
    removeModal()
    showNotification(paste("Test migration failed:", e$message), type = "error", duration = 10)
  }, finally = {
    # Clean up source connection
    if(!is.null(source_conn) && DBI::dbIsValid(source_conn)) {
      tryCatch(DBI::dbDisconnect(source_conn), error = function(e) {})
    }
  })
})

# Handle direct transition from test to live
observeEvent(input$execute_live_migration_after_test, {
  removeModal()
  # Trigger the live migration flow
  shinyjs::click("execute_live_migration")
})

# Download handler for migration log
output$download_migration_log <- downloadHandler(
  filename = function() {
    paste0("ispi_migration_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  },
  content = function(file) {
    log_content <- migration_log_text()
    if (!is.null(log_content) && nchar(log_content) > 0) {
      writeLines(log_content, file)
    } else {
      writeLines("No migration log available.", file)
    }
  }
)

# Download handler for LIVE migration log (captured in memory)
output$download_live_log <- downloadHandler(
  filename = function() {
    paste0("ispi_live_migration_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  },
  content = function(file) {
    if (!is.null(migration_values$live_log_content)) {
      writeLines(migration_values$live_log_content, file)
    } else {
      writeLines("No live migration log available.", file)
    }
  }
)

# Download handler for DQ audit report
output$download_dq_report <- downloadHandler(
  filename = function() {
    paste0("data_quality_audit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  },
  content = function(file) {
    dq_text <- migration_dq_report()
    if (!is.null(dq_text)) {
      writeLines(dq_text, file)
    } else {
      writeLines("No data quality report available. Report is generated after a successful LIVE migration.", file)
    }
  }
)

# Execute LIVE migration (commit)
observeEvent(input$execute_live_migration, {
  removeModal()

  # --- PRE-FLIGHT CHECK ---
  # Query what's already in the DB for this experiment before showing the confirmation modal.
  # This lets the user understand whether they're doing a first run or a re-run,
  # and choose the right mode so nothing gets duplicated.
  exp_acc <- if(isTRUE(input$create_new_experiment)) input$new_experiment_accession else input$target_experiment
  existing <- tryCatch(
    check_existing_migration_data(conn, exp_acc),
    error = function(e) NULL
  )

  has_existing <- !is.null(existing) && any(unlist(existing) > 0, na.rm = TRUE)

  if(has_existing) {
    showModal(modalDialog(
      title = div(icon("exclamation-triangle", style="color:#e67e22;"), " Pre-Flight Check: Existing Data Detected"),
      size = "l",
      div(
        style = "padding: 10px;",
        div(
          style = "background-color: #fff3cd; border-left: 4px solid #e67e22; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          h5(icon("database"), strong(" Experiment ", exp_acc, " already has data in the target database."),
             style = "color: #856404; margin-top: 0;"),
          p("Running the migration without choosing a mode below will ", strong("duplicate"), " all result data.",
            style = "color: #856404; margin-bottom: 0;")
        ),
        h6("Current row counts:", style = "font-weight: bold; margin-bottom: 8px;"),
        tags$table(
          class = "table table-sm table-bordered",
          style = "font-size: 0.9em;",
          tags$thead(tags$tr(tags$th("Table"), tags$th("Existing Rows"), tags$th("Without re-run mode"))),
          tags$tbody(
            tags$tr(tags$td("Expsamples"),           tags$td(existing$expsamples %||% 0),     tags$td(span(style="color:green;", "✅ safe (keyed)"))),
            tags$tr(tags$td("MBAA Results (samples)"),tags$td(existing$mbaa_expsample %||% 0),tags$td(if((existing$mbaa_expsample %||% 0) > 0) span(style="color:red;","⚠ will duplicate") else span(style="color:green;","✅ empty"))),
            tags$tr(tags$td("MBAA Results (controls)"),tags$td(existing$mbaa_control %||% 0), tags$td(if((existing$mbaa_control %||% 0) > 0) span(style="color:red;","⚠ will duplicate") else span(style="color:green;","✅ empty"))),
            tags$tr(tags$td("Control Samples"),      tags$td(existing$control_samples %||% 0),tags$td(span(style="color:green;","✅ safe (keyed)"))),
            tags$tr(tags$td("Standard Curves"),      tags$td(existing$standard_curves %||% 0),tags$td(if((existing$standard_curves %||% 0) > 0) span(style="color:red;","⚠ will duplicate") else span(style="color:green;","✅ empty"))),
            tags$tr(tags$td("Model QC"),             tags$td(existing$model_qc %||% 0),       tags$td(if((existing$model_qc %||% 0) > 0) span(style="color:red;","⚠ will duplicate") else span(style="color:green;","✅ empty"))),
            tags$tr(tags$td("Sample QC"),            tags$td(existing$sample_qc %||% 0),      tags$td(if((existing$sample_qc %||% 0) > 0) span(style="color:red;","⚠ will duplicate") else span(style="color:green;","✅ empty")))
          )
        ),
        hr(),
        h6("Choose how to proceed:", style = "font-weight: bold;"),
        radioButtons("rerun_mode_choice", NULL,
          choices = list(
            "Skip already-migrated tables — only fill gaps (safe re-run)" = "skip_existing",
            "Clean slate — delete existing results and re-insert everything fresh" = "clean_slate"
          ),
          selected = "skip_existing"
        ),
        div(
          style = "background-color: #f8d7da; padding: 10px; border-radius: 4px; margin-top: 5px;",
          p(icon("info-circle"),
            " 'Clean slate' deletes all result data for this experiment before re-inserting. ",
            "It runs inside the migration transaction, so it rolls back automatically if the migration fails.",
            style = "margin: 0; font-size: 0.85em; color: #721c24;")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("preflight_confirmed", "Continue to Confirmation →", class = "btn-warning")
      )
    ))
    return()
  }

  # No existing data — safe first run, go straight to final confirmation
  migration_values$rerun_mode <- "first_run"

  # Final confirmation with operator info
  showModal(modalDialog(
    title = "FINAL CONFIRMATION",
    div(
      style = "padding: 15px;",
      div(
        style = "background-color: #f8d7da; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
        h4(icon("exclamation-triangle"), " WARNING: PRODUCTION DATABASE", style = "color: #721c24; margin-top: 0;"),
        p("This will PERMANENTLY save data to the production database.", 
          style = "color: #721c24; font-size: 1.1em;"),
        p("This action cannot be undone!", 
          style = "color: #721c24; font-weight: bold;")
      ),
      div(
        style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #bee5eb;",
        h5(icon("user-shield"), " Operator Identification", style = "color: #0c5460; margin-top: 0;"),
        p("This information will be recorded in the audit trail.", style = "color: #6c757d; font-size: 0.85em;"),
        fluidRow(
          column(6,
            textInput("operator_name", "Full Name:", placeholder = "e.g., John Smith")
          ),
          column(6,
            textInput("operator_designation", "Designation / Role:", placeholder = "e.g., Data Engineer")
          )
        )
      ),
      p("Type 'CONFIRM' to proceed:", style = "font-weight: bold;"),
      textInput("final_confirmation", NULL, placeholder = "Type CONFIRM here")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("execute_live_confirmed", "Execute Live Migration", 
                  class = "btn-danger")
    )
  ))
})

# Pre-flight confirmed: store chosen rerun mode and proceed to final confirmation
observeEvent(input$preflight_confirmed, {
  removeModal()
  migration_values$rerun_mode <- input$rerun_mode_choice %||% "skip_existing"
  cat("[INFO] Pre-flight: rerun_mode set to", migration_values$rerun_mode, "\n")

  showModal(modalDialog(
    title = "FINAL CONFIRMATION",
    div(
      style = "padding: 15px;",
      div(
        style = "background-color: #f8d7da; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
        h4(icon("exclamation-triangle"), " WARNING: PRODUCTION DATABASE", style = "color: #721c24; margin-top: 0;"),
        p("This will PERMANENTLY save data to the production database.", style = "color: #721c24; font-size: 1.1em;"),
        p("This action cannot be undone!", style = "color: #721c24; font-weight: bold;")
      ),
      div(
        style = "background-color: #e2f0d9; padding: 10px; border-radius: 4px; margin-bottom: 15px; border: 1px solid #a9d18e;",
        p(icon("cog"), strong(" Re-run mode: "), migration_values$rerun_mode,
          style = "margin: 0; color: #375623;")
      ),
      div(
        style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #bee5eb;",
        h5(icon("user-shield"), " Operator Identification", style = "color: #0c5460; margin-top: 0;"),
        p("This information will be recorded in the audit trail.", style = "color: #6c757d; font-size: 0.85em;"),
        fluidRow(
          column(6, textInput("operator_name", "Full Name:", placeholder = "e.g., John Smith")),
          column(6, textInput("operator_designation", "Designation / Role:", placeholder = "e.g., Data Engineer"))
        )
      ),
      p("Type 'CONFIRM' to proceed:", style = "font-weight: bold;"),
      textInput("final_confirmation", NULL, placeholder = "Type CONFIRM here")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("execute_live_confirmed", "Execute Live Migration", class = "btn-danger")
    )
  ))
})

# Also handle the "Execute Live Migration Now" button from test results
observeEvent(input$execute_live_migration_after_test, {
  removeModal()
  
  # Show same final confirmation with operator info
  showModal(modalDialog(
    title = "FINAL CONFIRMATION",
    div(
      style = "padding: 15px;",
      div(
        style = "background-color: #f8d7da; padding: 20px; border-radius: 5px; margin-bottom: 15px;",
        h4(icon("exclamation-triangle"), " WARNING: PRODUCTION DATABASE", style = "color: #721c24; margin-top: 0;"),
        p("This will PERMANENTLY save data to the production database.", 
          style = "color: #721c24; font-size: 1.1em;"),
        p("This action cannot be undone!", 
          style = "color: #721c24; font-weight: bold;")
      ),
      div(
        style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #bee5eb;",
        h5(icon("user-shield"), " Operator Identification", style = "color: #0c5460; margin-top: 0;"),
        p("This information will be recorded in the audit trail.", style = "color: #6c757d; font-size: 0.85em;"),
        fluidRow(
          column(6,
            textInput("operator_name", "Full Name:", placeholder = "e.g., John Smith")
          ),
          column(6,
            textInput("operator_designation", "Designation / Role:", placeholder = "e.g., Data Engineer")
          )
        )
      ),
      p("Type 'CONFIRM' to proceed:", style = "font-weight: bold;"),
      textInput("final_confirmation", NULL, placeholder = "Type CONFIRM here")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("execute_live_confirmed", "Execute Live Migration", 
                  class = "btn-danger")
    )
  ))
})

# Actually execute the live migration after confirmation
observeEvent(input$execute_live_confirmed, {
  req(input$final_confirmation == "CONFIRM")
  
  # Validate operator info
  operator_name <- trimws(input$operator_name %||% "")
  operator_designation <- trimws(input$operator_designation %||% "")
  if(nchar(operator_name) == 0) {
    showNotification("Please enter your full name for the audit trail.", type = "warning", duration = 5)
    return()
  }
  if(nchar(operator_designation) == 0) {
    showNotification("Please enter your designation/role for the audit trail.", type = "warning", duration = 5)
    return()
  }
  
  removeModal()
  
  # Rebuild config one last time
  unique_timeperiods <- unique(migration_values$preview_data$timeperiod[!is.na(migration_values$preview_data$timeperiod)])
  unique_agroups <- unique(migration_values$preview_data$agroup[!is.na(migration_values$preview_data$agroup)])
  
  timeperiod_mapping <- list()
  for(tp in unique_timeperiods) {
    input_id <- paste0("timeperiod_map_", gsub("[^[:alnum:]]", "_", tp))
    timeperiod_mapping[[as.character(tp)]] <- input[[input_id]]
  }
  
  agroup_mapping <- list()
  for(ag in unique_agroups) {
    input_id <- paste0("agroup_map_", gsub("[^[:alnum:]]", "_", ag))
    agroup_mapping[[as.character(ag)]] <- input[[input_id]]
  }
  
  # Collect patient mappings for unmapped patients
  patient_mapping <- list()
  unique_patients <- unique(migration_values$preview_data$patientid)
  unique_patients <- unique_patients[!is.na(unique_patients)]
  
  patients_needing_mapping <- c()
  for(pid in unique_patients) {
    patient_rows <- migration_values$preview_data[migration_values$preview_data$patientid == pid, ]
    subject_acc <- patient_rows$subject_accession[1]
    
    if(is.null(subject_acc) || is.na(subject_acc) || nchar(subject_acc) == 0) {
      patients_needing_mapping <- c(patients_needing_mapping, pid)
      input_id <- paste0("patient_map_", gsub("[^[:alnum:]]", "_", pid))
      mapped_value <- input[[input_id]]
      if(!is.null(mapped_value) && nzchar(mapped_value)) {
        patient_mapping[[as.character(pid)]] <- mapped_value
      }
    }
  }
  
  # Validate patient mappings
  if(length(patient_mapping) != length(patients_needing_mapping)) {
    showNotification(
      paste0("Please map all ", length(patients_needing_mapping), " unmapped patient IDs before executing"),
      type = "warning", duration = 5
    )
    return()
  }
  
  # Apply manual patient mappings to preview data
  preview_data_with_mappings <- migration_values$preview_data
  if(length(patient_mapping) > 0) {
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- preview_data_with_mappings$patientid[i]
      if(!is.na(pid) && (is.null(preview_data_with_mappings$subject_accession[i]) || 
                         is.na(preview_data_with_mappings$subject_accession[i]) || 
                         nchar(preview_data_with_mappings$subject_accession[i]) == 0)) {
        mapped_subject <- patient_mapping[[as.character(pid)]]
        if(!is.null(mapped_subject)) {
          preview_data_with_mappings$subject_accession[i] <- mapped_subject
        }
      }
    }
  }
  
  # Apply file-based mapping override (from CSV/Excel upload)
  if(!is.null(migration_values$mapping_override)) {
    file_map <- migration_values$mapping_override
    for(i in 1:nrow(preview_data_with_mappings)) {
      pid <- as.character(preview_data_with_mappings$patientid[i])
      match_idx <- which(file_map$patientid == pid)
      if(length(match_idx) > 0) {
        preview_data_with_mappings$subject_accession[i] <- file_map$subject_accession[match_idx[1]]
      }
    }
  }
  
  config <- list(
    workspace_id = current_workspace(),
    study_accession = input$target_study,
    experiment_accession = if(input$create_new_experiment) input$new_experiment_accession else input$target_experiment,
    create_new_experiment = input$create_new_experiment %||% FALSE,
    new_experiment_name = input$new_experiment_name,
    new_experiment_description = input$new_experiment_description,
    new_experiment_tech = if(input$create_new_experiment) input$new_experiment_tech else NULL,
    measurement_technique = input$measurement_technique,
    sample_prefix = ifelse(is.null(input$sample_prefix) || input$sample_prefix == "", "ES_", input$sample_prefix),
    default_arm_type = input$default_arm_type %||% "Observational",

    default_species = input$default_species,
    result_schema_type = input$result_schema_type %||% "MBAA",
    result_schema = input$result_schema_type %||% "MBAA",
    source_schema = Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results"),
    source_study = input$study_accession_filter,
    timeperiod_mapping = timeperiod_mapping,
    agroup_mapping = agroup_mapping,
    ispi_project_id = input$ispi_project_id,
    operator_name = operator_name,
    operator_designation = operator_designation,
    rerun_mode = migration_values$rerun_mode %||% "first_run"
  )

  # Show processing modal
  showModal(modalDialog(
    title = "Migration In Progress",
    div(
      style = "text-align: center; padding: 30px;",
      tags$div(class = "fa fa-spinner fa-spin", style = "font-size: 48px; color: #e74c3c; margin-bottom: 20px;"),
      h4("Executing LIVE Migration...", style = "color: #e74c3c;"),
      h5(id = "live_progress_msg", "Initializing...", style = "color: #333; font-weight: bold; margin-top: 15px;"),
      p(id = "live_progress_detail", "Preparing database transaction.", style = "color: #666; font-size: 0.95em;"),
      p("This may take several minutes for large datasets. Please do not close this window.",
        style = "color: #999; font-size: 0.85em; font-style: italic;")
    ),
    footer = NULL,
    easyClose = FALSE,
    size = "m"
  ))

  # Execute LIVE migration with live step progress
  
  # In-memory log capture for download (no disk file)
  log_con <- textConnection("live_log_lines", "w", local = FALSE)
  
  # Start logging to memory
  cat(paste0("[INFO] Starting Live Migration Logging\n"))
  sink(log_con, split = TRUE)
  on.exit({
    try(sink(), silent = TRUE)
    try(close(log_con), silent = TRUE)
  }, add = TRUE)
  
  source_conn <- NULL
  tryCatch({
    # Create source DB connection using env vars
    source_conn <- tryCatch({
      get_source_conn()
    }, error = function(e) {
      cat("[WARN] Could not create source DB connection:", e$message, "\n")
      NULL
    })
    
    progress_cb <- function(msg, detail = "") {
      shinyjs::html(id = "live_progress_msg", html = msg)
      shinyjs::html(id = "live_progress_detail", html = detail)
    }
    
    results <<- execute_migration(
      conn, preview_data_with_mappings, config,
      commit = TRUE, source_conn = source_conn,
      progress_cb = progress_cb
    )
    
    removeModal()
    
    # Store log content for download handler
    migration_values$live_log_content <- tryCatch(
      paste(live_log_lines, collapse = "\n"),
      error = function(e) "Log capture failed."
    )
    
    # Insert audit trail record (success)
    tryCatch({
      DBI::dbExecute(conn, "
        INSERT INTO madi_dat.migration_audit_log (
          operator_name, operator_designation, migration_mode,
          study_accession, experiment_accession, source_study,
          status, subjects_validated, expsamples_inserted, expsamples_failed,
          mbaa_results_inserted, controls_inserted, standard_curves_inserted,
          model_qc_inserted, sample_qc_inserted,
          completed_at, workspace_id
        ) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,CURRENT_TIMESTAMP,$16)
      ", params = list(
        config$operator_name, config$operator_designation, "LIVE",
        config$study_accession, config$experiment_accession,
        config$source_study %||% "",
        "COMPLETED",
        if(!is.null(results$subjects$validated)) results$subjects$validated else 0L,
        if(!is.null(results$expsamples$inserted)) results$expsamples$inserted else 0L,
        if(!is.null(results$expsamples$failed)) length(results$expsamples$failed) else 0L,
        if(!is.null(results$mbaa_results$inserted_result)) results$mbaa_results$inserted_result else 0L,
        if(!is.null(results$controls$inserted)) results$controls$inserted else 0L,
        if(!is.null(results$standard_curves$inserted)) results$standard_curves$inserted else 0L,
        if(!is.null(results$model_qc$inserted)) results$model_qc$inserted else 0L,
        if(!is.null(results$sample_qc$inserted)) results$sample_qc$inserted else 0L,
        config$workspace_id
      ))
      cat("[OK] Audit trail record created.\n")
    }, error = function(e) {
      cat("[WARN] Failed to create audit trail:", e$message, "\n")
    })
    
    # Helper to safely extract result counts
    sc <- function(x, field = "inserted") {
      if(is.null(x)) return(0)
      val <- x[[field]]
      if(is.null(val)) return(0)
      if(is.list(val)) return(length(val))
      as.integer(val)
    }
    
    # Show success modal with full summary
    showModal(modalDialog(
      title = div(icon("check-circle", style = "color: #28a745;"), " Migration Completed Successfully!"),
      size = "l",
      div(
        style = "padding: 15px;",
        div(
          style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
          h4(icon("database"), " Data Successfully Migrated!", style = "color: #155724; margin-top: 0;"),
          p("All data has been committed to the production database.", 
            style = "color: #155724; margin: 0;"),
          p(icon("user"), " Operator: ", strong(config$operator_name), 
            " (", config$operator_designation, ")",
            style = "color: #155724; margin-top: 5px;"),
          downloadButton("download_live_log", "Download Migration Log", 
                        class = "btn-sm btn-outline-success", style = "margin-top: 8px;"),
          downloadButton("download_dq_report", "Download Data Quality Report", 
                        class = "btn-sm btn-outline-info", style = "margin-top: 8px; margin-left: 8px;")
        ),
        
        # Full Migration Summary
        h5(icon("clipboard-list"), " Migration Summary", style = "color: #0c5460; margin-bottom: 10px;"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
          tags$table(
            style = "width: 100%; font-size: 0.95em; border-collapse: collapse;",
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Experiment:"), style = "padding: 6px; width: 40%;"),
              tags$td(paste0(config$experiment_accession, 
                      if(!isTRUE(config$create_new_experiment)) " (existing)" else " (new)"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Subjects:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$subjects, "validated"), " validated, ", sc(results$subjects, "missing"), " missing"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Planned Visits:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$planned_visits, "created"), " created"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Biosamples:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$biosamples), " inserted, ", sc(results$biosamples, "existing"), " existing"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Arms/Cohorts:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$arms, "created"), " created"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Arm Associations:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$arm_associations), " inserted"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Experiment Samples:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$expsamples), " inserted, ", 
                             sc(results$expsamples, "existing"), " existing, ",
                             length(results$expsamples$failed %||% list()), " failed, ",
                             length(results$expsamples$skipped %||% list()), " skipped"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td("Biosample Links:", style = "padding: 6px; font-weight: bold;"),
              tags$td(paste0(sc(results$biosample_links, "linked"), " linked"), style = "padding: 6px; color: #28a745;"),
              tags$td(paste0(length(results$biosample_links$failed), " failed"), style = "padding: 6px; color: #dc3545;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td("MBAA Results:", style = "padding: 6px; font-weight: bold;"),
              tags$td(paste0(sc(results$mbaa_results, "inserted_result"), " inserted"), style = "padding: 6px; color: #28a745; font-weight: bold;"),
              tags$td("-", style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Control Samples:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$control_samples), " inserted, ", sc(results$control_samples, "failed"), " failed"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Control MBAA Results:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$control_results), " inserted, ", sc(results$control_results, "failed"), " failed"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Standard Curves:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$standard_curves), " inserted, ", sc(results$standard_curves, "failed"), " failed"), style = "padding: 6px;")
            ),
            tags$tr(style = "border-bottom: 1px solid #dee2e6;",
              tags$td(strong("Model QC Data:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$model_qc), " inserted, ", sc(results$model_qc, "failed"), " failed"), style = "padding: 6px;")
            ),
            tags$tr(
              tags$td(strong("Sample QC Data:"), style = "padding: 6px;"),
              tags$td(paste0(sc(results$sample_qc), " inserted, ", sc(results$sample_qc, "failed"), " failed"), style = "padding: 6px;")
            )
          )
        ),
        
        # Show failures/warnings summary if any
        if(length(results$all_failures %||% list()) > 0 || length(results$all_skipped %||% list()) > 0) {
          tagList(
            hr(),
            div(
              style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              h5(icon("exclamation-triangle"), " Items That Needed Attention", style = "color: #856404; margin-top: 0;"),
              if(length(results$all_failures %||% list()) > 0) {
                p(strong(length(results$all_failures), " items failed"), " - see log for details.", 
                  style = "color: #721c24;")
              },
              if(length(results$all_skipped %||% list()) > 0) {
                p(strong(length(results$all_skipped), " items skipped"), " - see log for details.", 
                  style = "color: #856404;")
              }
            )
          )
        },
        
        hr(),
        div(
          style = "background-color: #cce5ff; padding: 10px; border-radius: 5px;",
          p(icon("info-circle"), strong(" Note:"), " You can view the migrated data in the 'View Study' section.", 
            style = "color: #004085; margin: 0;")
        )
      ),
      footer = modalButton("Close")
    ))
    
    # Generate DQ audit report (runs after commit, outside transaction)
    tryCatch({
      message("[DQ] Generating data quality audit report for ", config$experiment_accession)
      dq_report <- generate_dq_audit_report(conn, config$experiment_accession)
      migration_dq_report(dq_report)
      message("[DQ] Report generated successfully (", nchar(dq_report), " chars)")
    }, error = function(e) {
      message("[DQ ERROR] Could not generate DQ audit report: ", e$message)
      cat("[WARN] Could not generate DQ audit report:", e$message, "\n")
    })
    
    showNotification("Migration completed successfully!", type = "message", duration = 5)
    
  }, error = function(e) {
    removeModal()
    showNotification(paste("Live migration failed:", e$message), type = "error", duration = 10)
    
    # Store log content for download even on failure
    migration_values$live_log_content <- tryCatch(
      paste(live_log_lines, collapse = "\n"),
      error = function(err) "Log capture failed."
    )
    
    # Insert audit trail record (failure)
    tryCatch({
      DBI::dbExecute(conn, "
        INSERT INTO madi_dat.migration_audit_log (
          operator_name, operator_designation, migration_mode,
          study_accession, experiment_accession,
          status, error_message, completed_at, workspace_id
        ) VALUES ($1,$2,$3,$4,$5,$6,$7,CURRENT_TIMESTAMP,$8)
      ", params = list(
        config$operator_name, config$operator_designation, "LIVE",
        config$study_accession, config$experiment_accession,
        "FAILED", e$message, config$workspace_id
      ))
    }, error = function(e2) {})
    
    showModal(modalDialog(
      title = div(icon("times-circle", style = "color: #dc3545;"), " Migration Failed"),
      div(
        style = "padding: 15px;",
        div(
          style = "background-color: #f8d7da; padding: 15px; border-radius: 5px;",
          h4(icon("exclamation-triangle"), " Error occurred during migration", style = "color: #721c24; margin-top: 0;"),
          p(e$message, style = "color: #721c24; font-family: monospace;")
        ),
        p("All changes have been rolled back. No data was saved to the database.", 
          style = "margin-top: 15px;"),
        downloadButton("download_live_log", "Download Error Log", 
                      class = "btn-sm btn-outline-danger", style = "margin-top: 8px;")
      ),
      footer = modalButton("Close")
    ))
  }, finally = {
    # Clean up source connection
    if(!is.null(source_conn) && DBI::dbIsValid(source_conn)) {
      tryCatch(DBI::dbDisconnect(source_conn), error = function(e) {})
    }
  })
})

# Handle the wizard button from sidebar
observeEvent(input$start_migration_wizard, {
  # Just switch to the migration tab since we've already set it up
  showNotification("Migration interface loaded! Configure connection and preview data.", 
                   type = "success", duration = 5)
})

# Populate Source Study List when access is validated
observeEvent(migration_values$access_validated, {
    req(isTRUE(migration_values$access_validated))
    source_schema <- Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results")
    project_id <- input$ispi_project_id
    tryCatch({
       conn_source <- get_source_conn()
       on.exit(DBI::dbDisconnect(conn_source))
       
       # Query source database for studies filtered by project_id
       studies <- get_source_studies_list(conn_source, source_schema, project_id)
       if(length(studies) > 0) {
          selected_study <- if(!is.null(input$target_study) && input$target_study %in% studies) input$target_study else studies[1]
          updateSelectInput(session, "study_accession_filter", choices = studies, selected = selected_study)
       } else {
          updateSelectInput(session, "study_accession_filter", choices = NULL)
       }
    }, error = function(e) {
       print(paste("DEBUG: Failed to load source studies:", e$message))
       updateSelectInput(session, "study_accession_filter", choices = NULL)
    })
})

# Populate Experiment list when study changes
observeEvent(input$study_accession_filter, {
    req(input$study_accession_filter, isTRUE(migration_values$access_validated))
    source_schema <- Sys.getenv("ISPI_SOURCE_SCHEMA", "madi_results")
    project_id <- input$ispi_project_id
    tryCatch({
       conn_source <- get_source_conn()
       on.exit(DBI::dbDisconnect(conn_source))
       
       experiments <- get_source_experiments_list(conn_source, source_schema, project_id, input$study_accession_filter)
       if(length(experiments) > 0) {
          updateSelectInput(session, "experiment_accession_filter", choices = experiments, selected = experiments[1])
       } else {
          updateSelectInput(session, "experiment_accession_filter", choices = NULL)
       }
    }, error = function(e) {
       print(paste("DEBUG: Failed to load source experiments:", e$message))
       updateSelectInput(session, "experiment_accession_filter", choices = NULL)
    })
})