#Porous pavement tabs
#This has a tab dropdown with four tabs, one for adding PPTs and one for viewing all PPTs and one for viewing future PPTs and one for viewing averages

#1.0 UI ----

porous_pavementUI <- function(id, label = "porous_pavement", html_req, surface_type, priority, con_phase, future_req){
  ns <- NS(id)
  navbarPage("Porous Pavement", id = "inTabset", #theme = shinytheme("cerulean"),
             #1.1 Add/Edit ----
             tabPanel("Add/Edit Porous Pavement Test", value = "ppt_tab", 
                      titlePanel("Add Porous Pavement Test"), 
                      #1.1.1 Sidebar Panel ----
                      sidebarPanel(selectizeInput(ns("smp_id"), future_req(html_req("SMP ID")), choices = NULL, 
                                                  options = list(
                                                    placeholder = 'Select an Option',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                  )), 
                                   dateInput(ns("date"), html_req("Test Date"), value = as.Date(NA)), 
                                   selectInput(ns("surface_type"), html_req("Surface Type"), choices = c("", surface_type$surface_type), selected = NULL), 
                                   selectInput(ns("con_phase"), html_req("Construction Phase"), choices = c("", con_phase$phase), selected = NULL),
                                   textInput(ns("location"), "Test Location"),
                                   fluidRow(
                                   column(6, selectInput(ns("ring_dia"), "Ring Diameter (in)", choices = c("","9.75", "11.625"), selected = NULL)),
                                   column(6, numericInput(ns("prewet_time"), "Prewet Time (sec)", value = NA, min = 0))
                                   ), 
                                   conditionalPanel(condition = "input.prewet_time > 599", 
                                                    ns = ns, 
                                                    disabled(numericInput(ns("pw_rate"), "Rate (in/hr) based on Prewet Time", value = NA, min = 0))),
                                   conditionalPanel(condition = "input.prewet_time < 599", 
                                                   ns = ns, 
                                                   selectInput(ns("weight"), "Mass of Water (lb)",
                                                                        choices = c("", 8.34, 41.7), selected = NULL),
                                                   fluidRow(
                                                     column(6, numericInput(ns("time_one"), "T1 Time (sec)", value = NA, min = 0)),
                                                     column(6, disabled(numericInput(ns("rate_one"), "T1 Rate (in/hr)", value = NA, min = 0)))
                                                     ),
                                                   fluidRow(
                                                     column(6, numericInput(ns("time_two"), "T2 Time (sec)", value = NA, min = 0)),
                                                     column(6, disabled(numericInput(ns("rate_two"), "T2 Rate (in/hr)", value = NA, min = 0)))
                                                   )
                                                   ),
                                   selectInput(ns("data"), "Data in Spreadsheet", choices = c("","Yes" = "1", "No" = "0"), selected = NULL), 
                                   selectInput(ns("folder"), "Test Location Map in Site Folder", choices = c("","Yes" = "1", "No" = "0"), selected = NULL),
                                   conditionalPanel(condition = "input.date === null", 
                                                    ns = ns, 
                                                    selectInput(ns("priority"), "Future Test Priority", 
                                                                choices = c("", priority$field_test_priority[1:3]), selected = NULL)),
                                   conditionalPanel(condition = "input.date === null", 
                                                    ns= ns, 
                                                    actionButton(ns("future_ppt"), "Add Future Porous Pavement Test")),
                                   actionButton(ns("add_ppt"), "Add Porous Pavement Test"), 
                                   actionButton(ns("clear_ppt"), "Clear All Fields"),
                                   #actionButton(ns("print_check"), "Print Check"),
                                   fluidRow(
                                     HTML(paste(html_req(""), " indicates required field for complete tests. ", future_req(""), " indicates required field for future tests.")))
                      ), 
                      #1.1.2 Main Panel -----
                      mainPanel(
                        conditionalPanel(condition = "input.smp_id",
                                         ns = ns, 
                                         h4(textOutput(ns("future_header"))),
                                         DTOutput(ns("future_ppt_table")),
                                         h4(textOutput(ns("header"))), 
                                         DTOutput(ns("ppt_table"))))
             ), 
             #1.2 View Completed Tests ----
             tabPanel("View Porous Pavement Tests", value = ns("view_ppt"), 
                      titlePanel("All Porous Pavement Tests"), 
                      reactableOutput(ns("all_ppt_table"))
             ), 
             # 1.3 View Future Tests ---------------------------------------------------
             tabPanel("View Future Porous Pavement Tests", value = ns("view_future_ppt"), 
                      titlePanel("All Future Porous Pavement Tests"), 
                      DTOutput(ns("all_future_ppt_table"))), 
             #1.4 View Averages -----
             tabPanel("View Averages", value = ns("view_avg"), 
                      fluidRow(column(6, selectizeInput(ns("smp_id_avg"), "SMP ID", choices = NULL, multiple = TRUE,
                                     options = list(
                                       placeholder = 'Select an Option',
                                       onInitialize = I('function() { this.setValue(""); }')
                                     ))),
                               column(6, downloadButton(ns("download_avg"), "Download Averages"))),
                      titlePanel("View Averages"), 
                      DTOutput(ns("averages")))
  )
  
}

#2.0 Server Function ----

porous_pavementServer <- function(id, parent_session, surface_type, poolConn, con_phase, smp_id){
  
  moduleServer(
    id, 
    function(input, output, session){
  
      #2.0.1 Initial Set up -----
      #define ns to use in modals
      ns <- session$ns
      
      #update SMP IDs
      updateSelectizeInput(session, "smp_id", choices = smp_id, selected = character(0), server = TRUE)
      
      #update SMP IDs
      updateSelectizeInput(session, "smp_id_avg", choices = smp_id, selected = character(0), server = TRUE)
      
      #initialize porous pavement testing (ppt) reactiveValues
      rv <- reactiveValues()
      
      #2.1 Add/Edit ----
      #2.1.1 Headers ----
      
      #Get the Project name, combine it with SMP ID, and create a reactive header
      rv$smp_and_name_step <- reactive(odbc::dbGetQuery(poolConn, paste0("select smp_id, project_name from external.mat_project_names where smp_id = '", input$smp_id, "'")))
      
      rv$smp_and_name <- reactive(paste(rv$smp_and_name_step()$smp_id[1], rv$smp_and_name_step()$project_name[1]))
      
      output$header <- renderText(
        paste("Porous Pavement Tests at", rv$smp_and_name())
      )
      
      output$future_header <- renderText(
        paste("Future Porous Pavement Tests at", rv$smp_and_name())
      )
      
      #2.1.2 toggle states based on what's selected ----
      #toggle state (enable/disable) buttons based on whether system id, test date, and type are selected (this is shinyjs)
      observe(toggleState(id = "add_ppt", condition = nchar(input$smp_id) > 0 & length(input$date) > 0 & 
                            nchar(input$surface_type) >0 & nchar(input$con_phase) > 0))
      
      #toggle state for future cet depending on whether smp_id is selected
      observe(toggleState(id = "future_ppt", condition = nchar(input$smp_id) > 0))
      
      #toggle state for metadata depending on whether a test date is included
      observe(toggleState(id = "data", condition = length(input$date) > 0))
      observe(toggleState(id = "folder", condition = length(input$date) > 0))
      observe(toggleState(id = "ring_dia", condition = length(input$date) > 0))
      observe(toggleState(id = "prewet_time", condition = length(input$date) > 0))
      observe(toggleState(id = "pw_rate", condition = length(input$date) > 0))
      observe(toggleState(id = "weight", condition = length(input$date) > 0))
      observe(toggleState(id = "time_one", condition = length(input$date) > 0))
      observe(toggleState(id = "time_two", condition = length(input$date) > 0))
      
      #add/edit button toggle
      rv$label <- reactive(if(length(input$ppt_table_rows_selected) == 0) "Add New" else "Edit Selected")
      observe(updateActionButton(session, "add_ppt", label = rv$label()))
      
      rv$future_label <- reactive(if(length(input$future_ppt_table_rows_selected) == 0) "Add Future Porous Pavement Test" 
                                  else "Edit Selected Future PPT")
      observe(updateActionButton(session, "future_ppt", label = rv$future_label()))
      
      #2.1.3 SMP views -----
      #query full smp porous pavement view
      rv$query <- reactive(paste0("SELECT * FROM fieldwork.viw_porous_pavement_wide WHERE smp_id = '", input$smp_id, "'"))
      
      rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
      
      #adjust table for viewing
      rv$ppt_table <- reactive(rv$ppt_table_db() %>% 
                                 mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                 mutate(across(where(is.POSIXlt), as.character)) %>%
                                 mutate(average_rate_inhr = round(average_rate_inhr, 1)) %>% 
                                 dplyr::select("test_date", "surface_type", "phase", "test_location", "average_rate_inhr"))
      
      #render datatable  for porous pavement
      output$ppt_table <- renderDT(
        rv$ppt_table(), 
        selection = 'single', 
        style = 'bootstrap', 
        class = 'table-responsive, table-hover', 
        colnames = c('Test Date', 'Surface Type', 'Construction Phase', 'Test Location', 'Average Rate (in/hr)')
      )
      
      
      #query future PPTs
      future_ppt_table_query <- reactive(paste0("SELECT * FROM fieldwork.viw_future_porous_pavement_full 
                                                WHERE smp_id = '", input$smp_id, "' 
                                                order by field_test_priority_lookup_uid"))
      rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
      
      rv$future_ppt_table <- reactive(rv$future_ppt_table_db() %>% 
                                        dplyr::select("smp_id", "surface_type", "phase", "test_location", "field_test_priority"))
      
      
      output$future_ppt_table <- renderDT(
        rv$future_ppt_table(), 
        selection = 'single', 
        style = 'bootstrap', 
        class = 'table-responsive, table-hover', 
        colnames = c('SMP ID', 'Surface Type', 'Construction Phase', 'Test Location', 'Priority') 
      )
      
      
      #2.1.4 Editing upon row selection ----
      
      #future table
      observeEvent(input$future_ppt_table_rows_selected, {
        #deselect other table
        dataTableProxy('ppt_table') %>% selectRows(NULL)
       
        #update to values from selected row
        updateSelectInput(session, "surface_type", selected = rv$future_ppt_table()$surface_type[input$future_ppt_table_rows_selected])
        updateSelectInput(session, "con_phase", selected = rv$future_ppt_table()$phase[input$future_ppt_table_rows_selected])
        updateNumericInput(session, "location", value = rv$future_ppt_table()$test_location[input$future_ppt_table_rows_selected])
        updateSelectInput(session, "priority", selected = rv$future_ppt_table()$field_test_priority[input$future_ppt_table_rows_selected])
        
        reset("date")
        reset("data")
        reset("folder")
        
      })
      
      #present table
      observeEvent(input$ppt_table_rows_selected,{ 
        
        #deselect other table
        dataTableProxy('future_ppt_table') %>% selectRows(NULL)
        
        #update to values from selected row
        updateDateInput(session, "date", value = rv$ppt_table_db()$test_date[input$ppt_table_rows_selected])
        updateSelectInput(session, "surface_type", selected = rv$ppt_table_db()$surface_type[input$ppt_table_rows_selected])
        updateSelectInput(session, "con_phase", selected = rv$ppt_table_db()$phase[input$ppt_table_rows_selected])
        updateNumericInput(session, "location", value = rv$ppt_table_db()$test_location[input$ppt_table_rows_selected])
        
        #update metadata values
        updateSelectInput(session, "data", selected = rv$ppt_table_db()$data_in_spreadsheet[input$ppt_table_rows_selected])
        updateSelectInput(session, "folder", selected = rv$ppt_table_db()$map_in_site_folder[input$ppt_table_rows_selected])
        
        #update general results values
        updateSelectInput(session, "ring_dia", selected = rv$ppt_table_db()$ring_diameter_in[input$ppt_table_rows_selected])
        updateSelectInput(session, "prewet_time", selected = rv$ppt_table_db()$prewet_time_s[input$ppt_table_rows_selected])
        updateSelectInput(session, "pw_rate", selected = rv$ppt_table_db()$prewet_rate_inhr[input$ppt_table_rows_selected])
        updateSelectInput(session, "weight", selected = rv$ppt_table_db()$test_one_weight_lbs[input$ppt_table_rows_selected])
        
        #update per test results values
        updateSelectInput(session, "time_one", selected = rv$ppt_table_db()$test_one_time_s[input$ppt_table_rows_selected])
        updateSelectInput(session, "rate_one", selected = rv$ppt_table_db()$test_one_rate_inhr[input$ppt_table_rows_selected])
        updateSelectInput(session, "time_two", selected = rv$ppt_table_db()$test_two_time_s[input$ppt_table_rows_selected])
        updateSelectInput(session, "rate_two", selected = rv$ppt_table_db()$test_two_rate_inhr[input$ppt_table_rows_selected])
        
      })
      
      #2.1.5 Prepare Inputs -----
      #change type from text to uid
      rv$surface_type <- reactive(surface_type %>% dplyr::filter(surface_type == input$surface_type) %>% 
                                    select(surface_type_lookup_uid) %>% pull())
      
      rv$phase <- reactive(con_phase %>% dplyr::filter(phase == input$con_phase) %>% 
                             select(con_phase_lookup_uid) %>% pull())
      
      rv$phase_null <- reactive(if(nchar(input$con_phase) == 0) "NULL" else paste0("'", rv$phase(), "'"))
      
      #set inputs to reactive values so "NULL" can be entered
      #important to correctly place quotations
      rv$type <- reactive(if(length(rv$surface_type()) == 0) "NULL" else paste0("'", rv$surface_type(), "'"))
      rv$location_step <- reactive(gsub('\'', '\'\'', input$location))
      rv$test_location <- reactive(if(nchar(rv$location_step()) == 0) "NULL" else paste0("'", rv$location_step(), "'"))
      rv$data <- reactive(if(nchar(input$data) == 0 | input$data == "N/A") "NULL" else paste0("'", input$data, "'"))
      rv$folder <- reactive(if(nchar(input$folder) == 0 | input$folder == "N/A") "NULL" else paste0("'", input$folder, "'"))
      
      rv$ring_dia <- reactive(if(nchar(input$ring_dia) == 0) "NULL" else paste0("'", input$ring_dia, "'"))
      rv$prewet_time <- reactive(if(is.na(input$prewet_time)) "NULL" else paste0("'", input$prewet_time, "'"))
      
      rv$priority_lookup_uid_query <- reactive(paste0("select field_test_priority_lookup_uid from fieldwork.field_test_priority_lookup where field_test_priority = '", input$priority, "'"))
      rv$priority_lookup_uid_step <- reactive(dbGetQuery(poolConn, rv$priority_lookup_uid_query()))
      rv$priority_lookup_uid <- reactive(if(nchar(input$priority) == 0) "NULL" else paste0("'", rv$priority_lookup_uid_step(), "'"))
      
      rv$pw_rate <- reactive(if(is.na(input$pw_rate)) "NULL" else paste0("'", input$pw_rate, "'"))
      
      rv$weight <- reactive(if(nchar(input$weight) == 0) "NULL" else paste0("'", input$weight, "'"))
      
      rv$time_one <- reactive(if(is.na(input$time_one)) "NULL" else paste0("'", input$time_one, "'"))
      rv$time_two <- reactive(if(is.na(input$time_two)) "NULL" else paste0("'", input$time_two, "'"))
      
      rv$rate_one <- reactive(if(is.na(input$rate_one)) "NULL" else paste0("'", input$rate_one, "'"))
      rv$rate_two <- reactive(if(is.na(input$rate_two)) "NULL" else paste0("'", input$rate_two, "'"))
      
      #2.1.6 Do the Math ----
      observeEvent(rv$prewet_time(), {
        
        if(rv$prewet_time() > 599.9){
           reset("weight")
           reset("time_one")
           reset("time_two")
           reset("rate_one")
           reset("rate_two")
          }else if(rv$prewet_time() < 599.89){
            #reset("pw_rate")
          }
      })
      
      #calculate infiltration rate
      #I = (KM)/(D^2*t)
      k <- 126870
      #D = ring diameter (in)
      #M = 8.34 lbs - 1 gal of water 
      m <- 8.34
      #t = prewet time
      #infiltration rate based on prewet time
      rv$pw_rate_inhr <- reactive(if(length(input$ring_dia) > 0 & !is.na(input$prewet_time)){
                                  if(input$prewet_time > 599 & input$prewet_time <= 3600){
        round(((k*m)/((as.numeric(input$ring_dia)^2)*input$prewet_time)),1)
                                  }else{
        NA
      }
        })
      
      observe(updateNumericInput(session = session, "pw_rate", value = rv$pw_rate_inhr()))
      
      #infiltration rate based on tests ONE
      rv$rate_inhr_one <- reactive(if(length(input$weight) > 0 & !is.na(input$time_one)){
        round(((k*as.numeric(input$weight))/((as.numeric(input$ring_dia)^2)*input$time_one)),1)
      })
      
      observe(updateNumericInput(session = session, "rate_one", value = rv$rate_inhr_one()))
      
      #infiltration rate based on tests  TWO
      rv$rate_inhr_two <- reactive(if(length(input$weight) > 0 & !is.na(input$time_two)){
        round(((k*as.numeric(input$weight))/((as.numeric(input$ring_dia)^2)*input$time_two)),1)
      })
      
      observe(updateNumericInput(session = session, "rate_two", value = rv$rate_inhr_two()))
      # observeEvent(input$prewet_time, {
      #   print(rv$pw_rate_inhr())
      # })
      # 
      
      rv$weight_react <- reactive(if(!is.na(input$prewet_time)){
                                  if(input$prewet_time < 30){
        41.7
      }else if(input$prewet_time >= 30){
        8.34
      }
      }else{
        8.34
      }
      )
      
      observeEvent(input$prewet_time,{
        if(length(input$ppt_table_rows_selected) == 0){
          updateSelectInput(session = session, "weight", selected = rv$weight_react())
        }
      }
      )

      #2.1.7 ADD/EDIT/CLEAR Buttons ----
      #on click 
      observeEvent(input$future_ppt, {
        if(length(input$future_ppt_table_rows_selected) == 0){
          #add to future_porous_pavement
          add_future_ppt_query <- paste0("INSERT INTO fieldwork.tbl_future_porous_pavement (smp_id, surface_type_lookup_uid, 
                                          con_phase_lookup_uid, test_location, field_test_priority_lookup_uid)
                                         VALUES ('", input$smp_id, "', ", rv$type(), ", ", rv$phase_null(), ", ", rv$test_location(), ", ", 
                                         rv$priority_lookup_uid(), ")")
          
          odbc::dbGetQuery(poolConn, add_future_ppt_query)
        }else{
          edit_future_ppt_query <- paste0("UPDATE fieldwork.tbl_future_porous_pavement SET smp_id = '", input$smp_id, "', 
                                           surface_type_lookup_uid = ", rv$type(),
                                          ", con_phase_lookup_uid = ", rv$phase_null(),
                                          ", test_location = ", rv$test_location(),
                                          ", field_test_priority_lookup_uid = ", rv$priority_lookup_uid(), "
            WHERE future_porous_pavement_uid = '", rv$future_ppt_table_db()[input$future_ppt_table_rows_selected, 1], "'")
          
          odbc::dbGetQuery(poolConn, edit_future_ppt_query)
        }
        
        rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
        rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
        
        reset("date")
        reset("surface_type")
        reset("con_phase")
        reset("location")
        reset("data")
        reset("priority")
        reset("folder")
      })
      
      #on click
      observeEvent(input$add_ppt, {
      
        #if there are no rows selected, add new data
        if(length(input$ppt_table_rows_selected) == 0){
          
          #if prewet time is > 599, only add to record table
          #if less, then prepare to add to record and results tables
          if(rv$prewet_time() > 599){
            #add to porous_pavement
            add_ppt_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement (test_date, smp_id, surface_type_lookup_uid, con_phase_lookup_uid,
                              test_location, data_in_spreadsheet, map_in_site_folder, ring_diameter_in, prewet_time_s, prewet_rate_inhr)
          	                  VALUES ('", input$date, "','", input$smp_id, "',",  rv$type(), ",", rv$phase(), ",", rv$test_location(), ",",
          	                                rv$data(), ", ", rv$folder(), ", ", rv$ring_dia(), ", ", rv$prewet_time(), ", ", rv$pw_rate(), ")")


            odbc::dbGetQuery(poolConn, add_ppt_query)
          }else{
            #add to porous pavement & porous results
            #add to porous_pavement table
            #then add to the porous_pavement_results table
            #use the MAX(porous_pavement_uid) from pp table to get the PP UID of the most recent addition to the table (calculated by SERIAL), which is the current addition
            #add to porous_pavement
            add_ppt_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement (test_date, smp_id, surface_type_lookup_uid, con_phase_lookup_uid,
                              test_location, data_in_spreadsheet, map_in_site_folder, ring_diameter_in, prewet_time_s, prewet_rate_inhr)
          	                  VALUES ('", input$date, "','", input$smp_id, "',",  rv$type(), ",", rv$phase(), ",", rv$test_location(), ",", 
                                    rv$data(), ", ", rv$folder(), ", ", rv$ring_dia(), ", ", rv$prewet_time(), ", ", rv$pw_rate(), ")") 
            
            #add first test to results
            add_ppr_one_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement_results (porous_pavement_uid, weight_lbs, time_s, rate_inhr) 
                                      vALUES ((SELECT MAX(porous_pavement_uid) FROM fieldwork.tbl_porous_pavement), ", rv$weight(), ", ", 
                                    rv$time_one(), ", ", rv$rate_one(), ")")
            
            #add second test to results
            add_ppr_two_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement_results (porous_pavement_uid, weight_lbs, time_s, rate_inhr) 
                                      vALUES ((SELECT MAX(porous_pavement_uid) FROM fieldwork.tbl_porous_pavement), ", rv$weight(), ", ", 
                                        rv$time_two(), ", ", rv$rate_two(), ")")
            
            odbc::dbGetQuery(poolConn, add_ppt_query)
            odbc::dbGetQuery(poolConn, add_ppr_one_query)
            odbc::dbGetQuery(poolConn, add_ppr_two_query)
          }
        }else{
          
          #edit records
          edit_ppt_query <- paste0(
            "UPDATE fieldwork.tbl_porous_pavement SET smp_id = '", input$smp_id, "', test_date = '", input$date, 
            "', surface_type_lookup_uid = ",  rv$type(),
            ", con_phase_lookup_uid = '", rv$phase(),
            "', test_location = ", rv$test_location(),
            ", map_in_site_folder = ", rv$folder(), 
            ", data_in_spreadsheet = ", rv$data(),
            ", ring_diameter_in = ", rv$ring_dia(), 
            ", prewet_time_s = ", rv$prewet_time(), 
            ", prewet_rate_inhr = ", rv$pw_rate(),
            " WHERE porous_pavement_uid = '", rv$ppt_table_db()[input$ppt_table_rows_selected, 1], "'")
          
          dbGetQuery(poolConn, edit_ppt_query)
          
          #if test one is already there, edit it. if it's not, add it
          if(!is.na(rv$ppt_table_db()$test_one_porous_pavement_results_uid[input$ppt_table_rows_selected])){
            edit_ppr_one_query <- paste0(
              "UPDATE fieldwork.tbl_porous_pavement_results SET weight_lbs =", rv$weight(),
              ", time_s = ", rv$time_one(),
              ", rate_inhr = ", rv$rate_one(),
              " WHERE porous_pavement_results_uid = ", rv$ppt_table_db()$test_one_porous_pavement_results_uid[input$ppt_table_rows_selected]
            )
            dbGetQuery(poolConn, edit_ppr_one_query)
          
          }else{
            #add first test to results
            add_ppr_one_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement_results (porous_pavement_uid, weight_lbs, time_s, rate_inhr) 
                                      vALUES ('", rv$ppt_table_db()[input$ppt_table_rows_selected, 1], "', ", rv$weight(), ", ", 
                                        rv$time_one(), ", ", rv$rate_one(), ")")
            
            odbc::dbGetQuery(poolConn, add_ppr_one_query)
          }
          
          #if test two is already there, edit it. if not, add it
          if(!is.na(rv$ppt_table_db()$test_two_porous_pavement_results_uid[input$ppt_table_rows_selected])){

            edit_ppr_two_query <- paste0(
              "UPDATE fieldwork.tbl_porous_pavement_results SET weight_lbs =", rv$weight(),
              ", time_s = ", rv$time_two(),
              ", rate_inhr = ", rv$rate_two(),
              " WHERE porous_pavement_results_uid = ", rv$ppt_table_db()$test_two_porous_pavement_results_uid[input$ppt_table_rows_selected]
            )
            
            dbGetQuery(poolConn, edit_ppr_two_query)
          
          }else{
            #add second test to results
            add_ppr_two_query <- paste0("INSERT INTO fieldwork.tbl_porous_pavement_results (porous_pavement_uid, weight_lbs, time_s, rate_inhr) 
                                      vALUES ('", rv$ppt_table_db()[input$ppt_table_rows_selected, 1], "', ", rv$weight(), ", ", 
                                        rv$time_two(), ", ", rv$rate_two(), ")")
            
            odbc::dbGetQuery(poolConn, add_ppr_two_query)
          }
        }
        
        if(length(input$future_ppt_table_rows_selected) > 0){
          odbc::dbGetQuery(poolConn, paste0("DELETE FROM fieldwork.tbl_future_porous_pavement 
                                            WHERE future_porous_pavement_uid = '", 
                                            rv$future_ppt_table_db()[input$future_ppt_table_rows_selected, 1], "'"))
        }
        
        #re-run query
        rv$ppt_table_db <- reactive(dbGetQuery(poolConn, rv$query()))
        
        rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
        rv$future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, future_ppt_table_query()))
        rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
        
        #clear fields
        reset("date")
        reset("surface_type")
        reset("con_phase")
        reset("location")
        reset("data")
        reset("priority")
        reset("ring_dia")
        reset("prewet_time")
        reset("pw_rate")
        reset("weight")
        reset("time_one")
        reset("time_two")
        reset("rate_one")
        reset("rate_two")
      })
      
      #clear fields on click
      observeEvent(input$clear_ppt, {
        showModal(modalDialog(title = "Clear All Fields", 
                              "Are you sure you want to clear all fields on this tab?", 
                              modalButton("No"), 
                              actionButton(ns("confirm_clear_ppt"), "Yes")))
      })
      
      observeEvent(input$confirm_clear_ppt, {
        reset("smp_id")
        reset("date")
        reset("surface_type")
        reset("con_phase")
        reset("location")
        reset("data")
        reset("folder")
        reset("priority")
        reset("ring_dia")
        reset("prewet_time")
        reset("pw_rate")
        reset("weight")
        reset("time_one")
        reset("time_two")
        reset("rate_one")
        reset("rate_two")
        removeModal()
      })
      
      #2.2 View all Porous Pavement Tests (Completed Tests) ------
      
      #2.2.1 Query and View Tables ----
      #query full porous pavement view
      rv$all_query <- reactive(paste0("SELECT * FROM fieldwork.viw_porous_pavement_wide ORDER BY test_date DESC"))
      
      rv$all_ppt_table_db <- reactive(dbGetQuery(poolConn, rv$all_query())) 
      
      rv$all_ppt_table <- reactive(rv$all_ppt_table_db() %>% 
                                     mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                     mutate(across(where(is.POSIXlt), as.character)) %>% 
                                     mutate(average_rate_inhr = round(average_rate_inhr, 1)) %>% 
                                     mutate(across(c("data_in_spreadsheet", "map_in_site_folder"), 
                                               ~ case_when(. == 1 ~ "Yes", 
                                                              . == 0 ~ "No"))) %>% 
                                     dplyr::select("test_date", "smp_id", "project_name", "surface_type", "phase", "test_location", "average_rate_inhr", "data_in_spreadsheet", "map_in_site_folder"))
      
      output$all_ppt_table <- renderReactable(
            reactable(rv$all_ppt_table(), 
                  columns = list(
                    test_date  = colDef(name = "Test Date"),
                    smp_id  = colDef(name = "SMP ID"),
                    project_name = colDef(name = "Project Name"),
                    surface_type = colDef(name = "Surface Type"),
                    phase = colDef(name = "Construction Phase"),
                    average_rate_inhr = colDef(name = "Avg Rate (in/hr)"),
                    test_location  = colDef(name = "Test Location"),
                    data_in_spreadsheet = colDef(name = "Date In Spreadsheet", style = function(value){
                      if(is.na(value) | value == "No"){
                        color = "#FFFC1C"
                      }else{
                        color = "#FFFFFF"
                      }
                      list(backgroundColor = color, fontweight = "bold")
                    }),
                    map_in_site_folder = colDef(name = "Map in Site Folder", style = function(value){
                      if(is.na(value) | value == "No"){
                        color = "#FFFC1C"
                      }else{
                        color = "#FFFFFF"
                      }
                      list(backgroundColor = color, fontweight = "bold")
                    })
                  ),
                  fullWidth = TRUE,
                  selection = "single",
                  searchable = TRUE,
                  onClick = "select",
                  selectionId = ns("ppt_selected"),
                  #searchable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50),
                  defaultPageSize = 10,
                  height = 750
        )
      )
      
      #2.2.2 Click a row ----
      observeEvent(input$ppt_selected, {
        # do not use shinyjs::reset() - it is too slow and will go after updating to the smp_id, resulting in a cleared field
        updateSelectizeInput(session, "smp_id", choices = smp_id, 
                             selected = rv$all_ppt_table()$smp_id[input$ppt_selected], 
                             server = TRUE)
        updateTabsetPanel(session = parent_session, "inTabset", selected = "ppt_tab")
        delay(300, {
          ppt_row <- which(rv$ppt_table_db()$porous_pavement_uid == rv$all_ppt_table_db()$porous_pavement_uid[input$ppt_selected], 
                           arr.ind = TRUE)
          dataTableProxy('ppt_table') %>% selectRows(ppt_row)
        })
      })
      
      #2.3 View all Future Porous Pavement ----
      #2.3.1 query and view table ----
      rv$all_future_ppt_query <- "SELECT * FROM fieldwork.viw_future_porous_pavement_full order by field_test_priority_lookup_uid"
      rv$all_future_ppt_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$all_future_ppt_query))
      rv$all_future_ppt_table <- reactive(rv$all_future_ppt_table_db() %>% 
                                            dplyr::select("smp_id", "project_name", "surface_type", "phase", "test_location", "field_test_priority"))
      
      output$all_future_ppt_table <- renderDT(
        rv$all_future_ppt_table(), 
        selection = 'single', 
        style = 'bootstrap', 
        class = 'table-responsive, table-hover', 
        colnames = c('SMP ID', 'Project Name',  'Surface Type', 'Construction Phase', 'Test Location', 'Priority'), 
        rownames = FALSE
      )
      
      #2.3.2. Click a row ----
      #on click in future porous pavement table
      #update smp id and change tabs
      #then select test
      observeEvent(input$all_future_ppt_table_rows_selected, {
        updateSelectizeInput(session, "smp_id", choices = smp_id, selected = rv$all_future_ppt_table()$smp_id[input$all_future_ppt_table_rows_selected], server = TRUE)
        updateTabsetPanel(session = parent_session, "inTabset", selected = "ppt_tab")
        delay(300, {
          future_ppt_row <- which(rv$future_ppt_table_db()$future_porous_pavement_uid == rv$all_future_ppt_table_db()$future_porous_pavement_uid[input$all_future_ppt_table_rows_selected], 
                                  arr.ind = TRUE)
          dataTableProxy('future_ppt_table') %>% selectRows(future_ppt_row)
        })
      })
      
      #2.4 View porous pavement averages ----- 
      #2.4.1 Query and view table -----
      rv$average_query <- "SELECT * FROM fieldwork.viw_porous_pavement_smp_averages"
      rv$average_table_db <- reactive(odbc::dbGetQuery(poolConn, rv$average_query)%>% 
                                        mutate(across(where(is.POSIXct), trunc, "days")) %>% 
                                        mutate(across(where(is.POSIXlt), as.character)) %>% 
                                        mutate(avg_rate_inhr = round(avg_rate_inhr, 1)))
      
      rv$average_table <- reactive(if(length(input$smp_id_avg) == 0){
        rv$average_table_db()
      }else{rv$average_table_db() %>% dplyr::filter(smp_id %in% input$smp_id_avg)
      })
      
      output$averages <- renderDT(
        rv$average_table(), 
        selection = 'single', 
        style = 'bootstrap', 
        class = 'table-response, table-hover', 
        colnames = c('SMP ID', 'Project Name', 'Test Date', 'Average Rate (in/hr)')
      )
      
      #2.4.2 Download -----
      output$download_avg <- downloadHandler(
        filename = function(){
          paste("porous_pavement_smp_averages_", Sys.Date(), ".csv", sep = "")
        }, 
        content = function(file){
          write.csv(rv$average_table(), file, row.names = FALSE)
        }
      )
      
    }
  )
}