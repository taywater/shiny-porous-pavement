#4/7/2021 Nick Manna, ARKF, Inc. 
#This file, app.R, works with the sourced porous_pavement.R file to create the PWD GSI MARS Porous Pavement Tests app
#See Fieldwork App R script for more details 

# SET UP
#0.0: load libraries --------------
#shiny
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#DT for datatables
library(DT)
#reactable for reactable tables
library(reactable)

#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connections are unified
#gets environmental variables saved in local or pwdrstudio environment
poolConn <- dbPool(odbc(), dsn = "mars_data", uid = Sys.getenv("new_shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 source scripts.  ----
#each script contains a module, which includes UI and server code
source("porous_pavement.R")


#1: UI FUNCTION -----
#initialize variables for UI and call all UI functions
#call all the UI functions

ui <- function(req){

  #1.1: load required variables -----
  #define global variables that will be required each time the UI runs
  
  #porous pavement surface types
  surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
  
  #construction phase types
  con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
  
  #this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
  html_req <- function(label){
    HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
  }
  
  #this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML. it is slightly Christian
  future_req <- function(label){
    HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
  }
  
  #field test priority
  priority <- dbGetQuery(poolConn, "select * from fieldwork.field_test_priority_lookup")
  
  # 1.2: actual UI------------------------
  
  #use tagList so tags and shinyjs can be called without being inside of the navbarPage. When they're inside navbarpage, they create small invisible fake tabs that take up space and act weird when clicked on
  tagList(
    #call jscode to warn when leaving page
    tags$head(tags$script(jscode)),
    #must call useShinyjs() for shinyjs() functionality to work in app
    useShinyjs(),
    #navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
               #Porous Pavement (Add/Edit Porous Pavement Test, View Porous Pavement Tests, View Future Porous Pavement Tests)
               porous_pavementUI("porous_pavement", html_req = html_req,
                                 surface_type = surface_type, con_phase = con_phase, priority = priority, future_req = future_req)
    )
 # )
  
}

# 2: server function ----
#call modules, referencing the UI names above. These are functions, so any data originating outside the function needs to be named as an argument, whether it is lookup data, or from another tab. Modules need to be assigned to variables so they can be used in other module functions. 
server <- function(input, output, session) {
  
  # 2.1: required variables -----
  #define global variables that will be defined each time server runs
  
  #porous pavement surface types
  surface_type <- dbGetQuery(poolConn, "select * from fieldwork.surface_type_lookup")
  
  #con phase
  con_phase <- dbGetQuery(poolConn, "select * from fieldwork.con_phase_lookup")
  
  #porous pavement smp ids
  pp_smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from external.assets where asset_type = 'Permeable Pavement'")) %>% 
    dplyr::arrange(smp_id) %>% 
    dplyr::pull()
  
  # 2.2: Server Module functions ---------------------------
  #Porous Pavement
  porous_pavement <- porous_pavementServer("porous_pavement", parent_session = session, surface_type = surface_type,
                                           poolConn = poolConn, con_phase = con_phase, smp_id = pp_smp_id)
  
}

#Run this function to run the app!
shinyApp(ui, server)
  