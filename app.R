library(shiny)
library(shinydashboard)
library(ggplot2)
#library(tidyverse)
library(amt)
library(rhandsontable)


# Example data
# Track / relocation data
fisher_ny <- readr::read_csv("data/Martes pennanti LaPoint New York.csv")
# Subset relevant columns
fisher_ny <- fisher_ny %>% select(
  longitude = "location-long", latitude = "location-lat", "timestamp", 
  id = "individual-local-identifier"
)
# Rename columns containing special characters e.g. "-"
names(fisher_ny) <- make.names(names(fisher_ny), unique = TRUE)

# Environmental data
land_use_fisher_ny <- raster::raster("data/landuse_study_area.tif")
# Rename TIF for usage in model building
names(land_use_fisher_ny) <- "land_use"

# EPSG Codes
epsg_data <- rgdal::make_EPSG()



# UI ----------------------------------------------------------------------


# Header --------------------------------------------------------

ui <- dashboardPage(skin = "green",
  
  dashboardHeader(title = "amtGUI"),

# Sidebar -----------------------------------------------------------------

  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data", icon = icon("file-upload")),
      menuItem("Upload Map", tabName = "map", icon = icon("upload")),
      menuItem("Configure Analysis", tabName = "configure", icon = icon("database"),
      menuSubItem("Create Track", tabName = "track", icon = icon("map-marked-alt")),
      menuSubItem("Add Covariates", tabName = "covariates", icon = icon("plus-square"))),
      menuItem("Modeling", tabName = "model", icon = icon("table")),
      menuItem("Visualize", tabName = "plot", icon = icon("chart-area"))
    )),

# Body ----------------------------------------------------------

  dashboardBody(
    tabItems(

# Data Upload Tab ---------------------------------------------------------

      tabItem(tabName = "data",
        fluidPage(
          # Sidebar layout with input and output definitions
          sidebarLayout(
            sidebarPanel = sidebarPanel(
              width = 3,
              # Input: Select a file
              fileInput(
                inputId = "dataset_csv",
                label = "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", ".csv")
              ),
              actionButton('reset', 'Reset Input'),
              #hr(),
              # Input: Checkbox if file has header
              checkboxInput(
                inputId = "header",
                label = "File has Header",
                value = TRUE
              ),
              # Input: Select separator
              radioButtons(
                inputId = "sep",
                label = "Separator",
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                selected = ","
              ),
              # Input: Select quotes
              radioButtons(
                inputId = "quote",
                label = "Quote",
                choices = c(None = "", "Double Quote" = '"', 
                            "Single Quote" = "'"),
                selected = '"'
              ),
              # Horizontal line
              hr(),
              # Example Datasets
              selectInput(
                inputId = "ex_data_csv",
                label = "Choose Example Data:",
                choices = c("None", "Fisher NY")
              ),
              # Input: Select EPSG Code
              selectInput(
                inputId = "epsg_csv",
                label = "Assign EPSG Code:",
                choices = sort(na.omit(epsg_data$code)), #rgdal::make_EPSG()["code"]
                selected = 4326
              ),
              hr(),
              #Input: Select data table or summary of data set
              radioButtons(
                inputId = "display",
                label = "Display",
                choices = c("Data Frame", "Column Summary"),
                selected = "Data Frame"
              )
            ),
            mainPanel = mainPanel(
              DT::dataTableOutput(outputId = "contents"),
              verbatimTextOutput(outputId = "summary")
            )
          )
        )
        
      ),

# Upload Map Tab ----------------------------------------------------------

      tabItem(tabName = "map",
              fluidPage(
                  # Sidebar layout with input and output definitions
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      width = 3,
                      # Input: Select a file
                      fileInput(
                        inputId = "dataset_env",
                        label = "Choose TIF File",
                        multiple = TRUE
                        #accept = c("tif", ".tif")
                      ),
                      # Action button to reset input
                      actionButton('reset_env', 'Reset Input'),
                      # Horizontal line
                      hr(),
                      # Example Datasets
                      uiOutput(outputId = "ex_data_env"),
                      # selectInput(
                      #   inputId = "ex_data_env",
                      #   label = "Choose Example Data:",
                      #   choices = c("None", "Fisher NY Land Use Area")
                      # ),
                      # EPSG Code TIF
                      uiOutput(outputId = "epsg_env")
                    ),
                    mainPanel = mainPanel(
                      # Headline
                      h4(textOutput(outputId = "epsg_head")),
                      # Sub headline (instructions)
                      h5(textOutput(outputId = "epsg_sub_head")),
                      br(),
                      DT::dataTableOutput(outputId = "contents_env")
                    )
                  )
              )),

# Track Creation Tab ------------------------------------------------------
tabItem(tabName = "track",
  fluidRow(
    # Create a track
    column(width = 4, #offset = 1,
           h4(textOutput(outputId = "track_head")),
           #h4("Create a track"),
           uiOutput(outputId = "x"),
           uiOutput(outputId = "y"),
           uiOutput(outputId = "ts")#,
           # uiOutput(outputId = "id")
    ),
    # Transform EPSG Codes of CSV and TIF and select ID(s)
    column(width = 4, #offset = 1,
           br(),
           br(),
           uiOutput(outputId = "epsg_trk"),
           uiOutput(outputId = "id"),
           uiOutput(outputId = "id_trk")
    ),
    # Resample track
    column(width = 4, #offset = 1,
           h4("Resample Track"),
           #br(),
           numericInput(
             inputId = "rate_min",
             label = "Resampling Rate (in min):",
             value = NA, #15,
             min = 0,
             step = 1
           ),
           #br(),
           #br(),
           numericInput(
             inputId = "tol_min",
             label = "Tolerance (in min):",
             value = NA, #2,
             min = 0,
             step = 1
           ),
           # Date range for track data frame ----
           # dateRangeInput(inputId = "daterange",
           #                label = "Choose a Date Range",
           #                start = min(trk()$ts),
           #                end = max(trk()$ts),
           #                max = Sys.Date(),
           #                format = "yyyy-mm-dd",
           #                separator = "to",
           #                startview = "year"
           #                )
           uiOutput(
             outputId = 'fetch_dr'
           )
    )
  ),
  br(),
  fluidRow(
    # Change color of tab titles from blue to green
    tags$style(type = "text/css", "li a{color: #15AE57;}"),
    column(width = 12,
      # Tabs within fluid row
      tabsetPanel(
        tabPanel(
          title = "Track",
          # Input: Select data table or summary of track
          column(width = 1,
                 br(),
                 br(),
                 radioButtons(
                   inputId = "display_trk",
                   label = "Display",
                   choices = c("Data Frame", "Column Summary"),
                   selected = "Data Frame"
                 )
          ),
          # Data table or summary
          column(width = 11, #offset = 1,
                 DT::dataTableOutput(outputId = "contents_trk"),
                 verbatimTextOutput(outputId = "summary_trk")
          )
        ),
        tabPanel(
          title = "Summary of Sampling Rate",
          # Data table: summary of sampling rate
          column(width = 12, #offset = 1,
                 h4(textOutput(outputId = "samp_rate_head")),
                 DT::dataTableOutput(outputId = "summary_samp_rate")
          )
        )
      )
    )
  )
  # fluidRow(
  #   # Data table or summary
  #   column(width = 5, #offset = 1,
  #          DT::dataTableOutput(outputId = "contents_trk"),
  #          verbatimTextOutput(outputId = "summary_trk")
  #   ),
  #   # Input: Select data table or summary of track
  #   column(width = 1,
  #          br(),
  #          br(),
  #          radioButtons(
  #            inputId = "display_trk",
  #            label = "Display",
  #            choices = c("Data Frame", "Summary"),
  #            selected = "Data Frame"
  #          )
  #   ),
  #   # Data table: summary of sampling rate
  #   column(width = 4, #offset = 1,
  #          h4(textOutput(outputId = "samp_rate_head")),
  #          DT::dataTableOutput(outputId = "summary_samp_rate")
  #   )
  # )
),

# Add Additional Covariates Tab -------------------------------------------
tabItem(tabName = "covariates",
        fluidRow(  
          column(width = 5,
                 # Headline
                 h4(textOutput(outputId = "min_burst_head")),
                 br(),
                 # Only retain bursts with a minimum number of relocations
                 uiOutput(outputId = "min_burst"),
                 br(),
                 # Headline
                 h4(textOutput(outputId = "bursts_head")),
                 # No. of bursts and observations remaining given minimum no.
                 # of relocations per burst
                 DT::dataTableOutput(outputId = "contents_bursts")
          ),
          column(width = 6, offset = 1,
                 # Headline
                 h4(textOutput(outputId = "env_info_head")),
                 # Sub headline (instructions)
                 h5(textOutput(outputId = "env_info_sub_head")),
                 # Environmental covariates data frame
                 rHandsontableOutput(outputId = "env_df"),
                 br(),
                 # Headline
                 h4(textOutput(outputId = "tod_head")),
                 # Sub headline (instructions)
                 h5(textOutput(outputId = "tod_sub_head")),
                 # Time of day
                 uiOutput(outputId = "tod"),
                 # Data frame Time of Day levels 
                 DT::dataTableOutput(outputId = "contents_tod")
          )
        )
),

# Visualize Tab -----------------------------------------------------------

tabItem(tabName = "plot",
        h2("Under Maintenance")),

# Modeling Tab ------------------------------------------------------------

tabItem(tabName = "model",
  # Enable to clear all inputs of modeling tab by "clear model" button
  shinyjs::useShinyjs(),
  div(id = "modeling_tab",
  fluidRow(
    column(width = 4,
           # Headline
           h4(textOutput(outputId = "modeling_head")),
           # Choose a model
           selectInput(
             inputId = "model",
             label = "Choose a Model:",
             choices = c("Integrated Step Selection Function",
                         "Resource Selection Function",
                         ''),
             selected = ''
           )
           # radioButtons(
           #   inputId = "model",
           #   label = h4("Choose a Model"),
           #   choices = c("Resource Selection Function",
           #               "Integrated Step Selection Function",
           #               "None"),
           #   selected = "None"
           # ),
           # # Fit model button
           # actionButton("fit_button", "Fit Model", icon = icon("poll")), #, width = "112%"),
           # # Clear button
           # actionButton("clear_button", "Clear Model", icon = icon("poll")), #, width = "112%"),
           # br(),
           # # Download button for model output
           # downloadButton("downloadData", "Download")
    ),
    column(
      width = 3,
      br(),
      br(),
      # Set number of random steps per relocation (ISSF)
      uiOutput(outputId = "rand_stps"),
      # Set number of random points (RSF)
      uiOutput(outputId = "rand_points")
    )
  ),
  fluidRow(
    column(width = 4,
           br(),
           # br(),
           # Select land use area
           #uiOutput(outputId = "lu"),
           # Assign land use covariate name
           #uiOutput(outputId = "lu_name"),
           # Select model variables
           uiOutput(outputId = "mod_var")#,
           # Select no. of interaction terms to add
           #uiOutput(outputId = "inter_no")
           # Select logarithmized model variables
           #uiOutput(outputId = "log_var")
    ),
    column(
      width = 3,
      br(),
      # Select no. of interaction terms to add
      uiOutput(outputId = "inter_no")
    )
  ),
  fluidRow(
    # column(width = 2,
    #        # Select 1st to 5th interaction
    #        uiOutput(outputId = "inter_1"),
    #        uiOutput(outputId = "inter_2"),
    #        uiOutput(outputId = "inter_3"),
    #        uiOutput(outputId = "inter_4"),
    #        uiOutput(outputId = "inter_5")
    #        )
    column(width = 2,
           # Select 1st interaction
           uiOutput(outputId = "inter_1")
    ),
    column(width = 2,
           # Select 2nd interaction
           uiOutput(outputId = "inter_2")
    ),
    column(width = 2,
           # Select 3rd interaction
           uiOutput(outputId = "inter_3")
    ),
    column(width = 2,
           # Select 4th interaction
           uiOutput(outputId = "inter_4")
    ),
    column(width = 2,
           # Select 5th interaction
           uiOutput(outputId = "inter_5")
    )
  )
  ), # End of clear inputs
  fluidRow(
    column(
      width = 2,
      # Fit model button
      actionButton("fit_button", "Fit Model", icon = icon("poll")), #, width = "112%")
      # Clear button
      actionButton("clear_button", "Clear Model", icon = icon("poll")), #, width = "112%")
      # Download button for model output
      downloadButton("downloadData", "Download")
    )
  ),
  fluidRow(
    column(width = 5,
        br(),
        DT::dataTableOutput(outputId = "contents_mod")
        #plotOutput(outputId = "mod_plot")
    )
  )
)

# End UI!!!
)))


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  

# Data Upload -------------------------------------------------------------


# input$dataset will be NULL initially. After the user selects
# and uploads a file, head of that data file by default,
# or summary if selected, will be shown.

# Increase maximum upload size from 5 MB to 30 MB
#old <- options(shiny.maxRequestSize = 5*1024^2)
options(shiny.maxRequestSize = 30*1024^2)

# Upload data and reset-button to switch between upload and R data sets
values_csv <- reactiveValues(upload_state = NULL)

observeEvent(input$dataset_csv, {
  values_csv$upload_state <- 'uploaded'
})
observeEvent(input$reset, {
  values_csv$upload_state <- 'reset'
})

csvInput <- reactive({
  if (is.null(values_csv$upload_state)){
    switch (input$ex_data_csv,
            "Fisher NY" = fisher_ny,
            "None" = return()
    )
  } else if (values_csv$upload_state == 'uploaded') {
    # Comma separated
    if (input$sep == ",") {
      csv_uploaded <- readr::read_csv(file = input$dataset_csv$datapath, 
                                      col_names = input$header,
                                      quote = input$quote)
      # Rename columns containing special characters e.g. "-"
      names(csv_uploaded) <- make.names(names(csv_uploaded), unique = TRUE)
    }
    # Semicolon separated
    if (input$sep == ";") {
      csv_uploaded <- readr::read_csv2(file = input$dataset_csv$datapath,
                                       col_names = input$header,
                                       quote = input$quote)
      # Rename columns containing special characters e.g. "-"
      names(csv_uploaded) <- make.names(names(csv_uploaded), unique = TRUE)
    }
    # Tab separated
    if (input$sep == "\t") {
      csv_uploaded <- readr::read_tsv(file = input$dataset_csv$datapath, 
                                      col_names = input$header,
                                      quote = input$quote)
      # Rename columns containing special characters e.g. "-"
      names(csv_uploaded) <- make.names(names(csv_uploaded), unique = TRUE)
    }
    return(csv_uploaded)
  
    } else if (values_csv$upload_state == 'reset') {
    switch (input$ex_data_csv,
            "Fisher NY" = fisher_ny,
            "None" = return()
    )
  }
})
# Display data frame or summary of data
output$contents <- DT::renderDataTable({
  # validate(
  #   need(csvInput(), '') # displays summary below table area when selected
  # )
  if (!is.null(csvInput())) {
    if (input$display == "Data Frame") {
      DT::datatable(csvInput(), 
                    rownames = FALSE,
                    options = list(
                      lengthMenu = list(c(5, 10, 20, 50, 100), 
                                        c('5', '10', '20', '50', '100')),
                      pageLength = 10
                    )
      )
    }
  }
})
# Summary
output$summary <- renderPrint({
  if (input$display == "Column Summary") {
    summary(object = csvInput())
  }
})


# Map Upload --------------------------------------------------------------


values_env <- reactiveValues(upload_state = NULL)

observeEvent(input$dataset_env, {
  values_env$upload_state <- 'uploaded'
})
observeEvent(input$reset_env, {
  values_env$upload_state <- 'reset'
})
# Environmental data input e.g. TIF-File
envInput <- reactive({
  validate(
    need(input$ex_data_env, ''), # Otherwise error shown during loading phase. 
    need(csvInput(), 'Please upload track data first.')
  )
  if (is.null(values_env$upload_state)){
    switch (input$ex_data_env,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  } else if (values_env$upload_state == 'uploaded') {
    # raster_up <- raster::raster(x = input$dataset_env$datapath)
    # # Rename uploaded TIF for usage in model building
    # names(raster_up) <- "land_use"
    # raster_up
    
    # Get names of uploaded TIFs (named X0, X1, ... otherwise)
    names_list <- lapply(input$dataset_env$name, function(x) x)
    names_list <- gsub(".tif", '', names_list)
    
    # Store uploaded TIF file(s) as raster layer(s) in list
    raster_list <- lapply(input$dataset_env$datapath, raster::raster)

    # Test whether multiple files are uploaded
    # Single file: access raster layer in list
    if (length(raster_list) == 1) {
      # Rename
      names(raster_list[[1]]) <- names_list
      raster_list[[1]]
    } else {
      # Multiple files: store raster layers in list as raster stack
      r_stack <- raster::stack(raster_list)
      # Rename
      names(r_stack) <- names_list
      r_stack
    }
    
  } else if (values_env$upload_state == 'reset') {
    switch (input$ex_data_env,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  }
})
# Rename environmental data input if required
env <- reactive({
  if (is.null(env_info())) {
    envInput()
  } else {
    env_renamed <- envInput()
    names(env_renamed) <- env_info()$Covariate
    env_renamed
  }
})
# Input: Upload example data
output$ex_data_env <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectInput(
    inputId = "ex_data_env",
    label = "Choose Example Data:",
    choices = c("None", "Fisher NY Land Use Area")
  )
})
# Detect EPSG Code of TIF-File by left joining data frame "epsg_data"
epsg_env_detected <- reactive({
  validate(
    need(env(), '')
  )
  env_prj4 <- data.frame("prj4" = raster::projection(env()),
                              stringsAsFactors = FALSE)
  detect_epsg <- dplyr::left_join(env_prj4, epsg_data, by = "prj4")
  # Multiple matches possible
  detect_epsg %>% select("EPSG Code(s)" = code, "Description" = note)
  })
# Input: EPSG Code TIF
output$epsg_env <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectInput(
    inputId = "epsg_env",
    label = "Assign EPSG Code:", 
    choices = sort(na.omit(epsg_data$code)),
    selected = ifelse(!is.null(env()) && !is.null(epsg_env_detected()), 
                      # Multiple matches possible select 1st one by default
                      yes = epsg_env_detected()[1, "EPSG Code(s)"],
                      no = input$epsg_csv
                      )
)
})
# ?????????
# Assign CRS if not described yet?
# projection(x) <- CRS("+init=epsg:28992")
# ?????????


# Show headline for EPSG Code table
output$epsg_head <- renderText({
  validate(
    need(epsg_env_detected(), '')
  )
  "Found EPSG Code(s) for Uploaded File(s)"
})
# Sub headline (instructions)
output$epsg_sub_head <- renderText({
  validate(
    need(epsg_env_detected(), '')
  )
  "Please verify and assign an appropriate EPSG code this may vary from the 
  option(s) below."
})


# Data frame of detected EPSG codes
output$contents_env <- DT::renderDataTable({
  validate(
    need(env(), 'Please upload an environmental data file.'),
    need(epsg_env_detected(), 'No EPSG code detected for uploaded file.')
  )
  DT::datatable(epsg_env_detected(),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE,
                               # Left align columns
                               columnDefs = list(
                                 list(className = 'dt-left', targets = 0:1)
                               )
                )
  )
})


# Track Creation ----------------------------------------------------------


# Update variable selection based on uploaded data set
# Show headline for track menu in sidebar
output$track_head <- renderText({
  if (!is.null(csvInput())) {
    "Select Track Variables"
  } else {
    "Please upload tracking data first."
  }
})
# Choose x (location-long)
output$x <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectizeInput(
    inputId = 'x', 
    label = "Longitude:", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign longitude', # 'Please select an option below'
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
})
# Choose y (location-lat)
output$y <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectizeInput(
    inputId = 'y', 
    label = "Latitude:", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign latitude',
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
})
# ts (timestamp)
output$ts <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectizeInput(
    inputId = 'ts', 
    label = "Timestamp:", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign timestamp',
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
})
# id (individual-local-identifier)
output$id <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectizeInput(
    inputId = 'id', 
    label = "ID:", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Group by ID (optional)',
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
})
# EPSG Code Transformation of tracking data (CSV)
output$epsg_trk <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectInput(
    inputId = "epsg_trk",
    label = "Transform CRS by EPSG Code:",
    choices = na.omit(epsg_data$code),
    selected = input$epsg_env
  )
})
# Filter IDs of the track
output$id_trk <- renderUI({
  validate(
    need(input$id, '')
  )
  selectizeInput(
    inputId = "id_trk", 
    label = "Select ID(s):",
    choices = sort(unique(dat()$id)), 
    multiple = TRUE,
    selected = sort(unique(dat()$id))[1:length(unique(dat()$id))]
  )
})

# Create a track: select relevant columns and omit NAs
dat <- reactive({
  validate(
    need(csvInput(), '')
  )
  csvInput() %>% 
  select(x = input$x, y = input$y, ts = input$ts, id = input$id) %>%  
  na.omit()
})

# Create a track: select relevant columns and omit NAs (excluding ID)
dat_excl_id <- reactive({
  validate(
    need(csvInput(), '')
  )
  csvInput() %>% 
    select(x = input$x, y = input$y, ts = input$ts) %>%  
    na.omit()
})
#trk()####
# Create a track
trk <- reactive({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, ''),
    need(input$daterange[1], 'Please select start of date range.'),
    need(input$daterange[2], 'Please select end of date range.')
  )
  # No ID selected (one model for all animals)
  if (input$id == '') {
    # Subset data according to selected dateRangeInput
    dat_excl_id_df <- dat_excl_id() %>% 
      filter(ts >= input$daterange[1] & ts <= input$daterange[2])
    # Assign known EPSG Code to tracking data
    track <- make_track(dat_excl_id_df, #dat_excl_id(), 
                        x, y, ts,
                        crs = sp::CRS(paste0("+init=epsg:", input$epsg_csv))
    )
    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      track
    } else {
      transform_coords(track, sp::CRS(paste0("+init=epsg:", input$epsg_trk)))
    }
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    # Multiple IDs selected (individual models)
    # Subset data according to selected dateRangeInput
    dat_df <- dat() %>% 
      filter(ts >= input$daterange[1] & ts <= input$daterange[2])
    # Assign known EPSG Code to tracking data (no transformation necessary)
    if (input$epsg_csv == input$epsg_trk) {
      track_multi <- dat_df %>% nest(-id) %>% #dat() %>% nest(-id) %>%
        mutate(track = lapply(data, function(d) {
          amt::make_track(d, x, y, ts, crs = sp::CRS(paste0("+init=epsg:",
                                                            input$epsg_csv))
          )
        }))
      # Subset filtered ID(s)
      track_multi[track_multi$id %in% input$id_trk, ]
    } else {
      # Transform CRS of track
      trk_multi_tr <- dat_df %>% nest(-id) %>% #dat() %>% nest(-id) %>%
        mutate(track = lapply(data, function(d) {
          amt::make_track(d, x, y, ts, crs = sp::CRS(paste0(
            "+init=epsg:", input$epsg_csv))) %>%
            amt::transform_coords(sp::CRS(paste0("+init=epsg:", input$epsg_trk))
            )
        }))
      # Subset filtered ID(s)
      trk_multi_tr[trk_multi_tr$id %in% input$id_trk, ]
    }
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) == 1) {
    # One ID selected
    # Subset data according to selected dateRangeInput
    dat_df <- dat() %>% 
      filter(ts >= input$daterange[1] & ts <= input$daterange[2])
    # Assign known EPSG Code to tracking data
    track_one <- make_track(dat_df, #dat(), 
                            x, y, ts, id = id,
                        crs = sp::CRS(paste0("+init=epsg:", input$epsg_csv))
    )
    # Subset filtered ID(s)
    track_one <- track_one[track_one$id %in% input$id_trk, ]

    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      track_one
    } else {
      transform_coords(track_one, sp::CRS(paste0("+init=epsg:", input$epsg_trk))
      )
    }
  }
})

# Summarize sampling rate
samp_rate <- reactive({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, ''),
    need(trk(), '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    trk() %>% mutate(sr = lapply(track, summarize_sampling_rate)) %>%
      select(id, sr) %>% unnest
  } else {
    # One/ no ID selected
    summarize_sampling_rate(trk())
  }
})

# Show head line for sampling rate (Output)
output$samp_rate_head <- renderText({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')
  )
  "Track's Summary of Sampling Rate (in min)"
})

# Summarize sampling rate (Output)
output$summary_samp_rate <- DT::renderDataTable({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    DT::datatable(
      # Exclude column "unit" (min), rename ID column and round numeric columns
      samp_rate() %>% select(-unit, ID = id) %>% dplyr::mutate_if(is.numeric, round, 2),
      rownames = FALSE,
      options = list(searching = FALSE, paging = FALSE
      )
    )
  } else {
    # One/ no ID selected
    DT::datatable(
      # Exclude column "unit" (min) and round numeric columns
      samp_rate() %>% select(-unit) %>% dplyr::mutate_if(is.numeric, round, 2),
      rownames = FALSE,
      options = list(searching = FALSE, paging = FALSE
      )
    )
  }
})

# Resample track (this will be used for model building not trk_df)!!!!!!!!!!!!!!
trk_resamp <- reactive({
  validate(
    need(input$rate_min, ''),
    need(input$tol_min, '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    trk() %>%
      mutate(track = lapply(track, function(x) {
        x %>% amt::track_resample(rate = minutes(input$rate_min),
                                  tolerance = minutes(input$tol_min))
      }))
    # group_by(trk(), id) %>% nest() %>%
    #   mutate(data = map(data, ~ .x %>%
    #                       track_resample(rate = minutes(input$rate_min),
    #                                      tolerance = minutes(input$tol_min))))

  } else {
    # One/ no ID selected
    trk() %>% track_resample(rate = minutes(input$rate_min),
                             tolerance = minutes(input$tol_min))  #%>%
      #filter(t_ >= input$daterange[1] & t_ <= input$daterange[2])
  }
})
#trk_df()####
# Track table displayed in app (dependent on resampling)
trk_df <- reactive({
  validate(
    need(trk(), '')
  )
  # Before resampling
  if (is.na(input$rate_min) && is.na(input$tol_min)) {
    # Multiple IDs selected
    if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
      trk_unnested_df <- trk() %>% select(id, track) %>% unnest 
      # Rename columns
      trk_unnested_df %>% select(ID = id, Longitude = x_, Latitude = y_, 
                                 Timestamp = t_)
    } else if (ifelse(input$id == '', yes = 0, 
                      no = length(input$id_trk)) == 1) {
      # One ID selected
      # Rename and swap columns for uniformity
      trk() %>% select(ID = id, Longitude = x_, Latitude = y_, Timestamp = t_)
    } else {
      # No ID selected (Subsetting according to selected dateRangeInput)
      # Rename columns
      trk() %>% select(Longitude = x_, Latitude = y_, Timestamp = t_)
    }
  } else {
    # Resampled track
    # Multiple IDs selected
    if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
      # Convert back to data frame for illustration
      trk_resamp_unnested_df <- trk_resamp() %>% select(id, track) %>% unnest
      # Rename Columns
      trk_resamp_unnested_df %>% select(ID = id, Longitude = x_, Latitude = y_, 
                                        Timestamp = t_, Burst = burst_)
    } else if (ifelse(input$id == '', yes = 0, 
                      no = length(input$id_trk)) == 1) {
      # One ID selected
      # Rename and swap columns for uniformity
      trk_resamp() %>% select(ID = id, Longitude = x_, Latitude = y_, 
                              Timestamp = t_, Burst = burst_)
    } else {
      # No ID selected
      # Rename Columns
      trk_resamp()  %>% select(Longitude = x_, Latitude = y_, Timestamp = t_, 
                               Burst = burst_)
    }
  }
}) 

# Display data frame of track
output$contents_trk <- DT::renderDataTable({
  validate(
    need(input$x, 'Please assign longitude.'),
    need(input$y, 'Please assign latitude.'),
    need(input$ts, 'Please assign timestamp.')
  )
  if (input$display_trk == "Data Frame") {
    DT::datatable(
      # Round numeric columns
      trk_df() %>% dplyr::mutate_if(is.numeric, round, 2),
      rownames = FALSE,
      options = list(searching = FALSE,
                     lengthMenu = list(
                       c(5, 10, 20, 50, 100),
                       c('5', '10', '20', '50', '100')),
                     pageLength = 10,
                     # Left align columns
                     columnDefs = list(
                       list(className = 'dt-left', 
                            targets = 0:(ncol(trk_df())-1)
                       )
                     )
      )
    )
  }
})

# Display summary of track
output$summary_trk <- renderPrint({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')
  )
  if (input$display_trk == "Column Summary") {
    summary(object = trk_df())
  }
})

# Dynamic dateRangeInput for track data frame ----

output$fetch_dr <- renderUI({
  validate(
    need(input$ts, '')
  )
  
  if (input$id == '') {
    min.date <- min(dat_excl_id()$ts)
    max.date <- max(dat_excl_id()$ts)
  } else {
    min.date <- min(dat()$ts)
    max.date <- max(dat()$ts)
  }
  
  dateRangeInput(inputId = "daterange",
                 label = "Choose a Date Range:",
                 start = min.date,
                 end = max.date,
                 max = Sys.Date(),
                 format = "yyyy-mm-dd",
                 separator = "to",
                 startview = "year"
  )
})


# Add Additional Covariates -----------------------------------------------

# Show headline for "Minimum No. of Relocations per Burst" drop down
output$min_burst_head <- renderText({
  validate(
    need(input$rate_min && input$tol_min,
         'Please resample the track on previous tab first.')
  )
  "Restrict Bursts of Resampled Track"
})
# Only retain bursts with a minimum number of relocations
output$min_burst <- renderUI({
  validate(
    need(input$rate_min && input$tol_min, '')
  )
  numericInput(
    inputId = "min_burst",
    label = "Minimum No. of Relocations per Burst:",
    value = 3, #NA,
    min = 1,
    step = 1
  )
})
# Show headline for bursts data frame
output$bursts_head <- renderText({
  validate(
    need(input$min_burst, '')
  )
  "No. of Bursts Remaining"
})
# Table: No. of bursts remaining (given minimum no. of relocations per burst)
bursts_df <- reactive({
  validate(
    need(input$min_burst, '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    bursts <- vector()
    observations <- vector()
    
    for (i in 1:nrow(trk_resamp())) {
      min_burst <- trk_resamp()$track[[i]]
      freq <- table(min_burst$burst_) 
      pos <- which(freq >= input$min_burst)
      # No. of bursts remaining per ID 
      bursts[i] <- length(freq[pos])
      # No. of observations over all bursts remaining per ID
      observations[i] <- sum(freq[pos])
    }
    # Create data frame
    b <- data.frame("ID" = trk_resamp()$id, "Bursts" = bursts, 
                    "n" = observations)
    # Check whether ID(s) with no bursts remain 
    remove_ids <- trk_resamp()[which(b$Bursts == 0), "id"]
    # Found ID(s) that need to be removed before model building
    if (nrow(remove_ids) > 0) {
      # Convert from tbl to vector to paste in notification below
      remove_ids <- dplyr::pull(remove_ids, id)
      # Notification: ID(s) not meeting chosen minimum no. of relocations
      # per burst
      showNotification(
        ui = paste0(
          "The minimum no. of relocations per burst is too high for the
          ID(s): ", paste0(remove_ids, collapse = ", "), ". Therefore, those
          will be removed. If you want to retain those ID(s) please choose a
          lower value. Alternatively you may choose a different resampling
          rate, i.e., adjust the interval (in min) to retain the current no.
          of minimum relocations per burst."),
        type = "warning",
        duration = NULL
        )
      # Return data frame (ordered by no. of bursts)
      b[order(b$Bursts), ]
    } else {
      # Did not found ID(s) that need to be removed before model building
      # Return data frame (ordered by no. of bursts)
      b[order(b$Bursts), ]
    }
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) == 1) {
    # One ID selected
    freq <- table(trk_resamp()$burst_) 
    pos <- which(freq >= input$min_burst)
    # No. of bursts remaining for ID 
    bursts <- length(freq[pos])
    # No. of observations over all bursts remaining for ID
    observations <- sum(freq[pos])
    # Create data frame
    b <- data.frame("ID" = input$id_trk, "Bursts" = bursts, "n" = observations)
    # Check whether bursts remain for ID  
    remove_id <- trk_resamp()[which(b$Bursts == 0), "id"]
    # Model building not possible since no bursts are left for ID
    if (nrow(remove_id) == 1) {
      # Notification: ID not meeting chosen minimum no. of relocations
      # per burst
      showNotification(
        ui = paste0(
          "The minimum no. of relocations per burst is too high for the
          ID: ", paste0(remove_id), ". Please choose a lower one. 
          Alternatively you may choose a different resampling rate, i.e., 
          adjust the interval (in min) to retain the current no. of minimum 
          relocations per burst."),
        type = "warning",
        duration = NULL
        )
      # Return data frame
      b
    } else {
      # Bursts remain for the ID 
      # Return data frame
      b
    }
  } else {
    # No ID selected
    freq <- table(trk_resamp()$burst_) 
    pos <- which(freq >= input$min_burst)
    # No. of bursts remaining for ID 
    bursts <- length(freq[pos])
    # No. of observations over all bursts remaining for ID
    observations <- sum(freq[pos])
    # Create data frame
    b <- data.frame("Bursts" = bursts, "n" = observations)
    # Check whether bursts remain  
    remove <- trk_resamp()[which(b$Bursts == 0), ]
    # Model building not possible since no bursts are left
    if (nrow(remove) == 1) {
      # Notification: not meeting chosen minimum no. of relocations
      # per burst
      showNotification(
        ui = paste0(
          "The minimum no. of relocations per burst is too high. Please choose 
          a lower one. Alternatively you may choose a different resampling rate,
          i.e., adjust the interval (in min) to retain the current no. of 
          minimum relocations per burst."),
        type = "warning",
        duration = NULL
      )
      # Return data frame
      b
    } else {
      # Bursts remain 
      # Return data frame
      b
    }
  }
})
# Display data frame of bursts
output$contents_bursts <- DT::renderDataTable({
  validate(
    need(bursts_df(), '')
  )
  DT::datatable(bursts_df(),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE,
                               lengthMenu = list(
                                 c(5, 10, 20, 50, 100),
                                 c('5', '10', '20', '50', '100')),
                               pageLength = 10
                )
  )
})
# Show headline for time of day drop down
output$tod_head <- renderText({
  validate(
    need(input$rate_min && input$tol_min, '')
  )
  "Time of Day Covariate"
})
# Sub headline (instruction when to use)
output$tod_sub_head <- renderText({
  validate(
    need(input$rate_min && input$tol_min, '')
  )
  "Applicable when building a model with integrated step selection function only."
})
# Time of Day
output$tod <- renderUI({
  selectInput(
    inputId = "tod",
    label = "Time of Day:",
    choices = c('',
                "excl. dawn and dusk" = FALSE, 
                "incl. dawn and dusk" = TRUE),
    selected = '' #"excl. dawn and dusk"
  )
})
# Time of Day info data frame
# No. of levels remaining after min. no. of relocations per burst is set
# Interaction terms can be applied only to factors with 2 or more levels
tod_df <- reactive({
  validate(
    need(input$min_burst, ''),
    need(bursts_df(), ''),
    need(input$tod != '', '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    # Keep all IDs meeting chosen minimum no. of relocations per burst
    keep_ids <- bursts_df()[which(bursts_df()$Bursts != 0), "ID"]
    t_res <- trk_resamp()[trk_resamp()$id %in% keep_ids, ]
    
    t_res <- t_res %>% 
      mutate(steps = lapply(track, function(x) {
        x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
          amt::steps_by_burst() %>%
          time_of_day(include.crepuscule = input$tod)
      }))
    # Get unique time of day levels per ID
    unique_tod <- tibble(id = t_res$id, tod = rep(NA, length(t_res$id)), 
                         levels = rep(NA, length(t_res$id)))
    for (i in 1:nrow(t_res)) {
      unique_tod$tod[i] <- paste0(sort(unique(t_res$steps[[i]][["tod_end_"]])), 
                                  collapse = ", ")
      unique_tod$levels[i] <- length(unique(t_res$steps[[i]][["tod_end_"]]))
    }
    # Return data frame sorted by no. of levels
    unique_tod %>% dplyr::arrange(levels) %>% 
      select(ID = id, "Time of Day" = tod, "Levels" = levels)
    
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) == 1) {
    # One ID selected
    t_res <- trk_resamp() %>% 
      amt::filter_min_n_burst(min_n = input$min_burst) %>% 
      amt::steps_by_burst() %>%
      # Add time of day
      time_of_day(include.crepuscule = input$tod)
    # Get unique time of day levels per ID
    unique_tod <- tibble(id = input$id_trk, tod = NA, levels = NA)
    unique_tod$tod <- paste0(sort(unique(t_res$tod_end_)), collapse = ", ")
    unique_tod$levels <- length(unique(t_res$tod_end_))
    # Return data frame
    unique_tod %>% select(ID = id, "Time of Day" = tod, "Levels" = levels)
  } else {
    # No ID selected
    t_res <- trk_resamp() %>% 
      amt::filter_min_n_burst(min_n = input$min_burst) %>% 
      amt::steps_by_burst() %>%
      # Add time of day
      time_of_day(include.crepuscule = input$tod)
    # Get unique time of day levels per ID
    unique_tod <- tibble(tod = NA, levels = NA)
    unique_tod$tod <- paste0(sort(unique(t_res$tod_end_)), collapse = ", ")
    unique_tod$levels <- length(unique(t_res$tod_end_))
    # Return data frame
    unique_tod %>% select("Time of Day" = tod, "Levels" = levels)
  }
})
# Display data frame of Time of Day levels
output$contents_tod <- DT::renderDataTable({
  validate(
    need(tod_df(), '')
  )
  DT::datatable(tod_df(),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE,
                               lengthMenu = list(
                                 c(5, 10, 20, 50, 100),
                                 c('5', '10', '20', '50', '100')),
                               pageLength = 10,
                               # Left align columns
                               columnDefs = list(
                                 list(className = 'dt-left', 
                                      targets = 0:(ncol(tod_df()) - 1)
                                 )
                               )
                )
  )
})
# Show headline for environmental covariates data frame
output$env_info_head <- renderText({
  validate(
    need(envInput(), 'Please upload map with environmental covariates first.')
  )
  "Environmental Covariates"
})
# Show sub headline (instructions) for environmental covariates data frame
output$env_info_sub_head <- renderText({
  validate(
    need(envInput(), '')
  )
  "Change names of covariates and convert to categorical or continuous variable."
})
# Create initial data frame with environmental covariates
env_info <- reactive({
  validate(
    need(envInput(), '')
  )
  #For initial data upload
  if (is.null(input$env_df)) {
    data.frame("Covariate" = names(envInput()), 
               "Categorical" = rep(TRUE, length(names(envInput()))),
               stringsAsFactors = FALSE)
  } else {
    # Convert handsontable data to R object
    hot_to_r(input$env_df)
  }
})
# Environmental covariates of raster layer or raster stack
output$env_df = renderRHandsontable({
  validate(
    need(env(), '')
  )
  rhandsontable(env_info(), rowHeaders = NULL) %>%
    # Center checkbox column "Categorical"
    hot_col(col = "Categorical", halign = "htCenter")
})


# Modeling ----------------------------------------------------------------

# Show headline for EPSG Code table
output$modeling_head <- renderText({
  validate(
    need(input$min_burst, 'Please create a track first.')
  )
  "Conditional Logit Model"
})
# Set number of random steps per relocation
output$rand_stps <- renderUI({
  validate(
    need(input$model == "Integrated Step Selection Function", '')
  )
  numericInput(
    inputId = "rand_stps",
    label = "Random Steps:",
    value = 3, #10, #NA,
    min = 1,
    step = 1
)
})
# Set random points (RSF)
output$rand_points <- renderUI({
  validate(
    need(input$model == "Resource Selection Function", '')
  )
  numericInput(
    inputId = "rand_points",
    label = "Random Points:",
    value = 100,
    min = 1,
    step = 1
  )
})
# Variable choices for variable and interaction term drop downs
var_choices <- reactive({
  # Positions of variables not to include in choices
  pos_excl <- which(sort(names(mod_pre_var())) %in% c(
    "case_", "dt_", "x_", "y_", "x1_", "x2_", "y1_", "y2_", "t1_", "t2_", 
    "burst_", "step_id_")
  )
  sort(names(mod_pre_var()))[-pos_excl]
})
# Filter model variables
output$mod_var <- renderUI({
  validate(
    need(mod_pre_var(), '')
  )
  selectizeInput(
    inputId = "mod_var", 
    label = "Select Variables:",
    choices = var_choices(),
    multiple = TRUE#,
    #selected = sort(names(mod_pre()))[-pos_excl]
  )
})
# Slider for no. of interaction terms to add
output$inter_no <- renderUI({
  validate(
    need(mod_pre(), '')
  )
  sliderInput(
    inputId = "inter_no",
    label = "Add Interaction Terms:",
    min = 0,
    max = 5,
    value = 2,
    step = 1
  )
})
# Filter interaction term (1)
output$inter_1 <- renderUI({
  validate(
    need(mod_pre(), ''),
    need(input$inter_no >= 1, '')
  )
  selectizeInput(
    inputId = "inter_1", 
    label = "1st Interaction:",
    choices = var_choices(),
    multiple = TRUE,
    options = list(maxItems = 2)
  )
})
# Filter interaction term (2)
output$inter_2 <- renderUI({
  validate(
    need(mod_pre(), ''),
    need(input$inter_no >= 2, '')
  )
  selectizeInput(
    inputId = "inter_2", 
    label = "2nd Interaction:",
    choices = var_choices(),
    multiple = TRUE,
    options = list(maxItems = 2)
  )
})
# Filter interaction term (3)
output$inter_3 <- renderUI({
  validate(
    need(mod_pre(), ''),
    need(input$inter_no >= 3, '')
  )
  selectizeInput(
    inputId = "inter_3", 
    label = "3rd Interaction:",
    choices = var_choices(),
    multiple = TRUE,
    options = list(maxItems = 2)
  )
})
# Filter interaction term (4)
output$inter_4 <- renderUI({
  validate(
    need(mod_pre(), ''),
    need(input$inter_no >= 4, '')
  )
  selectizeInput(
    inputId = "inter_4", 
    label = "4th Interaction:",
    choices = var_choices(),
    multiple = TRUE,
    options = list(maxItems = 2)
  )
})
# Filter interaction term (5)
output$inter_5 <- renderUI({
  validate(
    need(mod_pre(), ''),
    need(input$inter_no == 5, '')
  )
  selectizeInput(
    inputId = "5", 
    label = "5th Interaction:",
    choices = var_choices(),
    multiple = TRUE,
    options = list(maxItems = 2)
  )
})
# Select land use covariate
# output$lu <- renderUI({
#   # validate(
#   #   need(mod_pre(), '')
#   # )
#   selectInput(
#     inputId = "lu",
#     label = "Choose Land Use Area:",
#     choices = c('', sort(unique(raster::values(env())))),
#     selected = NULL
#   )
# })
# # Assign land use covariate name
# output$lu_name <- renderUI({
#   # validate(
#   #   need(mod_pre(), '')
#   # )
#   textInput(
#     inputId = "lu_name",
#     label = "Assign Name to Land Use Covariate:",
#     placeholder = "Please type in a name.",
#     value = NULL
#   )
# })

# Fit model (data preparation)
mod_pre <- reactive({
  validate(
    need(trk_resamp(), ''),
    need(input$model != '', '')
  )
  # Multiple IDs selected (individual models)
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    # Keep all IDs meeting chosen minimum no. of relocations per burst
    keep_ids <- bursts_df()[which(bursts_df()$Bursts != 0), "ID"]
    t_res <- trk_resamp()[trk_resamp()$id %in% keep_ids, ]

    if (input$model == "Resource Selection Function") {
      validate(
        need(input$rand_points, 'Please set no. of random points.')
      )
      t_res <- t_res %>% mutate(points = lapply(track, function(x) {
        x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
          amt::random_points(n = input$rand_points) %>% 
          amt::extract_covariates(env(), where = "both")
      }))
      # Convert environmental covariates to factor or numeric
      # Loop through IDs
      for (j in 1:nrow(t_res)) {
        # Loop through environmental covariates
        for (i in 1:length(names(env()))) {
          # Convert to factor
          if (env_info()$Categorical[i] && 
              is.numeric(t_res$points[[j]][[names(env())[i]]])) {
            t_res$points[[j]][[names(env())[i]]] <- as.factor(
              t_res$points[[j]][[names(env())[i]]]
            )
          } else if (!env_info()$Categorical[i] && 
                     is.factor(t_res$points[[j]][[names(env())[i]]])) {
            # Convert to numeric
            t_res$points[[j]][[names(env())[i]]] <- as.numeric(
              levels(t_res$points[[j]][[names(env())[i]]]
              )
            )[t_res$points[[j]][[names(env())[i]]]]
          }
        }
      }
      t_res
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$rand_stps, 'Please set no. of random steps.')
      )
      # Time of day is not selected
      if (input$tod == '') {
        t_res <- t_res %>%
          mutate(steps = lapply(track, function(x) {
            x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
              amt::steps_by_burst() %>% 
              amt::random_steps(n = input$rand_stps) %>% 
              amt::extract_covariates(env(), where = "both") %>% 
              mutate(log_sl_ = log(sl_), 
                     cos_ta_ = cos(ta_)
              )
          }))
        # Convert environmental covariates to factor or numeric
        for (j in 1:nrow(t_res)) {
          for (i in 1:length(names(env()))) {
            # Convert to factor
            if (env_info()$Categorical[i] && 
                is.numeric(
                  t_res$steps[[j]][[paste0(names(env())[i], "_end")]])) {
              # Step start (_start)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_start")]] <- as.factor(
                  t_res$steps[[j]][[paste0(names(env())[i], "_start")]]
                )
              # Step end (_end)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_end")]] <- as.factor(
                  t_res$steps[[j]][[paste0(names(env())[i], "_end")]]
                )
            } else if (!env_info()$Categorical[i] && 
                       is.factor(t_res$steps[[j]][[
                           paste0(names(env())[i], "_end")]])) {
              # Convert to numeric
              # Step start (_start)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_start")]] <- as.numeric(
                  levels(t_res$steps[[j]][[paste0(names(env())[i], "_start")]]
                  )
                )[t_res$steps[[j]][[paste0(names(env())[i], "_start")]]]
              # Step end (_end)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_end")]] <- as.numeric(
                  levels(t_res$steps[[j]][[paste0(names(env())[i], "_end")]]
                  )
                )[t_res$steps[[j]][[paste0(names(env())[i], "_end")]]]
            }
          }
        }
        t_res
      } else {
        # A time of day option is selected 
        t_res <- t_res %>% 
          mutate(steps = lapply(track, function(x) {
            x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
              amt::steps_by_burst() %>% 
              amt::random_steps(n = input$rand_stps) %>% 
              amt::extract_covariates(env(), where = "both") %>% 
              mutate(log_sl_ = log(sl_), 
                     cos_ta_ = cos(ta_)
              ) %>%
              # Add time of day
              time_of_day(include.crepuscule = input$tod)
          }))
        # Convert environmental covariates to factor or numeric
        for (j in 1:nrow(t_res)) {
          for (i in 1:length(names(env()))) {
            # Convert to factor
            if (env_info()$Categorical[i] && 
                is.numeric(
                  t_res$steps[[j]][[paste0(names(env())[i], "_end")]])) {
              # Step start (_start)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_start")]] <- as.factor(
                  t_res$steps[[j]][[paste0(names(env())[i], "_start")]]
                )
              # Step end (_end)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_end")]] <- as.factor(
                  t_res$steps[[j]][[paste0(names(env())[i], "_end")]]
                )
            } else if (!env_info()$Categorical[i] && 
                       is.factor(t_res$steps[[j]][[
                         paste0(names(env())[i], "_end")]])) {
              # Convert to numeric
              # Step start (_start)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_start")]] <- as.numeric(
                  levels(t_res$steps[[j]][[paste0(names(env())[i], "_start")]]
                  )
                )[t_res$steps[[j]][[paste0(names(env())[i], "_start")]]]
              # Step end (_end)
              t_res$steps[[j]][[
                paste0(names(env())[i], "_end")]] <- as.numeric(
                  levels(t_res$steps[[j]][[paste0(names(env())[i], "_end")]]
                  )
                )[t_res$steps[[j]][[paste0(names(env())[i], "_end")]]]
            }
          }
        }
        t_res
      }
    }
  } else {
    # One/ no ID selected (single model)
    if (input$model == "Resource Selection Function") {
      validate(
        need(input$rand_points, 'Please set no. of random points.')
      )
      set.seed(12345)
      t_res <- trk_resamp() %>% 
        amt::filter_min_n_burst(min_n = input$min_burst) %>% 
        amt::random_points(n = input$rand_points) %>% 
        amt::extract_covariates(env(), where = "both")
      
      # Convert environmental covariates to factor or numeric
      # where = "both" doesn't apply to random points unlike 
      # random steps (ISSF) i.e. we don't need to convert point start and end 
      for (i in 1:length(names(env()))) {
        # Convert to factor
        if (env_info()$Categorical[i] &&
            is.numeric(t_res[[names(env())[i]]])) {
          t_res[[names(env())[i]]] <- as.factor(t_res[[names(env())[i]]])
        } else if (!env_info()$Categorical[i] &&
                   is.factor(t_res[[names(env())[i]]])) {
          # Convert to numeric
          t_res[[names(env())[i]]] <- as.numeric(
            levels(t_res[[names(env())[i]]]))[t_res[[names(env())[i]]]]
        }
      }
      t_res
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$rand_stps, 'Please set no. of random steps.')
      )
      # Time of day is not selected
      if (input$tod == '') {
        set.seed(12345)
        t_res <- trk_resamp() %>% 
          amt::filter_min_n_burst(min_n = input$min_burst) %>% 
          amt::steps_by_burst() %>%
          amt::random_steps(n = input$rand_stps) %>% 
          amt::extract_covariates(env(), where = "both") %>%
          mutate(log_sl_ = log(sl_), 
                 cos_ta_ = cos(ta_)
          )
        # Convert environmental covariates to factor or numeric
        for (i in 1:length(names(env()))) {
          # Convert to factor
          if (env_info()$Categorical[i] && 
              is.numeric(t_res[[paste0(names(env())[i], "_end")]])) {
            # Step start (_start)
            t_res[[paste0(names(env())[i], "_start")]] <- as.factor(
              t_res[[paste0(names(env())[i], "_start")]]
            )
            # Step end (_end)
            t_res[[paste0(names(env())[i], "_end")]] <- as.factor(
              t_res[[paste0(names(env())[i], "_end")]]
            )
          } else if (!env_info()$Categorical[i] && 
                     is.factor(
                       t_res[[paste0(names(env())[i], "_end")]])) {
            # Convert to numeric
            # Step start (_start)
            t_res[[paste0(names(env())[i], "_start")]] <- as.numeric(
              levels(t_res[[paste0(names(env())[i], "_start")]]
              )
            )[t_res[[paste0(names(env())[i], "_start")]]]
            # Step end (_end)
            t_res[[paste0(names(env())[i], "_end")]] <- as.numeric(
              levels(t_res[[paste0(names(env())[i], "_end")]]
              )
            )[t_res[[paste0(names(env())[i], "_end")]]]
          }
        }
        t_res
      } else {
        # A time of day option is selected 
        set.seed(12345)
        t_res <- trk_resamp() %>% 
          amt::filter_min_n_burst(min_n = input$min_burst) %>% 
          amt::steps_by_burst() %>% 
          amt::random_steps(n = input$rand_stps) %>% 
          amt::extract_covariates(env(), where = "both") %>%
          mutate(log_sl_ = log(sl_), 
                 cos_ta_ = cos(ta_)
          ) %>%
          # Add time of day
          time_of_day(include.crepuscule = input$tod)
        
        # Convert environmental covariates to factor or numeric
        for (i in 1:length(names(env()))) {
          # Convert to factor
          if (env_info()$Categorical[i] && 
              is.numeric(t_res[[paste0(names(env())[i], "_end")]])) {
            # Step start (_start)
            t_res[[paste0(names(env())[i], "_start")]] <- as.factor(
              t_res[[paste0(names(env())[i], "_start")]]
            )
            # Step end (_end)
            t_res[[paste0(names(env())[i], "_end")]] <- as.factor(
              t_res[[paste0(names(env())[i], "_end")]]
            )
          } else if (!env_info()$Categorical[i] && 
                     is.factor(
                       t_res[[paste0(names(env())[i], "_end")]])) {
            # Convert to numeric
            # Step start (_start)
            t_res[[paste0(names(env())[i], "_start")]] <- as.numeric(
              levels(t_res[[paste0(names(env())[i], "_start")]]
              )
            )[t_res[[paste0(names(env())[i], "_start")]]]
            # Step end (_end)
            t_res[[paste0(names(env())[i], "_end")]] <- as.numeric(
              levels(t_res[[paste0(names(env())[i], "_end")]]
              )
            )[t_res[[paste0(names(env())[i], "_end")]]]
          }
        }
        t_res
      }
    }
  }
})

# Get variable names
mod_pre_var <- reactive({
  validate(
    need(mod_pre(), ''),
    need(input$model != '', '')
  )
  # Multiple IDs selected (individual models)
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    if (input$model == "Resource Selection Function") {
      # column names only using colnames() doesn't work here!
      mod_pre()$points[[1]] %>% head(n=0)
    } else if (input$model == "Integrated Step Selection Function") {
      mod_pre()$steps[[1]] %>% head(n=0) # column names only
    }
  } else {
    # One/ no ID selected (single model)
    if (input$model == "Resource Selection Function") {
      mod_pre() %>% head(n=0) # column names only
    } else if (input$model == "Integrated Step Selection Function") {
      mod_pre() %>% head(n=0) # column names only
    }
  }
})

# Create formula with variables for model building below
mod_all_var <- reactive({
  validate(
    need(input$mod_var, 'Please select model variables.')
  )
  # Model variables
  p_var <- paste0(input$mod_var, collapse = " + ")

  # Interaction terms
  p_inter_1 <- ifelse(length(input$inter_1) == 2, 
                      yes = paste0(" + ", input$inter_1[1], ":", 
                                   input$inter_1[2]), 
                      no = '')
  p_inter_2 <- ifelse(length(input$inter_2) == 2, 
                      yes = paste0(" + ", input$inter_2[1], ":", 
                                  input$inter_2[2]), 
                      no = '')
  p_inter_3 <- ifelse(length(input$inter_3) == 2, 
                      yes = paste0(" + ", input$inter_3[1], ":", 
                                  input$inter_3[2]), 
                      no = '')
  p_inter_4 <- ifelse(length(input$inter_4) == 2, 
                      yes = paste0(" + ", input$inter_4[1], ":", 
                                  input$inter_4[2]), 
                      no = '')
  p_inter_5 <- ifelse(length(input$inter_5) == 2, 
                      yes = paste0(" + ", input$inter_5[1], ":", 
                                  input$inter_5[2]), 
                      no = '')
  # Concatenate all variable types
  paste0(p_var, p_inter_1, p_inter_2, p_inter_3, p_inter_4, p_inter_5)
})

# Fit button (to start model fitting)
# fit <- eventReactive(input$fit_button, {
#   # Run model fitting part below
#   mod()
# })

# Clear model button (works for inputs only)
observeEvent(input$clear_button, {
  shinyjs::reset("modeling_tab")
})

# Observe fit model and clear model buttons for model output
values_model <- reactiveValues(model_state = NULL)

observeEvent(input$fit_button, {
  values_model$model_state <- 'fit'
})
observeEvent(input$clear_button, {
  values_model$model_state <- 'clear'
})

# Fit model
mod <- reactive({
  validate(
    need(input$model != '', 'Please choose a model.'),
    need(values_model$model_state == 'fit', '')
  )
  # Multiple IDs selected (individual models)
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    
    # Fit RSF (Resource Selection Function; logistic regression)
    if (input$model == "Resource Selection Function") {
      validate(
        need(input$mod_var, 'Please select model variables.')
      )
      set.seed(12345)
      rsf_multi <- mod_pre() %>% mutate(fit = map(
        points, ~ amt::fit_rsf(., as.formula(paste("case_ ~", mod_all_var())))))
      # Data frame with coefficients
      rsf_multi %>% mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>% 
        select(id, coef) %>% unnest() %>% tibble::as.tibble()
      
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$mod_var, 'Please select model variables.')
      )
      set.seed(12345)
      # Fit SSF (Step Selection Function; conditional logistic regression)
      issf_multi_fit <- mod_pre() %>%
        mutate(
          fit = map(steps, ~ amt::fit_issf(
            ., as.formula(paste("case_ ~", mod_all_var(), "+ strata(step_id_)"
              )))))
      # Data frame with coefficients
      issf_multi_fit %>% mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>% 
        select(id, coef) %>% unnest() %>% tibble::as.tibble()
    }
  } else {
    # One/ no ID selected (single model)
    # Fit RSF (Resource Selection Function; logistic regression)
    if (input$model == "Resource Selection Function") {
      validate(
        need(input$mod_var, 'Please select model variables.')
      )
      set.seed(12345)
      rsf_one_fit <- mod_pre() %>% 
        amt::fit_rsf(as.formula(paste("case_ ~", mod_all_var())))
      # Data frame with coefficients
      broom::tidy(rsf_one_fit$model) %>% tibble::as.tibble()
      
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$mod_var, 'Please select model variables.')
      )
      set.seed(12345)
      issf_one_fit <- mod_pre() %>% 
        amt::fit_issf(
          as.formula(paste("case_ ~", mod_all_var(), "+ strata(step_id_)"))
          )
      # Data frame with coefficients
      broom::tidy(issf_one_fit$model) %>% tibble::as.tibble()
    }
  }
})

# Output data frame with coefficients
output$contents_mod <- DT::renderDataTable({
  validate(
    need(input$model != '', ''),
    need(values_model$model_state == 'fit', '')
  )
  # Dependent on fit button above
  DT::datatable(
    # Round numeric columns
    mod() %>% dplyr::mutate_if(is.numeric, round, 4),
    rownames = FALSE,
    options = list(searching = FALSE, paging = FALSE))
})

# Download button for csv of model output ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("model_estimates", ".csv")
  },
  content = function(file) {
    write.csv(mod(), file, row.names = FALSE)
  }
)


# Visualize ----------------------------------------------------------------


# Insert Olli MAGIC here!



# End server!!!
} 


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
