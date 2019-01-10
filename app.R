library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(amt)


# Example data
# Tracking / relocation data
fisher_ny <- read_csv("data/Martes pennanti LaPoint New York.csv")
# Subset relevant columns
fisher_ny <- fisher_ny[, c("location-long", "location-lat", "timestamp", 
                           "individual-local-identifier")]
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
        fluidRow(
          title = "Tracking Data Upload",
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
              # Horizontal line
              hr(),
              # Input: Checkbox if file has header
              checkboxInput(
                inputId = "header",
                label = "Header",
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
              #Input: Select data table or summary of data set
              radioButtons(
                inputId = "display",
                label = "Display",
                choices = c("Data Frame", "Summary"),
                selected = "Data Frame"
              ),
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
              fluidRow(
                  title = "Environmental Data Upload",
                  # Sidebar layout with input and output definitions
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      width = 3,
                      # Input: Select a file
                      fileInput(
                        inputId = "dataset_env",
                        label = "Choose TIF File"#,
                        #accept = c("tif", ".tif")
                      ),
                      actionButton('reset_env', 'Reset Input'),
                      # Horizontal line
                      hr(),
                      # Example Datasets
                      selectInput(
                        inputId = "ex_data_env",
                        label = "Choose Example Data:",
                        choices = c("None", "Fisher NY Land Use Area")
                      ),
                      # EPSG Code TIF
                      uiOutput(outputId = "epsg_env")
                    ),
                    mainPanel = mainPanel(
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
           uiOutput(outputId = "ts"),
           uiOutput(outputId = "id")
    ),
    # Transform EPSG Codes of CSV and TIF and select ID(s)
    column(width = 4, #offset = 1,
           br(),
           br(),
           uiOutput(outputId = "epsg_trk"),
           uiOutput(outputId = "id_trk")
    ),
    # Resample track
    column(width = 4, #offset = 1,
           h4("Resample Track"),
           br(),
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
  hr(),
  fluidRow(
    #Input: Select data table or summary of data set
    column(width = 1,
           radioButtons(
             inputId = "display_trk",
             label = "Display",
             choices = c("Data Frame", "Summary"),
             selected = "Data Frame"
           )
    ),
    # Data table or summary
    column(width = 5, #offset = 1,
           DT::dataTableOutput(outputId = "contents_trk"),
           verbatimTextOutput(outputId = "summary_trk")
    ),
    column(width = 4, offset = 1,
           h4(textOutput(outputId = "samp_rate_head")),
           DT::dataTableOutput(outputId = "summary_samp_rate")
    )
  )
),

# Add Additional Covariates Tab -------------------------------------------
tabItem(tabName = "covariates",
        fluidRow(
          column(width = 4, #offset = 1,
                 # Only retain bursts with a minimum number of relocations
                 uiOutput(outputId = "min_burst"),
                 # Time of Day
                 uiOutput(outputId = "tod")
                 )
        ),
        hr(),
        fluidRow(
        )
),

# Visualize Tab -----------------------------------------------------------

tabItem(tabName = "plot",
        h2("Under Maintenance")),

# Modeling Tab ------------------------------------------------------------

tabItem(tabName = "model",
  fluidRow(
    column(width = 2, #offset = 1,
           radioButtons(
             inputId = "model",
             label = h4("Choose a Model"),
             choices = c("Resource Selection Function", 
                         "Integrated Step Selection Function",
                         "None"),
             selected = "None"
           ),
           # Set number of random steps per relocation (ISSF)
           uiOutput(outputId = "rand_stps"),
           # Set number of random points (RSF)
           uiOutput(outputId = "rand_points"),
           # Fit model button
           actionButton("fit_button", "Fit Model"),
           # Download button for model output
           downloadButton("downloadData", "Download")
    ),
    column(width = 3,
           br(),
           br(),
           # Select land use area
           #uiOutput(outputId = "lu"),
           # Assign land use covariate name
           #uiOutput(outputId = "lu_name"),
           # Select model variables
           uiOutput(outputId = "mod_var"),
           # Select no. of interaction terms to add
           uiOutput(outputId = "inter_no")
           # Select logarithmized model variables
           #uiOutput(outputId = "log_var")
    ),
    # column(width = 2,
    #        # Select 1st to 5th interaction
    #        uiOutput(outputId = "inter_1"),
    #        uiOutput(outputId = "inter_2"),
    #        uiOutput(outputId = "inter_3"),
    #        uiOutput(outputId = "inter_4"),
    #        uiOutput(outputId = "inter_5")
    #        )
    column(width = 1,
           br(),
           br(),
           # Select 1st interaction
           uiOutput(outputId = "inter_1")
    ),
    column(width = 1,
           br(),
           br(),
           # Select 2nd interaction
           uiOutput(outputId = "inter_2")
    ),
    column(width = 1,
           br(),
           br(),
           # Select 3rd interaction
           uiOutput(outputId = "inter_3")
    ),
    column(width = 1,
           br(),
           br(),
           # Select 4th interaction
           uiOutput(outputId = "inter_4")
    ),
    column(width = 1,
           br(),
           br(),
           # Select 5th interaction
           uiOutput(outputId = "inter_5")
    )
  ),
  br(), # break
  hr(), # horizontal line not showing for some reason???
  fluidRow(
    column(width = 5,
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
      csv_uploaded <- read_csv(file = input$dataset_csv$datapath, 
                               col_names = input$header,
                               quote = input$quote)
      # Rename columns containing special characters e.g. "-"
      names(csv_uploaded) <- make.names(names(csv_uploaded), unique = TRUE)
    }
    # Semicolon separated
    if (input$sep == ";") {
      csv_uploaded <- read_csv2(file = input$dataset_csv$datapath,
                                col_names = input$header,
                                quote = input$quote)
      # Rename columns containing special characters e.g. "-"
      names(csv_uploaded) <- make.names(names(csv_uploaded), unique = TRUE)
    }
    # Tab separated
    if (input$sep == "\t") {
      csv_uploaded <- read_tsv(file = input$dataset_csv$datapath, 
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
  if (input$display == "Summary") {
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
env <- reactive({
  if (is.null(values_env$upload_state)){
    switch (input$ex_data_env,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  } else if (values_env$upload_state == 'uploaded') {
    raster_up <- raster::raster(x = input$dataset_env$datapath)
    # Rename uploaded TIF for usage in model building
    names(raster_up) <- "land_use"
    raster_up
    # names(raster::raster(x = input$dataset_env$datapath)) <- "land_use"
  } else if (values_env$upload_state == 'reset') {
    switch (input$ex_data_env,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  }
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
  detect_epsg %>% select("Detected EPSG Code(s)" = code, "Description" = note)
  })
# EPSG Code TIF
output$epsg_env <- renderUI({
  selectInput(
    inputId = "epsg_env",
    label = "Assign EPSG Code:", 
    choices = sort(na.omit(epsg_data$code)),
    selected = ifelse(!is.null(env()) && !is.null(epsg_env_detected()), 
                      # Multiple matches possible select 1st one by default
                      yes = epsg_env_detected()[1, "Detected EPSG Code(s)"],
                      no = input$epsg_csv
                      )
)
})
# ?????????
# Assign CRS if not described yet?
# projection(x) <- CRS("+init=epsg:28992")
# ?????????


# Data frame of detected EPSG codes
output$contents_env <- DT::renderDataTable({
  validate(
    need(env(), 'Please upload an environmental data file.'),
    need(epsg_env_detected(), 'No EPSG code detected from uploaded file.')
  )
  DT::datatable(epsg_env_detected(),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE,
                columnDefs = list(list(className = 'dt-left', 
                                       targets = 0:1)))
                )
})




# Track Creation ----------------------------------------------------------


# Update variable selection based on uploaded data set
# Show head line for track menu in sidebar
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
    label = "x (location-long):", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign location-long', # 'Please select an option below'
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
    label = "y (location-lat):", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign location-lat',
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
    label = "ts (timestamp):", 
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
    label = "id (individual-local-identifier):", 
    choices = colnames(csvInput()),
    options = list(
      placeholder = 'Assign ID',
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

# Create a track
trk <- reactive({
    validate(
      need(input$x, ''),
      need(input$y, ''),
      need(input$ts, '')
      #need(input$id_trk, "Please select at least one ID.")
    )
  
  # No ID selected (one model for all animals)
  if (input$id == '') {
    # Assign known EPSG Code to tracking data
    track <- make_track(dat_excl_id(), x, y, ts,
                        crs = sp::CRS(paste("+init=epsg:", input$epsg_csv,
                                            sep = ''))
    )
    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      track
    } else {
      transform_coords(track, sp::CRS(paste("+init=epsg:",
                                            input$epsg_trk, sep = ''))
      )
    }
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    # Multiple IDs selected (individual models)
    if (input$epsg_csv == input$epsg_trk) {
      # Assign known EPSG Code to tracking data
      track_multi <- dat() %>% nest(-id) %>%
        mutate(track = lapply(data, function(d) {
          amt::make_track(d, x, y, ts, crs = sp::CRS(paste(
            "+init=epsg:", input$epsg_csv, sep = '')))
        }))
      # Subset filtered ID(s)
      track_multi[track_multi$id %in% input$id_trk, ]
    } else {
      # Transform CRS of track
      trk_multi_tr <- dat() %>% nest(-id) %>%
        mutate(track = lapply(data, function(d) {
          amt::make_track(d, x, y, ts, crs = sp::CRS(paste(
            "+init=epsg:", input$epsg_csv, sep = ''))) %>%
            amt::transform_coords(sp::CRS(paste(
              "+init=epsg:", input$epsg_trk, sep = '')))
        }))
      # Subset filtered ID(s)
      trk_multi_tr[trk_multi_tr$id %in% input$id_trk, ]
    }
  } else if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) == 1) {
    # One ID selected
    # Assign known EPSG Code to tracking data
    track_one <- make_track(dat(), x, y, ts, id = id,
                        crs = sp::CRS(paste("+init=epsg:", input$epsg_csv,
                                            sep = ''))
    )
    # Subset filtered ID(s)
    track_one <- track_one[track_one$id %in% input$id_trk, ]

    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      track_one
    } else {
      transform_coords(track_one, sp::CRS(paste("+init=epsg:",
                                            input$epsg_trk, sep = ''))
      )
    }
  }
})

# Summarize sampling rate
samp_rate <- reactive({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')
  )
  # Multiple IDs selected
 if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
   # trk_multi <- group_by(trk(), id) %>% nest()
   # map_df(trk_multi$data, summarize_sampling_rate) %>% as.data.frame()
   trk() %>% mutate(sr = lapply(track, summarize_sampling_rate)) %>%
     select(id, sr) %>% unnest
 } else {
    # One/ no ID selected
    summarize_sampling_rate(trk()) %>% as.data.frame()
 }
})

# Show head line for sampling rate (Output)
output$samp_rate_head <- renderText({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')#,
    #need(input$id, '')
  )
  "Summary of Sampling Rate (in min)"
})

# Summarize sampling rate (Output)
output$summary_samp_rate <- DT::renderDataTable({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')#,
    #need(input$id, '')
  )
  # Multiple IDs selected
  if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
    # Round results excluding columns "id" and "unit" (min)
    sr <- subset(samp_rate(), select = - c(id, unit)) %>% round(2)
    # Add id column
    sr <- cbind(subset(samp_rate(), select = id), sr)
    DT::datatable(sr,
                  rownames = FALSE,
                  options = list(searching = FALSE, paging = FALSE)
    )
  } else {
    # One/ no ID selected
    # Exclude column "unit" (min)
    DT::datatable(subset(samp_rate(), select = - unit) %>% round(2),
                  rownames = FALSE,
                  options = list(searching = FALSE, paging = FALSE)
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
      mutate(track = map(track, function(x) {
        x %>% amt::track_resample(rate = minutes(10), tolerance = seconds(120))
      })) #%>% filter(t_ >= input$daterange[1] & t_ <= input$daterange[2])
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

# Track table displayed in app (dependent on resampling)
trk_df <- reactive({
  # Before resampling
  if (is.na(input$rate_min) && is.na(input$tol_min)) {
    # Multiple IDs selected
    if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
      trk_unnested_df <- trk() %>% select(id, track) %>% unnest 
      #Subsetting according to selected dateRangeInput
      trk_unnested_df[trk_unnested_df$t_ >= input$daterange[1] & trk_unnested_df$t_ <= input$daterange[2], ]
    } else if (ifelse(input$id == '', yes = 0, 
                      no = length(input$id_trk)) == 1) {
      # One ID selected
      # Swap columns for uniformity
      trk()[, c("id", "x_", "y_", "t_")]
      #Subsetting according to selected dateRangeInput
      trk()[trk()$t_ >= input$daterange[1] & trk()$t_ <= input$daterange[2], ]
    } else {
      # No ID selected (Subsetting according to selected dateRangeInput)
      trk()[trk()$t_ >= input$daterange[1] & trk()$t_ <= input$daterange[2], ]
    }
  } else {
    # Resampled track
    # Multiple IDs selected
    if (ifelse(input$id == '', yes = 0, no = length(input$id_trk)) > 1) {
      # Convert back to data frame for illustration
      trk_resamp_unnested_df <- trk_resamp() %>% select(id, track) %>% unnest
      #trk_resamp_unnested_df <- trk_resamp() %>% unnest() %>% as.data.frame()
      #Subsetting according to selected dateRangeInput
      trk_resamp_unnested_df[trk_resamp_unnested_df$t_ >= input$daterange[1] & trk_resamp_unnested_df$t_ <= input$daterange[2], ]
    } else if (ifelse(input$id == '', yes = 0, 
                      no = length(input$id_trk)) == 1) {
      # One ID selected
      # Swap columns for uniformity
      trk_resamp()[, c("id", "x_", "y_", "t_", "burst_")]
      #Subsetting according to selected dateRangeInput
      trk_resamp()[trk_resamp()$t_ >= input$daterange[1] & trk_resamp()$t_ <= input$daterange[2], ]
    } else {
      # No ID selected (Subsetting according to selected dateRangeInput)
      trk_resamp()[trk_resamp()$t_ >= input$daterange[1] & trk_resamp()$t_ <= input$daterange[2], ]
    }
  }
}) 

# Display data frame of track
output$contents_trk <- DT::renderDataTable({
  validate(
    need(input$x, 'Please assign location-long.'),
    need(input$y, 'Please assign location-lat.'),
    need(input$ts, 'Please assign timestamp.')#,
    #need(input$id, 'Please assign ID.')
  )
  if (input$display_trk == "Data Frame") {
    DT::datatable(trk_df(),
                  rownames = FALSE,
                  options = list(searching = FALSE,
                                 lengthMenu = list(
                                   c(5, 10, 20, 50, 100),
                                   c('5', '10', '20', '50', '100')),
                                 pageLength = 10)
                  )
  }
})

# Display summary of track
output$summary_trk <- renderPrint({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, '')#,
    #need(input$id, '')
  )
  if (input$display_trk == "Summary") {
    summary(object = trk_df())
  }
})

# Dynamic dateRangeInput for track data frame ----
output$fetch_dr <- renderUI({
  validate(
    need(input$ts, '')
  )
  dateRangeInput(inputId = "daterange",
                 label = "Choose a Date Range",
                 start = min(trk()$t_),
                 end = max(trk()$t_),
                 max = Sys.Date(),
                 format = "yyyy-mm-dd",
                 separator = "to",
                 startview = "year"
  )
})

# Add Additional Covariates -----------------------------------------------

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
# Time of Day
output$tod <- renderUI({
  selectInput(
    inputId = "tod",
    label = "Time of Day:",
    choices = c("",
                "excl. dawn and dusk" = FALSE, 
                "incl. dawn and dusk" = TRUE),
    selected = "" #"excl. dawn and dusk"
  )
})




# Modeling ----------------------------------------------------------------

# Set number of random steps per relocation
output$rand_stps <- renderUI({
  validate(
    need(input$model == "Integrated Step Selection Function", '')
  )
  numericInput(
    inputId = "rand_stps",
    label = "Random Steps:",
    value = 10, #NA,
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
    label = "Set No. of Random Points:",
    value = 100,
    min = 1,
    step = 1
  )
})
# Variable choices for variable and interaction term drop downs
var_choices <- reactive({
  # Positions of variables not to include in choices
  pos_excl <- which(sort(names(mod_pre_var())) %in% c(
    "case_", "dt_", "x1_", "x2_", "y1_", "y2_", "t1_", "t2_", "burst_", 
    "step_id_"))
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
    label = "Select No. of Interaction Terms:",
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
    need(input$model != 'None', '')
  )
  # Multiple IDs selected (individual models)
  if (length(input$id_trk) > 1) {
    
    if (input$model == "Resource Selection Function") {
      validate(
        need(input$rand_points, 'Please set no. of random points.')
      )
      # Define data frame for usage below
      t_res <- trk_resamp()
      # Test wether chosen minimum no. of relocations per burst is too high 
      # for some IDs
      removed_ids <- vector()
      j = 0
      for (i in 1:nrow(t_res)) {
        min_burst <- t_res$track[[i]]
        freq <- table(min_burst["burst_"]) 
        pos <- which(freq >= 3)
        # No observations given min. no. of relocations per burst
        if (length(pos) == 0) {
          j = j + 1
          # Assign empty tibble (those tibbles will be removed below)
          t_res$track[[i]] <- tibble()
          # Store ID(s) in vector (for notification displayed to the user)
          removed_ids[j] <- t_res$id[i]
        }
      }
      # All IDs meet chosen minimum no. of relocations per burst
      if (length(removed_ids) == 0) {
        t_res %>% mutate(points = lapply(track, function(x) {
          x %>% filter_min_n_burst(min_n = input$min_burst) %>% 
            #time_of_day(include.crepuscule = FALSE) %>% 
            random_points(n = input$rand_points) %>% 
            extract_covariates(env(), where = "both") %>% 
            mutate(land_use = factor(land_use)) 
        }))
      } else if (length(removed_ids) > 0) {
        # Remove ID(s) not meeting chosen minimum no. of relocations per burst
        showNotification(
          ui = paste(
            "The minimum no. of relocations per burst is too high for the 
            ID(s): ", paste(removed_ids, collapse = ", "), ". Therefore, those 
            were removed. If you want to retain those ID(s) please choose a 
            lower value. Alternatively you may choose a lower resampling rate, 
            i.e., a larger interval (in min) to retain the current no. of 
            minimum relocations per burst.", sep = ''),
          type = "warning",
          duration = NULL
        )
        t_res %>% 
          # remove IDs not meeting min no. of relocations per burst
          filter(purrr::map_int(track, nrow) > 0) %>% # 100
          mutate(points = lapply(track, function(x) {
          x %>% filter_min_n_burst(min_n = input$min_burst) %>% 
            #time_of_day(include.crepuscule = FALSE) %>% 
            random_points(n = input$rand_points) %>% 
            extract_covariates(env(), where = "both") %>% 
            mutate(land_use = factor(land_use))
        }))
      }
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$rand_stps, 'Please set no. of random steps.')
      )
      # Define data frame for usage below
      t_res <- trk_resamp()
      # Test wether chosen minimum no. of relocations per burst is too high 
      # for some IDs
      removed_ids <- vector()
      j = 0
      for (i in 1:nrow(t_res)) {
        min_burst <- t_res$track[[i]]
        freq <- table(min_burst["burst_"]) 
        pos <- which(freq >= 3)
        # No observations given min. no. of relocations per burst
        if (length(pos) == 0) {
          j = j + 1
          # Assign empty tibble (those tibbles will be removed below)
          t_res$track[[i]] <- tibble()
          # Store ID(s) in vector (for notification displayed to the user)
          removed_ids[j] <- t_res$id[i]
        }
      }
      # All IDs meet chosen minimum no. of relocations per burst
      if (length(removed_ids) == 0) {
        # Time of day is not selected
        if (input$tod == "") {
          t_res %>%
            mutate(steps = lapply(track, function(x) {
              x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
                amt::steps_by_burst() %>% 
                amt::random_steps(n = input$rand_stps) %>% 
                amt::extract_covariates(env(), where = "both") %>% 
                mutate(log_sl_ = log(sl_), 
                       cos_ta_ = cos(ta_), 
                       land_use_end = factor(land_use_end))
            }))
        } else {
          # A time of day option is selected 
          t_res %>% 
            mutate(steps = lapply(track, function(x) {
              x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
                amt::steps_by_burst() %>% 
                amt::random_steps(n = input$rand_stps) %>% 
                amt::extract_covariates(env(), where = "both") %>% 
                mutate(log_sl_ = log(sl_), 
                       cos_ta_ = cos(ta_), 
                       land_use_end = factor(land_use_end)) %>% 
                time_of_day(include.crepuscule = input$tod)
            }))
        }
      } else if (length(removed_ids) > 0) {
        # Remove ID(s) not meeting chosen minimum no. of relocations per burst
        showNotification(
          ui = paste(
            "The minimum no. of relocations per burst is too high for the 
            ID(s): ", paste(removed_ids, collapse = ", "), ". Therefore, those 
            were removed. If you want to retain those ID(s) please choose a 
            lower value. Alternatively you may choose a lower resampling rate, 
            i.e., a larger interval (in min) to retain the current no. of 
            minimum relocations per burst.", sep = ''),
          type = "warning",
          duration = NULL
          )
          # Time of day is not selected
          if (input$tod == "") {
            t_res %>% 
              # remove IDs not meeting min no. of relocations per burst
              filter(purrr::map_int(track, nrow) > 0) %>% # 100
              mutate(steps = lapply(track, function(x) {
                x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
                  amt::steps_by_burst() %>% 
                  amt::random_steps(n = input$rand_stps) %>% 
                  amt::extract_covariates(env(), where = "both") %>% 
                  mutate(log_sl_ = log(sl_), 
                         cos_ta_ = cos(ta_), 
                         land_use_end = factor(land_use_end))
              }))
          } else {
            # A time of day option is selected 
            t_res %>% 
              # remove IDs not meeting min no. of relocations per burst
              filter(purrr::map_int(track, nrow) > 0) %>% # 100
              mutate(steps = lapply(track, function(x) {
                x %>% amt::filter_min_n_burst(min_n = input$min_burst) %>% 
                  amt::steps_by_burst() %>% 
                  amt::random_steps(n = input$rand_stps) %>% 
                  amt::extract_covariates(env(), where = "both") %>% 
                  mutate(log_sl_ = log(sl_), 
                         cos_ta_ = cos(ta_), 
                         land_use_end = factor(land_use_end)) %>% 
                  time_of_day(include.crepuscule = input$tod)
              }))
          }  
      }
    }
    
  } else {
    # One/ no ID selected (single model)
    if (input$model == "Resource Selection Function") {
      validate(
        need(input$rand_points, 'Please set no. of random points.'),
        need(nrow(trk_resamp() %>% 
                    filter_min_n_burst(min_n = input$min_burst)) != 0,
 'The minimum no. of relocations per burst is too high, please choose a lower 
 one. Alternatively you may choose a lower resampling rate, i.e., 
 a larger interval (in min) to keep the current no. of minimum relocations 
 per burst.')
      )
      # Time of day is not selected
      # random_points removes tod, adding tod afterwards doesn't work since
      # tod requires class track_xyt or steps_xyt
      #if (input$tod == "") {
        set.seed(12345)
        trk_resamp() %>% filter_min_n_burst(min_n = input$min_burst) %>%
          # time_of_day(include.crepuscule = input$tod) %>% 
          random_points(n = input$rand_points) %>% 
          extract_covariates(env(), where = "both") %>%
          # Add renamed land use column ("lu") and convert to factor 
          mutate(land_use = factor(land_use))
      #} else {
        # A time of day option is selected
      #}
      
    } else if (input$model == "Integrated Step Selection Function") {
      # Test whether minimum no. of relocations per burst is too high (no 
      # observations left) 
      validate(
        need(input$rand_stps, 'Please set no. of random steps.'),
        need(nrow(trk_resamp() %>% 
                    filter_min_n_burst(min_n = input$min_burst)) != 0,
'The minimum no. of relocations per burst is too high, please choose a lower 
 one. Alternatively you may choose a lower resampling rate, i.e., 
 a larger interval (in min) to keep the current no. of minimum relocations 
 per burst.')
      )
      # Time of day is not selected
      if (input$tod == "") {
        set.seed(12345)
        issf_one <- trk_resamp() %>% 
          filter_min_n_burst(min_n = input$min_burst) %>% 
          steps_by_burst() %>%
          random_steps(n = input$rand_stps) %>% 
          extract_covariates(env(), where = "both") %>%
          mutate(log_sl_ = log(sl_), 
                 cos_ta_ = cos(ta_), 
                 land_use_end = factor(land_use_end))
        issf_one
        
      } else {
        # A time of day option is selected 
        set.seed(12345)
        issf_one <- trk_resamp() %>% 
          filter_min_n_burst(min_n = input$min_burst) %>% 
          steps_by_burst() %>% 
          random_steps(n = input$rand_stps) %>% 
          extract_covariates(env(), where = "both") %>%
          mutate(log_sl_ = log(sl_), 
                 cos_ta_ = cos(ta_), 
                 land_use_end = factor(land_use_end)) %>%
          time_of_day(include.crepuscule = input$tod)
        
        issf_one
      }
    }
  }
})

# Get variable names
mod_pre_var <- reactive({
  # Multiple IDs selected (individual models)
  if (length(input$id_trk) > 1) {
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
  p_var <- paste(input$mod_var, collapse = " + ")

  # Interaction terms
  p_inter_1 <- ifelse(length(input$inter_1) == 2, 
                      yes = paste(" + ", input$inter_1[1], ":", 
                                  input$inter_1[2], sep = ''), 
                      no = '')
  p_inter_2 <- ifelse(length(input$inter_2) == 2, 
                      yes = paste(" + ", input$inter_2[1], ":", 
                                  input$inter_2[2], sep = ''), 
                      no = '')
  p_inter_3 <- ifelse(length(input$inter_3) == 2, 
                      yes = paste(" + ", input$inter_3[1], ":", 
                                  input$inter_3[2], sep = ''), 
                      no = '')
  p_inter_4 <- ifelse(length(input$inter_4) == 2, 
                      yes = paste(" + ", input$inter_4[1], ":", 
                                  input$inter_4[2], sep = ''), 
                      no = '')
  p_inter_5 <- ifelse(length(input$inter_5) == 2, 
                      yes = paste(" + ", input$inter_5[1], ":", 
                                  input$inter_5[2], sep = ''), 
                      no = '')
  # Concatenate all variable types
  paste(p_var, p_inter_1, p_inter_2, p_inter_3, p_inter_4, p_inter_5, sep = '')
})

# Fit button (to start model fitting)
fit <- eventReactive(input$fit_button, {
  # Run model fitting part below
  mod()
})

# Fit model
mod <- reactive({
  validate(
    need(input$model != 'None', 'Please choose a model.')
  )
  # Multiple IDs selected (individual models)
  if (length(input$id_trk) > 1) {
    
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
        select(id, coef) %>% unnest() %>% as.data.frame()
      
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
        select(id, coef) %>% unnest() %>% as.data.frame()
  }
    
  } else {
    # One ID selected (single model)
    # Fit RSF (Resource Selection Function; logistic regression)
    if (input$model == "Resource Selection Function") {
      set.seed(12345)
      rsf_one_fit <- mod_pre() %>% 
        fit_rsf(as.formula(paste("case_ ~", mod_all_var())))
      # Data frame with coefficients
      broom::tidy(rsf_one_fit$model) %>% as.data.frame()
      
    } else if (input$model == "Integrated Step Selection Function") {
      validate(
        need(input$mod_var, 'Please select model variables.')
      )
      set.seed(12345)
      issf_one_fit <- mod_pre() %>% 
        fit_issf(
          as.formula(paste("case_ ~", mod_all_var(), "+ strata(step_id_)"))
          )
      # Data frame with coefficients
      broom::tidy(issf_one_fit$model) %>% as.data.frame()
    }
  }
})

# Output data frame with coefficients
output$contents_mod <- DT::renderDataTable({
  validate(
    need(input$model != "None", '')
  )
  # Dependent on fit button above
  DT::datatable(fit(),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE))
})

# Download button for csv of model output ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("model_estimates", ".csv", sep = "")
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
