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
      menuItem("Create Track", tabName = "track", icon = icon("map-marked-alt")),
      menuItem("Modeling", tabName = "model", icon = icon("database")),
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
              width = 2,
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
                label = "Assign EPSG Code",
                choices = na.omit(epsg_data$code), #rgdal::make_EPSG()["code"]
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
                      width = 2,
                      # Input: Select a file
                      fileInput(
                        inputId = "dataset_tif",
                        label = "Choose TIF File"#,
                        #accept = c("tif", ".tif")
                      ),
                      actionButton('reset_tif', 'Reset Input'),
                      # Horizontal line
                      hr(),
                      # Example Datasets
                      selectInput(
                        inputId = "ex_data_tif",
                        label = "Choose Example Data:",
                        choices = c("None", "Fisher NY Land Use Area")
                      ),
                      # EPSG Code TIF
                      uiOutput(outputId = "epsg_tif")
                    ),
                    mainPanel = mainPanel(
                      verbatimTextOutput(outputId = "contents_tif")
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
    br(),
    br(),
    column(width = 4, #offset = 1,
           uiOutput(outputId = "epsg_trk"),
           #br(),
           #br(),
           uiOutput(outputId = "id_trk")
    ),
    # Resample track
    column(width = 4, #offset = 1,
           #uiOutput(outputId = "rate_min"),
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

# Visualize Tab -----------------------------------------------------------

tabItem(tabName = "plot",
        h2("Magic by Olli")),

# Modeling Tab ------------------------------------------------------------

tabItem(tabName = "model",
  fluidRow(
    column(width = 4, #offset = 1,
           radioButtons(
             inputId = "model",
             label = h4("Choose a Model:"),
             choices = c("Resource Selection Function", 
                         "Step Selection Function (SSF)", 
                         "Integrated SSF",
                         "None"),
             selected = "None"
           )
    )
  ),
  br(), # break
  hr(), # horizontal line not showing for some reason???
  fluidRow(
    column(width = 5,
        verbatimTextOutput(outputId = "contents_mod"),
        plotOutput(outputId = "mod_plot")
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


values_tif <- reactiveValues(upload_state = NULL)

observeEvent(input$dataset_tif, {
  values_tif$upload_state <- 'uploaded'
})
observeEvent(input$reset_tif, {
  values_tif$upload_state <- 'reset'
})

tifInput <- reactive({
  if (is.null(values_tif$upload_state)){
    switch (input$ex_data_tif,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  } else if (values_tif$upload_state == 'uploaded') {
    names(raster::raster(x = input$dataset_tif$datapath)) <- "land_use"
  } else if (values_tif$upload_state == 'reset') {
    switch (input$ex_data_tif,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  }
})
# Detect EPSG Code of TIF-File by left joining data frame "epsg_data"
epsg_tif_detected <- reactive({
  validate(
    need(tifInput(), '')
  )
  tifInput_prj4 <- data.frame("prj4" = raster::projection(tifInput()),
                              stringsAsFactors = FALSE)
  detect_epsg <- dplyr::left_join(tifInput_prj4, epsg_data, by = "prj4")
  detect_epsg$code[1] # multiple matches possible choose the first one
  })
# EPSG Code TIF
output$epsg_tif <- renderUI({
  selectInput(
    inputId = "epsg_tif",
    # Add general info and for case where detection isn't possible and default 
    # will be shown (epsg_csv)
    label = "Detected EPSG Code", 
    choices = na.omit(epsg_data$code),
    selected = ifelse(!is.null(tifInput()) && !is.null(epsg_tif_detected()), 
                      yes = epsg_tif_detected(),
                      no = input$epsg_csv
                      )
)
})
# Transform CRS of TIF Input
# Use "env()" for extract_covariates(env()) not tifInput)!!!!!!!!!!!!!!!!!!!!!!!
env <- reactive({
  if (input$epsg_tif != input$epsg_trk && !is.null(input$epsg_tif)) {
    # ????????????????????????????????????????????????
    # Large Raster Layer introduces quite a few NAs
    # Use 'to' argument and projectExtent(object, crs) as template? 
    raster::projectRaster(tifInput(),
                          crs = sp::CRS(paste("+init=epsg:", 
                                              input$epsg_trk, sep = '')),
                          res = raster::res(tifInput()), # keep resolution
                          # categorial vairiables (nearest neighbor)
                          method="ngb" #"bilinear",
    )
  } else {
    tifInput()
  }
})
# ?????????
# Assign CRS if not described yet?
# projection(x) <- CRS("+init=epsg:28992")
# ?????????


# Display data currently not working as it has more than 2 dimensions
# Equivalent of View(df) for shiny???
output$contents_tif <- reactive({
  validate(
    need(tifInput(), ''),
    need(input$epsg_trk, '')
  )
  raster::projection(env()) # to test if the transformation worked (testing only)
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
# EPSG Code Transformation
output$epsg_trk <- renderUI({
  validate(
    need(csvInput(), '')
  )
  selectInput(
    inputId = "epsg_trk",
    # add info! (CSV and TIF will be transformed if EPSG deviates from TIF EPSG)
    label = "Transform CRS by EPSG Code:",
    choices = na.omit(epsg_data$code),
    selected = input$epsg_tif
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

# Create a track: choose columns
dat <- reactive({
  validate(
    need(csvInput(), '')
  )
  # Geometrically subset raster and omit NAs in subset
  csvInput() %>% 
  select(x = input$x, y = input$y, ts = input$ts, id = input$id) %>%  
  na.omit()
})

# Create a track
trk <- reactive({
    validate(
      need(input$id_trk, "Please select at least one ID.")
    )
    # Assign known EPSG Code to tracking data
    track <- make_track(dat(), x, y, ts, id = id,
                        crs = sp::CRS(paste("+init=epsg:", input$epsg_csv, 
                                            sep = ''))
    )
    # Subset filtered ID(s)
    track <- track[track$id %in% input$id_trk, ]
    
    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      track
    } else {
      transform_coords(track, sp::CRS(paste("+init=epsg:", 
                                            input$epsg_trk, sep = ''))
      )
    }
})

# Summarize sampling rate
samp_rate <- reactive({
  validate(
    need(input$id_trk, '')
  )
  # Multiple IDs selected
  if (length(input$id_trk) > 1) {
    trk_multi <- group_by(trk(), id) %>% nest()
    map_df(trk_multi$data, summarize_sampling_rate) %>% as.data.frame()
  } else {
    # One ID selected
    summarize_sampling_rate(trk()) %>% as.data.frame()
  }
})

# Resample track (this will be used for model building not trk_df)!!!!!!!!!!!!!!
trk_resamp <- reactive({
  validate(
    need(input$rate_min, ''),
    need(input$tol_min, '')
  )
  # Multiple IDs selected
  if (length(input$id_trk) > 1) {
    group_by(trk(), id) %>% nest() %>% 
      mutate(data = map(data, ~ .x %>% 
                          track_resample(rate = minutes(input$rate_min),
                                         tolerance = minutes(input$tol_min))))
  } else {
    # One ID selected
    trk() %>% track_resample(rate = minutes(input$rate_min), 
                             tolerance = minutes(input$tol_min))
  }
})

# Track table displayed in app (dependent on resampling)
trk_df <- reactive({
  # Before resampling
  if (is.na(input$rate_min) && is.na(input$tol_min)) {
    trk()
  } else {
    # Resampled track  
    # Multiple IDs selected
    if (length(input$id_trk) > 1) {
      # Convert back to data frame for illustration
      trk_resamp_unnested_df <- trk_resamp() %>% unnest() %>% as.data.frame()
      # Swap columns
      trk_resamp_unnested_df[, c("x_", "y_", "t_", "id", "burst_")]
    } else {
      # One ID selected
      trk_resamp()
    }
  }
}) 

# Display data frame of track
output$contents_trk <- DT::renderDataTable({
  validate(
    need(input$x, 'Please assign location-long.'),
    need(input$y, 'Please assign location-lat.'),
    need(input$ts, 'Please assign timestamp.'),
    need(input$id, 'Please assign ID.')
  )
  if (input$display_trk == "Data Frame") {
    DT::datatable(trk_df(),
                  rownames = FALSE,
                  options = list(
                    lengthMenu = list(c(5, 10, 20, 50, 100),
                                      c('5', '10', '20', '50', '100')),
                    pageLength = 10))
  }
})

# Display summary of track
output$summary_trk <- renderPrint({
  validate(
    need(input$x, ''),
    need(input$y, ''),
    need(input$ts, ''),
    need(input$id, '')
  )
  if (input$display_trk == "Summary") {
    summary(object = trk_df())
  }
})

# Show head line for sampling rate (Output)
output$samp_rate_head <- renderText({
  validate(
    need(input$id_trk, '')
  )
  "Summary of Sampling Rate (in min)"
})

# Summarize sampling rate (Output)
output$summary_samp_rate <- DT::renderDataTable({
  validate(
    need(input$id_trk, '')
  )
  # Exclude column "unit" (min)
  DT::datatable(samp_rate()[, -9] %>% round(2),
                rownames = FALSE,
                options = list(searching = FALSE, paging = FALSE)
  )
})




# Modeling ----------------------------------------------------------------


# Fit model
mod <- reactive({
  validate(
    need(input$model != 'None', 'Please choose a model.')
  )
  # Multiple IDs selected (individual models)
  if (length(input$id_trk) > 1) {
    
    # Fit RSF (Resource Selection Function; logistic regression)
    if (input$model == "Resource Selection Function") {
      set.seed(12345)
      rsf_multi <- trk_resamp() %>% mutate(
        m1 = map(data, ~ .x %>% random_points() %>% 
                   extract_covariates(env()) %>%
                   # Add renamed land use column ("lu") and convert to factor,
                   mutate(lu = factor(land_use)) %>% 
                   fit_rsf(case_ ~ lu)))
      # Plot: look at coefficients
      rsf_multi %>% mutate(m1_sum = map(m1, ~ broom::tidy(.$model))) %>% 
        select(id, m1_sum) %>% unnest %>% 
        ggplot(aes(term, estimate, col = id)) + geom_point() + 
        ggtitle("Resource Selection Function")
      
    } else if (input$model == "Step Selection Function (SSF)") {
      set.seed(12345)
      # Fit SSF (Step Selection Function; conditional logistic regression)
      ssf_multi <- trk_resamp() %>% 
        mutate(steps = map(data, steps_by_burst)) %>% 
        filter(map_int(steps, nrow) > 100) %>% # 100 as option (any number)???
        mutate(
          m2 = map(steps, ~ .x %>% random_steps %>% 
                     extract_covariates(env()) %>%
                     # Add renamed land use column ("lu") and convert to factor 
                     mutate(lu = factor(land_use)) %>%
                     fit_ssf(case_ ~ lu + strata(step_id_))))
      
      # look at coefficients
      ssf_multi %>% mutate(m2 = map(ssf_multi$m2, ~ broom::tidy(.$model))) %>% 
        select(id, m2) %>% unnest %>% 
        ggplot(aes(term, estimate, col = id)) + geom_point() + 
        ggtitle("Step Selection Function")
    } else if (input$model == "Integrated SSF") {
      return()
    }
    
    
  } else {
    # One ID selected (single model)
    # Fit RSF (Resource Selection Function; logistic regression)
    if (input$model == "Resource Selection Function") {
      set.seed(12345)
      rsf_one <- trk_resamp() %>% random_points() %>% 
        extract_covariates(env()) %>%
        # Add renamed land use column ("lu") and convert to factor 
        mutate(lu = factor(land_use)) %>%
        fit_rsf(case_ ~ lu)
      summary(rsf_one)
      
    } else if (input$model == "Step Selection Function (SSF)") {
      # Fit SSF (Step Selection Function; conditional logistic regression)
      set.seed(12345)
      ssf_one <- trk_resamp() %>% steps_by_burst() %>% random_steps() %>% 
        extract_covariates(env()) %>% 
        # Add renamed land use column ("lu") and convert to factor 
        mutate(lu = factor(land_use)) %>%
        fit_ssf(case_ ~ lu + strata(step_id_))
      summary(ssf_one)
    } else if (input$model == "Integrated SSF") {
      return()
    }
  }
})

# Output model fit (one ID only)
output$contents_mod <- renderPrint({
  validate(
    # For one ID only
    need(length(input$id_trk) == 1, '')
  )
  mod()
})
# Output model plot (multiple IDs only)
output$mod_plot <- renderPlot({
  validate(
    # For multiple IDs only
    need(length(input$id_trk) > 1, '')
  )
  mod()
})




# Visualize ----------------------------------------------------------------


# Insert Olli MAGIC here!



# End server!!!
} 


# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
