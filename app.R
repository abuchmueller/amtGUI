library(shiny)
library(ggplot2)
library(tidyverse)
library(amt)


# Example data
fisher_id_1016 <- read_csv("data/fisher_1016.csv")
fisher_id_1016_day <- read_csv("data/fisher_1016_day.csv")
fisher_id_1016_night <- read_csv("data/fisher_1016_night.csv")
fisher_ny <- read_csv("data/Martes pennanti LaPoint New York.csv")
# Rename columns containing special characters e.g. "-"
names(fisher_ny) <- make.names(names(fisher_ny), unique = TRUE)
land_use_fisher_ny <- raster::raster("data/landuse_study_area.tif")

# EPSG Codes
epsg_data <- rgdal::make_EPSG()

# Plot a tif
#img <- tiff::readTIFF("data/landuse_study_area.tif")
#grid::grid.raster(img)


#### UI ####
ui <- fluidPage(
  
  titlePanel(h3("Habitat Selection Analyses")),
  
  tabsetPanel(
    #### Tab: Data upload ####
    tabPanel(
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
          #actionButton('clean', 'Clean Data'),
          # Horizontal line
          tags$hr(),
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
            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = '"'
          ),
          # Horizontal line
          tags$hr(),
          #Input: Select data table or summary of data set
          radioButtons(
            inputId = "display",
            label = "Display",
            choices = c("Data Frame", "Summary"),
            selected = "Data Frame"
          ),
          tags$hr(),
          # Example Datasets
          selectInput(
            inputId = "ex_data_csv",
            label = "Choose Example Data:",
            choices = c("None", "Fisher NY", "Fisher ID 1016", "Fisher ID 1016 Day",
                        "Fisher ID 1016 Night")
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
    ),
    #### Tab: Environmental Data upload ####
    tabPanel(
      title = "Environmental Data Upload",
      # Sidebar layout with input and output definitions
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          # Input: Select a file
          fileInput(
            inputId = "dataset_tif",
            label = "Choose TIF File"#,
            #multiple = TRUE#,
            #accept = c("tif", ".tif")
          ),
          actionButton('reset_tif', 'Reset Input'),
          # Horizontal line
          tags$hr(),
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
          #DT::dataTableOutput(outputId = "contents_tif")#,
          #verbatimTextOutput(outputId = "summary")
        )
      )
    ),
    #### Tab: Create a Track ####
    tabPanel(
      title = "Create a Track",
      # Sidebar layout with input and output definitions
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          # Create a track
          h4(textOutput(outputId = "track_head")),
          #h4("Create a track"),
          uiOutput(outputId = "x"),
          uiOutput(outputId = "y"),
          uiOutput(outputId = "ts"),
          uiOutput(outputId = "id"),
          tags$hr(),
          #Input: Select data table or summary of data set
          radioButtons(
            inputId = "display_trk",
            label = "Display",
            choices = c("Data Frame", "Summary"),
            selected = "Data Frame"
          ),
          tags$hr(),
          # Transform EPSG Codes of CSV and TIF
          uiOutput(outputId = "epsg_trk")
          ),
          mainPanel = mainPanel(
            DT::dataTableOutput(outputId = "contents_trk"),
            verbatimTextOutput(outputId = "summary_trk")
          )
            )
          ),
    
    
    
  #### Tab: Plot ####
  tabPanel("Plot",
   plotOutput('plot')#,
)
)
)


#### server ####
server <- function(input, output, session) {
  
#### Tab: Data upload ####
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
            "Fisher ID 1016" = fisher_id_1016,
            "Fisher ID 1016 Day" = fisher_id_1016_day,
            "Fisher ID 1016 Night" = fisher_id_1016_night,
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
    # csv_uploaded <- read.csv(file = input$dataset_csv$datapath, 
    #                          header = input$header, sep = input$sep, 
    #                          quote = input$quote) 
    return(csv_uploaded)
  } else if (values_csv$upload_state == 'reset') {
    switch (input$ex_data_csv,
            "Fisher NY" = fisher_ny,
            "Fisher ID 1016" = fisher_id_1016,
            "Fisher ID 1016 Day" = fisher_id_1016_day,
            "Fisher ID 1016 Night" = fisher_id_1016_night,
            "None" = return()
    )
  }
})

# Update variable selection based on uploaded data set
# Show head line for track menu in sidebar
output$track_head <- renderText({
  if (!is.null(csvInput())) {
    "Create a Track:"
  }
})
# Choose x (location-long)
output$x <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectizeInput(
      inputId = 'x', 
      label = "x (location-long)", 
      choices = colnames(csvInput()),
      options = list(
        placeholder = 'Assign location-long', # 'Please select an option below'
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }
})
# Choose y (location-lat)
output$y <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectizeInput(
      inputId = 'y', 
      label = "y (location-lat)", 
      choices = colnames(csvInput()),
      options = list(
        placeholder = 'Assign location-lat',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }
})
# ts (timestamp)
output$ts <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectizeInput(
      inputId = 'ts', 
      label = "ts (timestamp)", 
      choices = colnames(csvInput()),
      options = list(
        placeholder = 'Assign timestamp',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }
})
# id (individual-local-identifier)
output$id <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectizeInput(
      inputId = 'id', 
      label = "id (individual-local-identifier)", 
      choices = colnames(csvInput()),
      options = list(
        placeholder = 'Assign ID',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }
})
# Display data frame or summary of data
output$contents <- DT::renderDataTable({
  
  if (!is.null(csvInput())) {
      if (input$display == "Data Frame") {
        # Selected number of records shown per page (range to chose from dependent
        # on data set size)
        page_length <-  if (nrow(csvInput()) > 100){
          c(10, 15, 20, 50, 100, nrow(csvInput()) )
        } else if (nrow(csvInput()) <= 10) {
          nrow(csvInput())
        } else if (nrow(csvInput()) <= 15) {
          c(10, nrow(csvInput()) )
        } else if (nrow(csvInput()) <= 20) {
          c(10, 15, nrow(csvInput()) )
        } else if (nrow(csvInput()) <= 50) {
          c(10, 15, 20, nrow(csvInput()) )
        } else if (nrow(csvInput()) <= 100) {
          c(10, 15, 20, 50, nrow(csvInput()) )
        }
        DT::datatable(csvInput(), 
                      rownames = FALSE,
                      options = list(
                        lengthMenu = page_length,
                        pageLength = 15
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



#### Environmental Data Upload ####
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
    raster::raster(x = input$dataset_tif$datapath)
  } else if (values_tif$upload_state == 'reset') {
    switch (input$ex_data_tif,
            "Fisher NY Land Use Area" = land_use_fisher_ny,
            "None" = return()
    )
  }
})
# Detect EPSG Code of TIF-File via data frame "epsg_data"
epsg_tif_detected <- reactive({
  
  if (!is.null(tifInput())) {
    tifInput_prj4 <- data.frame("prj4" = raster::projection(tifInput()),
                                stringsAsFactors = FALSE)
    detect_epsg <- dplyr::left_join(tifInput_prj4, epsg_data, by = "prj4")
    detect_epsg$code[1] # multiple matches possible choose the first one
    }
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
  if (input$epsg_tif != input$epsg_trk) {
  raster::raster(raster::projectRaster(tifInput(), crs = sp::CRS(
  paste("+init=epsg:", input$epsg_trk, sep = '')))
  )
  } else {
    tifInput()
  }
})
# ?????????
# Assign CRS if not described yet?
# projection(x) <- CRS(“+init=epsg:28992”)
# ?????????


# Display data currently not working as it has more than 2 dimensions
# Equivalent of View(df) for shiny???
output$contents_tif <- reactive({
  
  if (is.null(tifInput())) {
    return()
  } else {
    #tifInput()
    validate(need(input$epsg_trk, ''))
    raster::projection(env())
  }
})



#### Tab: Create a Track ####

# EPSG Code Transformation
output$epsg_trk <- renderUI({
  selectInput(
    inputId = "epsg_trk",
    # add info! (CSV and TIF will be transformed if EPSG deviates from TIF EPSG)
    label = "Transform CRS by EPSG Code",
    choices = na.omit(epsg_data$code),
    selected = input$epsg_tif
  )
})

# Create a track: choose columns
dat <- reactive({
  if (!is.null(csvInput()) && !is.null(tifInput())) {
    # Geometrically subset raster and omit NAs in subset
      csvInput() %>% 
      select(x = input$x, y = input$y, ts = input$ts, id = input$id) %>%  
      na.omit()
  }
  })

# Create a track
trk <- reactive({
  if (!is.null(dat())) {
    track <- make_track(dat(), x, y, ts, id = id,
                        crs = sp::CRS(paste("+init=epsg:", input$epsg_csv, 
                                            sep = ''))
    )
    # Transform CRS of track
    if (input$epsg_csv == input$epsg_trk) {
      return(track)
    } else {
    transform_coords(track, sp::CRS(paste("+init=epsg:", input$epsg_trk, 
                                          sep = ''))
    )
    }
  }
})

# Display data frame or summary of data
output$contents_trk <- DT::renderDataTable({
  validate(
    need(input$x, 'Please assign location-long.'),
    need(input$y, 'Please assign location-lat.'),
    need(input$ts, 'Please assign timestamp.'),
    need(input$id, 'Please assign ID.')
  )
  if (input$display_trk == "Data Frame") {
    # Selected number of records shown per page (range to chose from dependent
    # on data set size)
    page_length <-  if (nrow(trk()) > 100){
      c(10, 15, 20, 50, 100, nrow(trk()) )
    } else if (nrow(trk()) <= 10) {
      nrow(trk())
    } else if (nrow(trk()) <= 15) {
      c(10, nrow(trk()) )
    } else if (nrow(trk()) <= 20) {
      c(10, 15, nrow(trk()) )
    } else if (nrow(trk()) <= 50) {
      c(10, 15, 20, nrow(trk()) )
    } else if (nrow(trk()) <= 100) {
      c(10, 15, 20, 50, nrow(trk()) )
    }
    DT::datatable(trk(), rownames = FALSE,
                  options = list(
                    lengthMenu = page_length,
                    pageLength = 15
                  )
    )
  }
})
# Summary
output$summary_trk <- renderPrint({
  if (input$display_trk == "Summary") {
    summary(object = trk())
  }
})

}
#### run app ####
shinyApp(ui = ui, server = server)
