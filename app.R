library(shiny)
library(ggplot2)
library(tidyverse)
library(amt)


# Example data
# For some reason "amtGUI/" in path necessary for "Run App" button to work!!!
fisher <- read.csv("data/fisher_1016.csv")
fisher_day <- read.csv("data/fisher_1016_day.csv")
fisher_night <- read.csv("data/fisher_1016_night.csv")
deer_ny <- read.csv("data/Martes pennanti LaPoint New York.csv", 
                    #stringsAsFactors = FALSE, 
                    check.names = TRUE)

land_use <- raster::raster("data/landuse_study_area.tif")

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
          # # Create a track
          # h4(textOutput(outputId = "track_head")),
          # #h4("Create a track"),
          # uiOutput(outputId = "x"),
          # uiOutput(outputId = "y"),
          # uiOutput(outputId = "id"),
          # uiOutput(outputId = "ts"),
          tags$hr(),
          # Example Datasets
          selectInput(
            inputId = "ex_data_csv",
            label = "Choose Example Data:",
            choices = c("None", "Deer NY", "Fisher", "Fisher Day",
                        "Fisher Night")
          )#,
          # # Input: Select EPSG Code
          # selectInput(
          #   inputId = "epsg",
          #   label = "Map with EPSG Code",
          #   choices = epsg_data$code, #rgdal::make_EPSG()["code"]
          #   selected = 4326
          # )
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
            choices = c("None", "Land Use Study Area")
          )#,
          # Input: Select EPSG Code
          # selectInput(
          #   inputId = "epsg",
          #   label = "Map with EPSG Code",
          #   choices = epsg$code #rgdal::make_EPSG()["code"]
          # )
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
          # Input: Select EPSG Code
          selectInput(
            inputId = "epsg",
            label = "Map with EPSG Code",
            choices = epsg_data$code, #rgdal::make_EPSG()["code"]
            selected = 4326
          )
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
# observeEvent(input$clean, {
#   values_csv$upload_state <- 'clean'
# })
csvInput <- reactive({
  if (is.null(values_csv$upload_state)){
    switch (input$ex_data_csv,
            "Deer NY" = deer_ny,
            "Fisher" = fisher,
            "Fisher Day" = fisher_day,
            "Fisher Night" = fisher_night,
            "None" = return()
    )
  } else if (values_csv$upload_state == 'uploaded') {
    csv_uploaded <- read.csv(file = input$dataset_csv$datapath, 
                             header = input$header, sep = input$sep, 
                             quote = input$quote) 
    return(csv_uploaded)
  } else if (values_csv$upload_state == 'reset') {
    switch (input$ex_data_csv,
            "Deer NY" = deer_ny,
            "Fisher" = fisher,
            "Fisher Day" = fisher_day,
            "Fisher Night" = fisher_night,
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
    selectInput(
      inputId = "x",
      label = "x (location-long)",
      choices = colnames(csvInput())
    )
  }
})
# Choose y (location-lat)
output$y <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectInput(
      inputId = "y",
      label = "y (location-lat)",
      choices = colnames(csvInput())
    )
  }
})
# ts (timestamp)
output$ts <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectInput(
      inputId = "ts",
      label = "ts (timestamp)",
      choices = colnames(csvInput())#,
      #selected = "timestamp"
    )
  }
})
# id (individual-local-identifier)
output$id <- renderUI({
  if (is.null(csvInput())) {
    return()
  } else {
    selectInput(
      inputId = "id",
      label = "id",
      choices = colnames(csvInput())
    )
  }
})
# Display data frame or summary of data
output$contents <- DT::renderDataTable({
  
  if (is.null(csvInput())) {
    return()
  } else if (!is.null(csvInput())) {
      
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
            "Land Use Study Area" = land_use,
            "None" = return()
    )
  } else if (values_tif$upload_state == 'uploaded') {
    raster::raster(x = input$dataset_tif$datapath)
  } else if (values_tif$upload_state == 'reset') {
    switch (input$ex_data_tif,
            "Land Use Study Area" = land_use,
            "None" = return()
    )
  }
})
# Display data frame currently not working as it has more than 2 dimensions
# Equivalent of View(df) for shiny???
output$contents_tif <- reactive({
  
  if (is.null(tifInput())) {
    return()
  } else {
      tifInput()
  }
})



#### Tab: Create a Track ####

# EPSG Code
epsg_code <- reactive({
  epsg <- switch(EXPR = input$epsg, epsg$code)
})

# Create a track: choose columns
dat <- reactive({
  if (!is.null(tifInput()) && !is.null(csvInput())) {
    csvInput()[!is.na(csvInput()[, input$x]), ] %>% # sufficient condition??? 
      select(x = input$x, y = input$y, id = input$id, ts = input$ts)
  }
  })

# Create a track
trk <- reactive({
  if (!is.null(tifInput()) && !is.null(csvInput())) {
    # dat <- csvInput()[!is.na(csvInput()[, input$x]), ] %>% # suffiecient condition??? 
    #   select(x = input$x, y = input$y, id = input$id, ts = input$ts)
    #return(dat)
    track <- make_track(dat(), x, y, ts, id = id,
                        crs = CRS(paste("+init=epsg:", input$epsg, sep = ''))
    )
    # Ensure CRS (coordinate reference systems match)
    transform_coords(track, CRS(raster::projection(tifInput())))
  }
})

# Display data frame or summary of data
output$contents_trk <- DT::renderDataTable({
  
  if (is.null(trk())) {
    return()
  } else {
    
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
