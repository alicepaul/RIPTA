library(DT)
library(geosphere)
library(gt)
library(gtsummary)
library(hms)
library(leaflet)
library(lubridate)
library(plotly)
library(sf)
library(shiny)
library(shinythemes)
library(sodium)
library(stringr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(geosphere)
library(parallel)
library(shinycssloaders)
library(stringr)
library(janitor)


# Update max file upload size
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

# External functions
source("common.R")
source("ridership_summary.R")
source("ridership_by_route.R")
source("route_summary.R")
source("process_data.R")

# csv file types
CSV_TYPES <- c("text/csv",
               "text/comma-separated-values",
               "text/plain",
               ".csv")

# UI
ui <- fluidPage(
  titlePanel("RIPTA Bus Ride Analysis"),
  p("Welcome! This app is designed to visualize and summarize RIPTA ridership.
    You can load existing data hosted on the server with valid credentials or
    upload previously processed data by clicking the checkbox in the sidebar.
    Otherwise, please start by going to the last tab to upload your RIPTA data
    sources. The app will process and merge the data, which you can then
    download. Tab 1 summarizes overall ridership, Tab 2 compares ridership
    across routes, and Tab 3 analyzes ridership for a specific route."),
  br(),
  p("This app was created by John Chung, Morgan Cunningham, and Alice Paul.
    Please email Dr. Paul (alice_paul@brown.edu) with any inquiries."),
  sidebarLayout(
    sidebarPanel(
      passwordInput("password", "Enter Password to Load Data"),
      actionButton("passwordButton", "Submit"),
      checkboxInput("uploadOption", "Upload Processed Data"),
      uiOutput("fileUploadUI"),
      selectInput("riderType",
                  "Type of Rider",
                  choices = c("All Types",
                              "Adult",
                              "Senior",
                              "Disabled",
                              "Senior/Disabled",
                              "Employee/Spouse/Retiree",
                              "Child"),
                  selected = "All Types"),
      uiOutput("tagFilter"),
      checkboxGroupInput("dayFilter",
                         "Day(s) of Travel",
                         choices = c("Mon", "Tue", "Wed", "Thu", "Fri",
                                     "Sat", "Sun")),
      checkboxGroupInput("sourceInput",
                         "Data Source(s)",
                         choices = c("WAVE", "TTP", "KEY"))),
    mainPanel(
      h4(textOutput("timeFrame")),
      tabsetPanel(
        tabPanel(
          "Ridership Summary",
          br(),
          p("This tab summarizes ridership across all routes. The table on
            the left shows the total number of rides for each characteristic,
            and the plots display average ridership trends. Specifically, the
            first graph shows the average number of rides across all routes
            throughout a day, whereas the second graph charts this relationship
            across days of the week."),
          fluidRow(column(width = 4,
                          gt_output("allSummary")),
                   column(width = 8,
                          plotOutput("allTimePlot"),
                          br(),
                          plotOutput("allDaysPlot")))),
        tabPanel(
          "Ridership by Route",
          br(),
          p("This tab compares ridership across routes. The graph displays the
            average number of rides per minute or per mile for each route and 
            sorts them in descending order. This information is captured in
            the table immediately below the graph, along with other statistics.
            Additionally, the last table shows weighted averages of estimates
            of various census demographics. These weights are determined by
            the number of riders at each stop for a given route."),
          fluidRow(column(width = 2,
                          radioButtons("rideMetric",
                                       "Ridership Metric",
                                       c("Per Minute" = "Minute",
                                         "Per Mile" = "Mile"))),
                   column(width = 10,
                          plotlyOutput("averageRidesPlot",
                                       height = "800px"))),
          br(),
          dataTableOutput("averageRides"),
          br(),
          p("This table displays census and transportation-related 
          statistics for each bus route, summarizing key metrics such as 
          population, household income, and transportation mode usage. 
          Each row corresponds to a specific route, providing a snapshot 
          of the community characteristics served by that route."),
          dataTableOutput("standardizedRides")),
        tabPanel(
          "Route Summary",
          br(),
          p("This tab summarizes ridership for a route selected by the user.
            The table on the left shows the total number of rides for each
            characteristic where the data has been filtered for the chosen
            route. Like the 'Ridership Summary' tab, the first graph shows the
            average number of rides throughout a day. However, the second graph
            plots the average number of rides at each stop along this route on
            a map of Rhode Island. The map is also overlayed with tract-level
            census data for a metric specified by the user."),
          uiOutput("routeFilter"),
          br(),
          fluidRow(
            column(width = 4, htmlOutput("routeSummary")),
            column(width = 8,
                   plotOutput("routeTimePlot"),
                   br(),
                   fluidRow(
                     column(width = 2,
                            radioButtons(
                              "censusMetric",
                              "Census Metric",
                              c("Population" = "population",
                                "Household Income" = "income",
                                "Below Poverty Line" = "poverty",
                                "Employed" = "employed",
                                "Commute via Car" = "car_commute",
                                "Commute via Carpool" = "carpool",
                                "Commute via Public Transp." = "public_transp",
                                "No Access to Vehicle" = "no_vehicle"))),
                     column(width = 10,
                            leafletOutput("ridesPerStopPlot")))))),
        tabPanel(
          "Process and Merge Data",
          br(),
          p("Please upload all WAVE, KEY, and TTP data sources before
            processing. You can also choose to upload Institution data.
            This will add a column with the institution name for WAVE
            observations associated with an institution."),
          br(),
          fluidRow(column(width = 3,
                          fileInput("waveData",
                                    "WAVE Data",
                                    accept = CSV_TYPES)),
                   column(width = 3,
                          fileInput("institutionData",
                                    "Institution Data",
                                    accept = CSV_TYPES)),
                   column(width = 3,
                          fileInput("ttpData",
                                    "TTP Data",
                                    accept = CSV_TYPES)),
                   column(width = 3,
                          fileInput("keyData",
                                    "KEY Data",
                                    accept = CSV_TYPES))),
          br(),
          actionButton("processButton", "Process Data"),
          br(),
          hr(),
          uiOutput("processedData")))
      )
  )
)

server <- function(input, output) {
  # Display the file upload UI when the user enables
  # the option to upload their own data
  output$fileUploadUI <- renderUI({
    if (input$uploadOption) {
      fileInput("userUploadedData", "", accept = CSV_TYPES)
    }
  })
  
  # --- This block loads the data
  # The data is either the password-protected
  # data hosted on the server or the data
  # uploaded by the user
  loaded_data <- reactiveVal()
  
  # Get password-protected data hosted on the server
  observeEvent(input$passwordButton, {
    private_key <- sha256(charToRaw(input$password))
    if (is_valid_password(private_key)) {
      loaded_data(decrypt_data(private_key))
    } else {
      showNotification("Incorrect password", type = "error")
    }
  })
  
  # Get data through the file upload
  observe({
    if (is.null(input$userUploadedData)) {
      loaded_data(NULL)
    } else {
      loaded_data(read.csv(input$userUploadedData$datapath))
    }
  })
  # ---
  
  # Get time frame of the loaded data
  all_dates <- reactive(get_all_dates(loaded_data()))
  output$timeFrame <- renderText(get_time_frame(all_dates()))
  
  # Get active tags
  active_tags <- reactive(get_active_tags(all_dates()))
  
  # Filter tags are active tags that the user can filter in the UI
  filter_tags <-
    reactive(c("College", "High.School", active_tags()))
  
  # Display active filter tags
  output$tagFilter <- renderUI({
    checkboxGroupInput("selectedTags",
                       "Rider Tag(s)",
                       choices = filter_tags())
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive(get_filtered_data(loaded_data(),
                                              active_tags(),
                                              input))
  
  # --- This block pertains to the "Ridership Summary" tab
  # Table summarizing ridership characteristics
  output$allSummary <- render_gt({
    req(filtered_data(), active_tags(), nrow(filtered_data()) > 0)
    get_ridership_summary_table(filtered_data(), active_tags())
  })
  
  # Plot ridership over a day
  output$allTimePlot <- renderPlot({
    plot_ridership_by_time(filtered_data(),
                           "Ridership Over a Day Across All Routes")
  })
  
  # Plot ridership across days of the week
  output$allDaysPlot <- renderPlot({
    plot_ridership_across_days(filtered_data())
  })
  # ---
  
  # --- This block pertains to the "Ridership by Route" tab
  # Average ridership across routes
  avg_rides_data <- reactiveVal()
  observe({
    avg_rides_data(get_average_ridership(filtered_data()))
  })
  output$averageRidesPlot <- renderPlotly({
    plot_average_ridership(avg_rides_data(), input$rideMetric)
  })
  
  output$averageRides <- renderDataTable({
    display_average_ridership(avg_rides_data())
  })

  # Standardized ridership with census demographics
  output$standardizedRides <- renderDataTable({
    get_standardized_rides(filtered_data())
  })
  # ---
  
  # --- This block pertains to the "Route Summary" tab
  # Display route filter
  output$routeFilter <- renderUI({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    selectInput("routeNumber",
                "Route",
                sort(unique(filtered$Route.Number)))
  })
  
  # Filter data for a specific route
  filtered_route_data <- reactive(get_data_for_route(filtered_data(),
                                                     input$routeNumber))
  
  # Summary table
  output$routeSummary <- render_gt({
    get_ridership_summary_table(filtered_route_data(),
                                active_tags())
  })
  
  # Plot ridership over a day
  title = reactive(paste("Ridership Over a Day for Route", input$routeNumber))
  output$routeTimePlot <- renderPlot({
    plot_ridership_by_time(filtered_route_data(), title())})
  
  # Plot ridership per stop
  rides_per_stop_data <- reactive({
    get_rides_per_stop(filtered_route_data(), input$censusMetric)
  })
  
  output$ridesPerStopPlot <- renderLeaflet({
    plot_rides_per_stop(rides_per_stop_data(), input$censusMetric)
  })
  # ---

  # --- This block pertains to the "Process
  # --- and Merge Data" tab
  # Action button to process dialog
  observeEvent(input$processButton, {
    # No trip data
    if (is.null(input$waveData) |
        is.null(input$ttpData) |
        is.null(input$keyData)) {
      showModal(modalDialog(title = "Data Needed!",
                            HTML("Please upload WAVE, TTP, and KEY data.")))
    } else {
      showModal(modalDialog(title = "Data Processing...",
                            "Your data is processing. Processing may
                            take a few minutes. This message will close
                            automatically.",
                            easyClose = FALSE))
    }
  })
  
  # Process data
  processed_data <- eventReactive(input$processButton, {
    # No trip data
    if (is.null(input$waveData) |
        is.null(input$ttpData) |
        is.null(input$keyData)) {
      return(NULL)
    }
    return(prep_full(input$waveData$datapath,
                     input$institutionData$datapath,
                     input$ttpData$datapath,
                     input$keyData$datapath))
  })
  
  # Display processed data
  output$processedData <- renderUI({
    req(nrow(processed_data()) > 1)
    removeModal()
    output <- tagList()
    output[[1]] <- renderDataTable({processed_data()})
    output[[2]] <- downloadButton("downloadData",
                                  "Download Processed Data")
    return(output)
  })
  
  # UI to download data
  output$downloadData <- downloadHandler(
    filename = function() {paste("ridership-merged-data-",
                                 Sys.Date(),
                                 ".csv",
                                 sep = "")},
    content = function(file) {write.csv(processed_data(),
                                        file,
                                        row.names = FALSE)})
  # ---
}

shinyApp(ui = ui, server = server)
