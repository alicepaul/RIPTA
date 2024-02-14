library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(gt)
library(gtsummary)
library(plotly)
library(leaflet)
library(hms)
library(sf)
library(DT)

# Update max file upload size
options(shiny.maxRequestSize = 200*1024^2)

# External functions
source("process_data.R")
source("utils.R")

# Constants
# Tag columns
TAG_COLS <- c("Student", "Low.Income", "Off.Peak", "Eco.Pass", "Transfer",
              "One.Hour.Pass", "Two.Hour.Pass", "Day.Pass", "Ten.Ride.Pass",
              "Week.Pass", "Monthly.Pass")
# csv file types
CSV_TYPES <- c("text/csv", "text/comma-separated-values", "text/plain", ".csv")

# gtfs data
stops_gfts <- read.csv("gtfs/stops.txt")

ui <- fluidPage(
  titlePanel("RIPTA Bus Ride Analysis"),
  p("Welcome! This app is designed to visualize and summarize RIPTA ridership. 
  If you have previously processed data, you can upload it using the sidebar. 
  Otherwise, please start by going to the last tab to upload your RIPTA data 
  sources. The app will process and merge the data, which you can then download. 
  Tab 1 displays a summary table on ridership, Tab 2 displays ridership by 
  route, and Tab 3 displays a route specific summary table and plots."),
  br(),
  p("This app was created by Alice Paul and Morgan Cunningham. 
    Please email Dr. Paul (alice_paul@brown.edu) with any inquiries."),
  sidebarLayout(
    sidebarPanel(
      fileInput("mergedInput", "Processed Data Upload", accept = CSV_TYPES),
      selectInput("typeInput", "Type of Rider",
                         choices = c("All Types", "Adult", "Senior", "Disabled",
                                     "Senior/Disabled", 
                                     "Employee/Spouse/Retiree", "Child"),
                         selected = "All Types"),
      checkboxGroupInput("tagInput", "Rider Tag(s)",
                         choices = TAG_COLS),
      checkboxGroupInput("dayInput", "Day(s) of Travel", 
                         choices = c("Mon", "Tue", "Wed", "Thu", "Fri", 
                                     "Sat", "Sun")),
      checkboxGroupInput("sourceInput", "Data Source(s)",
                         choices = c("WAVE", "TTP", "KEY"))
    ),
   mainPanel(
     h4(textOutput("time_frame")),
     tabsetPanel(
       tabPanel(
         "Ridership Summary",
         br(),
         fluidRow(
           column(width = 4,
                  gt_output("summary_ridership")
           ),
           column(width = 8,
                  plotOutput("all_routes_plot_time"),
                  br(),
                  plotOutput("all_routes_plot_day")
           )
         )
       ),
       tabPanel(
         "Ridership by Route",
          br(),
          plotlyOutput("plot_ridership", height = "800px"),
          br(),
          dataTableOutput("table_ridership"),
          br()
       ),
       tabPanel(
         "Route Summary",
         br(),
         uiOutput("routeOutput"),
         br(),
         fluidRow(
           column(width = 4,
              htmlOutput("summary_route")
           ),
           column(width = 8, 
                plotOutput("route_plot_time"),
                br(),
                leafletOutput("route_plot_stops")
           )
         )
       ),
       tabPanel(
         "Process and Merge Data",
         br(),
         p("Please upload all WAVE, KEY, and TTP data sources before processing. You can also choose to upload Institution data. This will add a column with the institution name for WAVE observations associated with an institution."),
         br(),
         fluidRow(
           column(width = 3,
                fileInput("waveInput", "WAVE Data", accept = CSV_TYPES)
           ),
           column(width = 3,
                fileInput("instInput", "Institution Data", 
                          accept = CSV_TYPES),
           ),
           column(width = 3,
                  fileInput("ttpInput", "TTP Data", accept = CSV_TYPES)
           ),
           column(width = 3,
                  fileInput("keyInput", "KEY Data", accept = CSV_TYPES)
           )
         ),
         br(),
         actionButton("process", "Process Data"),
         br(),
         hr(),
         uiOutput("processedData")
       )
      )
    ),
  )
)


server <- function(input,output){
  
  # Get data through the file upload
  merged_data <- reactive({
    if (is.null(input$mergedInput)) {
      return("")
    }
    return(read.csv(input$mergedInput$datapath))
  })
  
  # Get time frame of observed data
  output$time_frame <- renderText({
    # Ensure data is loaded
    req(merged_data())
    data <- merged_data()
    
    times <- as.Date(data$Time, format = "%Y-%m-%d")
    start_date <- format(min(times), "%B %d, %Y")
    end_date <- format(max(times), "%B %d, %Y")
    
    time_frame <- paste(start_date, end_date, sep = " - ")
    return(time_frame)
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    # Require upload
    req(merged_data())
    data <- merged_data()
    
    # Make tag columns factors
    data[, TAG_COLS] <- lapply(data[, TAG_COLS], factor)
    data$Day.of.Week <- factor(data$Day.of.Week, levels = c("Mon", "Tue", "Wed", 
                                                            "Thu", "Fri", 
                                                            "Sat", "Sun"))
    
    # Date time
    data$Time <- as.POSIXct(data$Time,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "EST")
    # Drop rows with dates that failed to get reformatted
    data <- drop_na(data = data, Time)
    
    # Filter by type of rider
    if (length(input$typeInput) == 0) {
      showNotification("Please select at least one type of rider.", 
                       type = "error")
      return(NULL)
    }
    if (input$typeInput == "Senior/Disabled") {
      data <- data %>% filter(Type %in% c("Senior", "Disabled"))
    } else if (input$typeInput != "All Types"){
      data <- data %>% filter(Type == input$typeInput)
    }
    
    # Filter by rider tags
    if (length(input$tagInput) > 0) {
      tags <- input$tagInput
      data <- data %>% filter(if_any(.cols = tags, ~ . == 1))
    }
    
    # Filter by days of travel
    if (length(input$dayInput) > 0) {
      days <- input$dayInput
      data <- data[data$Day.of.Week %in% days, ]
    }
    
    # Filter by source
    if(length(input$sourceInput)>0) {
      source_data <- input$sourceInput
      data <- data[data$Source %in% source_data, ]
    }
    
    if (nrow(data) > 0) {
      data <- data[rep(seq(nrow(data)), data$Ride.Count),]
      data$Ride.Count <- 1
    }
    return(data)
  })

  # Table summarizing ridership characteristics
  output$summary_ridership <- render_gt({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    tab <- filtered %>% tbl_summary(include = c("Source", "Day.of.Week",
                                                TAG_COLS), 
                                    type = list(TAG_COLS ~ "dichotomous")) %>%
      as_gt()
    return(tab)
  })
  
  # Summary of ridership times for each route
  route_times <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    filtered %>%
      mutate(Date = as.Date(Time)) %>%
      group_by(Route.Number, Date, Trip.Number) %>%
      # Get time for a single trip in minutes
      summarize(Trip.Time = difftime(max(Time),
                                     min(Time),
                                     units = "mins"),
                .groups = "drop_last") %>%
      # Get times for each route for each day
      summarize(Route.Time.Per.Day = sum(Trip.Time),
                .groups = "drop_last") %>%
      # Get summary of route times
      summarize(Num.Days = n(),
                Total.Time = sum(Route.Time.Per.Day),
                Avg.Time.Per.Day = mean(Route.Time.Per.Day),
                SD.Time.Per.Day = sd(Route.Time.Per.Day))
  })
  
  # Summary of ridership for each route
  route_ridership <- reactive({
    filtered <- filtered_data()
    route_times <- route_times()
    req(nrow(filtered) > 0 && nrow(route_times) > 0)
    
    # Calculate total riders per route and summarize
    filtered %>%
      group_by(Route.Number) %>%
      summarize(Total.Rides = sum(Ride.Count)) %>%
      inner_join(route_times,
                 by = "Route.Number") %>%
      mutate(Route.Number = as.factor(Route.Number),
             Avg.Time.Per.Day = round(Avg.Time.Per.Day, 3),
             SD.Time.Per.Day = round(SD.Time.Per.Day, 3),
             # Avoid division by 0
             Avg.Rides.Per.Day = ifelse(Num.Days == 0,
                                        0,
                                        round(Total.Rides / Num.Days, 3)),
             Avg.Rides.Per.Minute = ifelse(Total.Time == 0,
                                           0,
                                           round(Total.Rides / 
                                                   as.numeric(Total.Time),
                                                 3))) %>%
      arrange(desc(Avg.Rides.Per.Minute)) %>%
      select(c(Route.Number, Avg.Time.Per.Day, SD.Time.Per.Day,
               Total.Rides, Avg.Rides.Per.Day, Avg.Rides.Per.Minute))
  })
  
  # Render the table based on filtered data
  output$table_ridership <- renderDataTable({
    df <- route_ridership()
    req(nrow(df) > 0)
    
    colnames(df) <- c("Route Number",
                      "Average Min. Per Day",
                      "Std Dev Min. Per Day",
                      "Total Rides",
                      "Average Rides Per Day",
                      "Average Rides Per Minute")
    return(df)
  })
  
  # Ridership by route plot
  output$plot_ridership <- renderPlotly({
    df <- route_ridership()
    req(nrow(df) > 0)
    
    # horizontal bar plot
    p <- ggplot(df, aes(x = Avg.Rides.Per.Minute,
                        y = fct_reorder(Route.Number,
                                        Avg.Rides.Per.Minute),
                        text = paste0("Route: ",
                                      Route.Number,
                                      "\n Ridership: ",
                                      Avg.Rides.Per.Minute))) +
      geom_col(fill = "skyblue") +
      labs(x = "Average Riders Per Minute",
           y = "Route",
           title = "Average Rides Per Minute Across Routes") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9))
    ggplotly(p, tooltip = "text")
  })
  
  # Get routes available by filters
  output$routeOutput <- renderUI({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    selectInput("routeInput", "Route",
                sort(unique(filtered$Route.Number)))
  })
  
  # Data filtered for specified route
  rte_filtered <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0 && !is.null(input$routeInput))
    
    rte_df <- filtered %>% filter(Route.Number == input$routeInput)
    return(rte_df)
  })
  
  # Summary table for a route
  output$summary_route <- render_gt({
    filtered <- rte_filtered()
    req(nrow(filtered) > 0)
    
    tab <- filtered %>% tbl_summary(include = c("Source", "Day.of.Week",
                                                TAG_COLS),
                                    type = list(TAG_COLS ~ "dichotomous")) %>%
      as_gt()
    return(tab)
  })
  
  # Plot route ridership by time
  # All routes
  output$all_routes_plot_time <- renderPlot({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    plot_ridership_by_time(filtered) +
      labs(title = "Ridership Over a Day Across All Routes",
           x = "Time",
           y = "Ride Count")
  })
  
  # Route specified by user
  output$route_plot_time <- renderPlot({
    filtered <- rte_filtered()
    req(nrow(filtered) > 0)
    
    plot_ridership_by_time(filtered) +
      labs(
        title = paste("Ridership Over a Day for Route", input$routeInput),
        x = "Time",
        y = "Ride Count")
  })
  
  # Plot route ridership by day of week
  output$all_routes_plot_day <- renderPlot({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    filtered <- filtered %>%
      mutate(Tap.Date = as.Date(Time)) %>%
      group_by(Day.of.Week, Tap.Date) %>%
      summarize(Num.Riders = sum(Ride.Count),
                .groups = "drop_last") %>%
      summarize(Avg.Riders = sum(Num.Riders) / n(),
                SE = sd(Num.Riders) / sqrt(n())) %>%
      ungroup()
    
    # plot
    ggplot(filtered, aes(x = Day.of.Week,
                         y = Avg.Riders,
                         fill = Day.of.Week)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Ridership Across Days of the Week",
           x = "Day of Week",
           y = "Ride Count",
           fill = "Day of Week") +
      geom_errorbar(aes(x = Day.of.Week,
                        ymin = Avg.Riders - SE,
                        ymax = Avg.Riders + SE),
                    width = 0.2) +
      theme_minimal()
  })
  
  # Plot ridership on a route by stop
  output$route_plot_stops <- renderLeaflet({
    # Find trips per stop
    df <- rte_filtered()
    req(nrow(df) > 0)
    
    num_days <- n_distinct(as.Date(df$Time))
    stop_data <- df %>%
      group_by(Stop.Number) %>%
      summarize(Avg.Rides = round(sum(Ride.Count) / num_days, 3)) %>%
      mutate(Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
      left_join(stops_gfts, by = c( "Stop.Number"="stop_id")) %>%
      filter(!is.na(Avg.Rides) & !is.na(Stop.Number)) %>%
      select(Stop.Number, stop_lat, stop_lon, Avg.Rides) %>%
      mutate(col = cut(Avg.Rides, c(0, 1, 5, 10, 25, 50, max(Avg.Rides) + 1), 
                       include.lowest = T,
                       labels = c("<1", "1-4", "5-9", "10-24", "25-49", 
                                  "50+")))

    # plot
    pal <- colorFactor(palette = 'RdPu', stop_data$col)
    leaflet(data = stop_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, 
                   popup = as.character(stop_data$Avg.Rides),
                   radius = 2, opacity = 1,
                   col = ~pal(col)) %>%
      addLegend('bottomright', pal = pal, values = stop_data$col,
                opacity = 1)
  })
  
  # Action button to process dialog
  observeEvent(input$process, {
    # No trip data
    if(is.null(input$waveInput) | is.null(input$ttpInput) | 
        is.null(input$keyInput)) {
      showModal(
        modalDialog(
          title = "Data Needed!",
          HTML("Please upload WAVE, TTP, and KEY data."
          )
        )
      )
    } else {
        showModal(
          modalDialog(
            title = "Data Processing...",
            "Your data is processing. Processing may take a few minutes. 
            This message will close automatically.",
            easyClose = FALSE))
      
    }
  })
  
  # Action button to process 
  processed_data <- eventReactive(input$process, {
    # No trip data
    if(is.null(input$waveInput) | is.null(input$ttpInput) | 
       is.null(input$keyInput)) {
      full_df <- NULL
    } else {
      full_df <- prep_full(input$waveInput$datapath, 
                           input$instInput$datapath, 
                           input$ttpInput$datapath, 
                           input$keyInput$datapath)
    }
    return(full_df)
  })
  
  # UI for processed data
  output$processedData <- renderUI({
    req(nrow(processed_data())>1)
    removeModal()
    output = tagList()
    output[[1]] <- renderDataTable({processed_data()})
    output[[2]] <- downloadButton("downloadData", "Download Processed Data")
    return(output)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ridership-merged-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)