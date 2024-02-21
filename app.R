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
library(sodium)

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
# Keys for decryption
PUBLIC_KEY <- "c0 25 1a b3 6c 3d 79 a5 41 8e 67 07 41 c4 66 b7 eb 53 8c ec fd df b5 81 60 37 3f 8b 7e 8a f1 0d"

# gtfs data
# stops_gfts <- read.csv("gtfs/stops.txt")

ui <- fluidPage(
  titlePanel("RIPTA Bus Ride Analysis"),
  p("Welcome! This app is designed to visualize and summarize RIPTA ridership. 
  If you have previously processed data, you can upload it using the sidebar. 
  You can also load existing data hosted on the server with valid credentials. 
  Otherwise, please start by going to the last tab to upload your RIPTA data 
  sources. The app will process and merge the data, which you can then download. 
  Tab 1 displays a summary table on ridership, Tab 2 displays ridership by 
  route, and Tab 3 displays a route specific summary table and plots."),
  br(),
  p("This app was created by John Chung, Morgan Cunningham, and Alice Paul. 
    Please email Dr. Paul (alice_paul@brown.edu) with any inquiries."),
  sidebarLayout(
    sidebarPanel(
      fileInput("mergedInput", "Processed Data Upload", accept = CSV_TYPES),
      #passwordInput("password", "Enter Password"),
      #actionButton("passwordButton", "Submit"),
      selectInput("typeInput", "Type of Rider",
                         choices = c("All Types", "Adult", "Senior", "Disabled",
                                     "Senior/Disabled", 
                                     "Employee/Spouse/Retiree", "Child"),
                         selected = "All Types"),
      uiOutput("tagFilter"),
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
         p("This tab summarizes ridership across all routes. The table on the 
           left shows the total number of rides for each characteristic, 
           and the plots display average ridership trends. Specifically, 
           the first graph shows the average number of rides across all routes 
           throughout a day, whereas the second graph charts this relationship 
           across days of the week."),
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
         p("This tab compares ridership across routes. The graph displays the 
           average number of rides per minute for each route and sorts them in 
           descending order. In addition to the average number of rides per 
           minute, the table below the graph shows other ridership 
           information that can be compared."),
          plotlyOutput("plot_ridership", height = "800px"),
          br(),
          dataTableOutput("table_ridership"),
          br()
       ),
       tabPanel(
         "Route Summary",
         br(),
         p("This tab summarizes ridership for a specific route. The table on 
           the left shows the total number of rides for each characteristic 
           where the data has been filtered for the provided route. Like the 
           Ridership Summary tab, the first graph shows the average number of 
           rides throughout a day. However, the second graph shows the average 
           number of rides at each stop along this route."),
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


server <- function(input, output) {
  
  # Load data
  merged_data <- reactiveVal()
  # Get password-protected data hosted on server
  #observeEvent(input$passwordButton, {
  #  private_key <- sha256(charToRaw(input$password))
  #  # Validate string representation of private key
  #  if (paste(pubkey(private_key), collapse = " ") == PUBLIC_KEY) {
  #    encrypted_data <- readRDS("./data/EncryptedMergedData.rds")
  #    decrypted_data <- unserialize(simple_decrypt(encrypted_data, 
  #                                                 private_key))
  #    merged_data(decrypted_data)
  #  } else {
  #    showNotification("Incorrect password", type = "error")
  #  }
  #})
  
  # Get data through the file upload
  observe({
    if (is.null(input$mergedInput)) {
      merged_data(NULL)
    } else {
      merged_data(read.csv(input$mergedInput$datapath))
    }
  })
  
  # Get time frame of observed data
  all_dates <- reactive({
    if (is.null(merged_data())) {
      return(NULL)
    }
    data <- merged_data()
    return(as.Date(data$Time, format = "%Y-%m-%d"))
  })
  
  output$time_frame <- renderText({
    if (is.null(all_dates())) {
      return("")
    }
    all_dates <- all_dates()
    start_date <- format(min(all_dates), "%B %d, %Y")
    end_date <- format(max(all_dates), "%B %d, %Y")
    time_frame <- paste(start_date, end_date, sep = " - ")
    return(time_frame)
  })
  
  # Get active tags
  active_tags <- reactive({
    active_tags <- TAG_COLS
    if (is.null(all_dates())) {
      return(active_tags)
    }
    inactive_date <- as.Date("2022-1-15", format = "%Y-%m-%d")
    inactive_tags <- c("Two.Hour.Pass", "Ten.Ride.Pass", "Week.Pass")
    if (inactive_date < min(all_dates())) {
      active_tags <- active_tags[!(active_tags %in% inactive_tags)]
    }
    return(active_tags)
  })
  
  output$tagFilter<- renderUI({
    checkboxGroupInput("tagInput", "Rider Tag(s)",
                       choices = active_tags())
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    # Require upload
    req(merged_data())
    data <- merged_data()
    
    # Make tag columns factors
    data[, active_tags()] <- lapply(data[, active_tags()], factor)
    data$Day.of.Week <- factor(data$Day.of.Week, levels = c("Mon", "Tue", "Wed", 
                                                            "Thu", "Fri", 
                                                            "Sat", "Sun"))
    
    # Date time
    midnight_trips <- nchar(data$Time) < 19
    data$Time[midnight_trips] <- paste(data$Time[midnight_trips], "00:00:00")
    data$Time <- as.POSIXct(data$Time,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "EST")
    
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
                                                active_tags()), 
                                    type = list(active_tags() ~ "dichotomous")
                                    ) %>%
      as_gt()
    return(tab)
  })
  
  # Summary of ridership times for each route
  route_times <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    filtered %>%
      filter(!(Route.Number %in% c(89, 999, 9999))) %>%
      mutate(Date = as.Date(Time), Numeric.Time = as.numeric(Time)) %>%
      group_by(Route.Number, Date, Trip.Number) %>%
      # Get time for a single trip in minutes
      summarize(Trip.Time = (max(Numeric.Time)-min(Numeric.Time))/60,
                .groups="drop_last") %>%
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
                                                active_tags()),
                                    type = list(active_tags() ~ "dichotomous")
                                    ) %>%
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
      left_join(stops_gfts, by = c("Stop.Number" = "stop_id")) %>%
      filter(!is.na(Avg.Rides) & !is.na(Stop.Number)) %>%
      select(Stop.Number, stop_lat, stop_lon, Avg.Rides)
    
    # Ensure stop data exists
    req(nrow(stop_data) > 0)
    
    # Create breaks via quantiles
    # Remove duplicates because quantiles may be
    # same with limited data
    breaks <- unique(quantile(stop_data$Avg.Rides, 
                              probs = seq(0, 1, 0.2)))
    
    stop_data <- mutate(stop_data, 
                        Rides.Group = cut(Avg.Rides, 
                                          breaks, 
                                          include.lowest = T))
    
    # plot
    pal <- colorFactor(palette = 'RdPu', stop_data$Rides.Group)
    leaflet(data = stop_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, 
                   popup = as.character(stop_data$Avg.Rides),
                   radius = 2, opacity = 1,
                   col = ~pal(Rides.Group)) %>%
      addLegend('bottomright', pal = pal, values = stop_data$Rides.Group,
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