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
library(tidycensus)
library(tigris)

# Update max file upload size
options(shiny.maxRequestSize = 200*1024^2)

# External functions
source("process_data.R")
source("utils.R")

# API key
# census_api_key("Enter key here", overwite = TRUE, install = TRUE)

# Constants
# Tag columns
STUDENT_TAGS <- c("College", "High.School")
BINARY_TAGS <- c("Low.Income", "Off.Peak", "Eco.Pass", "Transfer",
                 "One.Hour.Pass", "Two.Hour.Pass", "Day.Pass",
                 "Ten.Ride.Pass", "Week.Pass", "Monthly.Pass")
ALL_TAGS <- c(STUDENT_TAGS, BINARY_TAGS, "Institution")

# csv file types
CSV_TYPES <- c("text/csv", "text/comma-separated-values", "text/plain", ".csv")
# Keys for decryption
PUBLIC_KEY <- "68 ab 9c 12 03 aa 76 ef 9a 54 0b a7 5f dc 2a cb 3c 45 3c 44 d2 04 0a 80 cd 88 84 05 5e 9a cb 20"
# Data
stops_gfts <- read.csv("gtfs/stops.txt")
census_df <-readRDS("./data/census_data.rds")
RI_ids <- tracts("RI", year=2022, class="sf") %>% select(GEOID)

ui <- fluidPage(
  titlePanel("RIPTA Bus Ride Analysis"),
  p("Welcome! This app is designed to visualize and summarize RIPTA ridership. 
  You can load existing data hosted on the server with valid credentials or 
  upload previously processed data by clicking the checkbox in the sidebar. 
  Otherwise, please start by going to the last tab to upload your RIPTA data 
  sources. The app will process and merge the data, which you can then download. 
  Tab 1 summarizes overall ridership, Tab 2 displays ridership by route, and 
  Tab 3 displays a route specific summary table and plots."),
  br(),
  p("This app was created by John Chung, Morgan Cunningham, and Alice Paul. 
    Please email Dr. Paul (alice_paul@brown.edu) with any inquiries."),
  sidebarLayout(
    sidebarPanel(
      passwordInput("password", "Enter Password to Load Data"),
      actionButton("passwordButton", "Submit"),
      checkboxInput("uploadOption", "Upload Processed Data"),
      uiOutput("uploadDisplay"),
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
         fluidRow(
           column(width = 2,
                  radioButtons("ride_metric",
                               "Ridership Metric",
                               c("Per Minute" = "minute",
                                 "Per Mile" = "mile"))),
           column(width = 10,
                  plotlyOutput("plot_ridership", height = "800px"))),
         br(),
         dataTableOutput("table_ridership")
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
                fluidRow(
                  column(width = 2,
                         radioButtons("census_metric",
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
                         h4(textOutput("census_avg")),
                         leafletOutput("route_plot_stops")))
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
  # Display file upload UI when user
  # enables option to upload data
  output$uploadDisplay <- renderUI({
    if (input$uploadOption) {
      fileInput("mergedInput", "", accept = CSV_TYPES)
    }
  })
  
  # Load data
  merged_data <- reactiveVal()
  # Get password-protected data hosted on server
  observeEvent(input$passwordButton, {
   private_key <- sha256(charToRaw(input$password))
   # Validate string representation of private key
   if (paste(pubkey(private_key), collapse = " ") == PUBLIC_KEY) {
     encrypted_data <- readRDS("./data/encrypted_data.rds")
     decrypted_data <- unserialize(simple_decrypt(encrypted_data,
                                                  private_key))
     merged_data(decrypted_data)
   } else {
     showNotification("Incorrect password", type = "error")
   }
  })
  
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
  active_binary_tags <- reactive({
    active_tags <- BINARY_TAGS
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
  
  # Filter tags are active tags that the user can filter in the UI
  filter_tags <- reactive({c(STUDENT_TAGS, active_binary_tags())})
  # Active tags are all active tags available in the displayed
  # tables
  active_tags <- reactive({c("Institution", filter_tags())})
  
  output$tagFilter<- renderUI({
    checkboxGroupInput("tagInput", "Rider Tag(s)",
                       choices = filter_tags())
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
      data <- data %>% filter(if_any(.cols=tags,
                                     # Second condition is for student tags
                                     ~ . == 1 | (. != 0 & . != "None")))
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
    
    tags <- c("Source", "Day.of.Week", "Institution", active_binary_tags())
    tab <- filtered %>%
      tbl_summary(include = tags,
                  type = list(active_binary_tags() ~ "dichotomous")) %>%
      as_gt()
    return(tab)
  })
  
  # Times per day for each route
  times_per_day <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    filtered %>%
      filter(!(Route.Number %in% c(89, 999, 9999))) %>%
      mutate(Date = as.Date(Time), Numeric.Time = as.numeric(Time)) %>%
      group_by(Route.Number, Date, Trip.Number) %>%
      # Get time for a single trip in minutes
      summarize(Trip.Time = (max(Numeric.Time)-min(Numeric.Time))/60,
                .groups = "drop_last") %>%
      # Get times for each route for each day
      summarize(Time.Per.Day = sum(Trip.Time), .groups = "drop_last")
  })
  
  # Miles per day for each route
  miles_per_day <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    filtered %>%
      # Remove trip 99
      filter(Trip.Number != 99) %>%
      mutate(Date = as.Date(Time),
             Numeric.Time = as.numeric(Time),
             Time = as.character(Time),
             Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
      inner_join(stops_gfts, by = c("Stop.Number" = "stop_id")) %>%
      group_by(Route.Number, Date, Trip.Number, Stop.Number) %>%
      # Only keep first row since we care about the
      # coordinates of each stop rather than the
      # ride counts
      filter(row_number() == 1) %>%
      ungroup(Stop.Number) %>%
      mutate(stop_lat_lag = lag(stop_lat, order_by = Numeric.Time),
             stop_lon_lag = lag(stop_lon, order_by = Numeric.Time),
             miles = calculate_miles(stop_lon,
                                     stop_lat,
                                     stop_lon_lag,
                                     stop_lat_lag)) %>%
      summarize(Miles.Per.Trip = sum(miles),
                .groups="drop_last") %>%
      # Total miles per day
      summarize(Miles.Per.Day = sum(Miles.Per.Trip),
                .groups = "drop_last")
  })
  
  # Summary of ridership for each route
  route_ridership <- reactive({
    filtered <- filtered_data()
    times_df <- times_per_day()
    miles_df <- miles_per_day()
    req(nrow(filtered) > 0 &&
          nrow(times_df) > 0 &&
          nrow(miles_df))
    
    filtered %>%
      mutate(Date = as.Date(Time)) %>%
      group_by(Route.Number, Date) %>%
      summarize(Rides.Per.Day = sum(Ride.Count), 
                .groups = "drop_last") %>%
      # Merge
      inner_join(times_df, by = c("Route.Number", "Date")) %>%
      inner_join(miles_df, by = c("Route.Number", "Date")) %>%
      # Drop rows (days) with 0 minutes or miles
      filter(Time.Per.Day > 0,
             Miles.Per.Day > 0) %>%
      # Compute daily ridership stats
      mutate(Route.Number = as.factor(Route.Number),
             Rides.Per.Minute = Rides.Per.Day / Time.Per.Day,
             Rides.Per.Mile = Rides.Per.Day / Miles.Per.Day) %>%
      # Compute averages
      summarize(Avg.Time.Per.Day = round(mean(Time.Per.Day), 3),
                SD.Time.Per.Day = round(sd(Time.Per.Day), 3),
                Num.Days = n(),
                Total.Rides = sum(Rides.Per.Day),
                Avg.Rides.Per.Minute = round(mean(Rides.Per.Minute), 3),
                Avg.Rides.Per.Mile = round(mean(Rides.Per.Mile), 3)) %>%
      mutate(Avg.Rides.Per.Day = round(Total.Rides / Num.Days, 3)) %>%
      select(-c(Num.Days))
  })
  
  # Render the table based on filtered data
  output$table_ridership <- renderDataTable({
    df <- route_ridership()
    req(nrow(df) > 0)
    
    colnames(df) <- c("Route Number",
                      "Average Min. Per Day",
                      "Std Dev Min. Per Day",
                      "Total Rides",
                      "Average Rides Per Minute",
                      "Average Rides Per Mile",
                      "Average Rides Per Day")
    return(df)
  })
  
  # Ridership by route plot
  output$plot_ridership <- renderPlotly({
    df <- route_ridership()
    req(nrow(df) > 0)
    
    col_name <- switch(input$ride_metric,
                       minute = "Avg.Rides.Per.Minute",
                       mile = "Avg.Rides.Per.Mile")
    
    # Horizontal bar plot
    plot <- ggplot(df,
                   aes(x = .data[[col_name]],
                       y = fct_reorder(Route.Number,
                                       .data[[col_name]]),
                       text = paste0("Route: ",
                                     Route.Number,
                                     "\n Ridership: ",
                                     .data[[col_name]]))) +
      labs(x = paste("Average Riders Per", input$ride_metric),
           y = "Route",
           title = paste("Average Rides Per",
                         input$ride_metric,
                         "Across Routes")) +
      geom_col(fill = "skyblue") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9))
    ggplotly(plot, tooltip = "text")
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
    
    tags <- c("Source", "Day.of.Week", "Institution", active_binary_tags())
    tab <- filtered %>%
      tbl_summary(include = tags,
                  type = list(active_binary_tags() ~ "dichotomous")) %>%
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
  
  # Ridership on a route by stop
  route_stops <- reactive({
    # Find trips per stop
    df <- rte_filtered()
    req(nrow(df) > 0)
    
    num_days <- n_distinct(as.Date(df$Time))
    stops_df <- df %>%
      group_by(Stop.Number) %>%
      summarize(Avg.Rides = round(sum(Ride.Count) / num_days, 3)) %>%
      mutate(Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
      inner_join(stops_gfts, by = c("Stop.Number" = "stop_id")) %>%
      select(Stop.Number, stop_lat, stop_lon, Avg.Rides) %>%
      # Map coordinates to census tract
      st_as_sf(coords = c("stop_lon", "stop_lat"),
               remove = FALSE,
               crs = "+proj=longlat +datum=WGS84")
    return(stops_df)
  })
  
  stops_with_census <- reactive({
    stops_df <- route_stops()
    req(nrow(stops_df) > 0)

    # Filter census data by metric
    # selected by user
    metric <- switch(input$census_metric,
                     population = "B01003_001",
                     income = "B19013_001",
                     poverty = "B05010_002",
                     employed = "B23025_001",
                     car_commute = "B08301_002",
                     carpool = "B08301_004",
                     public_transp = "B08301_010",
                     no_vehicle = "B08141_002")
    filtered_census_df <- census_df[census_df$variable == metric, ] %>%
      drop_na() %>%
      # Create copy of geometry column because
      # it gets overwritten when merging
      mutate(poly_coords = geometry)

    # Merge
    merged_df <- filtered_census_df %>%
      st_join(stops_df)
    return(merged_df)
  })
  
  output$route_plot_stops <- renderLeaflet({
    all_tracts_df <- stops_with_census()
    req(nrow(all_tracts_df) > 0)
    
    stops_df <- all_tracts_df %>% drop_na()
    title <- switch(input$census_metric,
                    population = "Population Count",
                    income = "Median Household Income",
                    poverty = "Num. Below Poverty Line",
                    employed = "Employment Count",
                    car_commute = "Num. Car Commuters",
                    carpool = "Num. Carpoolers",
                    public_transp = "Num. Take Public Transp.",
                    no_vehicle = "Num. Without Vehicle")
    
    # Plot
    # Create breaks via quantiles
    # Remove duplicates because quantiles
    # may be same with limited data
    breaks <- unique(quantile(stops_df$Avg.Rides,
                              probs = seq(0, 1, 0.2)))
    stops_pal <- colorBin(palette = 'RdPu',
                          stops_df$Avg.Rides,
                          breaks)
    census_pal <- colorBin(palette = "viridis",
                           all_tracts_df$estimate,
                           5)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Stop coordinates
      addCircleMarkers(data = stops_df,
                       lng = ~stop_lon,
                       lat = ~stop_lat,
                       popup = as.character(stops_df$Avg.Rides),
                       radius = 2,
                       opacity = 1,
                       col = ~stops_pal(Avg.Rides)) %>%
      addPolygons(data = all_tracts_df,
                  color = ~census_pal(estimate),
                  weight = 0.5,
                  fillOpacity = 0.1) %>%
      addLegend(position = 'topright',
                pal = stops_pal,
                values = stops_df$Avg.Rides,
                opacity = 1,
                title = "Ride Count") %>%
      addLegend(position = "bottomright",
                pal = census_pal,
                values = all_tracts_df$estimate,
                opacity = 1,
                title = title)
  })
  
  # Weighted average
  output$census_avg <- renderText({
    stops_df <- stops_with_census() %>% drop_na()
    req(nrow(stops_df) > 0)
    
    weighted_avg <- sum(stops_df$Avg.Rides * stops_df$estimate) / sum(stops_df$Avg.Rides)
    weighted_avg <- round(weighted_avg, 2)
    return(paste("Average weighted by ridership:", weighted_avg))
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