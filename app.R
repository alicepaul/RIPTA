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

# update max file upload size
options(shiny.maxRequestSize=200*1024^2)

# source in preprocessing for data
source("process_data.R")

# tag columns
tag_cols <- c("Student", "Low.Income", "Off.Peak", "Eco.Pass", "Transfer", 
  "One.Hour.Pass", "Two.Hour.Pass", "Day.Pass", "Ten.Ride.Pass", "Week.Pass", 
  "Monthly.Pass")

# csv file types
csv_types <- c("text/csv", "text/comma-separated-values", "text/plain", ".csv")

# gtfs data
stops_gfts <- read.csv("gtfs/stops.txt")
route_time <- read.csv("gtfs/route_time.csv")

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
      fileInput("mergedInput", "Processed Data Upload", accept = csv_types),
      selectInput("typeInput", "Type of Rider",
                         choices = c("All Types", "Adult", "Senior", "Disabled",
                                     "Senior/Disabled", 
                                     "Employee/Spouse/Retiree", "Child"),
                         selected = "All Types"),
      checkboxGroupInput("tagInput", "Rider Tag(s)",
                         choices = tag_cols),
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
         gt_output("summary_ridership"),
       ), 
       tabPanel(
         "Ridership by Route",
          br(),
          plotlyOutput("plot_ridership"),
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
                fileInput("waveInput", "WAVE Data", accept = csv_types)
           ),
           column(width = 3,
                fileInput("instInput", "Institution Data", 
                          accept = csv_types),
           ),
           column(width = 3,
                  fileInput("ttpInput", "TTP Data", accept = csv_types)
           ),
           column(width = 3,
                  fileInput("keyInput", "KEY Data", accept = csv_types)
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
    
    times <- as.Date(merged_data()$Time, format = "%Y-%m-%d")
    start_date <- format(min(times), "%B %Y")
    end_date <- format(max(times), "%B %Y")
    
    time_frame <- start_date
    # Create range if dataset contains
    # data from multiple months
    if (start_date != end_date) {
      time_frame <- paste(start_date, end_date, sep = " - ")
    }
    return(time_frame)
  })

  # Reactive function to filter data based on user inputs
  filtered_data <- reactive({
    
    # Require upload
    req(merged_data())
    data <- merged_data()
    
    # Make tag columns factors
    data[,tag_cols] <- lapply(data[,tag_cols], factor)
    data$Day.of.Week <- factor(data$Day.of.Week, levels = c("Mon", "Tue", "Wed", 
                                                            "Thu", "Fri", 
                                                            "Sat", "Sun"))
    
    # Date time
    data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
    
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
      data <- data %>% filter(if_any(.cols=tags, ~ . == 1))
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
    
    # Repeat rows based on ride count and then drop
    data <- data[rep(seq(nrow(data)), data$Ride.Count),]
    data$Ride.Count <- 1
    
    return(data)
  })

  # Table summarizing ridership characteristics
  output$summary_ridership <- render_gt({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    tab <- filtered %>% tbl_summary(include=c("Source", "Day.of.Week", 
                                              tag_cols)) %>% as_gt()
    return(tab)
  })
  
  # Summarized data by route
  sum_data <- reactive({
    filtered <- filtered_data()
    req(nrow(filtered) > 0)
    
    # Calculate total riders per route and summarize
    num_days <- n_distinct(as.Date(filtered$Time))
    summary_table <- filtered %>%
      group_by(Route.Number) %>%
      summarise(Total.Rides = sum(Ride.Count),
                Avg.Riders.Per.Day = round(Total.Rides/num_days,3)) %>%
      inner_join(route_time, by = c("Route.Number" = "route_id")) %>%
      mutate(Avg.Minutes.Per.Day = round(total_time, 3),
             Avg.Riders.Per.Minute = round(Total.Rides / 
                                             (total_time*num_days),3),
             Route.Number = as.factor(Route.Number)) %>%
      arrange(desc(Avg.Riders.Per.Minute))
    
    # update columns
    summary_table <- summary_table %>%
      select(c(Route.Number, Avg.Minutes.Per.Day, Total.Rides, 
               Avg.Riders.Per.Day, Avg.Riders.Per.Minute))
    return(summary_table)
  })
  
  # Render the table based on filtered data
  output$table_ridership <- renderDataTable({
    df <- sum_data()
    req(nrow(df) > 0)
    colnames(df) <- c("Route Number", "Average Min Per Day", "Total Rides", 
                                 "Average Rides Per Day",
                                 "Average Rides Per Minute")
    return(df)
  })
  
  # Ridership by route plot
  output$plot_ridership <- renderPlotly({
    df <- sum_data()
    req(nrow(df) > 0)
    # horizontal bar plot
    p <- ggplot(df) +
      geom_col(aes(y=fct_reorder(Route.Number, Avg.Riders.Per.Minute), 
                   x = Avg.Riders.Per.Minute, 
                   text = paste0(" Route: ", Route.Number, " \n Ridership: ",
                                Avg.Riders.Per.Minute)), 
               fill = "skyblue") +
      labs(x = "Average Riders Per Minute",
           y = "Route", title = "Average Rides Per Minute Across Routes") +
      theme_minimal()+
      theme(axis.text.y = element_text(size=3)) 
    ggplotly(p, tooltip="text")
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
    req(input$routeInput)
    rte_df <- filtered %>% filter(Route.Number == input$routeInput)
    return(rte_df)
  })
  
  # Summary table for a route
  output$summary_route <- render_gt({
    filtered <- rte_filtered()
    req(nrow(filtered) > 0)
    tab <- filtered %>% tbl_summary(include=c("Source", "Day.of.Week", 
                                              tag_cols)) %>% as_gt()
    return(tab)
  })
  
  # Plot route ridership by time
  output$route_plot_time <- renderPlot({
    
    # group by time interval
    filtered <- rte_filtered()
    req(nrow(filtered) > 0)
    ripta_day <- filtered %>%
      mutate(Tap.Date = as.Date(Time),
             Tap.DateTime.Rnd = floor_date(Time, "1 hour"),
             Tap.Interval = lubridate::hms(format(Tap.DateTime.Rnd, 
                                                  "%H:%M:%S"))) %>%
      group_by(Tap.Interval, Tap.Date) %>%
      summarize(Num.Riders = sum(Ride.Count)) %>%
      summarize(Avg.Riders = sum(Num.Riders)/n(),
                SE = sd(Num.Riders) / sqrt(n())) %>%
      filter(Avg.Riders > 10 & !is.nan(SE)) %>%
      ungroup()
   
    # plot
    ggplot(ripta_day) +
      geom_line(aes(x=Tap.Interval, y = Avg.Riders)) +
      scale_x_time() +
      geom_errorbar(aes(x = Tap.Interval,
                        ymin = Avg.Riders - SE, 
                        ymax = Avg.Riders + SE), width = 0.2) +
      labs(
        title = paste("Ridership Over a Day for Route", input$routeInput),
        x = "Time",
        y = "Ride Count"
      ) +
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
      summarize(Avg.Rides = round(sum(Ride.Count)/num_days,3)) %>%
      mutate(Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
      left_join(stops_gfts, by = c( "Stop.Number"="stop_id")) %>%
      filter(!is.na(Avg.Rides) & !is.na(Stop.Number)) %>%
      select(Stop.Number, stop_lat, stop_lon, Avg.Rides) %>%
      mutate(col = cut(Avg.Rides, c(0, 1, 5, 10, 25, 50, max(Avg.Rides)+1), 
                       include.lowest=T,
                       labels = c("<1", "1-4", "5-9", "10-24", "25-49", 
                                  "50+")))

    # plot
    pal <- colorFactor(palette = 'RdPu', stop_data$col)
    leaflet(data=stop_data) %>%
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