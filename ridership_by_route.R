# This file contains functions called in the server
# that are specific to the "Ridership by Route" tab

get_daily_times_per_route <- function(data) {
  #' Gets times per day for each route.
  #' 
  #' @param data data frame - Filtered data
  #' 
  #' @returns data frame - Data frame containing
  #' the times for each route for each day
  data <- data %>%
    filter(!(Route.Number %in% c(89, 999, 9999))) %>%
    mutate(Date = as.Date(Time), Numeric.Time = as.numeric(Time)) %>%
    group_by(Route.Number, Date, Trip.Number) %>%
    # Get time for a single trip in minutes
    summarize(Trip.Time = (max(Numeric.Time) - min(Numeric.Time)) / 60,
              .groups = "drop_last") %>%
    # Get times for each route for each day
    summarize(Time.Per.Day = sum(Trip.Time), .groups = "drop_last")
  return(data)
}


calculate_miles <- function(start_lons, start_lats, end_lons, end_lats) {
  #' Calculates the number of miles between each pair of stops.
  #' 
  #' @param start_lons numeric vector - Longitudes of starting stop
  #' @param start_lats numeric vector - Latitudes of starting stop
  #' @param end_lons numeric vector - Longitudes of ending stop
  #' @param end_lats numeric vector - Latitudes of ending stop
  #' 
  #' @returns numeric vector - Number of miles between
  #' each pair of stops
  miles_helper <- function(idx) {
    lon1 <- start_lons[idx]
    lat1 <- start_lats[idx]
    lon2 <- end_lons[idx]
    lat2 <- end_lats[idx]
    
    if (is.na(lon2) || is.na(lat2)) {
      return(0)
    }
    meters <- distHaversine(c(lon1, lat1), c(lon2, lat2))
    # Convert to miles
    return(meters / 1609.34)
  }
  return(sapply(1:length(start_lons), miles_helper))
}


get_daily_miles_per_route <- function(data) {
  #' Get total miles per day for each route.
  #' 
  #' @param data data frame - Filtered data
  #' 
  #' @returns data frame - Data frame containing
  #' the total number of miles per day for each
  #' route
  data <- data %>%
    # Remove trip 99
    filter(Trip.Number != 99) %>%
    mutate(Date = as.Date(Time),
           Numeric.Time = as.numeric(Time),
           Time = as.character(Time),
           Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
    inner_join(STOPS_DATA, by = c("Stop.Number" = "stop_id")) %>%
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
    summarize(Miles.Per.Trip = sum(miles), .groups = "drop_last") %>%
    # Total miles per day
    summarize(Miles.Per.Day = sum(Miles.Per.Trip), .groups = "drop_last")
  return(data)
}


get_average_ridership <- function(data) {
  #' Summarizes average ridership per minute and per
  #' mile across all routes.
  #' 
  #' @param data data frame - Filtered data
  #' 
  #' @returns data frame - Data frame containing
  #' average ridership per minute and per mile across
  #' all routes
  req(nrow(data) > 0)
  times_data <- get_daily_times_per_route(data)
  miles_data <- get_daily_miles_per_route(data)
  data <- data %>%
    mutate(Date = as.Date(Time)) %>%
    group_by(Route.Number, Date) %>%
    summarize(Rides.Per.Day = sum(Ride.Count), .groups = "drop_last") %>%
    # Merge
    inner_join(times_data, by = c("Route.Number", "Date")) %>%
    inner_join(miles_data, by = c("Route.Number", "Date")) %>%
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
  return(data)
}


plot_average_ridership <- function(data, metric) {
  #' Plots average ridership per minute or per mile
  #' across all routes.
  #' 
  #' @param data data frame - Summarized ridership
  #' across all routes
  #' @param metric string - Metric used to compute
  #' average ridership; either minute or mile
  #' 
  #' @returns ggplot - Horizontal bar plot of
  #' average ridership for each route
  req(nrow(data) > 0)
  col_name <- switch(metric,
                     Minute = "Avg.Rides.Per.Minute",
                     Mile = "Avg.Rides.Per.Mile")
  
  # Horizontal bar plot
  plot <- ggplot(data, aes(x = .data[[col_name]],
                           y = fct_reorder(Route.Number,
                                           .data[[col_name]]),
                           text = paste0("Route: ",
                                         Route.Number,
                                         "\n Ridership: ",
                                         .data[[col_name]]))) +
    labs(x = paste("Average Riders Per", metric),
         y = "Route",
         title = paste("Average Rides Per",
                       metric,
                       "Across Routes")) +
    geom_col(fill = "skyblue") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9))
  return(ggplotly(plot, tooltip = "text"))
}


display_average_ridership <- function(data) {
  #' Renames the columns of the summarized
  #' ridership table for display.
  #' 
  #' @param data data frame - Summarized
  #' ridership across all routes
  #' 
  #' @returns data frame - Input table with
  #' renamed columns
  req(nrow(data) > 0)
  colnames(data) <- c("Route Number",
                      "Average Min. Per Day",
                      "Std Dev Min. Per Day",
                      "Total Rides",
                      "Average Rides Per Minute",
                      "Average Rides Per Mile",
                      "Average Rides Per Day")
  return(data)
}


get_weighted_averages_for_route <- function(all_data, route_num) {
  #' Computes the weighted average for each
  #' census demographic for a given route.
  #'
  #' @param all_data data frame - Filtered data
  #' @param route_num integer - Route number
  #'
  #' @returns data frame - Data containing the
  #' weighted averages of each census demographic
  req(nrow(all_data) > 0)
  rides_per_stop_data <- all_data %>%
    # Get data for a specific route
    get_data_for_route(route_num) %>%
    # Get ridership for each stop in that route
    get_rides_per_stop_helper()
  
  # If data does not exist after filtering and
  # pre-processing, return 0 for weighted averages
  if (nrow(rides_per_stop_data) == 0) {
    census_vars <- c("B01003_001", # Population
                     "B19013_001", # Household income
                     "B05010_002", # Under poverty line
                     "B23025_001", # Employment status
                     "B08301_002", # Car truck or van
                     "B08301_004", # Carpooled
                     "B08301_010", # Public transportation
                     "B08141_002") # No vehicle
    weighted_avg_data <- data.frame(matrix(0, ncol=length(census_vars)))
    colnames(weighted_avg_data) <- census_vars
  } else {
    # Merge with census data then compute weighted
    # average for each census metric
    merged_data <- mutate(CENSUS_DATA, poly_coords = geometry) %>%
      st_join(rides_per_stop_data) %>%
      # Keep relevant columns
      select(c(variable, estimate, Avg.Rides)) %>%
      st_drop_geometry() %>%
      drop_na() %>%
      group_by(variable) %>%
      summarize(Weighted.Avg = round(
        sum(Avg.Rides * estimate) / sum(Avg.Rides),
        2))
    
    # Reformat to be combined with data from other routes
    weighted_avg_data <- data.frame(matrix(merged_data$Weighted.Avg, nrow=1))
    colnames(weighted_avg_data) <- merged_data$variable
  }
  weighted_avg_data$Route <- route_num
  return(weighted_avg_data)
}


get_standardized_rides <- function(data) {
  #' Computes the weighted average for
  #' each census demographic for each
  #' route.
  #'
  #' @param data data frame - Filtered data
  #'
  #' @returns data frame - Data containing
  #' the weighted averages for each census
  #' demographic for each route
  # Get valid route numbers
  route_nums <- unique(data$Route.Number)
  route_nums <- route_nums[!(route_nums %in% c(89, 999, 9999))]
  # Get weighted averages for each route
  data <- lapply(route_nums, function(route) {
    get_weighted_averages_for_route(data, route)
  })
  data <- do.call("rbind", data)
  # Rename columns
  colnames(data) <- c("Population",
                      "Below Poverty Line",
                      "No Vehicle",
                      "Car Truck or Van",
                      "Carpool",
                      "Public Transportation",
                      "Household Income",
                      "Employment Status",
                      "Route Number")
  data <- data[, c("Route Number",
                   "Population",
                   "Household Income",
                   "Below Poverty Line",
                   "Employment Status",
                   "Car Truck or Van",
                   "Carpool",
                   "Public Transportation",
                   "No Vehicle")]
  return(data)
}
