# This file contains functions that are not specific to any of the tabs.

# Constants
# CENSUS_DATA <<- readRDS("./data/census_data.rds")
# Stops GTFS data containing the coordinates of each
# stop and their corresponding census tract
STOPS_DATA <<- readRDS("./data/stops_data.rds")
CENSUS_DATA_MERGED <<- readRDS("./data/census_data_merged.rds")

get_ridership_summary_table <- function(data, active_tags) {
  #' Summarizes the filtered data.
  #' 
  #' @param data data frame - Filtered data;
  #' may also be filtered for a specific route
  #' @param active_tags character vector -
  #' Active tags
  #' 
  #' @returns gt table - Summary table
  req(nrow(data) > 0)
  all_tags <- c("Source", "Day.of.Week", "Institution", active_tags)
  
  # Summary table
  table <- data %>%
    tbl_summary(include = all_tags,
                type = list(active_tags ~ "dichotomous"),
                value = list(Low.Income ~ "1",
                             Off.Peak ~ "1",
                             Eco.Pass ~ "1",
                             Transfer ~ "1",
                             One.Hour.Pass ~ "1",
                             #Two.Hour.Pass ~ "1",
                             Day.Pass ~ "1",
                             #Ten.Ride.Pass ~ "1",
                             #Week.Pass ~ "1",
                             Monthly.Pass ~ "1")) %>%
    as_gt()
  return(table)
}


is_valid_password <- function(private_key) {
  #' Validates the password provided by the user.
  #'
  #' @param private_key character vector - Private
  #' key of the password provided by the user
  #'
  #' @returns boolean - TRUE if the password is
  #' valid, FALSE otherwise
  public_key <- "68 ab 9c 12 03 aa 76 ef 9a 54 0b a7 5f dc 2a cb 3c 45 3c 44 d2 04 0a 80 cd 88 84 05 5e 9a cb 20"
  # String representation of the private key
  private_key <- paste(pubkey(private_key), collapse = " ")
  return(private_key == public_key)
}


decrypt_data <- function(private_key) {
  #' Decrypts the password-protected data
  #' hosted on the server.
  #'
  #' @param private_key character vector -
  #' Private key of the password provided by
  #' the user
  #'
  #' @returns data frame - Data that has already
  #' been cleaned and merged
  encrypted_data <- readRDS("./data/encrypted_data.rds")
  return(unserialize(simple_decrypt(encrypted_data, private_key)))
}


get_all_dates <- function(data) {
  #' Gets all of the dates from the loaded data.
  #' 
  #' @param data data frame - Loaded data;
  #' NULL if the data has not been loaded yet
  #' 
  #' @returns Date - All dates from the loaded
  #' data; NULL if the data has not been loaded
  #' yet
  if (is.null(data)) {
    return(NULL)
  }
  return(as.Date(data$Time, format = "%Y-%m-%d"))
}


get_time_frame <- function(dates) {
  #' Gets the time frame of the loaded data.
  #' 
  #' @param dates Date - All dates from the
  #' loaded data; NULL if the data has not 
  #' been loaded yet
  #' 
  #' @returns string - Time frame of the
  #' loaded data; empty string if the data
  #' has not been loaded yet
  if (is.null(dates)) {
    return("")
  }
  start_date <- format(min(dates), "%B %d, %Y")
  end_date <- format(max(dates), "%B %d, %Y")
  return(paste(start_date, end_date, sep = " - "))
}


get_active_tags <- function(dates) {
  #' Removes inactive tags based on the dates in
  #' the loaded data.
  #' 
  #' @param dates Date - All dates from the loaded 
  #' data; NULL if the data has not been loaded yet
  #' 
  #' @returns character vector - Active tags
  active_tags <- c(
    "Low.Income",
    "Off.Peak",
    "Eco.Pass",
    "Transfer",
    "One.Hour.Pass",
    "Two.Hour.Pass",
    "Day.Pass",
    "Ten.Ride.Pass",
    "Week.Pass",
    "Monthly.Pass"
  )
  if (is.null(dates)) {
    return(active_tags)
  }
  inactive_date <- as.Date("2022-1-15", format = "%Y-%m-%d")
  inactive_tags <- c("Two.Hour.Pass", "Ten.Ride.Pass", "Week.Pass")
  # If the data was collected after the inactive date, remove
  # the inactive tags
  if (inactive_date < min(dates)) {
    active_tags <- active_tags[!(active_tags %in% inactive_tags)]
  }
  return(active_tags)
}


get_filtered_data <- function(data, active_tags, input) {
  #' Filters the data based on the filter categories
  #' that were enabled by the user.
  #' 
  #' @param data data frame - Loaded data; NULL
  #' if the data has not been loaded yet
  #' @param active_tags character vector - Active
  #' tags
  #' @param input list - Server input
  #' 
  #' @returns data frame - The filtered data
  # Require upload
  req(data)
  all_tags <- c("College", "High.School", "Institution", active_tags)
  
  # Make tag columns factors
  data[, all_tags] <- lapply(data[, all_tags], factor)
  data$Day.of.Week <- factor(data$Day.of.Week, 
                             levels = c("Mon", "Tue", "Wed", "Thu", 
                                        "Fri", "Sat", "Sun"))
  
  # Date time
  midnight_trips <- nchar(data$Time) < 19
  data$Time[midnight_trips] <- paste(data$Time[midnight_trips], "00:00:00")
  data$Time <- as.POSIXct(data$Time,
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "EST"
  )
  
  # Filter by type of rider
  if (length(input$riderType) == 0) {
    showNotification("Please select at least one type of rider.",
                     type = "error")
    return(NULL)
  }
  
  if (input$riderType == "Senior/Disabled") {
    data <- data %>% filter(Type %in% c("Senior", "Disabled"))
  } else if (input$riderType != "All Types") {
    data <- data %>% filter(Type == input$riderType)
  }
  
  # Filter by rider tags
  if (length(input$selectedTags) > 0) {
    data <- data %>% filter(if_any(.cols = input$selectedTags,
                                   # Second condition is for student tags
                                   ~ . == 1 | (. != 0 & . != "None")))
  }
  
  # Filter by days of travel
  if (length(input$dayFilter) > 0) {
    days <- input$dayFilter
    data <- data[data$Day.of.Week %in% days, ]
  }
  
  # Filter by source
  if (length(input$sourceInput) > 0) {
    source_data <- input$sourceInput
    data <- data[data$Source %in% source_data, ]
  }
  
  if (nrow(data) > 0) {
    data <- data[rep(seq(nrow(data)), data$Ride.Count), ]
    data$Ride.Count <- 1
  }
  return(data)
}


plot_ridership_by_time <- function(data, title) {
  #' Plots ridership over time.
  #'
  #' @param data data frame - Filtered data;
  #' either across all routes or for a specific
  #' route
  #' @param title string - Title of plot
  #'
  #' @return ggplot - Plot of ridership
  #' over a day
  req(nrow(data) > 0)
  ripta_day <- data %>%
    mutate(Tap.Date = as.Date(Time),
           Tap.DateTime.Rnd = floor_date(Time, "1 hour"),
           Tap.Interval = lubridate::hms(format(Tap.DateTime.Rnd,
                                                "%H:%M:%S"))) %>%
    group_by(Tap.Interval, Tap.Date) %>%
    summarize(Num.Riders = sum(Ride.Count),
              .groups = "drop_last") %>%
    summarize(Avg.Riders = sum(Num.Riders) / n(),
              SE = sd(Num.Riders) / sqrt(n())) %>%
    filter(Avg.Riders > 5 & !is.nan(SE)) %>%
    ungroup()
  
  # Plot
  plot <- ggplot(ripta_day) +
    geom_line(aes(x=Tap.Interval, y = Avg.Riders)) +
    scale_x_time() +
    geom_errorbar(aes(x = Tap.Interval,
                      ymin = Avg.Riders - SE,
                      ymax = Avg.Riders + SE), width = 0.2) +
    theme_minimal() +
    labs(title = title, x = "Time", y = "Ride Count")
  return(plot)
}


get_data_for_route <- function(data, route) {
  #' Filters data for a specific route.
  #' 
  #' @param data data frame - Filtered data
  #' @param route numeric - Route number
  #' selected by the user
  #' 
  #' @returns data frame - Input data filtered
  #' for the specified route
  req(nrow(data) > 0 && !is.null(route))
  data <- data %>% filter(Route.Number == route)
  return(data)
}


get_rides_per_stop_helper <- function(data) {
  #' Gets the average number of rides at each
  #' stop for a given route.
  #'
  #' @param data data frame - Filtered data
  #' for a given route
  #' 
  #' @returns data frame - Data containing
  #' average ridership at each stop
  req(nrow(data) > 0)
  # Find trips per stop
  num_days <- n_distinct(as.Date(data$Time))
  data <- data %>%
    group_by(Stop.Number) %>%
    summarize(Avg.Rides = round(sum(Ride.Count) / num_days, 3)) %>%
    mutate(Stop.Number = as.numeric(str_sub(Stop.Number, 2))) %>%
    inner_join(STOPS_DATA, by = c("Stop.Number" = "stop_id")) %>%
    select(Stop.Number, stop_lat, stop_lon, Avg.Rides, geometry) %>%
    st_as_sf()
  return(data)
}

# --- The functions below are not called in the server. Instead,
# --- they manipulate data that the server loads.

add_census_tracts <- function(new_file_path,
                              stops_gtfs_path="./gtfs/stops.txt") {
  #' Maps the location of each stop in the stops GTFS data to its
  #' corresponding census tract, then saves the resulting data.
  #'
  #' @param new_file_path string - Path to save the new data
  #' @param stops_gtfs_path string - Path to the original stops
  #' GTFS data
  data <- read.csv(stops_gtfs_path) %>%
    # Map coordinates to census tract
    st_as_sf(coords = c("stop_lon", "stop_lat"),
             remove = FALSE,
             crs = "+proj=longlat +datum=WGS84")
  saveRDS(data, new_file_path)
}


set_password <- function(new_password, raw_data_path, encrypted_data_path) {
  #' This function is used to change the password or the data hosted
  #' on the server. IMPORTANT: Note that you will need to print the
  #' public key printed from this function and update the public key
  #' stored in app.R!
  #' 
  #' @param new_password string - New password
  #' @param raw_data_path string - Path to the data you would like
  #' to display on the app
  #' @param encrypted_data_path string - Path for where you would
  #' like to store the encrypted data
  
  # Copy the public key printed in the console
  # and paste it into app.R
  private_key <- sha256(charToRaw(password))
  public_key <- pubkey(private_key)
  print(paste("Copy this key:", paste(public_key, collapse = " ")))
  
  raw_data <- read.csv(raw_data_path)
  # This line encrypts the data
  encrypted_data <- simple_encrypt(serialize(raw_data, NULL), public_key)
  
  # Save the encrypted data
  saveRDS(encrypted_data, encrypted_data_path)
}