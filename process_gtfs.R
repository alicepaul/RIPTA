
###################### Route Time ##############################################

# Incorporating the total travel time for routes
stop_times <- read.csv("gtfs/stop_times.txt")
trips_to_route <- read.csv("gtfs/trips.txt")

stop_times$arrival_time <- as.POSIXct(stop_times$arrival_time, 
                                      format = "%H:%M:%S")
stop_times$departure_time <- as.POSIXct(stop_times$departure_time, 
                                        format = "%H:%M:%S")

# Calculate total trip duration
stop_times <- stop_times %>%
  group_by(trip_id) %>%
  summarise(
    start_time = min(arrival_time),
    end_time = max(departure_time),
    total_duration = difftime(end_time, start_time, units = "mins")
  )

# Filter tips_to_route 
trips_to_route <- trips_to_route %>% select(route_id, trip_id)

# Merge together
route_time <- full_join(stop_times, trips_to_route, by= c("trip_id"= "trip_id")) 
route_time <- route_time %>% 
  group_by(route_id) %>% 
  summarize(total_time = sum(total_duration, na.rm=TRUE))

write.csv(route_time, "data/route_time.csv", row.names = FALSE)

