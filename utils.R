library(geosphere)

plot_ridership_by_time <- function(data) {
  #' Plots route ridership over time.
  #'
  #' @param data: provided data; may be
  #' filtered for a route specified by
  #' the user
  #'
  #' @return ggplot, plot without labels
  
  # group by time interval
  req(nrow(data) > 0)
  ripta_day <- data %>%
    mutate(Tap.Date = as.Date(Time),
           Tap.DateTime.Rnd = floor_date(Time, "1 hour"),
           Tap.Interval = lubridate::hms(format(Tap.DateTime.Rnd,
                                                "%H:%M:%S"))) %>%
    group_by(Tap.Interval, Tap.Date) %>%
    summarize(Num.Riders = sum(Ride.Count),
              .groups="drop_last") %>%
    summarize(Avg.Riders = sum(Num.Riders)/n(),
              SE = sd(Num.Riders) / sqrt(n())) %>%
    filter(Avg.Riders > 5 & !is.nan(SE)) %>%
    ungroup()
  
  # plot
  ggplot(ripta_day) +
    geom_line(aes(x=Tap.Interval, y = Avg.Riders)) +
    scale_x_time() +
    geom_errorbar(aes(x = Tap.Interval,
                      ymin = Avg.Riders - SE,
                      ymax = Avg.Riders + SE), width = 0.2) +
    theme_minimal()
}

calculate_miles <- function(start_lons, start_lats, end_lons, end_lats) {
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
  
  sapply(1:length(start_lons), miles_helper)
}
