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
    filter(Avg.Riders > 10 & !is.nan(SE)) %>%
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
