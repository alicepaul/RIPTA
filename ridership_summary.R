# This file contains functions called in the server
# that are specific to the "Ridership Summary" tab

plot_ridership_across_days <- function(data) {
  #' Plots ridership across days of the week.
  #' 
  #' @param data data frame - Filtered data
  #' 
  #' @returns ggplot - Bar plot of ridership
  #' across days of the week
  req(nrow(data) > 0)
  data <- data %>%
    mutate(Tap.Date = as.Date(Time)) %>%
    group_by(Day.of.Week, Tap.Date) %>%
    summarize(Num.Riders = sum(Ride.Count),
              .groups = "drop_last") %>%
    summarize(Avg.Riders = sum(Num.Riders) / n(),
              SE = sd(Num.Riders) / sqrt(n())) %>%
    ungroup()
  
  # Plot
  plot <- ggplot(data, aes(x = Day.of.Week,
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
  return(plot)
}
