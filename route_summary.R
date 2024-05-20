# This file contains functions that are specific to
# the "Route Summary" tab

get_rides_per_stop <- function(data, metric) {
  #' Gets the average ridership at each stop with census data
  #' for a given route and census metric specified by the user.
  #' 
  #' @param data data frame - Filtered data for a given route
  #' @param metric string - Census metric specified by the user
  #'
  #' @returns data frame - Data containing average ridership and
  #' census values at each stop
  req(nrow(data) > 0)
  rides_per_stop_data <- get_rides_per_stop_helper(data)
  req(nrow(rides_per_stop_data) > 0)
  
  # Filter census data by metric
  # selected by user
  metric <- switch(metric,
                   population = "B01003_001",
                   income = "B19013_001",
                   poverty = "B05010_002",
                   employed = "B23025_001",
                   car_commute = "B08301_002",
                   carpool = "B08301_004",
                   public_transp = "B08301_010",
                   no_vehicle = "B08141_002")
  filtered_census_data <- CENSUS_DATA[CENSUS_DATA$variable == metric,] %>%
    drop_na() %>%
    # Create copy of geometry column because
    # it gets overwritten when merging
    mutate(poly_coords = geometry)
  
  # Merge ridership data with census data
  merged_data <- filtered_census_data %>%
    st_join(rides_per_stop_data)
  return(merged_data)
}


plot_rides_per_stop <- function(all_tracts_data, metric) {
  #' Plots average ridership at each stop overlayed with
  #' census data at the tract level.
  #'
  #' @param all_tracts_data data frame - Data containing
  #' estimates for the filtered census metric for each
  #' tract in Rhode Island; also contains values for the
  #' average number of rides for tracts that contain stops
  #' @param metric string - Census metric specified by
  #' the user
  #'
  #' @returns leaflet - Plot of average ridership at
  #' each stop and tract-level census data
  req(nrow(all_tracts_data) > 0)
  
  # Tracts without stops are missing
  # values for average ridership
  rides_per_stop_data <- all_tracts_data %>% drop_na()
  census_title <- switch(metric,
                         population = "Population Count",
                         income = "Median Household Income",
                         poverty = "Num. Below Poverty Line",
                         employed = "Employment Count",
                         car_commute = "Num. Car Commuters",
                         carpool = "Num. Carpoolers",
                         public_transp = "Num. Take Public Transp.",
                         no_vehicle = "Num. Without Vehicle")
  
  # Create breaks via quantiles
  # Remove duplicates because quantiles
  # may be same with limited data
  breaks <- unique(quantile(rides_per_stop_data$Avg.Rides,
                            probs = seq(0, 1, 0.2)))
  
  # Bin size must be at least 2
  if (length(breaks) < 2) {
    breaks <- c(0, breaks)
  }
  stops_pal <- colorBin(palette = "RdPu",
                        rides_per_stop_data$Avg.Rides,
                        breaks)
  
  census_pal <- colorBin(palette = "viridis",
                         all_tracts_data$estimate,
                         5)
  
  # Plot
  plot <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Census data
    addPolygons(data = all_tracts_data,
                color = ~ census_pal(estimate),
                weight = 0.5,
                fillOpacity = 0.1) %>%
    # Stop coordinates
    addCircleMarkers(data = rides_per_stop_data,
                     lng = ~ stop_lon,
                     lat = ~ stop_lat,
                     popup = as.character(rides_per_stop_data$Avg.Rides),
                     radius = 2,
                     opacity = 1,
                     col = ~ stops_pal(Avg.Rides)) %>%
    addLegend(position = "topright",
              pal = stops_pal,
              values = rides_per_stop_data$Avg.Rides,
              opacity = 1,
              title = "Ride Count") %>%
    addLegend(position = "bottomright",
              pal = census_pal,
              values = all_tracts_data$estimate,
              opacity = 1,
              title = census_title)
  return(plot)
}


# --- The functions below are not called in the server. Instead,
# --- they manipulate data that the server loads.

get_census_data <- function(file_path) {
  #' Loads and saves census data in Rhode
  #' Island for specific census variables
  #' at the tract level.
  #' 
  #' @param file_path string - Path to
  #' save the census data
  data <- get_acs(geography = "tract",
                  variables = c("B01003_001", # Population
                                "B19013_001", # Household income
                                "B05010_002", # Under poverty line
                                "B23025_001", # Employment status
                                "B08301_002", # Car truck or van
                                "B08301_004", # Carpooled
                                "B08301_010", # Public transportation
                                "B08141_002"), # No vehicle
                  state = "RI",
                  geometry = TRUE) %>%
    st_transform("+proj=longlat +datum=WGS84")
  # Save data
  saveRDS(data, file_path)
}
