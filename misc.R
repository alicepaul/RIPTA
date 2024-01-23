library(gtfstools)
library(leaflet)
library(sf)
library(dplyr)


###################### GTFS ##############################################

# Find trips per stop 
stops <- full_df %>%
  group_by(Stop.Number) %>%
  summarize(Tot.Rides = sum(Ride.Count)) %>%
  mutate(Stop.Number.Rev = as.numeric(str_sub(Stop.Number, 2)))

# GTFS data
ripta_gtfs <- read_gtfs("RIPTA-GTFS.zip")
route_shapes <- get_trip_geometry(ripta_gtfs, file = "shapes")

# Get lat/lng with total rides
stops_gfts <- read.csv("stops.txt") %>%
  left_join(stops, by = c("stop_id" = "Stop.Number.Rev")) %>%
  filter(!is.na(Tot.Rides), Tot.Rides > 50) %>%
  select(Stop.Number, stop_lat, stop_lon, Tot.Rides) %>%
  arrange(desc(Tot.Rides)) %>%
  mutate(col = cut(Tot.Rides, c(0, 100, 500, 1000, 10000, 30000), include.lowest=T,
                   labels = c("<100", "100-499", "500-999", "1000-9999", "10000+")),
         radius = case_when(Tot.Rides < 100 ~ 0.1,
                            Tot.Rides < 500 ~ 0.5,
                            Tot.Rides < 1000 ~ 1,
                            Tot.Rides < 10000 ~ 2,
                            TRUE ~ 3))

pal <- colorFactor(palette = 'RdYlGn', stops_gfts$col)
leaflet(data=stops_gfts) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, 
                   popup = as.character(stops_gfts$Tot.Rides),
                   radius = ~radius*1,
                   col = ~pal(col)) %>%
  addLegend('bottomright', pal = pal, values = stops_gfts$col,
            opacity = 1)
