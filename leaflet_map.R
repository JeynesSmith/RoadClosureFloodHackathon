# This script makes a leaflet map

# install.packages("leaflet")
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=153.026, lat=-27.4705, popup="Brisbane")

print(m)
