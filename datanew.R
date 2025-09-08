library(rgeoboundaries)
library(sf)
library(raster)
library(ggplot2)
library(viridis)

swiss_bound <- rgeoboundaries::geoboundaries("Switzerland")
elevation_data <- elevatr::get_elev_raster(locations = swiss_bound, z = 9, clip = "locations")


elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"
# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

ggplot() +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = swiss_bound, color = "white", fill = NA) +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(title = "Elevation in Switzerland", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")