library(elevatr) #to extract elevations
library(rnaturalearth)
library(sp) # to load raster & extract points
library(raster) # to plot the raster of the elevations
library(ggplot2)
library(rayshader) #for sphere_shade
library(sf)


list_countries <- c("Switzerland","Germany","Italy", "Austria")

map_countries <- ne_countries(scale = "medium", returnclass = "sf",country = list_countries)


sf_use_s2(FALSE)

# extracting elevations
elevation_countries <- get_elev_raster(map_countries, z = 8)
plot(elevation_countries)

# croping elevation data for my countries
r2 <- crop(elevation_countries, extent(map_countries))
elevation_countries <- mask(r2, map_countries)
plot(elevation_countries)

# extracting elevation data for my species data points

coord <- data.frame(matrix_full$longitude,matrix_full$latitude)

# Extract elevation values
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(coord, proj4string = sp::CRS(SRS_string = ll_prj))

elevation_points <- raster::extract(elevation_countries, points, method = "bilinear")

matrix_full_elev <- data.frame(matrix_full, elevation_points)

