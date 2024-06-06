library(raster)
library(sp)

list_countries <- c("Switzerland","Germany","Italy", "Austria")

map_countries <- ne_countries(scale = "medium", returnclass = "sf",country = list_countries)


# Read the raster GeoTIFF
ecosystem_raster <- raster("data/WorldEcosystem.tif")

## crop and mask
#windows()
r2 <- crop(ecosystem_raster, extent(map_countries))
ecosystem <- mask(r2, map_countries)
#plot(ecosystem)

# points
spatial_points <- sp::SpatialPoints(coords = matrix_full_elev[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points, add=TRUE)

# Extract the ecosystem associated with each point 
eco_values <- raster::extract(ecosystem, spatial_points)

matrix_full_elev_ecos <- data.frame(matrix_full_elev, eco_values)

# access metadata

metadat_eco <- read.delim("data/WorldEcosystem.metadata.tsv")

matrix_full_elev_ecos <- merge(matrix_full_elev_ecos, metadat_eco, by.x = "eco_values", by.y = "Value", all.x=T) 

matrix_full_elev_ecos$Landcover <- as.factor(matrix_full_elev_ecos$Landcover)

matrix_full_elev_ecos[, c("Red", "Blue", "Green", "color")] <- list(NULL)

windows()
plot(matrix_full_elev_ecos$Landcover, xlab="Landcover type", ylab = "Number of occurences", col="lightblue")