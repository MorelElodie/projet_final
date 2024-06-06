library(sf)
#library(MODIStsp)
library(raster)
library(here)
library(viridis)
library(rgeoboundaries)

# Downloading the country boundary

list_countries <- c("Switzerland","Germany","Italy", "Austria")
map_boundary <- geoboundaries(list_countries[4])

#dir.create("./data/modis", recursive = TRUE)


# Defining filepath to save downloaded spatial file
#spatial_filepath <- "./data/modis/countries.shp"

# Saving downloaded spatial file on to our computer
#st_write(map_boundary, paste0(spatial_filepath))


#library(MODIStsp)
#### check available data
#MODIStsp_get_prodlayers("M*D13Q1")

#MODIStsp(
#  gui = FALSE,
#  out_folder = "./data/modis",
#  out_folder_mod = "./data/modis",
#  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
#  bandsel = "NDVI",
#  user = "mstp_test",
#  password = "MSTP_test_01",
# start_date = "2020.06.01",
#  end_date = "2020.06.01",
#  verbose = FALSE,
#  spatmeth = "file",
#  spafile = spatial_filepath,
#  out_format = "GTiff"
#)



print("Downloading NDVI data")
# Reading in the downloaded NDVI raster data
NDVI_raster_CH <- raster("./data/NDVI_CH.tif")
NDVI_raster_GER <- raster("./data/NDVI_GER.tif")
NDVI_raster_IT <- raster("./data/NDVI_IT.tif")
NDVI_raster_AUT <- raster("./data/NDVI_AUT.tif")

NDVI_raster <- merge(NDVI_raster_CH, NDVI_raster_GER, NDVI_raster_IT, NDVI_raster_AUT)


# Transforming the data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#windows()
#plot(NDVI_raster)

# Cropping the data
print("Cropping and plotting data")
map_boundary_all <- geoboundaries(list_countries)
NDVI_raster_countries <- raster::mask(NDVI_raster, as_Spatial(map_boundary_all))
windows()
plot_NDVI <- plot(NDVI_raster_countries)
print(plot_NDVI)

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(NDVI_raster) <- 0.0001


spatial_points <- SpatialPoints(coords = matrix_full_elev_ecos_clim[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
#plot(spatial_points,add=T)


# Extract values
NDVI <- raster::extract(NDVI_raster, spatial_points)


matrix_full_elev_ecos_clim_sat <- data.frame(matrix_full_elev_ecos_clim, NDVI)