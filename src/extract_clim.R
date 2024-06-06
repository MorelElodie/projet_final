library(sp)
library(raster)
library(geodata) # to get clim data


spatial_points <- sp::SpatialPoints(coords = matrix_full_elev_ecos[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))


#################### Temperature data

# Retrieve temperature data for Switzerland

list_countries <- c("Switzerland","Germany","Italy", "Austria")

clim_temp_1 <- worldclim_country(list_countries[1], var = "tavg", path = tempdir())
clim_temp_2 <- worldclim_country(list_countries[2], var = "tavg", path = tempdir())
clim_temp_3 <- worldclim_country(list_countries[3], var = "tavg", path = tempdir())
clim_temp_4 <- worldclim_country(list_countries[4], var = "tavg", path = tempdir())

clim_temp_tot <- merge(clim_temp_1, clim_temp_2, clim_temp_3, clim_temp_4)
clim_temp_br <- brick(clim_temp_tot)

#print("Extracting temperature data:")
#clim_temp_tot <- NULL

#for (i in list_countries){
  #print(i)
  #clim_temp <- worldclim_country(i, var = "tavg", path = tempdir())
  
  #clim_temp_tot <- merge(clim_temp_tot, clim_temp)
#}


# extracting mean temperature in summer (May- August)
matrix_temp <- NULL

for (i in 5:8) {

  raster_temp <- as(clim_temp_br[[i]], "Raster")

  temp <- raster::extract(raster_temp, spatial_points, method = 'bilinear')
  matrix_temp <- cbind(matrix_temp,temp)
  }

mean_temp <- rowMeans(matrix_temp)

matrix_full_elev_ecos_temp <- data.frame(matrix_full_elev_ecos, mean_temp)


temp_plot <- ggplot(matrix_full_elev_ecos_temp, aes(x = temp)) +
  geom_density(color = "darkblue", fill = "lightpink", adjust = 3) +
  theme_bw() +
  labs(title="Temperature density")




#################### Precipitation data

# Retrieve precipitation data for each country
clim_precip_1 <- worldclim_country(list_countries[1], var = "prec", path = tempdir())
clim_precip_2 <- worldclim_country(list_countries[2], var = "prec", path = tempdir())
clim_precip_3 <- worldclim_country(list_countries[3], var = "prec", path = tempdir())
clim_precip_4 <- worldclim_country(list_countries[4], var = "prec", path = tempdir())

clim_precip_tot <- merge(clim_precip_1, clim_precip_2, clim_precip_3, clim_precip_4)
clim_precip_br <- brick(clim_precip_tot)

# Precipitation data for summer (May to August)
matrix_pre <- NULL

for (i in 5:8) {

  raster_precip <- as(clim_precip_br[[i]], "Raster")

  precip <- raster::extract(raster_precip, spatial_points, method = 'bilinear')
  matrix_pre <- cbind(matrix_pre,precip)
  }

mean_precip <- rowMeans(matrix_pre)

matrix_full_elev_ecos_clim <- data.frame(matrix_full_elev_ecos_temp, mean_precip)



precip_plot <- ggplot(matrix_full_elev_ecos_clim, aes(x = mean_precip)) +
  geom_density(color = "black", fill = "lightblue", adjust = 2) +
  theme_bw() +
  labs(title="Precipitation density")


windows()
print(precip_plot)
windows()
print(temp_plot)