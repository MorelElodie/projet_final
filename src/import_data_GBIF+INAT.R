library(rgbif) # to import gbif data
library(rinat) # to import data from inat
library(rnaturalearth) # for map



######################################################
########### Importing data from GBIF  ################


import_gbif <- function(myspecies, namecols, map_countries, list_countries_gbif){
    
    print("GBIF import:")

    matrix_gbif <- matrix(, nrow = 0, ncol = 4)
    nspecies <- length(myspecies)

    for(i in myspecies){

        #to verify that the function is running well
        print(i)

        #import data
        gbif <- occ_data(scientificName = i, hasCoordinate = TRUE, limit = 2000, decimalLongitude = "-10, 20", decimalLatitude = "40, 55") 
        
        #take occurences
        occur  <- gbif$data

        #cut occurences for CH
        gbif_country <- occur[occur$country == list_countries_gbif,]

        # create matrix with the data we want
        matrix_sp <- data.frame(gbif_country$species, gbif_country$decimalLatitude, gbif_country$decimalLongitude)

        matrix_gbif <- rbind(matrix_gbif, matrix_sp)
 
    }

    colnames(matrix_gbif) <- namecols
    matrix_gbif$source <- rep("gbif", nrow(matrix_gbif))

    plottest <- ggplot(data = map_countries) +
        geom_sf()   +
        geom_point(data = matrix_gbif, aes(x = longitude, y = latitude, fill = species), 
        size = 1, shape = 20) + labs(title="Occurence of my species for GBIF data")
    
    print(plottest)
    return(matrix_gbif)
}

# defining variables
list_countries_gbif <- c("Switzerland","Germany","Italy", "Austria")

map_countries <- ne_countries(scale = "medium", returnclass = "sf",country = list_countries_gbif)

names <- c("species", "latitude", "longitude")

species_list <- c("Bombus argillaceus", "Bombus ruderatus")


# extracting data
matrix_gbif <- import_gbif(species_list, names, map_countries, list_countries_gbif)





######################################################
########### Importing data from INAT  ################


import_inat <- function(myspecies, namecols, map_countries, list_countries_inat){
    
    print("INAT import:")

    matrix_inat <- matrix(, nrow=0, ncol=4)
    inat_data <- matrix(, nrow=0, ncol=4)

    nspecies <- length(myspecies)

    for(i in myspecies){

        #to keep track of what the function does
        print(i)

        #import data
        for(j in list_countries_inat){
            inat_1_c <- get_inat_obs(query=i, place_id = j)
            inat_data <- rbind(inat_data, inat_1_c)
        }

        # create matrix with the data we want
        # i know that the name is sometimes to subspecies level, which does not interests me 
        # --> keeping scientific name to species level:     

        if(length(unique(inat_data$scientific_name))>1){
            name_species <- rep(i, times = length(inat_data$scientific_name))
        }else{
            name_species <- inat_data$scientific_name
        } 

        matrix_sp <- data.frame(name_species, inat_data$latitude, inat_data$longitude) 
        matrix_inat <- rbind(matrix_inat, matrix_sp)
 
    }

    colnames(matrix_inat) <- namecols
    matrix_inat$source <- rep("inat", nrow(matrix_inat))

    plottest <- ggplot(data = map_countries) +
            geom_sf() +
            geom_point(data = matrix_inat, aes(x = longitude, y = latitude, fill = species), size = 1, shape = 20) + 
            labs(title="Occurence of my species from INaturalist data")
    
    print(plottest)
    return(matrix_inat)
    
    
}

# same variables defined above except for the name of countries

list_countries_inat <- c("switzerland","germany", "italy", "austria")


# extracting data
matrix_inat <- import_inat(species_list, names, map_countries, list_countries_inat)


######################################################
########### merging data sources #####################



matrix_full <- rbind(matrix_inat, matrix_gbif)
matrix_full <- data.frame(matrix_full)


final_plot <- ggplot(data = map_countries) +
    geom_sf()   +
    geom_point(data = matrix_full, aes(x = longitude, y = latitude, col = species), size = 3, 
    shape = 20) + labs(title = "Occurences of my species")

print(final_plot)