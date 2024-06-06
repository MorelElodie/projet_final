library(fmsb)
library(ggridges)
library(waffle)
library(dplyr)
library(plotly)
library(corrplot)
library(emmeans)

# Load and clean the dataset
df <- matrix_full_elev_ecos_clim_sat
df <- na.omit(df)  # Remove rows with missing values


###########################################################
############          Analyses                 ############
###########################################################

# Separate continuous and discrete variables
df_continous <- df[, colnames(df) %in% c("mean_temp", "elevation_points", "NDVI", "mean_precip")]

mydata.cor <- cor(df_continous)

# Plot the correlation matrix with hierarchical clustering
windows()
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3)

# we can see that elevation and mean temperature are heavly correlated, as expect. 


P_fact <- ggplot(data = df, mapping = aes(x = Landcover, y = mean_temp, fill = Landcover))

P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

library(plotly)
ggplotly(P_fact)

# Mean temperature seems to be slighlty lower in Grasslands
# than in cropland and especially shrubland (but we only have 2 data points in shrubland)
# to verify if the differences that we see are satistically significant,
# we need to create a model and perform an anova (and post-hoc tests)


linear_model <- lm(mean_temp ~ Landcover, data = df)

# Perform ANOVA on the linear model
anova(linear_model)

# Conduct post-hoc tests with Tukey adjustment
em <- emmeans(linear_model, list(pairwise ~ Landcover), adjust = "tukey")
print(em)

# we can see that: 
# Cropland have a significantly higher mean temperature than Forests and grasslands.
# Forests are significantly higher than grasslands but lower than Settlement.
# Grasslands are significantly lower than settlements.
# I am not looking at tests for Shrubland and Sparsely or Non vegetated since
# there is so few data points.

# Overall, we can say that the order of highest to lowest mean temperature is: 
# Cropland > Forest > Settlement > Grassland


########################################################################
############   Repartition of Landcover with altitude       ############
########################################################################

# only two data points in shrubland --> removing them 
df_land <- subset(df, !df$Landcover=="Shrubland" & !df$Landcover=="Sparsely or Non vegetated")

windows()
ggplot(df_land, aes(x = elevation_points, y = Landcover, fill = Landcover)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  labs(x = "Elevation", y= "Landcover")

# Here we can see the density of occurences in each Landcover type as a function of elevation. It is interesting
# to see each Landcover is most present at slightly different elevations except Crops and settlements. This makes
# sense when we think that framing is done close to people and at altitudes that allow crops to grow.


###########################################################
############          Radar chart              ############
###########################################################

# Now that we looked at the environmental data, we can start comparing the environmental
# preferences of my two species.

# Agréger les données
aggregated_data_species_1 <- aggregate(
  cbind(elevation_points, mean_precip, mean_temp, NDVI) ~ species, 
  data = df, 
  FUN = mean
)

# Créer les min et max pour le graphe
min1 <- c(0,50,5,1200)
max2 <- c(apply(aggregated_data_species_1[,2:5],2,max))
sp1 <- aggregated_data_species_1[1,2:5]
sp2 <- aggregated_data_species_1[2,2:5]
row_id <- c(1,2,aggregated_data_species_1$species)
 
aggregated_data_species_1 <- rbind(min1,max2,sp1,sp2)
row.names(aggregated_data_species_1) <- row_id


colors_border=c("#404080","#69b3a2")
colors_in=c("#4040807e", "#69b3a270")

windows()
radarchart( aggregated_data_species_1, axistype=1 , 
    #custom polygon
    pcol=colors_border, pfcol=colors_in , plwd=4 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
    #custom labels
    vlcex=0.8, 
    vlabels = c("Elevation", "Mean precipitation \n in summer", "Mean temperature\n in summer", "NDVI")
     
    )
legend("topleft", legend = c("Bombus ruderatus","Bombus argillaceus"), col = colors_border, lwd = 4, bty = "n")

# we can see that both species have a very similar niche. Bombus ruderatus seems to prefer a slightly higher
# mean temperature in summer, but there is only a difference of around 1 degree. This plot therefore shows
# that both species probably share the same niche.





###########################################################
############          Waffle chart              ###########
###########################################################



#number of each individual for each species in each landcover
df_waffle <- df_land %>%
  group_by(species, Landcover) %>%
  summarise(count = n()) %>%
  ungroup()

windows()
ggplot(df_waffle, aes(fill=species, values=count)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, alpha=0.6) +
  facet_wrap(~Landcover, nrow = 1, strip.position = "bottom") +
  labs(title = "Number of individual in each Landcover type for both species")+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal()

# This graph is not very informative, but the visual is cool. We can compare visually the number
# of each individual (rectangles) of each species in different Landcover types. It does not seem like
# there is a huge difference in terms of quantity of individuals in each Landcover between both species.
# Again, the repartition of both species does not seem different between Landcovers.



###########################################################
############        Interactive histograms      ###########
###########################################################


precip <- df %>%
  ggplot( aes(x=mean_precip, fill=species)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_minimal() +
    labs(x = "Mean precipitation in summer", y= "Counts")
ggplotly(precip)


temp <- df %>%
  ggplot( aes(x=mean_temp, fill=species)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_minimal() +
    labs(x = "Mean precipitation in summer", y= "Counts")
ggplotly(temp)

# We can compare on both graphs the temperature and precipitation niches of my species. As we saw in the 
# radar chart, there doesn't seem to be any major differences between Bombus ruderatus and Bombus argillaceus.



################################################################
############  Conclusions on my research question    ###########
################################################################


#The data analyses do not show any important differences in environmental preferences between both species. 
#We can hypothetize that if the two species never occur at the same location, competition might be a better
#explanatory factor. In switzerland, Bombus ruderatus seems to be colonizing the current areas of presence of 
#Bombus agrillaceus (first individual found in the Vallée du Rhône). It would be interesting to follow the 
#expansion of B. ruderatus and see if it indeed competes against B. argillaceus in the same environment.  