# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                       11. Study region plot                            #
# ---------------------------------------------------------------------- #

# Load needed packages
library(ggplot2)
library(terra)
library(viridis)
library(maps)
library(showtext)



#-------------------------------------------------------------------------------

# 1. Data preparation ----------------------------------------------------------

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
load("input_data/observed_occ_pacific_islands.RData") # Contains observed presences on the Pacific islands
load("input_data/spatial_data/islandgroups_clim.RData") # Contains the island groups that have a climatic data coverage > 50%
load("input_data/spatial_data/islandgroups_edaclim.RData") # Contains the island groups that have a climatic and edaphic data coverage > 50%
load("input_data/spatial_data/geoentities_plus_newname.RData") # Island group shapefiles


# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Retrieve names of all island groups
islandgroup <- colnames(observed_occ_pacific_islands)

# Transform to terra SpatVect object
geoentities_SpatVect <- vect(geoentities, crs = "EPSG:4326")



# (a) Calculate centroid y and x points of island groups -----------------------

# (extra case of Solomon Bismark and Northern Palau as they have unreliable 
# lon/lat values, therefore corrected shapefiles are used)

# Create an empty data frame to store centroid results
island_group_centroid <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(island_group_centroid) <- c("island_group", "lon", "lat", "new_lon")


for (i in islandgroup) { # Start of the loop over all island groups
  
  print(i)
  
  if (i == "Solomon_Bismark") { 
    
    # Load in the corrected shapefile
    island_group_shape <- terra::vect("input_data/spatial_data/solomon_bismark.shp")
    
  } else if (i == "Northern Palau") {
    
    # Load in the corrected shapefile
    island_group_shape <- terra::vect("input_data/spatial_data/northern_palau.shp")
    
  } else { 
    
    # Load in the island group shapefiles
    island_group_shape <- subset(geoentities_SpatVect, geoentities_SpatVect$new_archipelago_name == i)
    
  }
  
  # Extract points of each island group
  points_x <- island_group_shape$point_x
  points_y <- island_group_shape$point_y
  
  # Put them in a data frame
  points_x_y <- data.frame(points_x, points_y)
  
  # Extract centroid point by averaging the x and y coordinates
  x <- round(mean(points_x_y$points_x), 2)
  y <- round(mean(points_x_y$points_y), 2)
  
  # For easier plotting the longitude in form of 0-360° is added
  if (x < 0) { 
    
    # Calculate the difference from -180
    diff_value <- 180 + (x)
    
    # Add the difference to the old longitude value
    new_lon <- 180 + diff_value

  } else { new_lon <- x
  }
  
  if (i == "Fiji") { # As the Fiji islands are located along the meridian, the 
                     # x centroid is determined to be 180
    
    # Create a vector containing the centroid information
    island_group_info <- c(i, 180, y, 180)
    
  } else { 
    
    # Create a vector containing the centroid information
    island_group_info <- c(i, x, y, new_lon)
    
  }
  
  # Add the information to the results data frame
  island_group_centroid <- rbind(island_group_centroid, island_group_info)
  

} # End of the loop over all island groups

# Make sure that column names are correct
colnames(island_group_centroid) <- c("island_group", "lon", "lat", "new_lon")

# Save the data frame containing the centroid x and y coordinates
save(island_group_centroid, file = "input_data/spatial_data/island_group_centroid.RData")






# (b) Sum up observed presences per island group -------------------------------

# Add up the number of observed study species on the Pacific island groups
sum_observed_occ_pacific_islands <- data.frame(observed_occurrences = colSums(observed_occ_pacific_islands))

# Move row names (island groups) into first column
sum_observed_occ_pacific_islands$island_group <- rownames(sum_observed_occ_pacific_islands)
rownames(sum_observed_occ_pacific_islands) <- NULL



# (c) Island groups used for analysis ------------------------------------------

# Create a data frame that contains all island groups and a column that indicates
# whether these island groups are suitable for all analysis or just for the 
# analysis using climatic data

# Extract the suitable island groups of the climatic analysis and the analysis
# with climatic and edaphic data
islandgroups_clim <- islandgroups_clim[,1, drop = FALSE]

islandgroups_edaclim <- islandgroups_edaclim[,1, drop = FALSE]
islandgroups_edaclim$analysis <- "climate + edaphic"

# Merge both data frames and retain, keep all island groups and keep the values 
# from the second data frame if values do not match
env_analysis <- merge(islandgroups_clim, islandgroups_edaclim, by = "island_group", all.x = TRUE)
env_analysis$analysis <- replace(env_analysis$analysis, is.na(env_analysis$analysis), "climate")



# (d) Put all information together -----------------------------------------

# Merge the information of centroid coordinates, observed study species richness,
# and their suitability for a climatic or climatic and edaphic analysis

# Merge the centroid information and observed species richness on the island groups
study_region_plot <- merge(island_group_centroid, sum_observed_occ_pacific_islands, by = "island_group")

# Merge with environmental suitability for analysis
study_region_plot  <- merge(study_region_plot, env_analysis, by = "island_group")




#-------------------------------------------------------------------------------

# 2. Study region plot ---------------------------------------------------------

# Get the data of the Pacific-centered world map
map_pacific      <- map("world2", fill = TRUE, col = "grey")
map_pacific_data <- map_data(map_pacific)

# Convert coordinate values and observed species numbers into numeric values
study_region_plot$new_lon <- as.numeric(as.character(study_region_plot$new_lon))
study_region_plot$lat <- as.numeric(as.character(study_region_plot$lat))
study_region_plot$observed_occurrences <- as.numeric(as.character(study_region_plot$observed_occurrences))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot a Pacific centered map, add the island groups with their observed study species
# numbers and indicate whether an island group is only suitable for a purely climatic
# analysis or a climatic and edaphic analysis
ggplot(study_region_plot, aes(x = new_lon, y = lat)) +
  geom_polygon(data = map_pacific_data, aes(x=long, y = lat, group = group)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick3", linewidth = 0.5, alpha = 0.7) +
  geom_vline(xintercept = 180, linetype = "dashed", color = "firebrick3", linewidth = 0.5,  alpha = 0.7) +
  geom_point(aes(colour = factor(analysis), fill = observed_occurrences), shape = 21, size = 10, alpha = 0.8, position = "jitter", stroke = 1.3) +
  theme_minimal() +
  labs(y= "Latitude", x = "Longitude") +
  geom_text(label=study_region_plot$island_group, nudge_x = 0, nudge_y = -5.5, check_overlap = T, size = 3.4) +
  #geom_text(label = study_region_plot$observed_occurrences, size = 3) +
  coord_sf(ylim=c(-53, 35), xlim=c(110, 320)) +
  scale_x_continuous(breaks=c(100, 150, 200, 250, 300),
                     labels=c("100°E", "150°E", "160°W", "110°W", "60°W"))+
  scale_y_continuous(breaks=c(-40, -20, 0, 20),
                     labels=c("40°S", "20°S", "0°N/S", "20°N"))  +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8), plot.title = element_text(hjust = 0, size = 22), 
        axis.title = element_text(size = 11, color = "black"), axis.text = element_text(color = "black", size = 9), text = element_text(family = "Calibri")) +
  scale_colour_manual(values=c("navyblue", "firebrick4"), name = "Predictor availablility") +
  scale_fill_viridis(name = "Observed non-native\nspecies richness", option = "G", limits=c(0,82), direction = -1) 


# Save the plot
ggsave("output_data/plots/study_region/study_region_plot.svg", width = 16, height = 5, unit = "cm")



