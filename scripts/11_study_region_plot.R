# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                       11. Study region plot                            #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)
library(terra)



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
study_species <- c(study_species, "Arrhenatherum_elatius", "Tanacetum_parthenium", 
                   "Bellis_perennis", "Phleum_pratense", "Cirsium_vulgare") # needs to be erased at some point

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
    island_group_info <- c(i, 180, y, new_lon)
    
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
islandgroups_edaclim$analysis <- "edaclim"

# Merge both data frames and retain, keep all island groups and keep the values 
# from the second data frame if values do not match
env_analysis <- merge(islandgroups_clim, islandgroups_edaclim, by = "island_group", all.x = TRUE)
env_analysis$analysis <- replace(env_analysis$analysis, is.na(env_analysis$analysis), "clim")



# (d) Put all information together -----------------------------------------

# Merge the information of centroid coordinates, observed study species richness,
# and their suitability for a climatic or climatic and edaphic analysis

# Merge the centroid information and observed species richness on the island groups
study_region_plot <- merge(island_group_centroid, sum_observed_occ_pacific_islands, by = "island_group")

# Merge with environmental suitability for analysis
study_region_plot  <- merge(study_region_plot, env_analysis, by = "island_group")




#-------------------------------------------------------------------------------

# 2. Study region plot ---------------------------------------------------------


