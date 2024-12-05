# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#     03a. Relation of environmental data to native occurrence data      #
# ---------------------------------------------------------------------- #

# Load needed packages
library(terra)
library(stringr)
library(tidyr)

# Load needed objects
load("input_data/occurrence_numbers_thinned_filtered.RData") # data frame that contains study species names

Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable raster

SoilGrids <- terra::rast(str_sort(list.files("input_data/environmental_data/SoilGrids_V2", 
                                             pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Edaphic variable rasters

#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve study species names
study_species <- unique(as.character(occurrence_numbers_thinned_filtered$species)) 

# Change climate variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_'))

# Exclude the climatic variables bio 8, bio 9, bio 18, bio 19 (that mix
# temperature and precipitation)
Chelsa <- Chelsa[[setdiff(names(Chelsa), c("bio_8", "bio_9", "bio_18", "bio_19"))]]


# Stack climatic and edaphic data
Chelsa_SoilGrids <- c(Chelsa, SoilGrids)


#-------------------------------------------------------------------------------

# 2. Climatic data -------------------------------------------------------------

for (sp in study_species) {
  try({
    
  print(sp)
  
  print("clim")
  
  # Check if distribution file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  # Check if joined distribution and climatic data file already exists
  file_exists_2 <- file.exists(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
  
  
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with joining climatic data if output 
  # of joined distribution and climatic data does not exist yet but the distribution data set exists
  
  print("start of process")
  
  # Load data frame containing thinned native presences and absences
  load(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  # Join species and climate data
  species_occ_clim_native <- cbind(species_occ_native, terra::extract(x = Chelsa, y = data.frame(species_occ_native[,c('lon','lat')])))
  
  # Drop NA for the environmental variables 
  species_occ_clim_native <- species_occ_clim_native %>% drop_na()
  
  # Check for duplicates
  duplicated(species_occ_clim_native$ID)
  
  # Only retain non-duplicated cells
  species_occ_clim_native <- species_occ_clim_native[!duplicated(species_occ_clim_native$ID),]
  
  # Save the data frame used for model fitting
  save(species_occ_clim_native, file = paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
  
  
  } else if (file_exists_2 == TRUE) { print("already done")
  } else if (file_exists_1 == FALSE) { print("input file not available yet")
  } # End of if condition
  
  
})} # end of try and for loop over species


#-------------------------------------------------------------------------------

# 3. Climatic and edaphic data -------------------------------------------------

for (sp in study_species) {
  try({
  
  print(sp)
  
  print("edaclim")
  
  # Check if distribution file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  # check if joined distribution and climatic and edaphic data file already exists
  file_exists_2 <- file.exists(paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
  
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with joining climatic and edaphic data if output 
  # of joined distribution and climatic and edaphic data does not exist yet but the distribution data set exists
    
  print("start of process")
  
  # Load data frame containing thinned native presences and absences
  load(paste0("output_data/distribution_data/native/species_occ_native_",sp,".RData"))
  
  # Join species data with climatic and edaphic data
  species_occ_edaclim_native <- cbind(species_occ_native, terra::extract(x = Chelsa_SoilGrids, y = data.frame(species_occ_native[,c('lon','lat')])))
  
  # Drop NA for the environmental variables 
  species_occ_edaclim_native <- species_occ_edaclim_native %>% drop_na()
  
  # Check for duplicates
  duplicated(species_occ_edaclim_native$ID)
  
  # Only retain non-duplicated cells
  species_occ_edaclim_native <- species_occ_edaclim_native[!duplicated(species_occ_edaclim_native$ID),]
  
  # Save the data frame used for model fitting
  save(species_occ_edaclim_native, file = paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
  
  
  } else if (file_exists_2 == TRUE) { print("already done")
  } else if (file_exists_1 == FALSE) { print("input file not available yet")
  } # End of if condition
  
  
})} # end of try and for loop over species
  

  
#-------------------------------------------------------------------------------

# 4. Final species selection after environmental relation ----------------------

# Prepare a data frame to store the result of occurrence numbers
occ_numbers_thinned_env_nat <- data.frame(expand.grid(species=c(paste(study_species))), natclim_occurrences=NA, natedaclim_occurrences=NA)

for (sp in study_species) {
  try({

  print(sp)

  # Check if joined distribution with purely climatic
  # and combined climatic and edaphic data file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
  file_exists_2 <- file.exists(paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
  

  if (file_exists_1 == TRUE && file_exists_2 == TRUE) { # Just continue with the species if file exists

  print("file available")

  # Load in the data frame with species occurrences and all environmental variables
  load(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
  load(paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))

  # Subset by native presences
  native_presences_clim <- subset(species_occ_clim_native, species_occ_clim_native$occ == 1)
  native_presences_edaclim <- subset(species_occ_edaclim_native, species_occ_edaclim_native$occ == 1)

  # Sum up number of native presences
  number_native_presences_clim <- nrow(native_presences_clim)
  number_native_presences_edaclim <- nrow(native_presences_edaclim)

  # Store the results in data frame
  occ_numbers_thinned_env_nat[occ_numbers_thinned_env_nat$species == sp, "natclim_occurrences"] <- number_native_presences_clim
  occ_numbers_thinned_env_nat[occ_numbers_thinned_env_nat$species == sp, "natedaclim_occurrences"] <- number_native_presences_edaclim

  } else if (file_exists_1 == FALSE && file_exists_2 == FALSE) { print("file not available")
    next # If file does not exist, skip to the next species
  }

})}

# Remove the species with less than 40 occurrences
occ_numbers_thinned_env_nat_filtered <- subset(occ_numbers_thinned_env_nat, occ_numbers_thinned_env_nat$natclim_occurrences >= 40 & occ_numbers_thinned_env_nat$natedaclim_occurrences >= 40)

# Save the data frame
save(occ_numbers_thinned_env_nat_filtered, file = "input_data/occ_numbers_thinned_env_nat_filtered.RData")


  
  
  