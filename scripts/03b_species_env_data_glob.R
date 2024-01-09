# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#     03b. Relation of environmental data to global occurrence data      #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(terra)
library(stringr)
library(tidyr)

# Load needed objects
load("input_data/occurrence_numbers_thinned_filtered.RData") # data frame that contains study species names

Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable rasters

SoilGrids <- terra::rast(str_sort(list.files("input_data/environmental_data/SoilGrids_V2", 
                                             pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Edaphic variable rasters

#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve study species names
study_species <- unique(as.character(occurrence_numbers_thinned_filtered$species)) 

# Change climate variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_'))

# Stack climatic and edaphic data
Chelsa_SoilGrids <- c(Chelsa, SoilGrids)


#-------------------------------------------------------------------------------

# 2. Climatic data -------------------------------------------------------------

for (sp in study_species) {
  try({
  
  print(sp)
  
  print("clim")
  
  # Check if distribution file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
  
  # check if joined distribution and climatic data file already exists
  file_exists_2 <- file.exists(paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
  
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with joining climatic data if output 
  # of joined distribution and climatic data does not exist yet but the distribution dataset exists
    
  print("start of process")
  
  # Load data frame containing thinned native presences and absences
  load(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
  
  # Join species and climate data
  species_occ_clim_global <- cbind(species_occ_global, terra::extract(x = Chelsa, y = data.frame(species_occ_global[,c('lon','lat')])))
  
  # Drop NA for the environmental variables 
  species_occ_clim_global <- species_occ_clim_global %>% drop_na()
  
  # Check for duplicates
  duplicated(species_occ_clim_global$ID)
  
  # Only retain non-duplicated cells
  species_occ_clim_global <- species_occ_clim_global[!duplicated(species_occ_clim_global$ID),]
  
  # Save the data frame used for model fitting
  save(species_occ_clim_global, file = paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
  
  
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
  file_exists_1 <- file.exists(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
  
  # check if joined distribution and climatic and edaphic data file already exists
  file_exists_2 <- file.exists(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
  
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with joining climatic and edaphic data if output 
  # of joined distribution and climatic and edaphic data does not exist yet but the distribution dataset exists
    
  print("start of process")
  
  # Load data frame containing thinned native presences and absences
  load(paste0("output_data/distribution_data/global/species_occ_global_",sp,".RData"))
  
  # Join species data with climatic and edaphic data
  species_occ_edaclim_global <- cbind(species_occ_global, terra::extract(x = Chelsa_SoilGrids, y = data.frame(species_occ_global[,c('lon','lat')])))
  
  # Drop NA for the environmental variables 
  species_occ_edaclim_global <- species_occ_edaclim_global %>% drop_na()
  
  # Check for duplicates
  duplicated(species_occ_edaclim_global$ID)
  
  # Only retain non-duplicated cells
  species_occ_edaclim_global <- species_occ_edaclim_global[!duplicated(species_occ_edaclim_global$ID),]
  
  # Save the data frame used for model fitting
  save(species_occ_edaclim_global, file = paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
  
  
  } else if (file_exists_2 == TRUE) { print("already done")
  } else if (file_exists_1 == FALSE) { print("input file not available yet")
  } # End of if condition
  
  
})} # end of try and for loop over species



#-------------------------------------------------------------------------------

# 4. Global occurrence numbers after environmental relation --------------------

# Prepare a data frame to store the result of occurrence numbers
occ_numbers_thinned_env_glob <- data.frame(expand.grid(species=c(paste(study_species))), global_occurrences=NA)

for (sp in study_species) {
  try({
    
  print(sp)
    
  # check if joined distribution and climatic and edaphic data file already exists
  file_exists <- file.exists(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
  
  if (file_exists == TRUE) { # Just continue with the species if file exists
    
  print("file available")
  
  # Load in the data frame with species occurrences and all environmental variables
  load(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
  
  # Subset by native presences
  global_presences <- subset(species_occ_edaclim_global, species_occ_edaclim_global$occ == 1)
  
  # Sum up number of native presences
  number_global_presences <- nrow(global_presences)
  
  # Store the results in data frame
  occ_numbers_thinned_env_glob[occ_numbers_thinned_env_glob$species == sp, "global_occurrences"] <- number_global_presences
  
  } else if (file_exists == FALSE) { print("file not available")
    next # If file does not exist, skip to the next species
  }
  
})}

# Remove the species with less than 40 occurrences
occ_numbers_thinned_env_glob_filtered <- subset(occ_numbers_thinned_env_glob, occ_numbers_thinned_env_glob$global_occurrences >= 40)

# Save the data frame
save(occ_numbers_thinned_env_glob_filtered, file = "input_data/occ_numbers_thinned_env_glob_filtered.RData")
