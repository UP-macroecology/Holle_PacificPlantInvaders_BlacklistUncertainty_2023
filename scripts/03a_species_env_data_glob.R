# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#     03a. Relation of environmental data to global occurrence data      #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(terra)
library(stringr)

# Load needed objects
load("input_data/occurrence_numbers_thinned_filtered.RData") # data frame that contains study species names

Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable rasters
names(Chelsa) <- c(paste('bio',1:19, sep='_')) # Change climate variable names

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
  
  print(sp)
  
  print(clim)
  
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
  
} # End of loop


#-------------------------------------------------------------------------------

# 3. Climatic and edaphic data -------------------------------------------------

for (sp in study_species) {
  
  print(sp)
  
  print(edaclim)
  
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
  
} # End of loop
