# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                   01. Species selection                                #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Required path 
path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")


# Load needed packages
library(foreach)
library(doParallel)
library(stringr)

# Load needed objects
source("scripts/utils.R") # thin function
world_mask <- terra::rast('input_data/world_mask.tif') # mask with 1 km resolution 


#-------------------------------------------------------------------------------


# 1. Species data processing ---------------------------------------------------

# Load in the list of the 120 plant species occurring as naturalized on the Hawaiian Islands
load("input_data/blacklist_final.RData")

# Get all species names of the potential study species
species_Hawaii <- unique(blacklist_final$species)

# Load in cleaned the species occurrence data with their biogeographical status
# of all naturalized plant species in the Pacific downloaded from GBIF and BIEN (2023)
load("input_data/occ_status_resolved.RData")

# Subset the data frame to only contain species occurring on the Hawaiian Islands
occurrences_biogeo_Hawaii <- occ_status_resolved[occ_status_resolved$species %in% species_Hawaii, ]

# Load in the species occurrence data downloaded with coordinate information
load("input_data/occ_cleaned_slim.RData")

# Subset the data frame to only contain species occurring on the Hawaiian Islands
occurrences_coord_Hawaii <- occ_cleaned_slim[occ_cleaned_slim$species %in% species_Hawaii, ]

# Merge the data frame into one that contains coordinate information and 
# biogeographical status information
occurrences_Hawaii <- merge(occurrences_coord_Hawaii[, c(1:4)], occurrences_biogeo_Hawaii[, c(1,15)], by = "occ_id", all.x = TRUE)

# Insert a _ in species name
occurrences_Hawaii$species <- str_replace_all(occurrences_Hawaii$species, " ", "_")

# Get the species names that have occurrence data
species_Hawaii <- unique(occurrences_Hawaii$species) # 118 species


#-------------------------------------------------------------------------------


# 2. Species selection step 1: before spatial thinning -------------------------

# As a threshold solely species with >= 40 native occurrences based on the criterion 2 are included

# Prepare a data frame to store the result of occurrence numbers
occurrence_numbers <- data.frame(expand.grid(species=c(paste(species_Hawaii))), global_occurrences=NA, native_occurrences=NA)

# Loop over the species and retrieve numbers of native and global occurrences
for (sp in species_Hawaii) {
  
  # Create a subset for each species
  subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)
  
  # Get the number of all occurrences
  occurrences_global <- nrow(subset_species)
  
  # Subset only native occurrences
  subset_native <- subset(subset_species, subset_species$criterion_2 == "native")
  
  # Get the number of native occurrences
  occurrences_native <- nrow(subset_native)
  
  # Store the results in data frame
  occurrence_numbers[occurrence_numbers$species == sp, "global_occurrences"] <- occurrences_global
  occurrence_numbers[occurrence_numbers$species == sp, "native_occurrences"] <- occurrences_native
  
} 
  

# Remove all species that have less than 40 native occurrences
occurrence_numbers_filtered <- subset(occurrence_numbers, occurrence_numbers$native_occurrences >= 40) # 70 species excluded

# Get species names for further usage
species_Hawaii_filtered <- as.character(occurrence_numbers_filtered$species)




### Test
#load("input_data/spp_pre_thinning.RData")


#species_Anna <- occurrence_numbers[occurrence_numbers_filtered$species %in% spp_pre_thinning, ] # 0 species

#-------------------------------------------------------------------------------


# 3. Species selection step 2: after spatial thinning of presences -------------

# Prepare a data frame to store the result of occurrence numbers
occurrence_numbers_thinned <- data.frame(expand.grid(species=c(paste(species_Hawaii_filtered))), global_occurrences=NA, native_occurrences=NA)

# Start parallel computing
cl <- makeCluster(5)
registerDoParallel(cl)

foreach (sp = 1:length(species_Hawaii_filtered), .packages = c("terra", "sf", "sfheaders", "furrr", "purrr")) %dopar% {
  
  try({ 
    
    # Create a subset for each species
    subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)
    
    # Extract coordinate information
    presences_coords <- subset_species[c("lon", "lat")]
    
    # Remove rows with duplicate numbers at a 1 km resolution
    cellnumbers <- terra::extract(world_mask, presences_coords, cells = TRUE)
    presences_coords <- presences_coords[!duplicated(cellnumbers[, "cell"]), ]
    
    # Transform the coordinate information into sf object
    presences_coords_sf <- st_as_sf(presences_coords, coords = c("lon", "lat"), crs = crs(world_mask))
    
    # Spatial thinning with distance of 3 km using the thin function
    presences_thinned <- thin(presences_coords_sf, thin_dist = 3000, runs = 10, ncores = 1)
    
    # Combine the thinned presences with important species information based on the lon and lat information
    species_presences_thinned <- merge(presences_thinned, occurrences_Hawaii, by = c("lon", "lat"))
    
    # Save the thinned presence points
    save(species_presences_thinned, file = paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
    
    # Get the number of all occurrences
    occurrences_global_thinned <- nrow(species_presences_thinned)
    
    # Get the number of native occurrences
    subset_native <- subset(species_presences_thinned, species_presences_thinned$criterion2 == "native")
    occurrences_native_thinned <- nrow(subset_native)
    
    # Store results in data frame
    occurrence_numbers_thinned[occurrence_numbers_thinned$species == sp, "global_occurrences"] <- occurrences_global_thinned
    occurrence_numbers_thinned[occurrence_numbers_thinned$species == sp, "native_occurrences"] <- occurrences_native_thinned
    
    
})} # End of the foreach loop

    
stopCluster(cl)

# Save the thinned occurrence numbers of the species
save(occurrence_numbers_thinned, file = "input_data/occurrence_numbers_thinned.RData")

# Remove the species with less than 40 occurrences
occurrence_numbers_thinned_filtered <- subset(occurrence_numbers_thinned, occurrence_numbers_thinned$native_occurrences >= 40)

# Remove species with equal native and global occurrences
occurrence_numbers_thinned_filtered <- occurrence_numbers_thinned_filtered[occurrence_numbers_thinned_filtered$global_occurrences != occurrence_numbers_thinned_filtered$native_occurrences, ]

# Save the data frame with study species that are suitable for analysis
save(occurrence_numbers_thinned_filtered, file = "input_data/occurrence_numbers_thinned.RData")


