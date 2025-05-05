# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                   01. Initial Species selection                        #
# ---------------------------------------------------------------------- #


# Load needed packages
library(foreach)
library(doParallel)
library(stringr)
library(terra)
library(sf)
library(sfheaders)
library(purrr)
library(furrr)
library(dplyr)
library(readr)

# Load needed objects
source("scripts/00_functions.R") # thin function
world_mask <- terra::rast("input_data/spatial_data/world_mask.tif") # mask with 1 km resolution 


#-------------------------------------------------------------------------------

# 1. Species data processing ---------------------------------------------------

# Retrieve the plant species occurring as naturalized on the Hawaiian Islands
# Pacific_Invaders_GIFT_22_01.csv = Wohlwends Pacific invaders list
# (author of: Wohlwend, MR, Craven, D, Weigelt, P, et al. Anthropogenic and environmental drivers shape diversity of naturalized plants across the Pacific. Divers Distrib. 2021; 27: 1120–1133)
# Species names are based on LCVP (Leipzig Catalogue of Vascular Plants; Freiberg et al. 2020), but don’t include author information
Wohlwend_data <- read_delim("input_data/Pacific_Invaders_GIFT_22_01.csv", 
                            delim = ";", locale = locale(decimal_mark = ",")) %>%
                 filter(inva_stat == "T", Islandgroup == "Hawaiian") %>% # "T" stand for true, they are recognized as invasive by PIER
                 distinct(Species)

species_Hawaii <- Wohlwend_data$Species

# Load in cleaned the species occurrence data with coordinate information 
# (downloaded from GBIF and BIEN 2023) and their biogeographical status 
# of all naturalized plant species in the Pacific
load("input_data/occ_status_resolved.RData")

# Subset the data frame to only contain species occurring on the Hawaiian Islands
occurrences_Hawaii <- occ_status_resolved[occ_status_resolved$species %in% species_Hawaii, ]

# Insert a _ in species name
occurrences_Hawaii$species <- str_replace_all(occurrences_Hawaii$species, " ", "_")

# and save the outcome
save(occurrences_Hawaii, file = "input_data/occurrences_Hawaii.RData")

# Get the species names that have occurrence data
species_Hawaii <- unique(occurrences_Hawaii$species) # 120 species

# Exclude 4 species that are occurring as native on the Hawaiian Islands according 
# to the status assignment as this speaks against the initial criterion of
# species selection
species_Hawaii <- species_Hawaii[!species_Hawaii %in% c("Tribulus_cistoides", "Guaiacum_sanctum", "Piper_aduncum", "Solanum americanum" )]



#-------------------------------------------------------------------------------


# 2. Species selection step 1: before spatial thinning -------------------------

# As a threshold solely species with >= 40 native occurrences based on the 
# criterion 1 or 2 are included

# Prepare a data frame to store the result of occurrence numbers
occurrence_numbers <- data.frame(expand.grid(species=c(paste(species_Hawaii))), global_occurrences=NA, 
                                 native_occurrences_crit1=NA, native_occurrences_crit2=NA)

# Loop over the species and retrieve numbers of native and global occurrences based o
for (sp in species_Hawaii) {
  
  # Create a subset for each species
  subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)
  
  # Get the number of all occurrences
  occurrences_global <- nrow(subset_species)
  
  # Subset only native occurrences using criteria 1 and 2
  subset_native_crit1 <- subset(subset_species, subset_species$criterion_1 == "native")
  subset_native_crit2 <- subset(subset_species, subset_species$criterion_2 == "native")
  
  # Get the number of native occurrences
  occurrences_native_crit1 <- nrow(subset_native_crit1)
  occurrences_native_crit2 <- nrow(subset_native_crit2)
  
  # Store the results in data frame
  occurrence_numbers[occurrence_numbers$species == sp, "global_occurrences"] <- occurrences_global
  occurrence_numbers[occurrence_numbers$species == sp, "native_occurrences_crit1"] <- occurrences_native_crit1
  occurrence_numbers[occurrence_numbers$species == sp, "native_occurrences_crit2"] <- occurrences_native_crit2
  
} 
  

# Remove all species that have less than 40 native occurrences using the two different criteria
occurrence_numbers_filtered_crit1 <- subset(occurrence_numbers, occurrence_numbers$native_occurrences_crit1 >= 40) # 7 species excluded
occurrence_numbers_filtered_crit2 <- subset(occurrence_numbers, occurrence_numbers$native_occurrences_crit2 >= 40) # 7 species excluded

# Get species names for further usage
# There is no difference between both criteria, the one that gives the same 
# weight to all three sources after checking if the sources refer to the same
# area size is used (criterion 1)
species_Hawaii_filtered <- as.character(occurrence_numbers_filtered_crit1$species) # 41 of these species are included in Annas analysis


#-------------------------------------------------------------------------------


# 3. Species selection step 2: after spatial thinning of presences -------------

# Prepare a data frame to store the result of occurrence numbers
occurrence_numbers_thinned <- data.frame(expand.grid(species=c(paste(species_Hawaii_filtered))), global_occurrences=NA, native_occurrences=NA)

# Start parallel computing
# cl <- makeCluster(5)
# registerDoParallel(cl)

# foreach (sp = 1:length(species_Hawaii_filtered), .packages = c("terra", "sf", "sfheaders", "furrr", "purrr", "dplyr")) %dopar% {
  
  #try({ 

for (sp in species_Hawaii_filtered) {
  
    print(sp)
  
    # Check if file with thinned occurrence data already exists
    file_exists <- file.exists(paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
    
    if (file_exists == FALSE) { # If file does not exist, continue with thinning of occurrences
  
    print("start of occurrence thinning")
    
    # Create a subset for each species
    subset_species <- subset(occurrences_Hawaii, occurrences_Hawaii$species == sp)
    
    # Remove rows with duplicate numbers at a 1 km resolution
    cellnumbers <- terra::extract(world_mask, subset_species[c("lon", "lat")], cells = TRUE)
    subset_species_wd <- subset_species[!duplicated(cellnumbers[, "cell"]), ]
    
    # Extract coordinate information
    presences_coords <- subset_species_wd[c("lon", "lat")]
    
    # Transform the coordinate information into sf object
    presences_coords_sf <- st_as_sf(presences_coords, coords = c("lon", "lat"), crs = crs(world_mask))
    
    # Spatial thinning of presences with distance of 3 km using the thin function
    presences_thinned <- thin(presences_coords_sf, thin_dist = 3000, runs = 1, ncores = 1)
    
    # Combine the thinned presences with important species information based on the lon and lat information
    species_presences_thinned <- merge(subset_species_wd[,c(1, 2, 13, 15, 16)], presences_thinned, by = c("lon", "lat"))
    
    # Save the thinned presence points
    save(species_presences_thinned, file = paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
    
    } else if (file_exists == TRUE) { # If the file exists, load it
      
      print("occurrence thinning already done")
      load(paste0("output_data/presences_thinned/species_presences_thinned_",sp,".RData"))
      
    } # End of if condition
    
    # Get the number of all occurrences
    occurrences_global_thinned <- nrow(species_presences_thinned)
    
    # Get the number of native occurrences
    subset_native <- subset(species_presences_thinned, species_presences_thinned$criterion_1 == "native")
    occurrences_native_thinned <- nrow(subset_native)
    
    # Store results in data frame
    occurrence_numbers_thinned[occurrence_numbers_thinned$species == sp, "global_occurrences"] <- occurrences_global_thinned
    occurrence_numbers_thinned[occurrence_numbers_thinned$species == sp, "native_occurrences"] <- occurrences_native_thinned
    
} # End of the loop
   
# })} # End of the foreach loop

    
# stopCluster(cl)

# Save the thinned occurrence numbers of the species
save(occurrence_numbers_thinned, file = "input_data/occurrence_numbers_thinned.RData")

# Remove the species with less than 40 occurrences
occurrence_numbers_thinned_filtered <- subset(occurrence_numbers_thinned, occurrence_numbers_thinned$native_occurrences >= 40)

# Remove species where the minimum difference between native and global occurrences is below 40
occurrence_numbers_thinned_filtered <- subset(occurrence_numbers_thinned_filtered, (global_occurrences - native_occurrences) >= 40) 

# Save the data frame with study species that are suitable for analysis
save(occurrence_numbers_thinned_filtered, file = "input_data/occurrence_numbers_thinned_filtered.RData")


