# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                  03c. Subset of the background data                    #
# ---------------------------------------------------------------------- #

# Load needed packages
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 



#-------------------------------------------------------------------------------

# 1. Based on native occurrences and purely climatic data ----------------------

# Create a random subset of the absences to contain 10 times more absences than presences
for(sp in study_species) {
  try({
    
    print(sp)
    
    # check if joined distribution and climatic and edaphic data file already exists
    file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
    
    # check if the subsetted joined distribution and climatic and edaphic data file already exists
    file_exists_2 <- file.exists(paste0("output_data/distribution_env_data_subset/native/clim/species_occ_clim_native_",sp,".RData"))
    
    if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with subset if output 
      # of subsetted data does not exist yet and file of joint distribution and
      # environmental data exists
      
      # Load the data frame of native species occurrences joined with climate variables
      load(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData"))
      
      # Create a random subset of the absences to contain 10 times more absences than presences
      indices_absence <- which(species_occ_clim_native$occ == 0)
      subset_indices <- sample(indices_absence, ifelse(length(indices_absence) < sum(species_occ_clim_native$occ==1)*10, length(indices_absence), sum(species_occ_clim_native$occ==1)*10), replace = FALSE)
      combined_indices <- c(which(species_occ_clim_native$occ == 1), subset_indices)
      species_occ_clim_native <- species_occ_clim_native[combined_indices, ]
      
      # Save the subsetted data frame
      save(species_occ_clim_native, file = paste0("output_data/distribution_env_data_subset/native/clim/species_occ_clim_native_",sp,".RData"))
      
    } else if (file_exists_2 == TRUE) { print("already done")
    } else if (file_exists_1 == FALSE) { print("input file not available yet")
    } # End of if condition
    
    
})} # end of try and for loop over species
  




#-------------------------------------------------------------------------------

# 2. Based on native occurrences and combined climatic and edaphic data --------

# Create a random subset of the absences to contain 10 times more absences than presences
for(sp in study_species) {
  try({
    
    print(sp)
    
    # check if joined distribution and climatic and edaphic data file already exists
    file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
    
    # check if the subsetted joined distribution and climatic and edaphic data file already exists
    file_exists_2 <- file.exists(paste0("output_data/distribution_env_data_subset/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
    
    if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with subset if output 
      # of subsetted data does not exist yet and file of joint distribution and
      # environmental data exists
      
      # Load the data frame of native species occurrences joined with climate variables
      load(paste0("output_data/distribution_env_data/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
      
      # Create a random subset of the absences to contain 10 times more absences than presences
      indices_absence <- which(species_occ_edaclim_native$occ == 0)
      subset_indices <- sample(indices_absence, ifelse(length(indices_absence) < sum(species_occ_edaclim_native$occ==1)*10, length(indices_absence), sum(species_occ_edaclim_native$occ==1)*10), replace = FALSE)
      combined_indices <- c(which(species_occ_edaclim_native$occ == 1), subset_indices)
      species_occ_edaclim_native <- species_occ_edaclim_native[combined_indices, ]
      
      # Save the subsetted data frame
      save(species_occ_edaclim_native, file = paste0("output_data/distribution_env_data_subset/native/edaclim/species_occ_edaclim_native_",sp,".RData"))
      
    } else if (file_exists_2 == TRUE) { print("already done")
    } else if (file_exists_1 == FALSE) { print("input file not available yet")
    } # End of if condition
    
    
})} # end of try and for loop over species




#-------------------------------------------------------------------------------

# 3. Based on global occurrences and purely climatic data ----------------------

# Create a random subset of the absences to contain 10 times more absences than presences
for(sp in study_species) {
  try({
    
    print(sp)
    
    # check if joined distribution and climatic and edaphic data file already exists
    file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
    
    # check if the subsetted joined distribution and climatic and edaphic data file already exists
    file_exists_2 <- file.exists(paste0("output_data/distribution_env_data_subset/global/clim/species_occ_clim_global_",sp,".RData"))
    
    if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with subset if output 
      # of subsetted data does not exist yet and file of joint distribution and
      # environmental data exists
      
      # Load the data frame of native species occurrences joined with climate variables
      load(paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
      
      # Create a random subset of the absences to contain 10 times more absences than presences
      indices_absence <- which(species_occ_clim_global$occ == 0)
      subset_indices <- sample(indices_absence, ifelse(length(indices_absence) < sum(species_occ_clim_global$occ==1)*10, length(indices_absence), sum(species_occ_clim_global$occ==1)*10), replace = FALSE)
      combined_indices <- c(which(species_occ_clim_global$occ == 1), subset_indices)
      species_occ_clim_global <- species_occ_clim_global[combined_indices, ]
      
      # Save the subsetted data frame
      save(species_occ_clim_global, file = paste0("output_data/distribution_env_data_subset/global/clim/species_occ_clim_global_",sp,".RData"))
      
    } else if (file_exists_2 == TRUE) { print("already done")
    } else if (file_exists_1 == FALSE) { print("input file not available yet")
    } # End of if condition
    
    
})} # end of try and for loop over species



#-------------------------------------------------------------------------------

# 4. Based on global occurrences and combined climatic and edaphic data --------

# Create a random subset of the absences to contain 10 times more absences than presences
for(sp in study_species) {
  try({
    
    print(sp)
    
    # check if joined distribution and climatic and edaphic data file already exists
    file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
    
    # check if the subsetted joined distribution and climatic and edaphic data file already exists
    file_exists_2 <- file.exists(paste0("output_data/distribution_env_data_subset/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
    
    if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with subset if output 
      # of subsetted data does not exist yet and file of joint distribution and
      # environmental data exists
      
      # Load the data frame of native species occurrences joined with climate variables
      load(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
      
      # Create a random subset of the absences to contain 10 times more absences than presences
      indices_absence <- which(species_occ_edaclim_global$occ == 0)
      subset_indices <- sample(indices_absence, ifelse(length(indices_absence) < sum(species_occ_edaclim_global$occ==1)*10, length(indices_absence), sum(species_occ_edaclim_global$occ==1)*10), replace = FALSE)
      combined_indices <- c(which(species_occ_edaclim_global$occ == 1), subset_indices)
      species_occ_edaclim_global <- species_occ_edaclim_global[combined_indices, ]
      
      # Save the subsetted data frame
      save(species_occ_edaclim_global, file = paste0("output_data/distribution_env_data_subset/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
      
    } else if (file_exists_2 == TRUE) { print("already done")
    } else if (file_exists_1 == FALSE) { print("input file not available yet")
    } # End of if condition
    
    
})} # end of try and for loop over species
