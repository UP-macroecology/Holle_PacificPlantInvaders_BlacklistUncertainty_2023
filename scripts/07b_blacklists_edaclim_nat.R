# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                  07b. Blacklisting based on native occurrences         #
#                    and combined climatic and edaphic data              #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(dplyr)

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
load("output_data/model_predictions/native/edaclim/islandgroups_results_edaclim_native.RData") # Contains area calculations based on predictions to 25 island groups


#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species))

# Retrieve the island group names included in a purely climatic analysis
islandgroup_climate_soil <- unique(islandgroups_results_edaclim_native$islandgroup)
islandgroup_climate_soil <- setdiff(islandgroup_climate_soil, "Pacific")

# Write a vector with the used algorithms
algorithm <- c("GLM", "GAM", "RF", "BRT", "Ensemble")


#-------------------------------------------------------------------------------

# 2. Blacklist based on Pacific-wide suitable habitat fraction -----------------
# (considering the different algorithms)


# Subset the prediction results data frame to exclusively obtain the Pacific-wide predictions
Pacific_suitable_habitat_fraction_edaclim_native <- subset(islandgroups_results_edaclim_native, islandgroups_results_edaclim_native$islandgroup == "Pacific")

# Create a data frame to store the results
results_rank_suitable_habitat_fraction_edaclim_native <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(results_rank_suitable_habitat_fraction_edaclim_native) <- c("suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Subset the data frame by each algorithm
  subset_algorithm <- subset(Pacific_suitable_habitat_fraction_edaclim_native, Pacific_suitable_habitat_fraction_edaclim_native$algorithm == a)
  
  # Rank the species by their predicted Pacific-wide suitable habitat fraction
  subset_algorithm$rank <- dense_rank(desc(as.numeric(subset_algorithm$suitable_habitat_fraction)))
  
  # Just keep the relevant data frame columns
  subset_algorithm_keep <- subset_algorithm[, c(4:9)]
  
  # Bind the data frame with the resulting rank information to the results data frame
  results_rank_suitable_habitat_fraction_edaclim_native <- rbind(results_rank_suitable_habitat_fraction_edaclim_native, subset_algorithm_keep)
  
  
} # End the loop over all algorithms

# Make sure the column names are correct
colnames(results_rank_suitable_habitat_fraction_edaclim_native) <- c("suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank")

# Save the final data frame with ranks
save(results_rank_suitable_habitat_fraction_edaclim_native, file = "output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")




#-------------------------------------------------------------------------------

# 3. Blacklist based on Pacific-wide number of suitable island groups ----------
# (considering the different algorithms)


# Create a data frame to store the results
results_number_suitable_islandgroups_edaclim_native <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_number_suitable_islandgroups_edaclim_native) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species")


for (sp in study_species) { # Start loop over all species
  
  # Subset data frame for each species
  subset_species <- subset(islandgroups_results_edaclim_native, islandgroups_results_edaclim_native$species == sp)
  
  
  for (a in algorithm) { # Start loop over all algorithms
    
    # Create a data frame to store results for each species over the island groups
    species_alg_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(species_alg_df) <- c("islandgroup", "prediction", "algorithm", "species")
    
    # Subset the data frame  of area calculations by each algorithm
    subset_species_algorithm <- subset(subset_species, subset_species$algorithm == a) 
    
    
    for (i in islandgroup_climate_soil) { # Start loop over all island groups
      
      # Get the value of predicted suitable habitat fraction
      subset_species_algorithm_islandgroup <- as.numeric(subset_species_algorithm[subset_species_algorithm$islandgroup == i, "suitable_habitat_fraction"])
      
      # Check if the predicted suitable island group fraction is > 0; if yes, 
      # it gets the value 1 (predicted to be suitable), if not the value 0 (predicted to be unsuitable)
      if (subset_species_algorithm_islandgroup > 0) { check_prediction <- 1
      } else { check_prediction <- 0
      }
      
      # Create vector with results
      result_species_alg_df <- c(i, check_prediction, a, sp)
      
      # Add result vector to data frame
      species_alg_df <- rbind(species_alg_df, result_species_alg_df)
      
      # Make sure the data frame has the correct column names
      colnames(species_alg_df) <- c("islandgroup", "prediction", "algorithm", "species")
      
    } # End loop over all island groups
    
    # Sum up the number of island groups with predicted suitable area per species
    number_islandgroups <- sum(as.numeric(species_alg_df$prediction))
    
    # Create a results vector with number of predicted suitable island groups
    results_number_islandgroups <- c(number_islandgroups, a, "edaclim", "native", sp)
    
    # Add the results vector to the data frame
    results_number_suitable_islandgroups_edaclim_native <- rbind(results_number_suitable_islandgroups_edaclim_native, results_number_islandgroups)
    
    # Make sure the column names are correct
    colnames(results_number_suitable_islandgroups_edaclim_native) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species")
    
    
  } # End loop over all algorithms
  
  
  
} # End loop over all species

# Calculate the rank per species and algorithm based on the number of 
# predicted suitable island groups

# Create a data frame to store results
results_rank_number_suitable_islandgroups_edaclim_native <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(results_rank_number_suitable_islandgroups_edaclim_native) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species", "rank")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Subset the data frame containing the number of suitable island groups by algorithm
  subset_algorithm <- subset(results_number_suitable_islandgroups_edaclim_native, results_number_suitable_islandgroups_edaclim_native$algorithm == a)
  
  # Rank the species per algorithm by their predicted suitable number of island groups
  subset_algorithm$rank <- dense_rank(desc(as.numeric(subset_algorithm$number_suitable_islandgroups)))
  
  # Add the data frame including rank to the results data frame
  results_rank_number_suitable_islandgroups_edaclim_native <- rbind(results_rank_number_suitable_islandgroups_edaclim_native, subset_algorithm)
  
} # End loop over all algorithms

# Make sure that the column names are correct 
colnames(results_rank_number_suitable_islandgroups_edaclim_native) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species", "rank")

# save the resulting data frame
save(results_rank_number_suitable_islandgroups_edaclim_native, file = "output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")




#-------------------------------------------------------------------------------

# 4. Blacklist based on mean rank over all Pacific island groups ---------------
# (considering the different algorithms)


# Create a data frame to store the results
islandgroups_rank_edaclim_native <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(islandgroups_rank_edaclim_native) <- c("islandgroup", "algorithm", "predictor_type", "niche", "species", "rank")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Subset the data frame  of area calculations by each algorithm
  subset_algorithm <- subset(islandgroups_results_edaclim_native, islandgroups_results_edaclim_native$algorithm == a)
  
  for (i in islandgroup_climate_soil) { # Start the loop over all island groups
    
    # Subset the data frame for each island group
    subset_algorithm_islandgroup <- subset(subset_algorithm, subset_algorithm$islandgroup == i)
    
    # Calculate the rank for each species based on the predicted suitable island group fraction
    subset_algorithm_islandgroup$rank <- dense_rank(desc(as.numeric(subset_algorithm_islandgroup$suitable_habitat_fraction)))
    
    # Solely retain relevant columns
    subset_algorithm_islandgroup_rank <- subset_algorithm_islandgroup[, -c(2:4)]
    
    # Add the resulting data frames
    islandgroups_rank_edaclim_native <- rbind(islandgroups_rank_edaclim_native, subset_algorithm_islandgroup_rank)
    
    
  } # End loop over all island groups
  
} # End the loop over all algorithms

# Make sure column names are correct 
colnames(islandgroups_rank_edaclim_native) <- c("islandgroup", "algorithm", "predictor_type", "niche", "species", "rank")


# After obtaining the ranks per island group for all species, the mean rank per species
# is calculated

# Create a data frame to store results
results_mean_rank_islandgroups_edaclim_native <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_mean_rank_islandgroups_edaclim_native) <- c("rank", "algorithm", "predictor_type", "niche", "species")


for (sp in study_species) { # Start the loop over all species
  
  # Subset the data frame by species
  subset_species <- subset(islandgroups_rank_edaclim_native, islandgroups_rank_edaclim_native$species == sp)
  
  
  for (a in algorithm) { # Start the loop over all algorithms
    
    # Subset the data frame by algorithm
    subset_species_algorithm <- subset(subset_species, subset_species$algorithm == a)
    
    # Calculate the mean of all the rank values over all island groups per species
    mean_rank <- round(colMeans(subset_species_algorithm["rank"]), 2)
    
    # Create a vector that contains all result information
    results_mean_rank <- c(mean_rank, a, "edaclim", "native", sp)
    
    # Add the results vector to the data frame
    results_mean_rank_islandgroups_edaclim_native <- rbind(results_mean_rank_islandgroups_edaclim_native, results_mean_rank)
    
    
  } # End loop over all algorithms
  
} # End loop over all species

# Make sure all column names are correct
colnames(results_mean_rank_islandgroups_edaclim_native) <- c("rank", "algorithm", "predictor_type", "niche", "species")

# Save the resulting data frame
save(results_mean_rank_islandgroups_edaclim_native, file = "output_data/blacklists/native/edaclim/results_mean_rank_islandgroups_edaclim_native.RData")



