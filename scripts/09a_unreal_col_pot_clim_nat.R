# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             09a. Unrealized colonization potential based on            #
#               native occurrences and purely climatic data              #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(readr)
library(stringr)




#-------------------------------------------------------------------------------

# 1. Observed occurrences in Pacific region ------------------------------------

# Create a data frame that contains the information of presence and absence of
# all study species on each considered Pacific island group

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
Wohlwend_data <- read_delim("input_data/Pacific_Invaders_GIFT_22_01.csv", 
                            delim = ";", locale = locale(decimal_mark = ",")) # Contains species presences data from Wohlwend et al. (2021)
load("input_data/spatial_data/pacific_islands_extent.RData") # Island group names and extent information

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Add an underscore to the species names to match them
Wohlwend_data$Species <- str_replace_all(Wohlwend_data$Species, " ", "_")

# Retrieve island group names (change two parts of Fiji into Fiji islands)
island_group <- as.character(pacific_islands_extent$island_group)
island_group <- island_group[!island_group %in% c("Fiji_1", "Fiji_2")]
island_group <- c(island_group, "Fiji")

# Create an empty data frame to store results
observed_occ_pacific_islands <- data.frame(matrix(ncol = length(island_group), nrow = length(study_species)))
colnames(observed_occ_pacific_islands) <- paste0(island_group)
rownames(observed_occ_pacific_islands) <- paste0(study_species)


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  # Subset the information from Wohlwend for each species
  species_info <- subset(Wohlwend_data, Wohlwend_data$Species == sp)
  
  for (i in island_group) {
    
    # Check if island group is listed for the species
    islandgroup_presence <- any(species_info$Islandgroup == i)
    
    # If the island group is listed for the species, insert a 1 (presence) in the results
    # data frame. If not, insert a 0 (absence)
    if (islandgroup_presence == TRUE) { observed_occ_pacific_islands[sp, i] <- 1
    } else if (islandgroup_presence == FALSE) { observed_occ_pacific_islands[sp, i] <- 0
    }
    
  } # End of the loop over all island groups
  
} # End of the loop over all species

# Save the results data frame
save(observed_occ_pacific_islands, file = "input_data/observed_occ_pacific_islands.RData")




#-------------------------------------------------------------------------------

# 2. Unrealized colonization potential -----------------------------------------

# Create a data frame for each species containing information if observed presences
# and absences (Wohlwend et al. 2021) coincide with predicted presences and
# absences on each island group. This is done calculating the False Positive Rate.
# Additionally the True Positives and False Positives are extracted, where: 

# a: True Positive = Species occurs on the island group which is also predicted to be suitable
# b: False Positive = Species does not occur on island group but it is predicted to be suitable -> predicted to potentially occur there
# c: False Negative = Species does occur on the island group even though it is not predicted to be suitable
# d: True Negative = Species does not occur on the island group which is also not predicted to be suitable


# (a) based on 49 island groups covered by climatic data -----------------------

# Load needed objects
load("output_data/model_predictions/native/clim/islandgroups_results_clim_native.RData") # Data frame with prediction results of all species based on 49 island groups
load("input_data/observed_occ_pacific_islands.RData") # observed occurrences
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve island group names (remove "Pacific")
islandgroup_climate <- unique(islandgroups_results_clim_native$islandgroup)
islandgroup_climate <- setdiff(islandgroup_climate, "Pacific")

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Create a data frame to store the results
unreal_col_pot_clim_native <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(unreal_col_pot_clim_native) <- c("unrealized_col_pot", "TP", "FP", "TP_and_FP", "TN", "algorithm", "predictor_type", "niche", "predictor_set", "species")

# Write a vector with the used algorithms and their ensemble
algorithm <- c("GLM", "GAM", "RF", "BRT", "Ensemble")


for (sp in study_species) { # Start of the loop over all study species
  
  print(sp)
  
  # Subset the prediction data frame by each species
  subset_species <- subset(islandgroups_results_clim_native, islandgroups_results_clim_native$species == sp)
  
  
  for (a in algorithm) { # Start the loop over all algorithms
    
    # Create a data frame to store the results per species and algorithm
    unreal_col_pot_clim_native_species <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(unreal_col_pot_clim_native_species) <- c("islandgroup", "a", "b", "c", "d", "predictor_set", "species", "algorithm")
    
    # Subset the species data frame by the algorithm
    subset_species_algorithm <- subset(subset_species, subset_species$algorithm == a)
    
    
    for (i in islandgroup_climate) { # Start of the loop over all island groups
      
      # Subset the prediction data frame of each species by each island group
      subset_species_algorithm_islandgroup <- subset(subset_species_algorithm, subset_species_algorithm$islandgroup == i)
      
      # Check if species is present or absent on the island group according to Wohlwend
      check_observation <- observed_occ_pacific_islands[sp,i]
      
      # Check if the species is predicted to be present on the island group with
      # a suitable habitat fraction values above 0, if yes a value of 1 (present)
      # is given
      check_prediction <- as.numeric(subset_species_algorithm_islandgroup[subset_species_algorithm_islandgroup$islandgroup == i, "suitable_habitat_fraction"])
      
      if (check_prediction > 0) {check_prediction <- 1 # If the predicted suitable habitat fraction is above 0, a value of 1 is given (present)
      } else if (check_prediction == 0) {check_prediction <- 0 # If not, a value of 0 is given (absent)
      }
      
      # Go through the four different possibilities of coinciding information and 
      # store the information that matches
      
      if (check_observation == 1 & check_prediction == 1) { results_col_pot <- c(i, 1, 0, 0, 0, 1, sp, a)
      } else if (check_observation == 0 & check_prediction == 0) { results_col_pot <- c(i, 0, 0, 0, 1, 1, sp, a)
      } else if (check_observation == 0 & check_prediction == 1) { results_col_pot <- c(i, 0, 1, 0, 0, 1, sp, a)
      } else if (check_observation == 1 & check_prediction == 0) { results_col_pot <- c(i, 0, 0, 1, 0, 1, sp, a) 
      }
      
      # Add the vector containing the results to the data frame
      unreal_col_pot_clim_native_species <- rbind(unreal_col_pot_clim_native_species, results_col_pot)
      
      # Make sure the column names are correct
      colnames(unreal_col_pot_clim_native_species) <- c("islandgroup", "a", "b", "c", "d", "predictor_set", "species", "algorithm")
      
      # Save the data frame for each species
      #save(unreal_col_pot_clim_native_species, file = paste0("output_data/unrealized_col_pot/native/clim/unreal_col_pot_clim_native_species_",sp,"_",a,".RData"))
      
      
      
    } # End of the loop over all island groups
    
    
    # Calculate the unrealized colonization potential (False Positive Rate) per species
    col_pot_a <- sum(as.numeric(unreal_col_pot_clim_native_species$a)) # a
    col_pot_b <- sum(as.numeric(unreal_col_pot_clim_native_species$b)) # b
    col_pot_c <- sum(as.numeric(unreal_col_pot_clim_native_species$c)) # c
    col_pot_d <- sum(as.numeric(unreal_col_pot_clim_native_species$d)) # d
    
    false_positive_rate <- round(col_pot_b/(col_pot_b + col_pot_d), 2)
    
    # Extract other measures of interest
    true_positives <- col_pot_a
    false_positives <- col_pot_b
    all_positives <- col_pot_a + col_pot_b
    true_negatives <- col_pot_d
    
    # Create a vector with results
    results_unreal_col_pot <- c(false_positive_rate, true_positives, false_positives, all_positives, true_negatives, a, "clim", "native", 1, sp)
    
    # Add result to the data frame
    unreal_col_pot_clim_native <- rbind(unreal_col_pot_clim_native, results_unreal_col_pot)
    
    # Make sure the column names are correct
    colnames(unreal_col_pot_clim_native) <- c("unrealized_col_pot", "TP", "FP", "TP_and_FP", "TN", "algorithm", "predictor_type", "niche", "predictor_set", "species")
    
    
  } # Close the loop over all algorithms
    
    
} # End of the loop over all species
    
  

# Save the results data frame containing the unrealized colonization potential 
# of all study species
save(unreal_col_pot_clim_native, file = "output_data/unrealized_col_pot/native/clim/unreal_col_pot_clim_native.RData")




# (b) based on 25 island groups covered by climatic and edaphic data -----------

# Load needed objects
load("output_data/model_predictions/native/clim_comp/islandgroups_results_clim_native_comp.RData") # Data frame with prediction results of all species based on 25 island groups
load("input_data/observed_occ_pacific_islands.RData") # observed occurrences
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve island group names (remove "Pacific")
islandgroup_climate_soil <- unique(islandgroups_results_clim_native_comp$islandgroup)
islandgroup_climate_soil <- setdiff(islandgroup_climate_soil, "Pacific")

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Create a data frame to store the results
unreal_col_pot_clim_native_comp <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(unreal_col_pot_clim_native_comp) <- c("unrealized_col_pot", "TP", "FP", "TP_and_FP", "TN", "algorithm", "predictor_type", "niche", "predictor_set", "species")

# Write a vector with the used algorithms and their ensemble
algorithm <- c("GLM", "GAM", "RF", "BRT", "Ensemble")


for (sp in study_species) { # Start of the loop over all study species
  
  print(sp)
  
  
  # Subset the prediction data frame by each species
  subset_species <- subset(islandgroups_results_clim_native_comp, islandgroups_results_clim_native_comp$species == sp)
  
  
  for (a in algorithm) { # Start the loop over all algorithms
    
    # Create a data frame to store the results per species and algorithm
    unreal_col_pot_clim_native_species_comp <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(unreal_col_pot_clim_native_species_comp) <- c("islandgroup", "a", "b", "c", "d", "predictor_set", "species", "algorithm")
    
    # Subset the species data frame by the algorithm
    subset_species_algorithm <- subset(subset_species, subset_species$algorithm == a)
    
    
    for (i in islandgroup_climate_soil) { # Start of the loop over all island groups
      
      # Subset the prediction data frame of each species by each island group
      subset_species_algorithm_islandgroup <- subset(subset_species_algorithm, subset_species_algorithm$islandgroup == i)
      
      # Check if species is present or absent on the island group according to Wohlwend
      check_observation <- observed_occ_pacific_islands[sp,i]
      
      # Check if the species is predicted to be present on the island group with
      # a suitable habitat fraction values above 0, if yes a value of 1 (present)
      # is given
      check_prediction <- as.numeric(subset_species_algorithm_islandgroup[subset_species_algorithm_islandgroup$islandgroup == i, "suitable_habitat_fraction"])
      
      if (check_prediction > 0) {check_prediction <- 1 # If the predicted suitable habitat fraction is above 0, a value of 1 is given (present)
      } else if (check_prediction == 0) {check_prediction <- 0 # If not, a value of 0 is given (absent)
      }
      
      # Go through the four different possibilities of coinciding information and 
      # store the information that matches
      
      if (check_observation == 1 & check_prediction == 1) { results_col_pot <- c(i, 1, 0, 0, 0, 1, sp, a)
      } else if (check_observation == 0 & check_prediction == 0) { results_col_pot <- c(i, 0, 0, 0, 1, 1, sp, a)
      } else if (check_observation == 0 & check_prediction == 1) { results_col_pot <- c(i, 0, 1, 0, 0, 1, sp, a)
      } else if (check_observation == 1 & check_prediction == 0) { results_col_pot <- c(i, 0, 0, 1, 0, 1, sp, a) 
      }
      
      # Add the vector containing the results to the data frame
      unreal_col_pot_clim_native_species_comp <- rbind(unreal_col_pot_clim_native_species_comp, results_col_pot)
      
      # Make sure the column names are correct
      colnames(unreal_col_pot_clim_native_species_comp) <- c("islandgroup", "a", "b", "c", "d", "predictor_set", "species", "algorithm")
      
      # Save the data frame for each species
      #save(unreal_col_pot_clim_native_species_comp, file = paste0("output_data/unrealized_col_pot/native/clim_comp/unreal_col_pot_clim_native_species_comp_",sp,"_",a,".RData"))
      
      
      
    } # End of the loop over all island groups
    
    
    # Calculate the unrealized colonization potential (False Positive Rate) per species
    col_pot_a <- sum(as.numeric(unreal_col_pot_clim_native_species_comp$a)) # a
    col_pot_b <- sum(as.numeric(unreal_col_pot_clim_native_species_comp$b)) # b
    col_pot_c <- sum(as.numeric(unreal_col_pot_clim_native_species_comp$c)) # c
    col_pot_d <- sum(as.numeric(unreal_col_pot_clim_native_species_comp$d)) # d
    
    false_positive_rate <- round(col_pot_b/(col_pot_b + col_pot_d), 2)
    
    # Extract other measures of interest
    true_positives <- col_pot_a
    false_positives <- col_pot_b
    all_positives <- col_pot_a + col_pot_b
    true_negatives <- col_pot_d
    
    # Create a vector with results
    results_unreal_col_pot <- c(false_positive_rate, true_positives, false_positives, all_positives, true_negatives, a, "clim", "native", 1, sp)
    
    # Add result to the data frame
    unreal_col_pot_clim_native_comp <- rbind(unreal_col_pot_clim_native_comp, results_unreal_col_pot)
    
    # Make sure the column names are correct
    colnames(unreal_col_pot_clim_native_comp) <- c("unrealized_col_pot", "TP", "FP", "TP_and_FP", "TN", "algorithm", "predictor_type", "niche", "predictor_set", "species")
    
    
  } # End of the loop over algorithms
  
} # End of the loop over all species
  


# Save the results data frame containing the unrealized colonization potential 
# of all study species
save(unreal_col_pot_clim_native_comp, file = "output_data/unrealized_col_pot/native/clim_comp/unreal_col_pot_clim_native_comp.RData")




