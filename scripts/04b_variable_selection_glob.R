# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#        04b. Variable selection based on global occurrence data         #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages

# Load needed objects
source("scripts/functions.R") # select07_cv function
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 



#-------------------------------------------------------------------------------

# 1. Climatic data -------------------------------------------------------------

for (sp in study_species) { # Start the loop over all species
  try({ 
    
  print(sp)
    
  print("clim")
  
  # check if joined distribution and climatic and edaphic data file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
    
  # check if variable selection data file already exists
  file_exists_2 <- file.exists(paste0("output_data/variable_selection/global/clim/pred_sel_clim_global_",sp,".RData"))
    
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with variable selection if output 
  # of variable selection data does not exist yet and file of joint distribution and
  # environmental data exists
    
  print("start of process")
  
  # Load the data frame of native species occurrences joined with climate variables
  load(paste0("output_data/distribution_env_data/global/clim/species_occ_clim_global_",sp,".RData"))
  
  # Consideration of equal weights of presences and absences
  weights_clim <- ifelse(species_occ_clim_global$occ==1, 1, sum(species_occ_clim_global$occ==1) / sum(species_occ_clim_global$occ==0))
  
  # Run the variable selection using the select07_cv function based on the Boyce index
  var_sel_clim <- select07_cv_boyce(X=species_occ_clim_global[,-c(1:4)], 
                                    y=species_occ_clim_global$occ, 
                                    threshold=0.7,
                                    weights = weights_clim)
  
  # Extract the four most important and weakly correlated climate variables
  pred_sel_clim <- var_sel_clim$pred_sel
  pred_sel_clim_global <- pred_sel_clim[1:4]
  
  # Save the variables
  save(pred_sel_clim_global, file = paste0("output_data/variable_selection/global/clim/pred_sel_clim_global_",sp,".RData"))
  
  } else if (file_exists_2 == TRUE) { print("already done")
  } else if (file_exists_1 == FALSE) { print("input file not available yet")
  } # End of if condition
  
  
})} # end of try and for loop over species
  



#-------------------------------------------------------------------------------

# 2. Climatic and edaphic data -------------------------------------------------

for (sp in study_species) { # Start the loop over all species
  try({ 
    
  print(sp)
    
  print("edaclim")
  
  # check if joined distribution and climatic and edaphic data file already exists
  file_exists_1 <- file.exists(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
    
  # check if variable selection data file already exists
  file_exists_2 <- file.exists(paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData"))
    
  if (file_exists_1 == TRUE && file_exists_2 == FALSE) { # just continue with variable selection if output 
  # of variable selection data does not exist yet and file of joint distribution and
  # environmental data exists
  
  print("start of process")
    
  # Load the data frame of native species occurrences joined with climatic as well as edaphic variables
  load(paste0("output_data/distribution_env_data/global/edaclim/species_occ_edaclim_global_",sp,".RData"))
  
  # Consideration of equal weights of presences and absences
  weights_edaclim <- ifelse(species_occ_edaclim_global$occ==1, 1, sum(species_occ_edaclim_global$occ==1) / sum(species_occ_edaclim_global$occ==0))
  
  # Run the variable selection using the select07_cv function based on the Boyce index
  var_sel_edaclim <- select07_cv_boyce(X=species_occ_edaclim_global[,-c(1:4)], 
                                       y=species_occ_edaclim_global$occ, 
                                       threshold=0.7,
                                       weights = weights_edaclim)
  
  pred_sel_edaclim <- var_sel_edaclim$pred_sel
  
  # Get the names from all variables
  variables <- colnames(species_occ_edaclim_global)
  climatic_predictors <- variables[5:19] # Get the names from the climate variables
  edaphic_predictors <- variables[20:33] # Get the names from the edaphic variables
  
  # Create an empty vector of type character 
  pred_sel_edaclim_global <- character()
  
  # Loop through the selected predictors and identify the first two climate variables and store them
  for (p in pred_sel_edaclim) {
    answer1 <- any(p == climatic_predictors)
    if (answer1  == TRUE & length(pred_sel_edaclim_global) < 2) { pred_sel_edaclim_global <- append(pred_sel_edaclim_global, p)
    } else {
      next
    }
  }
  
  # Loop through the selected predictors and identify the first two edaphic variables and store them
  for (p in pred_sel_edaclim) {
    answer2 <- any(p == edaphic_predictors)
    if (answer2  == TRUE & length(pred_sel_edaclim_global) < 4) { pred_sel_edaclim_global <- append(pred_sel_edaclim_global, p)
    } else {
      next
    }
  }
  
  
  
  # Save the variables
  save(pred_sel_edaclim_global, file = paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData"))
  
  
  } else if (file_exists_2 == TRUE) { print("already done")
  } else if (file_exists_1 == FALSE) { print("input file not available yet")
  } # End of if condition
  
  
})} # end of try and for loop over species



### How often are the variables "organic carbon content" and "bulk density" selected?
# (Due to its low coverage values of the Easter Islands)

edaclim_Easter_global <- data.frame(expand.grid(species=c(paste(study_species))), organic_carbon_content=NA, 
                                    bulk_density=NA, both=NA)

for (sp in study_species) { # Start the loop over all species
  try({
    
    print(sp)
    
    file_exists <- file.exists(paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData"))
    
    if (file_exists == TRUE) {
      
      load(paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData"))
      
      if ("organic_carbon_content" %in% pred_sel_edaclim_global) {
        
        edaclim_Easter_global[edaclim_Easter_global$species == sp, "organic_carbon_content"] <- 1
        
      } else if ("bulk_density" %in% pred_sel_edaclim_global) {
        
        edaclim_Easter_global[edaclim_Easter_global$species == sp, "bulk_density"] <- 1
        
      } else if ("organic_carbon_content" %in% pred_sel_edaclim_global & "bulk_density" %in% pred_sel_edaclim_global) {
        
        edaclim_Easter_global[edaclim_Easter_global$species == sp, "both"] <- 1
        
      } else { next
      }}
    
    if (file_exists == FALSE) { next
    }
    
  })}
