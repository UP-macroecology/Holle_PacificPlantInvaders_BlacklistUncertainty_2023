# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                        10a. Final blacklisting                         #
# ---------------------------------------------------------------------- #




# Create a vector containing the different thresholding methods for binarising predictions
threshold_methods <- c("maxTSS", "meanProb", "tenthPer")


for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  
#-------------------------------------------------------------------------------
  
# 1. Final blacklisting based on the ensemble predictions based on native
# occurrences and purely climatic data -----------------------------------------
  
# (a) based on 25 island groups covered by climatic and edaphic data -----------
  
  print("Final blacklisting natclim based on 25 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/native/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_native_comp.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/native/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/native/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_native_comp.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_native_comp, results_rank_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native_comp, results_rank_mean_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_clim_native_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_native_comp, results_rank_number_suitable_islandgroups_clim_native_comp$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_native_comp_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_native_comp_ens$blacklist <- "number_suitable"
  
  # Bind the data frames
  blacklists_final_native_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                             results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                             results_rank_number_suitable_islandgroups_clim_native_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_native_clim_comp) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_native_clim_comp, file = paste0("output_data/final_blacklisting/blacklists_final_native_clim_comp_",t,".RData"))
  
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  print("Final blacklisting natclim based on 49 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/native/clim/",t,"/results_rank_suitable_habitat_fraction_clim_native.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/native/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/native/clim/",t,"/results_rank_number_suitable_islandgroups_clim_native.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_suitable_habitat_fraction_clim_native, results_rank_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native, results_rank_mean_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_clim_native_ens <- subset(results_rank_number_suitable_islandgroups_clim_native, results_rank_number_suitable_islandgroups_clim_native$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_native_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_native_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_native_ens$blacklist <- "number_suitable"
  
  
  
  # Bind the data frames
  blacklists_final_native_clim <- rbind(results_rank_suitable_habitat_fraction_clim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                        results_rank_mean_suitable_habitat_fraction_clim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                        results_rank_number_suitable_islandgroups_clim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_native_clim) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_native_clim, file = paste0("output_data/final_blacklisting/blacklists_final_native_clim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 2. Final blacklisting based on the ensemble predictions based on native
# occurrences and combined climatic and edaphic data ---------------------------
  
  print("Final blacklisting natclim+eda")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/native/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_native.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/native/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/native/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_native.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_native, results_rank_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_native, results_rank_mean_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_edaclim_native_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_native, results_rank_number_suitable_islandgroups_edaclim_native$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_edaclim_native_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_edaclim_native_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_edaclim_native_ens$blacklist <- "number_suitable"
  
  
  # Bind the data frames
  blacklists_final_native_edaclim <- rbind(results_rank_suitable_habitat_fraction_edaclim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                           results_rank_mean_suitable_habitat_fraction_edaclim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                           results_rank_number_suitable_islandgroups_edaclim_native_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_native_edaclim) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_native_edaclim, file = paste0("output_data/final_blacklisting/blacklists_final_native_edaclim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 3. Final blacklisting based on the ensemble predictions based on global
# occurrences and purely climatic data -----------------------------------------
  
# (a) based on 25 island groups covered by climatic and edaphic data -----------
  
  print("Final blacklisting globclim based on 25 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/global/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_global_comp.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/global/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/global/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_global_comp.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_clim_global_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_global_comp_ens$blacklist <- "number_suitable"
  
  # Bind the data frames
  blacklists_final_global_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_global_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                             results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                             results_rank_number_suitable_islandgroups_clim_global_comp_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_global_clim_comp) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_global_clim_comp, file = paste0("output_data/final_blacklisting/blacklists_final_global_clim_comp_",t,".RData"))
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  print("Final blacklisting globclim based on 49 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/global/clim/",t,"/results_rank_suitable_habitat_fraction_clim_global.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/global/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/global/clim/",t,"/results_rank_number_suitable_islandgroups_clim_global.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_suitable_habitat_fraction_clim_global, results_rank_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global, results_rank_mean_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_clim_global_ens <- subset(results_rank_number_suitable_islandgroups_clim_global, results_rank_number_suitable_islandgroups_clim_global$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_global_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_global_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_global_ens$blacklist <- "number_suitable"
  
  # Bind the data frames
  blacklists_final_global_clim <- rbind(results_rank_suitable_habitat_fraction_clim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                        results_rank_mean_suitable_habitat_fraction_clim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                        results_rank_number_suitable_islandgroups_clim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_global_clim) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_global_clim, file = paste0("output_data/final_blacklisting/blacklists_final_global_clim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 4. Final blacklisting based on the ensemble predictions based on global
# occurrences and combined climatic and edaphic data ---------------------------
  
  print("Final blacklisting globclim+eda")
  
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists/global/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists/global/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists/global/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_global.RData"))
  
  # Solely retain the results based on ensemble models
  results_rank_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
  results_rank_mean_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_global, results_rank_mean_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
  results_rank_number_suitable_islandgroups_edaclim_global_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == "Ensemble")
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_edaclim_global_ens$blacklist <- "number_suitable"
  
  # Bind the data frames
  blacklists_final_global_edaclim <- rbind(results_rank_suitable_habitat_fraction_edaclim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")], 
                                           results_rank_mean_suitable_habitat_fraction_edaclim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")],
                                           results_rank_number_suitable_islandgroups_edaclim_global_ens[,c("predictor_type", "niche", "species", "rank", "blacklist")])
  
  # Make sure the column names are correct
  colnames(blacklists_final_global_edaclim) <- c("predictor_type", "niche", "species", "rank", "blacklist")
  
  # Save the data frame containing final blacklists
  save(blacklists_final_global_edaclim, file = paste0("output_data/final_blacklisting/blacklists_final_global_edaclim_",t,".RData"))
  
  
} # Close the loop over the different thresholding methods


