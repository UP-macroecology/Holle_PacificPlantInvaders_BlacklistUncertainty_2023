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
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_native_comp.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_native_comp.RData"))
  
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
  save(blacklists_final_native_clim_comp, file = paste0("output_data/final_blacklisting_rev/blacklists_final_native_clim_comp_",t,".RData"))
  
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  print("Final blacklisting natclim based on 49 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_suitable_habitat_fraction_clim_native.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_number_suitable_islandgroups_clim_native.RData"))
  
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
  save(blacklists_final_native_clim, file = paste0("output_data/final_blacklisting_rev/blacklists_final_native_clim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 2. Final blacklisting based on the ensemble predictions based on native
# occurrences and combined climatic and edaphic data ---------------------------
  
  print("Final blacklisting natclim+eda")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_native.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_native.RData"))
  
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
  save(blacklists_final_native_edaclim, file = paste0("output_data/final_blacklisting_rev/blacklists_final_native_edaclim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 3. Final blacklisting based on the ensemble predictions based on global
# occurrences and purely climatic data -----------------------------------------
  
# (a) based on 25 island groups covered by climatic and edaphic data -----------
  
  print("Final blacklisting globclim based on 25 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_global_comp.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_global_comp.RData"))
  
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
  save(blacklists_final_global_clim_comp, file = paste0("output_data/final_blacklisting_rev/blacklists_final_global_clim_comp_",t,".RData"))
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  print("Final blacklisting globclim based on 49 island groups")
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_suitable_habitat_fraction_clim_global.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_number_suitable_islandgroups_clim_global.RData"))
  
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
  save(blacklists_final_global_clim, file = paste0("output_data/final_blacklisting_rev/blacklists_final_global_clim_",t,".RData"))
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 4. Final blacklisting based on the ensemble predictions based on global
# occurrences and combined climatic and edaphic data ---------------------------
  
  print("Final blacklisting globclim+eda")
  
  
  # Read in the data frames containing the blacklisting results
  # Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Mean suitable habitat fraction
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Number of suitable Pacific island groups
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_global.RData"))
  
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
  save(blacklists_final_global_edaclim, file = paste0("output_data/final_blacklisting_rev/blacklists_final_global_edaclim_",t,".RData"))
  
  
} # Close the loop over the different thresholding methods




#-------------------------------------------------------------------------------

# 5. Descriptive statistics ----------------------------------------------------

# Ranking differences between predictor sets based on the used niche
blacklists_niche_clim <- merge(blacklists_final_native_clim_comp[, c(1:3)], blacklists_final_global_clim_comp[, c(1:3)], by = c("species", "blacklist"))
blacklists_niche_edaclim <- merge(blacklists_final_native_edaclim[, c(1:3)], blacklists_final_global_edaclim[, c(1:3)], by = c("species", "blacklist"))

# Total suitable habitat fraction: Niche climate
total_suitable_niche_clim <- subset(blacklists_niche_clim, blacklists_niche_clim$blacklist == "total_suitable_fraction")
total_suitable_niche_clim$difference <- abs(total_suitable_niche_clim$rank.x - total_suitable_niche_clim$rank.y)
mean(total_suitable_niche_clim$difference) # 11.77
sd(total_suitable_niche_clim$difference) # 11.1
min(total_suitable_niche_clim$difference) # 0
max(total_suitable_niche_clim$difference) # 56

# Mean suitable habitat fraction: Niche climate
mean_suitable_niche_clim <- subset(blacklists_niche_clim, blacklists_niche_clim$blacklist == "mean_suitable_fraction")
mean_suitable_niche_clim$difference <- abs(mean_suitable_niche_clim$rank.x - mean_suitable_niche_clim$rank.y)
mean(mean_suitable_niche_clim$difference) # 11.62
sd(mean_suitable_niche_clim$difference) # 11.37
min(mean_suitable_niche_clim$difference) # 0
max(mean_suitable_niche_clim$difference) # 56

# Number of suitable island groups: Niche climate
number_suitable_niche_clim <- subset(blacklists_niche_clim, blacklists_niche_clim$blacklist == "number_suitable")
number_suitable_niche_clim$difference <- abs(number_suitable_niche_clim$rank.x - number_suitable_niche_clim$rank.y)
mean(number_suitable_niche_climdifference) # 3.1
sd(number_suitable_niche_clim$difference) # 3.53
min(number_suitable_niche_clim$difference) # 0
max(number_suitable_niche_climdifference) # 13

# Total suitable habitat fraction: Niche climate + edaphic
total_suitable_niche_edaclim <- subset(blacklists_niche_edaclim, blacklists_niche_edaclim$blacklist == "total_suitable_fraction")
total_suitable_niche_edaclim$difference <- abs(total_suitable_niche_edaclim$rank.x - total_suitable_niche_edaclim$rank.y)
mean(total_suitable_niche_edaclim$difference) # 13.51
sd(total_suitable_niche_edaclim$difference) # 12.37
min(total_suitable_niche_edaclim$difference) # 0
max(total_suitable_niche_edaclim$difference) # 58

# Mean suitable habitat fraction: Niche climate + edaphic
mean_suitable_niche_edaclim <- subset(blacklists_niche_edaclim, blacklists_niche_edaclim$blacklist == "mean_suitable_fraction")
mean_suitable_niche_edaclim$difference <- abs(mean_suitable_niche_edaclim$rank.x - mean_suitable_niche_edaclim$rank.y)
mean(mean_suitable_niche_edaclim$difference) # 13.90
sd(mean_suitable_niche_edaclim$difference) # 13.4
min(mean_suitable_niche_edaclim$difference) # 0
max(mean_suitable_niche_edaclim$difference) # 56 

# Number of suitable island groups: Niche climate + edaphic
number_suitable_niche_edaclim <- subset(blacklists_niche_edaclim, blacklists_niche_edaclim$blacklist == "number_suitable")
number_suitable_niche_edaclim$difference <- abs(number_suitable_niche_edaclim$rank.x - number_suitable_niche_edaclim$rank.y)
mean(number_suitable_niche_edaclim$difference) # 3.12
sd(number_suitable_niche_edaclim$difference) # 3.45
min(number_suitable_niche_edaclim$difference) # 0
max(number_suitable_niche_edaclim$difference) # 14



# Ranking differences between predictor sets based on the used predictor type
blacklists_predictors_native <- merge(blacklists_final_native_clim_comp[, c(1:3)], blacklists_final_native_edaclim[, c(1:3)], by = c("species", "blacklist"))
blacklists_predictors_global <- merge(blacklists_final_global_clim_comp[, c(1:3)], blacklists_final_global_edaclim[, c(1:3)], by = c("species", "blacklist"))


# Total suitable habitat fraction: native
total_suitable_predictors_native <- subset(blacklists_predictors_native, blacklists_predictors_native$blacklist == "total_suitable_fraction")
total_suitable_predictors_native$difference <- abs(total_suitable_predictors_native$rank.x - total_suitable_predictors_native$rank.y)
mean(total_suitable_predictors_native$difference) # 11.23
sd(total_suitable_predictors_native$difference) # 9.54
min(total_suitable_predictors_native$difference) # 0
max(total_suitable_predictors_native$difference) # 44

# Mean suitable habitat fraction: native
mean_suitable_predictors_native <- subset(blacklists_predictors_native, blacklists_predictors_native$blacklist == "mean_suitable_fraction")
mean_suitable_predictors_native$difference <- abs(mean_suitable_predictors_native$rank.x - mean_suitable_predictors_native$rank.y)
mean(mean_suitable_predictors_native$difference) # 12.60
sd(mean_suitable_predictors_native$difference) # 10.25
min(mean_suitable_predictors_native$difference) # 0
max(mean_suitable_predictors_native$difference) # 42

# Number of suitable island groups: native
number_suitable_predictors_native <- subset(blacklists_predictors_native, blacklists_predictors_native$blacklist == "number_suitable")
number_suitable_predictors_native$difference <- abs(number_suitable_predictors_native$rank.x - number_suitable_predictors_native$rank.y)
mean(number_suitable_predictors_native$difference) # 2.94
sd(number_suitable_predictors_native$difference) # 3.42
min(number_suitable_predictors_native$difference) # 0
max(number_suitable_predictors_native$difference) # 16

# Total suitable habitat fraction: global
total_suitable_predictors_global <- subset(blacklists_predictors_global, blacklists_predictors_global$blacklist == "total_suitable_fraction")
total_suitable_predictors_global$difference <- abs(total_suitable_predictors_global$rank.x - total_suitable_predictors_global$rank.y)
mean(total_suitable_predictors_global$difference) # 9.77
sd(total_suitable_predictors_global$difference) # 9.78
min(total_suitable_predictors_global$difference) # 0
max(total_suitable_predictors_global$difference) # 47

# Mean suitable habitat fraction: global
mean_suitable_predictors_global <- subset(blacklists_predictors_global, blacklists_predictors_global$blacklist == "mean_suitable_fraction")
mean_suitable_predictors_global$difference <- abs(mean_suitable_predictors_global$rank.x - mean_suitable_predictors_global$rank.y)
mean(mean_suitable_predictors_global$difference) # 9.83
sd(mean_suitable_predictors_global$difference) # 9.41
min(mean_suitable_predictors_global$difference) # 0
max(mean_suitable_predictors_global$difference) # 43

# Number of suitable island groups: global
number_suitable_predictors_global <- subset(blacklists_predictors_global, blacklists_predictors_global$blacklist == "number_suitable")
number_suitable_predictors_global$difference <- abs(number_suitable_predictors_global$rank.x - number_suitable_predictors_global$rank.y)
mean(number_suitable_predictors_global$difference) # 2.28
sd(number_suitable_predictors_global$difference) # 3.18
min(number_suitable_predictors_global$difference) # 0
max(number_suitable_predictors_global$difference) # 11



