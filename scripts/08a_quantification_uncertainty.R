# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    8a. Quantification of uncertainty                   #
# ---------------------------------------------------------------------- #

# Load needed packages
library(randomForest)
library(dplyr)

# Create a vector containing the different thresholding methods for binarising predictions
threshold_methods <- c("maxTSS", "meanProb", "tenthPer")



  

#-------------------------------------------------------------------------------

# 1. Quantify uncertainty based on 25 island groups ----------------------------
# using the establishment risk indicators (pre-ranking results)

# Prepare three empty data frames to store the blacklisting results based on the 
# three different blacklisting methods
results_rank_suitable_fraction_comp <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_suitable_fraction_comp) <- c("suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")

results_rank_mean_suitable_fraction_comp <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_mean_suitable_fraction_comp) <- c("mean_suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")

results_rank_number_suitable_islandgroups_comp <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_number_suitable_islandgroups_comp) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")



for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  print("Start quantifying uncertainty based on 25 island groups")
  
  # Load needed objects
  # Ranking based on the Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_native_comp.RData"))
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_suitable_habitat_fraction_clim_global_comp.RData"))
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Ranking based on the mean suitable habitat fraction over all island groups
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData"))
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData"))
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData"))
  
  # Ranking based on predicted suitable number of island groups
  load(paste0("output_data/blacklists_rev/native/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_native_comp.RData"))
  load(paste0("output_data/blacklists_rev/native/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim_comp/",t,"/results_rank_number_suitable_islandgroups_clim_global_comp.RData"))
  load(paste0("output_data/blacklists_rev/global/edaclim/",t,"/results_rank_number_suitable_islandgroups_edaclim_global.RData"))
  
  
  
  
# (a) Total suitable habitat fraction ------------------------------------------
  
  # Bind the four data frames
  results_rank_suitable_fraction_comp_prep <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp,
                                                    results_rank_suitable_habitat_fraction_edaclim_native,
                                                    results_rank_suitable_habitat_fraction_clim_global_comp,
                                                    results_rank_suitable_habitat_fraction_edaclim_global)
  
  # Remove the results based on the ensemble models
  results_rank_suitable_fraction_comp_prep <- results_rank_suitable_fraction_comp_prep[results_rank_suitable_fraction_comp_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_suitable_fraction_comp_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_suitable_fraction_comp <- rbind(results_rank_suitable_fraction_comp, results_rank_suitable_fraction_comp_prep)
  
  
# (b) Mean suitable habitat fraction -------------------------------------------
  
  # Bind the four data frames
  results_rank_mean_suitable_fraction_comp_prep <- rbind(results_rank_mean_suitable_habitat_fraction_clim_native_comp,
                                                         results_rank_mean_suitable_habitat_fraction_edaclim_native,
                                                         results_rank_mean_suitable_habitat_fraction_clim_global_comp,
                                                         results_rank_mean_suitable_habitat_fraction_edaclim_global)
  
  # Remove the results based on the ensemble models
  results_rank_mean_suitable_fraction_comp_prep <- results_rank_mean_suitable_fraction_comp_prep[results_rank_mean_suitable_fraction_comp_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_mean_suitable_fraction_comp_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_mean_suitable_fraction_comp <- rbind(results_rank_mean_suitable_fraction_comp, results_rank_mean_suitable_fraction_comp_prep)
  
  
  
  
# (c) Number of suitable island groups  ----------------------------------------
  
  # Bind the four data frames
  results_rank_number_suitable_islandgroups_comp_prep <- rbind(results_rank_number_suitable_islandgroups_clim_native_comp,
                                                               results_rank_number_suitable_islandgroups_edaclim_native,
                                                               results_rank_number_suitable_islandgroups_clim_global_comp,
                                                               results_rank_number_suitable_islandgroups_edaclim_global)
  
  # Remove the results based on the ensemble models
  results_rank_number_suitable_islandgroups_comp_prep <- results_rank_number_suitable_islandgroups_comp_prep[results_rank_number_suitable_islandgroups_comp_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_number_suitable_islandgroups_comp_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_number_suitable_islandgroups_comp <- rbind(results_rank_number_suitable_islandgroups_comp, results_rank_number_suitable_islandgroups_comp_prep)

  
  
} # Close the loop over the three thresholding methods 


# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction_comp$algorithm <- as.factor(results_rank_suitable_fraction_comp$algorithm)
results_rank_suitable_fraction_comp$niche <- as.factor(results_rank_suitable_fraction_comp$niche)
results_rank_suitable_fraction_comp$predictor_type <- as.factor(results_rank_suitable_fraction_comp$predictor_type)
results_rank_suitable_fraction_comp$thresh_method <- as.factor(results_rank_suitable_fraction_comp$thresh_method)

results_rank_mean_suitable_fraction_comp$algorithm <- as.factor(results_rank_mean_suitable_fraction_comp$algorithm)
results_rank_mean_suitable_fraction_comp$niche <- as.factor(results_rank_mean_suitable_fraction_comp$niche)
results_rank_mean_suitable_fraction_comp$predictor_type <- as.factor(results_rank_mean_suitable_fraction_comp$predictor_type)
results_rank_mean_suitable_fraction_comp$thresh_method <- as.factor(results_rank_mean_suitable_fraction_comp$thresh_method)

results_rank_number_suitable_islandgroups_comp$algorithm <- as.factor(results_rank_number_suitable_islandgroups_comp$algorithm)
results_rank_number_suitable_islandgroups_comp$niche <- as.factor(results_rank_number_suitable_islandgroups_comp$niche)
results_rank_number_suitable_islandgroups_comp$predictor_type <- as.factor(results_rank_number_suitable_islandgroups_comp$predictor_type)
results_rank_number_suitable_islandgroups_comp$thresh_method <- as.factor(results_rank_number_suitable_islandgroups_comp$thresh_method)



# Convert dependent variable (establishment risk indicators) into numeric values
results_rank_suitable_fraction_comp$suitable_habitat_fraction <- as.numeric(results_rank_suitable_fraction_comp$suitable_habitat_fraction)
results_rank_mean_suitable_fraction_comp$mean_suitable_habitat_fraction <- as.numeric(results_rank_mean_suitable_fraction_comp$mean_suitable_habitat_fraction)
results_rank_number_suitable_islandgroups_comp$number_suitable_islandgroups <- as.numeric(results_rank_number_suitable_islandgroups_comp$number_suitable_islandgroups)



# (e) Build random forest models  ----------------------------------------------
# for blacklists individually

# One model for each blacklist definition
model_suitable_fraction_RF_comp <- randomForest(x = results_rank_suitable_fraction_comp[, c("algorithm", "niche", "predictor_type", "thresh_method")],
                                                y = results_rank_suitable_fraction_comp$suitable_habitat_fraction,
                                                ntree = 1000, importance = TRUE)

model_mean_suitable_fraction_RF_comp <- randomForest(x = results_rank_mean_suitable_fraction_comp[, c("algorithm", "niche", "predictor_type", "thresh_method")],
                                                     y = results_rank_mean_suitable_fraction_comp$mean_suitable_habitat_fraction,
                                                     ntree = 1000, importance = TRUE)

model_number_suitable_RF_comp <- randomForest(x = results_rank_number_suitable_islandgroups_comp[, c("algorithm", "niche", "predictor_type", "thresh_method")],
                                              y = results_rank_number_suitable_islandgroups_comp$number_suitable_islandgroups,
                                              ntree = 1000, importance = TRUE)




# (f) Save random forest models ------------------------------------------------
  
# Save the models
save(model_suitable_fraction_RF_comp, model_mean_suitable_fraction_RF_comp, model_number_suitable_RF_comp,
     file = "output_data/uncertainty_quantification_rev/models_uncertainty_RF_comp.RData")  
  




  
#-------------------------------------------------------------------------------
  
# 2. Quantify uncertainty based on 49 island groups ----------------------------
# using the establishment risk indicators (pre-ranking results)

# Prepare three empty data frames to store the blacklisting results based on the 
# three different blacklisting methods
results_rank_suitable_fraction <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_suitable_fraction) <- c("suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")

results_rank_mean_suitable_fraction <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_mean_suitable_fraction) <- c("mean_suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")

results_rank_number_suitable_islandgroups <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_rank_number_suitable_islandgroups) <- c("number_suitable_islandgroups", "algorithm", "predictor_type", "niche", "species", "rank", "thresh_method")



for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  
  print("Start quantifying uncertainty based on 49 island groups")
  
  # Load needed objects
  # Ranking based on the Pacific-wide total suitable habitat fraction
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_suitable_habitat_fraction_clim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_suitable_habitat_fraction_clim_global.RData"))
  
  # Ranking based on the mean suitable habitat fraction over all island groups
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_mean_suitable_habitat_fraction_clim_global.RData"))
  
  # Ranking based on predicted suitable number of island groups
  load(paste0("output_data/blacklists_rev/native/clim/",t,"/results_rank_number_suitable_islandgroups_clim_native.RData"))
  load(paste0("output_data/blacklists_rev/global/clim/",t,"/results_rank_number_suitable_islandgroups_clim_global.RData"))
  
  
  
  
  # (a) Total suitable habitat fraction ------------------------------------------
  
  # Bind the two data frames
  results_rank_suitable_fraction_prep <- rbind(results_rank_suitable_habitat_fraction_clim_native,
                                               results_rank_suitable_habitat_fraction_clim_global)
  
  # Remove the results based on the ensemble models
  results_rank_suitable_fraction_prep <- results_rank_suitable_fraction_prep[results_rank_suitable_fraction_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_suitable_fraction_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_suitable_fraction <- rbind(results_rank_suitable_fraction, results_rank_suitable_fraction_prep)
  
  
  
  # (b) Mean suitable habitat fraction -------------------------------------------
  
  # Bind the two data frames
  results_rank_mean_suitable_fraction_prep <- rbind(results_rank_mean_suitable_habitat_fraction_clim_native,
                                                    results_rank_mean_suitable_habitat_fraction_clim_global)
  
  # Remove the results based on the ensemble models
  results_rank_mean_suitable_fraction_prep <- results_rank_mean_suitable_fraction_prep[results_rank_mean_suitable_fraction_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_mean_suitable_fraction_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_mean_suitable_fraction <- rbind(results_rank_mean_suitable_fraction, results_rank_mean_suitable_fraction_prep)
  
  
  
  # (c) Number of suitable island groups  ----------------------------------------
  
  # Bind the two data frames
  results_rank_number_suitable_islandgroups_prep <- rbind(results_rank_number_suitable_islandgroups_clim_native,
                                                          results_rank_number_suitable_islandgroups_clim_global)
  
  # Remove the results based on the ensemble models
  results_rank_number_suitable_islandgroups_prep <- results_rank_number_suitable_islandgroups_prep[results_rank_number_suitable_islandgroups_prep$algorithm != "Ensemble", ]
  
  # Add a column indicating the applied thresholding method
  results_rank_number_suitable_islandgroups_prep$thresh_method <- t
  
  # Bind the resulting data frame to the prepared data frame
  results_rank_number_suitable_islandgroups <- rbind(results_rank_number_suitable_islandgroups, results_rank_number_suitable_islandgroups_prep)
  
  
} # Close the loop over the three thresholding methods



# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction$algorithm <- as.factor(results_rank_suitable_fraction$algorithm)
results_rank_suitable_fraction$niche <- as.factor(results_rank_suitable_fraction$niche)
results_rank_suitable_fraction$thresh_method <- as.factor(results_rank_suitable_fraction$thresh_method)

results_rank_mean_suitable_fraction$algorithm <- as.factor(results_rank_mean_suitable_fraction$algorithm)
results_rank_mean_suitable_fraction$niche <- as.factor(results_rank_mean_suitable_fraction$niche)
results_rank_mean_suitable_fraction$thresh_method <- as.factor(results_rank_mean_suitable_fraction$thresh_method)

results_rank_number_suitable_islandgroups$algorithm <- as.factor(results_rank_number_suitable_islandgroups$algorithm)
results_rank_number_suitable_islandgroups$niche <- as.factor(results_rank_number_suitable_islandgroups$niche)
results_rank_number_suitable_islandgroups$thresh_method <- as.factor(results_rank_number_suitable_islandgroups$thresh_method)



# Convert dependent variable (establishment risk indicators) into numeric values
results_rank_suitable_fraction$suitable_habitat_fraction <- as.numeric(results_rank_suitable_fraction$suitable_habitat_fraction)
results_rank_mean_suitable_fraction$mean_suitable_habitat_fraction <- as.numeric(results_rank_mean_suitable_fraction$mean_suitable_habitat_fraction)
results_rank_number_suitable_islandgroups$number_suitable_islandgroups <- as.numeric(results_rank_number_suitable_islandgroups$number_suitable_islandgroups)



# (e) Build random forest models  ----------------------------------------------

# One model for each blacklist definition
model_suitable_fraction_RF <- randomForest(x = results_rank_suitable_fraction[, c("algorithm", "niche", "thresh_method")],
                                           y = results_rank_suitable_fraction$suitable_habitat_fraction,
                                           ntree = 1000, importance = TRUE)

model_mean_suitable_fraction_RF <- randomForest(x = results_rank_mean_suitable_fraction[, c("algorithm", "niche", "thresh_method")],
                                                y = results_rank_mean_suitable_fraction$mean_suitable_habitat_fraction,
                                                ntree = 1000, importance = TRUE)

model_number_suitable_RF <- randomForest(x = results_rank_number_suitable_islandgroups[, c("algorithm", "niche", "thresh_method")],
                                         y = results_rank_number_suitable_islandgroups$number_suitable_islandgroups,
                                         ntree = 1000, importance = TRUE)





# (f) Save random forest models ------------------------------------------------

# Save the models
save(model_suitable_fraction_RF, model_mean_suitable_fraction_RF, model_number_suitable_RF,
     file = "output_data/uncertainty_quantification_rev/models_uncertainty_RF.RData")





#-------------------------------------------------------------------------------

# 3. Quantify uncertainty based on 25 island groups ----------------------------
# using the ranking results
# Assess the ranking differences caused only by one uncertainty factor while 
# keeping the other factors constant
# For each species and each combination of the other factors, compare the rankings
# across the different studied factor levels

# (a) Total suitable habitat fraction ------------------------------------------
# Studied factor: Algorithm
algo_diff <- results_rank_suitable_fraction_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Predictor type
pred_diff <- results_rank_suitable_fraction_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(diff_pred = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_pred = mean(diff_pred, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_suitable_fraction_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_suitable_fraction_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_total_suitable_fraction_comp <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                         mean_rank_diff_predictortype = pred_diff$mean_diff_pred,
                                                         mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                         mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)


# (b) Mean suitable habitat fraction -------------------------------------------
# Studied factor: Algorithm
algo_diff <- results_rank_mean_suitable_fraction_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Predictor type
pred_diff <- results_rank_mean_suitable_fraction_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(diff_pred = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_pred = mean(diff_pred, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_mean_suitable_fraction_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_mean_suitable_fraction_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_mean_suitable_fraction_comp <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                        mean_rank_diff_predictortype = pred_diff$mean_diff_pred,
                                                        mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                        mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)



# (c) Number of suitable island groups  ----------------------------------------
algo_diff <- results_rank_number_suitable_islandgroups_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Predictor type
pred_diff <- results_rank_number_suitable_islandgroups_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(diff_pred = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_pred = mean(diff_pred, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_number_suitable_islandgroups_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_number_suitable_islandgroups_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_number_suitable_islandgroups_comp <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                              mean_rank_diff_predictortype = pred_diff$mean_diff_pred,
                                                              mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                              mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)




# (d) Create results data frame  -----------------------------------------------

# Bind the three data frames with mean ranking differences per factor while
# holding all else factors constant
summary_ranking_differences_factors_comp <- rbind(summary_table_total_suitable_fraction_comp,
                                                  summary_table_mean_suitable_fraction_comp,
                                                  summary_table_number_suitable_islandgroups_comp)

# Add the blacklist definition as row names
rownames(summary_ranking_differences_factors_comp) <- c("Total fraction", "Mean fraction", "Island groups")


# (e) Save results data frame  -------------------------------------------------
save(summary_ranking_differences_factors_comp, file = "output_data/uncertainty_quantification_rev/summary_ranking_differences_factors_comp.RData")





#-------------------------------------------------------------------------------

# 4. Quantify uncertainty based on 49 island groups ----------------------------
# using the ranking results
# Assess the ranking differences caused only by one uncertainty factor while 
# keeping the other factors constant
# For each species and each combination of the other factors, compare the rankings
# across the different studied factor levels

# (a) Total suitable habitat fraction ------------------------------------------
# Studied factor: Algorithm
algo_diff <- results_rank_suitable_fraction %>%
  group_by(species, niche, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_suitable_fraction %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_suitable_fraction %>%
  group_by(species, algorithm, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_total_suitable_fraction <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                    mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                    mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)


# (b) Mean suitable habitat fraction -------------------------------------------
# Studied factor: Algorithm
algo_diff <- results_rank_mean_suitable_fraction %>%
  group_by(species, niche, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_mean_suitable_fraction %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_mean_suitable_fraction %>%
  group_by(species, algorithm, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_mean_suitable_fraction <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                   mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                   mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)



# (c) Number of suitable island groups  ----------------------------------------
algo_diff <- results_rank_number_suitable_islandgroups %>%
  group_by(species, niche, thresh_method) %>%  
  summarise(diff_algo = max(rank) - min(rank), .groups = "drop") %>% 
  summarise(mean_diff_algo = mean(diff_algo, na.rm = TRUE))

# Studied factor: Niche
niche_diff <- results_rank_number_suitable_islandgroups %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(diff_niche = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_niche = mean(diff_niche, na.rm = TRUE))

# Studied factor: Thresholding method
thresh_diff <- results_rank_number_suitable_islandgroups %>%
  group_by(species, algorithm, niche) %>%
  summarise(diff_thresh = max(rank) - min(rank), .groups = "drop") %>%
  summarise(mean_diff_thresh = mean(diff_thresh, na.rm = TRUE))

# Summarise the tables
summary_table_number_suitable_islandgroups <- data.frame(mean_rank_diff_algorithm = algo_diff$mean_diff_algo,
                                                         mean_rank_diff_niche = niche_diff$mean_diff_niche,
                                                         mean_rank_diff_threshold = thresh_diff$mean_diff_thresh)


# (d) Create results data frame  -----------------------------------------------

# Bind the three data frames with mean ranking differences per factor while
# holding all ense factors constant
summary_ranking_differences_factors <- rbind(summary_table_total_suitable_fraction,
                                             summary_table_mean_suitable_fraction,
                                             summary_table_number_suitable_islandgroups)

# Add the blacklist definition as row names
rownames(summary_ranking_differences_factors) <- c("Total fraction", "Mean fraction", "Island groups")


# (e) Save results data frame  -------------------------------------------------
save(summary_ranking_differences_factors, file = "output_data/uncertainty_quantification_rev/summary_ranking_differences_factors.RData")






#-------------------------------------------------------------------------------

# 5. Uncertainty analysis based on 25 island groups ----------------------------
# using grouped ranking results (top 10, top 20, top 30, top 40, top 50, top 60, rest)
# Assess the number of cases when species were ranked into different groupings
# du to the different uncertainty factors (the rank differences caused 
# only by one factor while keeping the other factors constant when grouping the data

# (a) Total suitable habitat fraction ------------------------------------------
# Define rank groups
results_rank_suitable_fraction_groups_comp <- results_rank_suitable_fraction_comp %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_suitable_fraction_groups_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Predictor type
pred_group_shifts <- results_rank_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


pred_group_change_summary <- pred_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Niche
niche_group_shifts <- results_rank_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_total_suitable_fraction_comp <- rbind(algo_group_change_summary,
                                                    pred_group_change_summary,
                                                    niche_group_change_summary,
                                                    thresh_group_change_summary)
                                                   
# Add the uncertainty factors as information
summary_table_total_suitable_fraction_comp$factor <- c("Algorithm", "Predictor type", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_total_suitable_fraction_comp$blacklist_definition <- "Total_suitable_habitat"



# (b) Mean suitable habitat fraction -------------------------------------------
# Define rank groups
results_rank_mean_suitable_fraction_groups_comp <- results_rank_mean_suitable_fraction_comp %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_mean_suitable_fraction_groups_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Predictor type
pred_group_shifts <- results_rank_mean_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


pred_group_change_summary <- pred_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Niche
niche_group_shifts <- results_rank_mean_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_mean_suitable_fraction_groups_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_mean_suitable_fraction_comp <- rbind(algo_group_change_summary,
                                                   pred_group_change_summary,
                                                   niche_group_change_summary,
                                                   thresh_group_change_summary)

# Add the uncertainty factors as information
summary_table_mean_suitable_fraction_comp$factor <- c("Algorithm", "Predictor type", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_mean_suitable_fraction_comp$blacklist_definition <- "Mean_suitable_habitat"



# (c) Number of suitable island groups  ----------------------------------------

# Define rank groups
results_rank_number_suitable_islandgroups_groups_comp <- results_rank_number_suitable_islandgroups_comp %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_number_suitable_islandgroups_groups_comp %>%
  group_by(species, niche, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Predictor type
pred_group_shifts <- results_rank_number_suitable_islandgroups_groups_comp %>%
  group_by(species, algorithm, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


pred_group_change_summary <- pred_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Niche
niche_group_shifts <- results_rank_number_suitable_islandgroups_groups_comp %>%
  group_by(species, algorithm, predictor_type, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_number_suitable_islandgroups_groups_comp %>%
  group_by(species, algorithm, predictor_type, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_number_suitable_islandgroups_comp <- rbind(algo_group_change_summary,
                                                         pred_group_change_summary,
                                                         niche_group_change_summary,
                                                         thresh_group_change_summary)

# Add the uncertainty factors as information
summary_table_number_suitable_islandgroups_comp$factor <- c("Algorithm", "Predictor type", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_number_suitable_islandgroups_comp$blacklist_definition <- "Number_suitable_islandgroups"



# (d) Create results data frame  -----------------------------------------------
# Bind the data frames of rank group changes for the three blacklist definitions
summary_ranking_group_differences_factors_comp <- rbind(summary_table_total_suitable_fraction_comp,
                                                        summary_table_mean_suitable_fraction_comp,
                                                        summary_table_number_suitable_islandgroups_comp)



# (e) Save results data frame  -----------------------------------------------
save(summary_ranking_group_differences_factors_comp, file = "output_data/uncertainty_quantification_rev/summary_ranking_group_differences_factors_comp.RData")






#-------------------------------------------------------------------------------

# 6. Uncertainty analysis based on 49 island groups ----------------------------
# using grouped ranking results (top 10, top 20, top 30, top 40, top 50, top 60, rest)
# Assess the number of cases when species were ranked into different groupings
# du to the different uncertainty factors (the rank differences caused 
# only by one factor while keeping the other factors constant when grouping the data

# (a) Total suitable habitat fraction ------------------------------------------
# Define rank groups
results_rank_suitable_fraction_groups <- results_rank_suitable_fraction %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_suitable_fraction_groups %>%
  group_by(species, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Studied factor: Niche
niche_group_shifts <- results_rank_suitable_fraction_groups %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_suitable_fraction_groups %>%
  group_by(species, algorithm, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_total_suitable_fraction <- rbind(algo_group_change_summary,
                                               niche_group_change_summary,
                                               thresh_group_change_summary)

# Add the uncertainty factors as information
summary_table_total_suitable_fraction$factor <- c("Algorithm", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_total_suitable_fraction$blacklist_definition <- "Total_suitable_habitat"



# (b) Mean suitable habitat fraction -------------------------------------------
# Define rank groups
results_rank_mean_suitable_fraction_groups <- results_rank_mean_suitable_fraction %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_mean_suitable_fraction_groups %>%
  group_by(species, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )



# Studied factor: Niche
niche_group_shifts <- results_rank_mean_suitable_fraction_groups %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_mean_suitable_fraction_groups %>%
  group_by(species, algorithm, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_mean_suitable_fraction <- rbind(algo_group_change_summary,
                                              niche_group_change_summary,
                                              thresh_group_change_summary)

# Add the uncertainty factors as information
summary_table_mean_suitable_fraction$factor <- c("Algorithm", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_mean_suitable_fraction$blacklist_definition <- "Mean_suitable_habitat"



# (c) Number of suitable island groups  ----------------------------------------

# Define rank groups
results_rank_number_suitable_islandgroups_groups <- results_rank_number_suitable_islandgroups %>%
  mutate(rank_group = case_when(
    rank >= 1 & rank <= 10 ~ "Top 10",
    rank >= 11 & rank <= 20 ~ "Top 20",
    rank >= 21 & rank <= 30 ~ "Top 30",
    rank >= 31 & rank <= 40 ~ "Top 40",
    rank >= 41 & rank <= 50 ~ "Top 50",
    rank >= 51 & rank <= 60 ~ "Top 60",
    rank >= 61 & rank <= 70 ~ "Top 70",
    rank >= 71 & rank <= 80 ~ "Top 80",
    rank >= 81 & rank <= 82 ~ "Rest"))

# Studied factor: Algorithm
# Calculate 1. the number of different ranking groups within the studied factor
# Calculate 2. the share of cases where we have no change in grouping, and share 
# of cases, in which we have a change in grouping
algo_group_shifts <- results_rank_number_suitable_islandgroups_groups %>%
  group_by(species, niche, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


algo_group_change_summary <- algo_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Studied factor: Niche
niche_group_shifts <- results_rank_number_suitable_islandgroups_groups %>%
  group_by(species, algorithm, thresh_method) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


niche_group_change_summary <- niche_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )

# Studied factor: Thresholding method
thresh_group_shifts <- results_rank_number_suitable_islandgroups_groups %>%
  group_by(species, algorithm, niche) %>%
  summarise(
    n_groups = n_distinct(rank_group),
    .groups = "drop"
  )


thresh_group_change_summary <- thresh_group_shifts %>%
  summarise(
    total = n(),
    no_change = sum(n_groups == 1),
    change = sum(n_groups > 1),
    share_no_change = no_change / total,
    share_change = change / total
  )


# Summarise the tables
summary_table_number_suitable_islandgroups <- rbind(algo_group_change_summary,
                                                    niche_group_change_summary,
                                                    thresh_group_change_summary)

# Add the uncertainty factors as information
summary_table_number_suitable_islandgroups$factor <- c("Algorithm", "Niche", "Threshold")

# Add the blacklist definition as column
summary_table_number_suitable_islandgroups$blacklist_definition <- "Number_suitable_islandgroups"



# (d) Create results data frame  -----------------------------------------------
# Bind the data frames of rank group changes for the three blacklist definitions
summary_ranking_group_differences_factors <- rbind(summary_table_total_suitable_fraction,
                                                   summary_table_mean_suitable_fraction,
                                                   summary_table_number_suitable_islandgroups)



# (e) Save results data frame  -----------------------------------------------
save(summary_ranking_group_differences_factors, file = "output_data/uncertainty_quantification_rev/summary_ranking_group_differences_factors.RData")




