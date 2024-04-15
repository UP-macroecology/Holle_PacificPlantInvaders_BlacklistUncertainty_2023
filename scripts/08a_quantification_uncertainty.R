# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    8a. Quantification of uncertainty                   #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(randomForest)




#-------------------------------------------------------------------------------

# 1. Quantify uncertainty based on 25 island groups ----------------------------

# Load needed objects
# Ranking based on the Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_global.RData")

# Ranking based on the mean suitable habitat fraction over all island groups
load("output_data/blacklists/native/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData")

# Ranking based on predicted suitable number of island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_global.RData")




# (a) Total suitable habitat fraction ------------------------------------------

# Bind the four data frames
results_rank_suitable_fraction_comp <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp,
                                             results_rank_suitable_habitat_fraction_edaclim_native,
                                             results_rank_suitable_habitat_fraction_clim_global_comp,
                                             results_rank_suitable_habitat_fraction_edaclim_global)

# Remove the results based on the ensemble models
results_rank_suitable_fraction_comp <- results_rank_suitable_fraction_comp[results_rank_suitable_fraction_comp$algorithm != "Ensemble", ]


# (b) Mean suitable habitat fraction -------------------------------------------

# Bind the four data frames
results_rank_mean_suitable_fraction_comp <- rbind(results_rank_mean_suitable_habitat_fraction_clim_native_comp,
                                                  results_rank_mean_suitable_habitat_fraction_edaclim_native,
                                                  results_rank_mean_suitable_habitat_fraction_clim_global_comp,
                                                  results_rank_mean_suitable_habitat_fraction_edaclim_global)

# Remove the results based on the ensemble models
results_rank_mean_suitable_fraction_comp <- results_rank_mean_suitable_fraction_comp[results_rank_mean_suitable_fraction_comp$algorithm != "Ensemble", ]



# (c) Number of suitable island groups  ----------------------------------------

# Bind the four data frames
results_rank_number_suitable_islandgroups_comp <- rbind(results_rank_number_suitable_islandgroups_clim_native_comp,
                                                        results_rank_number_suitable_islandgroups_edaclim_native,
                                                        results_rank_number_suitable_islandgroups_clim_global_comp,
                                                        results_rank_number_suitable_islandgroups_edaclim_global)

# Remove the results based on the ensemble models
results_rank_number_suitable_islandgroups_comp <- results_rank_number_suitable_islandgroups_comp[results_rank_number_suitable_islandgroups_comp$algorithm != "Ensemble", ]





# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction_comp$algorithm <- as.factor(results_rank_suitable_fraction_comp$algorithm)
results_rank_suitable_fraction_comp$niche <- as.factor(results_rank_suitable_fraction_comp$niche)
results_rank_suitable_fraction_comp$predictor_type <- as.factor(results_rank_suitable_fraction_comp$predictor_type)

results_rank_mean_suitable_fraction_comp$algorithm <- as.factor(results_rank_mean_suitable_fraction_comp$algorithm)
results_rank_mean_suitable_fraction_comp$niche <- as.factor(results_rank_mean_suitable_fraction_comp$niche)
results_rank_mean_suitable_fraction_comp$predictor_type <- as.factor(results_rank_mean_suitable_fraction_comp$predictor_type)

results_rank_number_suitable_islandgroups_comp$algorithm <- as.factor(results_rank_number_suitable_islandgroups_comp$algorithm)
results_rank_number_suitable_islandgroups_comp$niche <- as.factor(results_rank_number_suitable_islandgroups_comp$niche)
results_rank_number_suitable_islandgroups_comp$predictor_type <- as.factor(results_rank_number_suitable_islandgroups_comp$predictor_type)


# Convert dependent variable (blacklist rank) into numeric values
results_rank_suitable_fraction_comp$rank <- as.numeric(results_rank_suitable_fraction_comp$rank)
results_rank_mean_suitable_fraction_comp$rank <- as.numeric(results_rank_mean_suitable_fraction_comp$rank)
results_rank_number_suitable_islandgroups_comp$rank <- as.numeric(results_rank_number_suitable_islandgroups_comp$rank)



# (d) Build random forest models  ----------------------------------------------

# One model for each blacklist definition
model_suitable_fraction_RF_comp <- randomForest(x = results_rank_suitable_fraction_comp[, c(2:4)],
                                                y = results_rank_suitable_fraction_comp$rank,
                                                ntree = 1000, importance = TRUE)

model_mean_suitable_fraction_RF_comp <- randomForest(x = results_rank_mean_suitable_fraction_comp[, c(2:4)],
                                                     y = results_rank_mean_suitable_fraction_comp$rank,
                                                     ntree = 1000, importance = TRUE)

model_number_suitable_RF_comp <- randomForest(x = results_rank_number_suitable_islandgroups_comp[, c(2:4)],
                                              y = results_rank_number_suitable_islandgroups_comp$rank,
                                              ntree = 1000, importance = TRUE)



# Save the models
save(model_suitable_fraction_RF_comp, model_mean_suitable_fraction_RF_comp, model_number_suitable_RF_comp, file = "output_data/uncertainty_quantification/models_uncertainty_RF_comp.RData")





#-------------------------------------------------------------------------------

# 2. Quantify uncertainty based on 49 island groups ----------------------------

# Load needed objects
# Ranking based on the Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_suitable_habitat_fraction_clim_global.RData")

# Ranking based on the mean suitable habitat fraction over all island groups
load("output_data/blacklists/native/clim/results_rank_mean_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_mean_suitable_habitat_fraction_clim_global.RData")

# Ranking based on predicted suitable number of island groups
load("output_data/blacklists/native/clim/results_rank_number_suitable_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_number_suitable_islandgroups_clim_global.RData")




# (a) Total suitable habitat fraction ------------------------------------------

# Bind the two data frames
results_rank_suitable_fraction <- rbind(results_rank_suitable_habitat_fraction_clim_native,
                                        results_rank_suitable_habitat_fraction_clim_global)

# Remove the results based on the ensemble models
results_rank_suitable_fraction <- results_rank_suitable_fraction[results_rank_suitable_fraction$algorithm != "Ensemble", ]



# (b) Mean suitable habitat fraction -------------------------------------------

# Bind the two data frames
results_rank_mean_suitable_fraction <- rbind(results_rank_mean_suitable_habitat_fraction_clim_native,
                                             results_rank_mean_suitable_habitat_fraction_clim_global)

# Remove the results based on the ensemble models
results_rank_mean_suitable_fraction <- results_rank_mean_suitable_fraction[results_rank_mean_suitable_fraction$algorithm != "Ensemble", ]



# (c) Number of suitable island groups  ----------------------------------------

# Bind the two data frames
results_rank_number_suitable_islandgroups <- rbind(results_rank_number_suitable_islandgroups_clim_native,
                                                   results_rank_number_suitable_islandgroups_clim_global)

# Remove the results based on the ensemble models
results_rank_number_suitable_islandgroups <- results_rank_number_suitable_islandgroups[results_rank_number_suitable_islandgroups$algorithm != "Ensemble", ]



# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction$algorithm <- as.factor(results_rank_suitable_fraction$algorithm)
results_rank_suitable_fraction$niche <- as.factor(results_rank_suitable_fraction$niche)

results_rank_mean_suitable_fraction$algorithm <- as.factor(results_rank_mean_suitable_fraction$algorithm)
results_rank_mean_suitable_fraction$niche <- as.factor(results_rank_mean_suitable_fraction$niche)

results_rank_number_suitable_islandgroups$algorithm <- as.factor(results_rank_number_suitable_islandgroups$algorithm)
results_rank_number_suitable_islandgroups$niche <- as.factor(results_rank_number_suitable_islandgroups$niche)



# Convert dependent variable (blacklist rank) into numeric values
results_rank_suitable_fraction$rank <- as.numeric(results_rank_suitable_fraction$rank)
results_rank_mean_suitable_fraction$rank <- as.numeric(results_rank_mean_suitable_fraction$rank)
results_rank_number_suitable_islandgroups$rank <- as.numeric(results_rank_number_suitable_islandgroups$rank)



# (d) Build random forest models  ----------------------------------------------

# One model for each blacklist definition
model_suitable_fraction_RF <- randomForest(x = results_rank_suitable_fraction[, c(2,4)],
                                           y = results_rank_suitable_fraction$rank,
                                           ntree = 1000, importance = TRUE)

model_mean_suitable_fraction_RF <- randomForest(x = results_rank_mean_suitable_fraction[, c(2,4)],
                                                y = results_rank_mean_suitable_fraction$rank,
                                                ntree = 1000, importance = TRUE)

model_number_suitable_RF <- randomForest(x = results_rank_number_suitable_islandgroups[, c(2,4)],
                                         y = results_rank_number_suitable_islandgroups$rank,
                                         ntree = 1000, importance = TRUE)



# Save the models
save(model_suitable_fraction_RF, model_mean_suitable_fraction_RF, model_number_suitable_RF, file = "output_data/uncertainty_quantification/models_uncertainty_RF.RData")
