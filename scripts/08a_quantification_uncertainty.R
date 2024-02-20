# Uncertainty paper


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
# Ranking based on the predicted suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_global.RData")

# Ranking based on predicted suitable number of island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_global.RData")

# Ranking based on mean rank over all Pacific island groups
load("output_data/blacklists/native/clim_comp/results_mean_rank_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_mean_rank_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_mean_rank_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_mean_rank_islandgroups_edaclim_global.RData")


# (a) Suitable habitat fraction ------------------------------------------------

# Bind the four data frames
results_rank_suitable_fraction_comp <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp,
                                             results_rank_suitable_habitat_fraction_edaclim_native,
                                             results_rank_suitable_habitat_fraction_clim_global_comp,
                                             results_rank_suitable_habitat_fraction_edaclim_global)

# Remove the results based on the ensemble models
results_rank_suitable_fraction_comp <- results_rank_suitable_fraction_comp[results_rank_suitable_fraction_comp$algorithm != "Ensemble", ]



# (b) Number of suitable island groups  ----------------------------------------

# Bind the four data frames
results_rank_number_suitable_islandgroups_comp <- rbind(results_rank_number_suitable_islandgroups_clim_native_comp,
                                                        results_rank_number_suitable_islandgroups_edaclim_native,
                                                        results_rank_number_suitable_islandgroups_clim_global_comp,
                                                        results_rank_number_suitable_islandgroups_edaclim_global)

# Remove the results based on the ensemble models
results_rank_number_suitable_islandgroups_comp <- results_rank_number_suitable_islandgroups_comp[results_rank_number_suitable_islandgroups_comp$algorithm != "Ensemble", ]


# (c) Mean rank over all island groups  ----------------------------------------

# Bind the four data frames
results_mean_rank_islandgroups_comp <- rbind(results_mean_rank_islandgroups_clim_native_comp,
                                             results_mean_rank_islandgroups_edaclim_native,
                                             results_mean_rank_islandgroups_clim_global_comp,
                                             results_mean_rank_islandgroups_edaclim_global)

# Remove the results based on the ensemble models
results_mean_rank_islandgroups_comp <- results_mean_rank_islandgroups_comp[results_mean_rank_islandgroups_comp$algorithm != "Ensemble", ]


# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction_comp$algorithm <- as.factor(results_rank_suitable_fraction_comp$algorithm)
results_rank_suitable_fraction_comp$niche <- as.factor(results_rank_suitable_fraction_comp$niche)
results_rank_suitable_fraction_comp$predictor_type <- as.factor(results_rank_suitable_fraction_comp$predictor_type)

results_rank_number_suitable_islandgroups_comp$algorithm <- as.factor(results_rank_number_suitable_islandgroups_comp$algorithm)
results_rank_number_suitable_islandgroups_comp$niche <- as.factor(results_rank_number_suitable_islandgroups_comp$niche)
results_rank_number_suitable_islandgroups_comp$predictor_type <- as.factor(results_rank_number_suitable_islandgroups_comp$predictor_type)

results_mean_rank_islandgroups_comp$algorithm <- as.factor(results_mean_rank_islandgroups_comp$algorithm)
results_mean_rank_islandgroups_comp$niche <- as.factor(results_mean_rank_islandgroups_comp$niche)
results_mean_rank_islandgroups_comp$predictor_type <- as.factor(results_mean_rank_islandgroups_comp$predictor_type)

# Convert dependent variable (blacklist rank) into numeric values
results_rank_suitable_fraction_comp$rank <- as.numeric(results_rank_suitable_fraction_comp$rank)
results_rank_number_suitable_islandgroups_comp$rank <- as.numeric(results_rank_number_suitable_islandgroups_comp$rank)
results_mean_rank_islandgroups_comp$mean_rank <- as.numeric(results_mean_rank_islandgroups_comp$mean_rank)


# (d) Build random forest models  ----------------------------------------------

# One model for each blacklist definition
model_suitable_fraction_RF_comp <- randomForest(x = results_rank_suitable_fraction_comp[, c(2:4)],
                                                y = results_rank_suitable_fraction_comp$rank,
                                                ntree = 1000, importance = TRUE)

model_number_suitable_RF_comp <- randomForest(x = results_rank_number_suitable_islandgroups_comp[, c(2:4)],
                                              y = results_rank_number_suitable_islandgroups_comp$rank,
                                              ntree = 1000, importance = TRUE)

model_mean_rank_RF_comp <- randomForest(x = results_mean_rank_islandgroups_comp[, c(2:4)],
                                        y = results_mean_rank_islandgroups_comp$rank,
                                        ntree = 1000, importance = TRUE)

# Save the models
save(model_suitable_fraction_RF_comp, model_number_suitable_RF_comp, model_mean_rank_RF_comp, file = "output_data/uncertainty_quantification/models_uncertainty_RF_comp.RData")





#-------------------------------------------------------------------------------

# 2. Quantify uncertainty based on 49 island groups ----------------------------

# Load needed objects
# Ranking based on the predicted suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global.RData")


# Ranking based on predicted suitable number of island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global.RData")

# Ranking based on mean rank over all Pacific island groups
load("output_data/blacklists/native/clim_comp/results_mean_rank_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim_comp/results_mean_rank_islandgroups_clim_global.RData")


# (a) Suitable habitat fraction ------------------------------------------------

# Bind the two data frames
results_rank_suitable_fraction <- rbind(results_rank_suitable_habitat_fraction_clim_native,
                                        results_rank_suitable_habitat_fraction_clim_global)

# Remove the results based on the ensemble models
results_rank_suitable_fraction <- results_rank_suitable_fraction[results_rank_suitable_fraction$algorithm != "Ensemble", ]



# (b) Number of suitable island groups  ----------------------------------------

# Bind the two data frames
results_rank_number_suitable_islandgroups <- rbind(results_rank_number_suitable_islandgroups_clim_native,
                                                        results_rank_number_suitable_islandgroups_clim_global)

# Remove the results based on the ensemble models
results_rank_number_suitable_islandgroups <- results_rank_number_suitable_islandgroups[results_rank_number_suitable_islandgroups$algorithm != "Ensemble", ]



# (c) Mean rank over all island groups  ----------------------------------------

# Bind the two data frames
results_mean_rank_islandgroups <- rbind(results_mean_rank_islandgroups_clim_native,
                                             results_mean_rank_islandgroups_clim_global)

# Remove the results based on the ensemble models
results_mean_rank_islandgroups <- results_mean_rank_islandgroups[results_mean_rank_islandgroups$algorithm != "Ensemble", ]



# (d) Prepare random forest regression  ----------------------------------------

# Convert predictors into factors
results_rank_suitable_fraction$algorithm <- as.factor(results_rank_suitable_fraction$algorithm)
results_rank_suitable_fraction$niche <- as.factor(results_rank_suitable_fraction$niche)

results_rank_number_suitable_islandgroups$algorithm <- as.factor(results_rank_number_suitable_islandgroups$algorithm)
results_rank_number_suitable_islandgroups$niche <- as.factor(results_rank_number_suitable_islandgroups$niche)

results_mean_rank_islandgroups$algorithm <- as.factor(results_mean_rank_islandgroups$algorithm)
results_mean_rank_islandgroups$niche <- as.factor(results_mean_rank_islandgroups$niche)

# Convert dependent variable (blacklist rank) into numeric values
results_rank_suitable_fraction$rank <- as.numeric(results_rank_suitable_fraction$rank)
results_rank_number_suitable_islandgroups$rank <- as.numeric(results_rank_number_suitable_islandgroups$rank)
results_mean_rank_islandgroups$mean_rank <- as.numeric(results_mean_rank_islandgroups$mean_rank)


# (d) Build random forest models  ----------------------------------------------

# One model for each blacklist definition
model_suitable_fraction_RF <- randomForest(x = results_rank_suitable_fraction[, c(2,4)],
                                           y = results_rank_suitable_fraction$rank,
                                           ntree = 1000, importance = TRUE)

model_number_suitable_RF <- randomForest(x = results_rank_number_[suitable_islandgroups, c(2,4)],
                                         y = results_rank_number_suitable_islandgroups$rank,
                                         ntree = 1000, importance = TRUE)

model_mean_rank_RF <- randomForest(x = results_mean_rank_islandgroups[, c(2,4)],
                                   y = results_mean_rank_islandgroup$rank,
                                   ntree = 1000, importance = TRUE)

# Save the models
save(model_suitable_fraction_RF , model_number_suitable_RF, model_mean_rank_RF, file = "output_data/uncertainty_quantification/models_uncertainty_RF.RData")