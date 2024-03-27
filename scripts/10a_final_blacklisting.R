# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                        10a. Final blacklisting                         #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(dplyr)

#-------------------------------------------------------------------------------

# 1. Final blacklisting --------------------------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Needed objects (results of blacklists of all four predictor sets)
# Pacific-wide suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_global.RData")

# Mean rank over all Pacific island groups
load("output_data/blacklists/native/clim_comp/results_mean_rank_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_mean_rank_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_mean_rank_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_mean_rank_islandgroups_edaclim_global.RData")

# Subset the data frames to just contain the results of ensemble models
# Pacific-wide suitable habitat fraction
results_rank_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_native_comp, results_rank_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_native, results_rank_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")

# Number of suitable Pacific island groups
results_rank_number_suitable_islandgroups_clim_native_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_native_comp, results_rank_number_suitable_islandgroups_clim_native_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_native_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_native, results_rank_number_suitable_islandgroups_edaclim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_global_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == "Ensemble")

# Mean rank over all Pacific island groups
results_mean_rank_islandgroups_clim_native_comp_ens <- subset(results_mean_rank_islandgroups_clim_native_comp, results_mean_rank_islandgroups_clim_native_comp$algorithm == "Ensemble")
results_mean_rank_islandgroups_edaclim_native_ens <- subset(results_mean_rank_islandgroups_edaclim_native, results_mean_rank_islandgroups_edaclim_native$algorithm == "Ensemble")
results_mean_rank_islandgroups_clim_global_comp_ens <- subset(results_mean_rank_islandgroups_clim_global_comp, results_mean_rank_islandgroups_clim_global_comp$algorithm == "Ensemble")
results_mean_rank_islandgroups_edaclim_global_ens <- subset(results_mean_rank_islandgroups_edaclim_global, results_mean_rank_islandgroups_edaclim_global$algorithm == "Ensemble")

# Only retain the needed information of the data frames (species names, values of ranking)
# Pacific-wide suitable habitat fraction
suitable_habitat_fraction_clim_native_comp <- results_rank_suitable_habitat_fraction_clim_native_comp_ens[,c(1,5)]
suitable_habitat_fraction_edaclim_native <- results_rank_suitable_habitat_fraction_edaclim_native_ens[,c(1,5)]
suitable_habitat_fraction_clim_global_comp <- results_rank_suitable_habitat_fraction_clim_global_comp_ens[,c(1,5)]
suitable_habitat_fraction_edaclim_global <- results_rank_suitable_habitat_fraction_edaclim_global_ens[,c(1,5)]

# Number of suitable Pacific island groups
number_suitable_islandgroups_clim_native_comp <- results_rank_number_suitable_islandgroups_clim_native_comp_ens[,c(1,5)]
number_suitable_islandgroups_edaclim_native <- results_rank_number_suitable_islandgroups_edaclim_native_ens[,c(1,5)]
number_suitable_islandgroups_clim_global_comp <- results_rank_number_suitable_islandgroups_clim_global_comp_ens[,c(1,5)]
number_suitable_islandgroups_edaclim_global <- results_rank_number_suitable_islandgroups_edaclim_global_ens[,c(1,5)]

# Mean rank over all Pacific island groups
mean_rank_islandgroups_clim_native_comp <- results_mean_rank_islandgroups_clim_native_comp_ens[,c(1,5)]
mean_rank_islandgroups_edaclim_native <- results_mean_rank_islandgroups_edaclim_native_ens[,c(1,5)]
mean_rank_islandgroups_clim_global_comp <- results_mean_rank_islandgroups_clim_global_comp_ens[,c(1,5)]
mean_rank_islandgroups_edaclim_global <- results_mean_rank_islandgroups_edaclim_global_ens[,c(1,5)]

# Merge all these data frames per species for each of the three blacklisting definitions
# Pacific-wide suitable habitat fraction
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_clim_native_comp, suitable_habitat_fraction_edaclim_native, by = "species")
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_data_prep, suitable_habitat_fraction_clim_global_comp, by = "species")
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_data_prep, suitable_habitat_fraction_edaclim_global, by = "species")

colnames(suitable_habitat_fraction_data_prep) <- c("species", "suitable_fraction_1", "suitable_fraction_2", "suitable_fraction_3", "suitable_fraction_4")

suitable_habitat_fraction_data_prep$suitable_fraction_1 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_1)
suitable_habitat_fraction_data_prep$suitable_fraction_2 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_2)
suitable_habitat_fraction_data_prep$suitable_fraction_3 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_3)
suitable_habitat_fraction_data_prep$suitable_fraction_4 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_4)

# Number of suitable Pacific island groups
number_suitable_data_prep <- merge(number_suitable_islandgroups_clim_native_comp, number_suitable_islandgroups_edaclim_native, by = "species")
number_suitable_data_prep <- merge(number_suitable_data_prep, number_suitable_islandgroups_clim_global_comp, by = "species")
number_suitable_data_prep <- merge(number_suitable_data_prep, number_suitable_islandgroups_edaclim_global, by = "species")

colnames(number_suitable_data_prep) <- c("species", "number_suitable_1", "number_suitable_2", "number_suitable_3", "number_suitable_4")

number_suitable_data_prep$number_suitable_1 <- as.numeric(number_suitable_data_prep$number_suitable_1)
number_suitable_data_prep$number_suitable_2 <- as.numeric(number_suitable_data_prep$number_suitable_2)
number_suitable_data_prep$number_suitable_3 <- as.numeric(number_suitable_data_prep$number_suitable_3)
number_suitable_data_prep$number_suitable_4 <- as.numeric(number_suitable_data_prep$number_suitable_4)

# Mean rank over all Pacific island groups
mean_rank_data_prep <- merge(mean_rank_islandgroups_clim_native_comp, mean_rank_islandgroups_edaclim_native, by = "species")
mean_rank_data_prep <- merge(mean_rank_data_prep, mean_rank_islandgroups_clim_global_comp, by = "species")
mean_rank_data_prep <- merge(mean_rank_data_prep, mean_rank_islandgroups_edaclim_global, by = "species")

colnames(mean_rank_data_prep) <- c("species", "mean_rank_1", "mean_rank_2", "mean_rank_3", "mean_rank_4")

mean_rank_data_prep$mean_rank_1 <- as.numeric(mean_rank_data_prep$mean_rank_1)
mean_rank_data_prep$mean_rank_2 <- as.numeric(mean_rank_data_prep$mean_rank_2)
mean_rank_data_prep$mean_rank_3 <- as.numeric(mean_rank_data_prep$mean_rank_3)
mean_rank_data_prep$mean_rank_4 <- as.numeric(mean_rank_data_prep$mean_rank_4)

# Add a column containing the average ranking value over the four predictor sets
suitable_habitat_fraction_data_prep$suitable_fraction_avg <- round(((rowSums(suitable_habitat_fraction_data_prep[,c(2:5)]))/4),2)
number_suitable_data_prep$number_suitable_avg <- round(((rowSums(number_suitable_data_prep[,c(2:5)]))/4),0)
mean_rank_data_prep$final_rank <- round(((rowSums(mean_rank_data_prep[,c(2:5)]))/4),2)

# Add a column with the new ranking position
suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_avg)))
number_suitable_data_prep$final_rank <- dense_rank(desc(as.numeric(number_suitable_data_prep$number_suitable_avg)))


# Add a column indicating the blacklist definition
suitable_habitat_fraction_data_prep$blacklist <- "suitable_fraction"
number_suitable_data_prep$blacklist <- "number_suitable"
mean_rank_data_prep$blacklist <- "mean_rank"

# Bind the three data frame, only retain important information (species, final rank, blacklist)
blacklists_final_comp <- rbind(suitable_habitat_fraction_data_prep[, c(1,7,8)], number_suitable_data_prep[, c(1,7,8)])
blacklists_final_comp <- rbind(blacklists_final_comp, mean_rank_data_prep[, c(1,6,7)])

# Make sure the column names are correct
colnames(blacklists_final_comp) <- c("species", "final_rank", "blacklist")

# Save the final blacklist
save(blacklists_final_comp, file = "output_data/final_blacklisting/blacklists_final_comp.RData")





# (b) based on 49 island groups covered by climatic data -----------------------

# Needed objects (results of blacklists of the predictor sets using climatic data)
# Pacific-wide suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_suitable_habitat_fraction_clim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim/results_rank_number_suitable_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_number_suitable_islandgroups_clim_global.RData")

# Mean rank over all Pacific island groups
load("output_data/blacklists/native/clim_/results_mean_rank_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim/results_mean_rank_islandgroups_clim_global.RData")


# Subset the data frames to just contain the results of ensemble models
# Pacific-wide suitable habitat fraction
results_rank_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_suitable_habitat_fraction_clim_native, results_rank_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_suitable_habitat_fraction_clim_global, results_rank_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")

# Number of suitable Pacific island groups
results_rank_number_suitable_islandgroups_clim_native_ens <- subset(results_rank_number_suitable_islandgroups_clim_native, results_rank_number_suitable_islandgroups_clim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_ens <- subset(results_rank_number_suitable_islandgroups_clim_global, results_rank_number_suitable_islandgroups_clim_global$algorithm == "Ensemble")

# Mean rank over all Pacific island groups
results_mean_rank_islandgroups_clim_native_ens <- subset(results_mean_rank_islandgroups_clim_native, results_mean_rank_islandgroups_clim_native$algorithm == "Ensemble")
results_mean_rank_islandgroups_clim_global_ens <- subset(results_mean_rank_islandgroups_clim_global, results_mean_rank_islandgroups_clim_global$algorithm == "Ensemble")

# Only retain the needed information of the data frames (species names, values of ranking)
# Pacific-wide suitable habitat fraction
suitable_habitat_fraction_clim_native <- results_rank_suitable_habitat_fraction_clim_native_ens[,c(1,5)]
suitable_habitat_fraction_clim_global <- results_rank_suitable_habitat_fraction_clim_global_ens[,c(1,5)]

# Number of suitable Pacific island groups
number_suitable_islandgroups_clim_native <- results_rank_number_suitable_islandgroups_clim_native_ens[,c(1,5)]
number_suitable_islandgroups_clim_global <- results_rank_number_suitable_islandgroups_clim_global_ens[,c(1,5)]


# Mean rank over all Pacific island groups
mean_rank_islandgroups_clim_native <- results_mean_rank_islandgroups_clim_native_ens[,c(1,5)]
mean_rank_islandgroups_clim_global <- results_mean_rank_islandgroups_clim_global_ens[,c(1,5)]

# Merge all these data frames per species for each of the three blacklisting definitions
# Pacific-wide suitable habitat fraction
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_clim_native, suitable_habitat_fraction_clim_global_comp, by = "species")

colnames(suitable_habitat_fraction_data_prep) <- c("species", "suitable_fraction_1", "suitable_fraction_3")

suitable_habitat_fraction_data_prep$suitable_fraction_1 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_1)
suitable_habitat_fraction_data_prep$suitable_fraction_3 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_3)

# Number of suitable Pacific island groups
number_suitable_data_prep <- merge(number_suitable_islandgroups_clim_native, number_suitable_islandgroups_clim_global, by = "species")

colnames(number_suitable_data_prep) <- c("species", "number_suitable_1", "number_suitable_3")

number_suitable_data_prep$number_suitable_1 <- as.numeric(number_suitable_data_prep$number_suitable_1)
number_suitable_data_prep$number_suitable_3 <- as.numeric(number_suitable_data_prep$number_suitable_3)

# Mean rank over all Pacific island groups
mean_rank_data_prep <- merge(mean_rank_islandgroups_clim_native, mean_rank_islandgroups_clim_global, by = "species")

colnames(mean_rank_data_prep) <- c("species", "mean_rank_1", "mean_rank_3")

mean_rank_data_prep$mean_rank_1 <- as.numeric(mean_rank_data_prep$mean_rank_1)
mean_rank_data_prep$mean_rank_3 <- as.numeric(mean_rank_data_prep$mean_rank_3)

# Add a column containing the average ranking value over the four predictor sets
suitable_habitat_fraction_data_prep$suitable_fraction_avg <- round(((rowSums(suitable_habitat_fraction_data_prep[,c(2:3)]))/2),2)
number_suitable_data_prep$number_suitable_avg <- round(((rowSums(number_suitable_data_prep[,c(2:3)]))/2),0)
mean_rank_data_prep$final_rank <- round(((rowSums(mean_rank_data_prep[,c(2:3)]))/2),2)

# Add a column with the new ranking position
suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_avg)))
number_suitable_data_prep$final_rank <- dense_rank(desc(as.numeric(number_suitable_data_prep$number_suitable_avg)))


# Add a column indicating the blacklist definition
suitable_habitat_fraction_data_prep$blacklist <- "suitable_fraction"
number_suitable_data_prep$blacklist <- "number_suitable"
mean_rank_data_prep$blacklist <- "mean_rank"

# Bind the three data frame, only retain important information (species, final rank, blacklist)
blacklists_final <- rbind(suitable_habitat_fraction_data_prep[, c(1,5,6)], number_suitable_data_prep[, c(1,5,6)])
blacklists_final <- rbind(blacklists_final_comp, mean_rank_data_prep[, c(1,4,5)])

# Make sure the column names are correct
colnames(blacklists_final) <- c("species", "final_rank", "blacklist")

# Save the final blacklist
save(blacklists_final, file = "output_data/final_blacklisting/blacklists_final.RData")


