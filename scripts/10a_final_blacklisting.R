# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


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

# 1. Final blacklisting based on the ensemble predictions based on native
# occurrences and purely climatic data -----------------------------------------

# based on 25 island groups covered by climatic and edaphic data ---------------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_native_comp, results_rank_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native_comp, results_rank_mean_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_native_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_native_comp, results_rank_number_suitable_islandgroups_clim_native_comp$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_clim_native_comp_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_clim_native_comp_ens$blacklist <- "number_suitable"

# Order the first data frame by rank
results_rank_suitable_habitat_fraction_clim_native_comp_ens <- results_rank_suitable_habitat_fraction_clim_native_comp_ens[order(results_rank_suitable_habitat_fraction_clim_native_comp_ens$rank), ]

# Bind the data frames
blacklists_final_native_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp_ens[,c(6:8)], results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens[,c(6:8)],
                                     results_rank_number_suitable_islandgroups_clim_native_comp_ens[,c(6:8)])

# Make sure the column names are correct
colnames(blacklists_final_native_clim_comp) <- c("species", "rank", "blacklist")

# Save the data frame containing final blacklists
save(blacklists_final_native_clim_comp, file = "output_data/final_blacklisting/blacklists_final_native_clim_comp.RData")





#-------------------------------------------------------------------------------

# 2. Final blacklisting based on the ensemble predictions based on native
# occurrences and combined climatic and edaphic data ---------------------------


# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_native, results_rank_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_native, results_rank_mean_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_native_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_native, results_rank_number_suitable_islandgroups_edaclim_native$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_edaclim_native_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_edaclim_native_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_edaclim_native_ens$blacklist <- "number_suitable"

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_edaclim_native_ens$species, results_rank_suitable_habitat_fraction_clim_native_comp_ens$species)
results_rank_suitable_habitat_fraction_edaclim_native_ens <- results_rank_suitable_habitat_fraction_edaclim_native_ens[order_indices, ]

# Bind the data frames
blacklists_final_native_edaclim_comp <- rbind(results_rank_suitable_habitat_fraction_edaclim_native_ens[,c(6:8)], results_rank_mean_suitable_habitat_fraction_edaclim_native_ens[,c(6:8)],
                                        results_rank_number_suitable_islandgroups_edaclim_native_ens[,c(6:8)])

# Make sure the column names are correct
colnames(blacklists_final_native_edaclim) <- c("species", "rank", "blacklist")

# Save the data frame containing final blacklists
save(blacklists_final_native_edaclim, file = "output_data/final_blacklisting/blacklists_final_native_edaclim.RData")





#-------------------------------------------------------------------------------

# 3. Final blacklisting based on the ensemble predictions based on global
# occurrences and purely climatic data -----------------------------------------

# based on 25 island groups covered by climatic and edaphic data ---------------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/global/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_climglobal_comp$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_clim_global_comp_ens$blacklist <- "number_suitable"

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_clim_global_comp_ens$species, results_rank_suitable_habitat_fraction_clim_native_comp_ens$species)
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- results_rank_suitable_habitat_fraction_clim_global_comp_ens[order_indices, ]

# Bind the data frames
blacklists_final_global_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_global_comp_ens[,c(6:8)], results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens[,c(6:8)],
                                     results_rank_number_suitable_islandgroups_clim_global_comp_ens[,c(6:8)])

# Make sure the column names are correct
colnames(blacklists_final_global_clim_comp) <- c("species", "rank", "blacklist")

# Save the data frame containing final blacklists
save(blacklists_final_global_clim_comp, file = "output_data/final_blacklisting/blacklists_final_global_clim_comp.RData")




#-------------------------------------------------------------------------------

# 4. Final blacklisting based on the ensemble predictions based on global
# occurrences and combined climatic and edaphic data ---------------------------


# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/global/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_global, results_rank_mean_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_global_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_edaclim_global_ens$blacklist <- "number_suitable"

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_edaclim_global_ens$species, results_rank_suitable_habitat_fraction_clim_native_comp_ens$species)
results_rank_suitable_habitat_fraction_edaclim_global_ens <- results_rank_suitable_habitat_fraction_edaclim_global_ens[order_indices, ]

# Bind the data frames
blacklists_final_global_edaclim_comp <- rbind(results_rank_suitable_habitat_fraction_edaclim_global_ens[,c(6:8)], results_rank_mean_suitable_habitat_fraction_edaclim_global_ens[,c(6:8)],
                                              results_rank_number_suitable_islandgroups_edaclim_global_ens[,c(6:8)])

# Make sure the column names are correct
colnames(blacklists_final_global_edaclim) <- c("species", "rank", "blacklist")

# Save the data frame containing final blacklists
save(blacklists_final_global_edaclim, file = "output_data/final_blacklisting/blacklists_final_global_edaclim.RData")





#-------------------------------------------------------------------------------

# 5. Final blacklisting total ensemble -----------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Needed objects (results of blacklists of all four predictor sets)
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_global.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")
load("output_data/blacklists/native/edaclim/results_rank_number_suitable_islandgroups_edaclim_native.RData")
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_global.RData")



# Subset the data frames to just contain the results of ensemble models
# Pacific-wide total suitable habitat fraction
results_rank_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_native_comp, results_rank_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_native, results_rank_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")

# Mean suitable habitat fraction 
results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native_comp, results_rank_mean_suitable_habitat_fraction_clim_native_comp$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_edaclim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_native, results_rank_mean_suitable_habitat_fraction_edaclim_native$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_global, results_rank_mean_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")

# Number of suitable Pacific island groups
results_rank_number_suitable_islandgroups_clim_native_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_native_comp, results_rank_number_suitable_islandgroups_clim_native_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_native_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_native, results_rank_number_suitable_islandgroups_edaclim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_global_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == "Ensemble")



# Only retain the needed information of the data frames (species names, values of ranking)
# Pacific-wide total suitable habitat fraction
suitable_habitat_fraction_clim_native_comp <- results_rank_suitable_habitat_fraction_clim_native_comp_ens[,c(1,5)]
suitable_habitat_fraction_edaclim_native <- results_rank_suitable_habitat_fraction_edaclim_native_ens[,c(1,5)]
suitable_habitat_fraction_clim_global_comp <- results_rank_suitable_habitat_fraction_clim_global_comp_ens[,c(1,5)]
suitable_habitat_fraction_edaclim_global <- results_rank_suitable_habitat_fraction_edaclim_global_ens[,c(1,5)]

# Mean suitable habitat fraction
mean_suitable_habitat_fraction_clim_native_comp <- results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens[,c(1,5)]
mean_suitable_habitat_fraction_edaclim_native <- results_rank_mean_suitable_habitat_fraction_edaclim_native_ens[,c(1,5)]
mean_suitable_habitat_fraction_clim_global_comp <- results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens[,c(1,5)]
mean_suitable_habitat_fraction_edaclim_global <- results_rank_mean_suitable_habitat_fraction_edaclim_global_ens[,c(1,5)]

# Number of suitable Pacific island groups
number_suitable_islandgroups_clim_native_comp <- results_rank_number_suitable_islandgroups_clim_native_comp_ens[,c(1,5)]
number_suitable_islandgroups_edaclim_native <- results_rank_number_suitable_islandgroups_edaclim_native_ens[,c(1,5)]
number_suitable_islandgroups_clim_global_comp <- results_rank_number_suitable_islandgroups_clim_global_comp_ens[,c(1,5)]
number_suitable_islandgroups_edaclim_global <- results_rank_number_suitable_islandgroups_edaclim_global_ens[,c(1,5)]



# Merge all these data frames per species for each of the three blacklisting definitions
# Pacific-wide total suitable habitat fraction
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_clim_native_comp, suitable_habitat_fraction_edaclim_native, by = "species")
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_data_prep, suitable_habitat_fraction_clim_global_comp, by = "species")
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_data_prep, suitable_habitat_fraction_edaclim_global, by = "species")

colnames(suitable_habitat_fraction_data_prep) <- c("species", "suitable_fraction_1", "suitable_fraction_2", "suitable_fraction_3", "suitable_fraction_4")

suitable_habitat_fraction_data_prep$suitable_fraction_1 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_1)
suitable_habitat_fraction_data_prep$suitable_fraction_2 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_2)
suitable_habitat_fraction_data_prep$suitable_fraction_3 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_3)
suitable_habitat_fraction_data_prep$suitable_fraction_4 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_4)

# Mean suitable habitat fraction
mean_suitable_habitat_fraction_data_prep <- merge(mean_suitable_habitat_fraction_clim_native_comp, mean_suitable_habitat_fraction_edaclim_native, by = "species")
mean_suitable_habitat_fraction_data_prep <- merge(mean_suitable_habitat_fraction_data_prep, mean_suitable_habitat_fraction_clim_global_comp, by = "species")
mean_suitable_habitat_fraction_data_prep <- merge(mean_suitable_habitat_fraction_data_prep, mean_suitable_habitat_fraction_edaclim_global, by = "species")

colnames(mean_suitable_habitat_fraction_data_prep) <- c("species", "mean_suitable_fraction_1", "mean_suitable_fraction_2", "mean_suitable_fraction_3", "mean_suitable_fraction_4")

mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_1 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_1)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_2 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_2)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_3 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_3)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_4 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_4)

# Number of suitable Pacific island groups
number_suitable_data_prep <- merge(number_suitable_islandgroups_clim_native_comp, number_suitable_islandgroups_edaclim_native, by = "species")
number_suitable_data_prep <- merge(number_suitable_data_prep, number_suitable_islandgroups_clim_global_comp, by = "species")
number_suitable_data_prep <- merge(number_suitable_data_prep, number_suitable_islandgroups_edaclim_global, by = "species")

colnames(number_suitable_data_prep) <- c("species", "number_suitable_1", "number_suitable_2", "number_suitable_3", "number_suitable_4")

number_suitable_data_prep$number_suitable_1 <- as.numeric(number_suitable_data_prep$number_suitable_1)
number_suitable_data_prep$number_suitable_2 <- as.numeric(number_suitable_data_prep$number_suitable_2)
number_suitable_data_prep$number_suitable_3 <- as.numeric(number_suitable_data_prep$number_suitable_3)
number_suitable_data_prep$number_suitable_4 <- as.numeric(number_suitable_data_prep$number_suitable_4)



# Add a column containing the average ranking value over the four predictor sets
suitable_habitat_fraction_data_prep$suitable_fraction_avg <- round(((rowSums(suitable_habitat_fraction_data_prep[,c(2:5)]))/4),2)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_avg <- round(((rowSums(mean_suitable_habitat_fraction_data_prep[,c(2:5)]))/4),2)
number_suitable_data_prep$number_suitable_avg <- round(((rowSums(number_suitable_data_prep[,c(2:5)]))/4),0)


# Add a column with the new ranking position
suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_avg)))
mean_suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_avg)))
number_suitable_data_prep$final_rank <- dense_rank(desc(as.numeric(number_suitable_data_prep$number_suitable_avg)))


# Add a column indicating the blacklist definition
suitable_habitat_fraction_data_prep$blacklist <- "total_suitable_fraction"
mean_suitable_habitat_fraction_data_prep$blacklist <- "mean_suitable_fraction"
number_suitable_data_prep$blacklist <- "number_suitable"


# Bind the three data frame, only retain important information (species, final rank, blacklist)
blacklists_final_comp <- rbind(suitable_habitat_fraction_data_prep[, c(1,7,8)], mean_suitable_habitat_fraction_data_prep[, c(1,7,8)])
blacklists_final_comp <- rbind(blacklists_final_comp, number_suitable_data_prep[, c(1,7,8)])

# Make sure the column names are correct
colnames(blacklists_final_comp) <- c("species", "final_rank", "blacklist")

# Save the final blacklist
save(blacklists_final_comp, file = "output_data/final_blacklisting/blacklists_final_comp.RData")





# (b) based on 49 island groups covered by climatic data -----------------------

# Needed objects (results of blacklists of the predictor sets using climatic data)
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_suitable_habitat_fraction_clim_global.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_mean_suitable_habitat_fraction_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_mean_suitable_habitat_fraction_clim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim/results_rank_number_suitable_islandgroups_clim_native.RData")
load("output_data/blacklists/global/clim/results_rank_number_suitable_islandgroups_clim_global.RData")




# Subset the data frames to just contain the results of ensemble models
# Pacific-wide total suitable habitat fraction
results_rank_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_suitable_habitat_fraction_clim_native, results_rank_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
results_rank_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_suitable_habitat_fraction_clim_global, results_rank_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")

# Mean suitable habitat fraction
results_rank_mean_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native, results_rank_mean_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global, results_rank_mean_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")

# Number of suitable Pacific island groups
results_rank_number_suitable_islandgroups_clim_native_ens <- subset(results_rank_number_suitable_islandgroups_clim_native, results_rank_number_suitable_islandgroups_clim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_ens <- subset(results_rank_number_suitable_islandgroups_clim_global, results_rank_number_suitable_islandgroups_clim_global$algorithm == "Ensemble")



# Only retain the needed information of the data frames (species names, values of ranking)
# Pacific-wide total suitable habitat fraction
suitable_habitat_fraction_clim_native <- results_rank_suitable_habitat_fraction_clim_native_ens[,c(1,5)]
suitable_habitat_fraction_clim_global <- results_rank_suitable_habitat_fraction_clim_global_ens[,c(1,5)]

# Mean suitable habitat fraction
mean_suitable_habitat_fraction_clim_native <- results_rank_mean_suitable_habitat_fraction_clim_native_ens[,c(1,5)]
mean_suitable_habitat_fraction_clim_global <- results_rank_mean_suitable_habitat_fraction_clim_global_ens[,c(1,5)]

# Number of suitable Pacific island groups
number_suitable_islandgroups_clim_native <- results_rank_number_suitable_islandgroups_clim_native_ens[,c(1,5)]
number_suitable_islandgroups_clim_global <- results_rank_number_suitable_islandgroups_clim_global_ens[,c(1,5)]



# Merge all these data frames per species for each of the three blacklisting definitions
# Pacific-wide total suitable habitat fraction
suitable_habitat_fraction_data_prep <- merge(suitable_habitat_fraction_clim_native, suitable_habitat_fraction_clim_global, by = "species")

colnames(suitable_habitat_fraction_data_prep) <- c("species", "suitable_fraction_1", "suitable_fraction_3")

suitable_habitat_fraction_data_prep$suitable_fraction_1 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_1)
suitable_habitat_fraction_data_prep$suitable_fraction_3 <- as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_3)

# Mean suitable habitat fraction
mean_suitable_habitat_fraction_data_prep <- merge(mean_suitable_habitat_fraction_clim_native, mean_suitable_habitat_fraction_clim_global, by = "species")

colnames(mean_suitable_habitat_fraction_data_prep) <- c("species", "mean_suitable_fraction_1", "mean_suitable_fraction_3")

mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_1 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_1)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_3 <- as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_3)

# Number of suitable Pacific island groups
number_suitable_data_prep <- merge(number_suitable_islandgroups_clim_native, number_suitable_islandgroups_clim_global, by = "species")

colnames(number_suitable_data_prep) <- c("species", "number_suitable_1", "number_suitable_3")

number_suitable_data_prep$number_suitable_1 <- as.numeric(number_suitable_data_prep$number_suitable_1)
number_suitable_data_prep$number_suitable_3 <- as.numeric(number_suitable_data_prep$number_suitable_3)



# Add a column containing the average ranking value over the four predictor sets
suitable_habitat_fraction_data_prep$suitable_fraction_avg <- round(((rowSums(suitable_habitat_fraction_data_prep[,c(2:3)]))/2),2)
mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_avg <- round(((rowSums(mean_suitable_habitat_fraction_data_prep[,c(2:3)]))/2),2)
number_suitable_data_prep$number_suitable_avg <- round(((rowSums(number_suitable_data_prep[,c(2:3)]))/2),0)


# Add a column with the new ranking position
suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(suitable_habitat_fraction_data_prep$suitable_fraction_avg)))
mean_suitable_habitat_fraction_data_prep$final_rank <- dense_rank(desc(as.numeric(mean_suitable_habitat_fraction_data_prep$mean_suitable_fraction_avg)))
number_suitable_data_prep$final_rank <- dense_rank(desc(as.numeric(number_suitable_data_prep$number_suitable_avg)))


# Add a column indicating the blacklist definition
suitable_habitat_fraction_data_prep$blacklist <- "total_suitable_fraction"
mean_suitable_habitat_fraction_data_prep$blacklist <- "mean_suitable_fraction"
number_suitable_data_prep$blacklist <- "number_suitable"


# Bind the three data frame, only retain important information (species, final rank, blacklist)
blacklists_final <- rbind(suitable_habitat_fraction_data_prep[, c(1,5,6)], mean_suitable_habitat_fraction_data_prep[, c(1,5,6)])
blacklists_final <- rbind(blacklists_final, number_suitable_data_prep[, c(1,5,6)])

# Make sure the column names are correct
colnames(blacklists_final) <- c("species", "final_rank", "blacklist")

# Save the final blacklist
save(blacklists_final, file = "output_data/final_blacklisting/blacklists_final.RData")


