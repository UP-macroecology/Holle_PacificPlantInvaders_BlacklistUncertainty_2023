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

# (a) based on 25 island groups covered by climatic and edaphic data -----------

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

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_clim_native_comp_ens$rank_2 <- results_rank_suitable_habitat_fraction_clim_native_comp_ens$rank
results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens$rank
results_rank_number_suitable_islandgroups_clim_native_comp_ens <- results_rank_number_suitable_islandgroups_clim_native_comp_ens[order(results_rank_number_suitable_islandgroups_clim_native_comp_ens$rank), ]
results_rank_number_suitable_islandgroups_clim_native_comp_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_native_comp_ens$rank)

# Order the first data frame by rank
results_rank_suitable_habitat_fraction_clim_native_comp_ens <- results_rank_suitable_habitat_fraction_clim_native_comp_ens[order(results_rank_suitable_habitat_fraction_clim_native_comp_ens$rank), ]

# Bind the data frames
blacklists_final_native_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_native_comp_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_clim_native_comp_ens[,c(5:8)],
                                     results_rank_number_suitable_islandgroups_clim_native_comp_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_native_clim_comp) <- c("species", "rank", "blacklist", "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_native_clim_comp, file = "output_data/final_blacklisting/blacklists_final_native_clim_comp.RData")



# (b) based on 49 island groups covered by climatic data -----------------------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_suitable_habitat_fraction_clim_native.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/clim/results_rank_mean_suitable_habitat_fraction_clim_native.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim/results_rank_number_suitable_islandgroups_clim_native.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_suitable_habitat_fraction_clim_native, results_rank_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_native_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_native, results_rank_mean_suitable_habitat_fraction_clim_native$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_native_ens <- subset(results_rank_number_suitable_islandgroups_clim_native, results_rank_number_suitable_islandgroups_clim_native$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_clim_native_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_clim_native_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_clim_native_ens$blacklist <- "number_suitable"

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_clim_native_ens$rank_2 <- results_rank_suitable_habitat_fraction_clim_native_ens$rank
results_rank_mean_suitable_habitat_fraction_clim_native_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_native_ens$rank
results_rank_number_suitable_islandgroups_clim_native_ens <- results_rank_number_suitable_islandgroups_clim_native_ens[order(results_rank_number_suitable_islandgroups_clim_native_ens$rank), ]
results_rank_number_suitable_islandgroups_clim_native_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_native_ens$rank)

# Order the first data frame by rank
results_rank_suitable_habitat_fraction_clim_native_ens <- results_rank_suitable_habitat_fraction_clim_native_ens[order(results_rank_suitable_habitat_fraction_clim_native_ens$rank), ]

# Bind the data frames
blacklists_final_native_clim <- rbind(results_rank_suitable_habitat_fraction_clim_native_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_clim_native_ens[,c(5:8)],
                                      results_rank_number_suitable_islandgroups_clim_native_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_native_clim) <- c("species", "rank", "blacklist", "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_native_clim, file = "output_data/final_blacklisting/blacklists_final_native_clim.RData")





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

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_edaclim_native_ens$rank_2 <- results_rank_suitable_habitat_fraction_edaclim_native_ens$rank
results_rank_mean_suitable_habitat_fraction_edaclim_native_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_edaclim_native_ens$rank
results_rank_number_suitable_islandgroups_edaclim_native_ens <- results_rank_number_suitable_islandgroups_edaclim_native_ens[order(results_rank_number_suitable_islandgroups_edaclim_native_ens$rank), ]
results_rank_number_suitable_islandgroups_edaclim_native_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_edaclim_native_ens$rank)

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_clim_native_comp_ens$species, results_rank_suitable_habitat_fraction_edaclim_native_ens$species)
results_rank_suitable_habitat_fraction_edaclim_native_ens <- results_rank_suitable_habitat_fraction_edaclim_native_ens[order_indices, ]

# Bind the data frames
blacklists_final_native_edaclim <- rbind(results_rank_suitable_habitat_fraction_edaclim_native_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_edaclim_native_ens[,c(5:8)],
                                         results_rank_number_suitable_islandgroups_edaclim_native_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_native_edaclim) <- c("species", "rank", "blacklist", "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_native_edaclim, file = "output_data/final_blacklisting/blacklists_final_native_edaclim.RData")





#-------------------------------------------------------------------------------

# 3. Final blacklisting based on the ensemble predictions based on global
# occurrences and purely climatic data -----------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/global/clim_comp/results_rank_suitable_habitat_fraction_clim_global_comp.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/global/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_global_comp.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/global/clim_comp/results_rank_number_suitable_islandgroups_clim_global_comp.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_comp_ens <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_clim_global_comp_ens$blacklist <- "number_suitable"

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_clim_global_comp_ens$rank_2 <- results_rank_suitable_habitat_fraction_clim_global_comp_ens$rank
results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens$rank
results_rank_number_suitable_islandgroups_clim_global_comp_ens <- results_rank_number_suitable_islandgroups_clim_global_comp_ens[order(results_rank_number_suitable_islandgroups_clim_global_comp_ens$rank), ]
results_rank_number_suitable_islandgroups_clim_global_comp_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_global_comp_ens$rank)

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_clim_native_comp_ens$species, results_rank_suitable_habitat_fraction_clim_global_comp_ens$species)
results_rank_suitable_habitat_fraction_clim_global_comp_ens <- results_rank_suitable_habitat_fraction_clim_global_comp_ens[order_indices, ]

# Bind the data frames
blacklists_final_global_clim_comp <- rbind(results_rank_suitable_habitat_fraction_clim_global_comp_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_clim_global_comp_ens[,c(5:8)],
                                     results_rank_number_suitable_islandgroups_clim_global_comp_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_global_clim_comp) <- c("species", "rank", "blacklist", "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_global_clim_comp, file = "output_data/final_blacklisting/blacklists_final_global_clim_comp.RData")



# (b) based on 49 island groups covered by climatic data -----------------------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/global/clim/results_rank_suitable_habitat_fraction_clim_global.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/global/clim/results_rank_mean_suitable_habitat_fraction_clim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/global/clim/results_rank_number_suitable_islandgroups_clim_global.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_suitable_habitat_fraction_clim_global, results_rank_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_clim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_clim_global, results_rank_mean_suitable_habitat_fraction_clim_global$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_clim_global_ens <- subset(results_rank_number_suitable_islandgroups_clim_global, results_rank_number_suitable_islandgroups_clim_global$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_clim_global_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_clim_global_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_clim_global_ens$blacklist <- "number_suitable"

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_clim_global_ens$rank_2 <- results_rank_suitable_habitat_fraction_clim_global_ens$rank
results_rank_mean_suitable_habitat_fraction_clim_global_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_global_ens$rank
results_rank_number_suitable_islandgroups_clim_global_ens <- results_rank_number_suitable_islandgroups_clim_global_ens[order(results_rank_number_suitable_islandgroups_clim_global_ens$rank), ]
results_rank_number_suitable_islandgroups_clim_global_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_global_ens$rank)

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_clim_native_ens$species, results_rank_suitable_habitat_fraction_clim_global_ens$species)
results_rank_suitable_habitat_fraction_clim_global_ens <- results_rank_suitable_habitat_fraction_clim_global_ens[order_indices, ]

# Bind the data frames
blacklists_final_global_clim <- rbind(results_rank_suitable_habitat_fraction_clim_global_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_clim_global_ens[,c(5:8)],
                                      results_rank_number_suitable_islandgroups_clim_global_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_global_clim) <- c("species", "rank", "blacklist", "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_global_clim, file = "output_data/final_blacklisting/blacklists_final_global_clim.RData")





#-------------------------------------------------------------------------------

# 4. Final blacklisting based on the ensemble predictions based on global
# occurrences and combined climatic and edaphic data ---------------------------


# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/global/edaclim/results_rank_suitable_habitat_fraction_edaclim_global.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/global/edaclim/results_rank_mean_suitable_habitat_fraction_edaclim_global.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/global/edaclim/results_rank_number_suitable_islandgroups_edaclim_global.RData")

# Solely retain the results based on ensemble models
results_rank_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_global, results_rank_mean_suitable_habitat_fraction_edaclim_global$algorithm == "Ensemble")
results_rank_number_suitable_islandgroups_edaclim_global_ens <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == "Ensemble")

# Add a column that indicates the blacklist definition
results_rank_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "total_suitable_fraction"
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens$blacklist <- "mean_suitable_fraction"
results_rank_number_suitable_islandgroups_edaclim_global_ens$blacklist <- "number_suitable"

# Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
# (when 10 species take ranking position one, the 11th species takes ranking position eleven)
results_rank_suitable_habitat_fraction_edaclim_global_ens$rank_2 <- results_rank_suitable_habitat_fraction_edaclim_global_ens$rank
results_rank_mean_suitable_habitat_fraction_edaclim_global_ens$rank_2 <- results_rank_mean_suitable_habitat_fraction_edaclim_global_ens$rank
results_rank_number_suitable_islandgroups_edaclim_global_ens <- results_rank_number_suitable_islandgroups_edaclim_global_ens[order(results_rank_number_suitable_islandgroups_edaclim_global_ens$rank), ]
results_rank_number_suitable_islandgroups_edaclim_global_ens$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_edaclim_global_ens$rank)

# Order the data frame based on the first data frame based on predictor set "natclim"
order_indices <- match(results_rank_suitable_habitat_fraction_clim_native_comp_ens$species, results_rank_suitable_habitat_fraction_edaclim_global_ens$species)
results_rank_suitable_habitat_fraction_edaclim_global_ens <- results_rank_suitable_habitat_fraction_edaclim_global_ens[order_indices, ]

# Bind the data frames
blacklists_final_global_edaclim <- rbind(results_rank_suitable_habitat_fraction_edaclim_global_ens[,c(5:8)], results_rank_mean_suitable_habitat_fraction_edaclim_global_ens[,c(5:8)],
                                         results_rank_number_suitable_islandgroups_edaclim_global_ens[,c(5:8)])

# Make sure the column names are correct
colnames(blacklists_final_global_edaclim) <- c("species", "rank", "blacklist",  "rank_2")

# Save the data frame containing final blacklists
save(blacklists_final_global_edaclim, file = "output_data/final_blacklisting/blacklists_final_global_edaclim.RData")








