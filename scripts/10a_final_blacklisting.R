# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                        10a. Final blacklisting                         #
# ---------------------------------------------------------------------- #


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



