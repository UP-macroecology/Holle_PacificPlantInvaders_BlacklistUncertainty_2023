# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                   10c. Algorithm blacklisting plots                    #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)
library(dplyr)
library(showtext)

# Create needed objects
# Write a vector with the used algorithms
algorithm <- c("GLM", "GAM", "RF", "BRT")




#-------------------------------------------------------------------------------

# 1. Final blacklisting based on the predictions of the four different
# model algorithms based on native occurrences and purely climatic data --------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Read in the data frames containing the blacklisting results
# Pacific-wide total suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_suitable_habitat_fraction_clim_native_comp.RData")

# Mean suitable habitat fraction
load("output_data/blacklists/native/clim_comp/results_rank_mean_suitable_habitat_fraction_clim_native_comp.RData")

# Number of suitable Pacific island groups
load("output_data/blacklists/native/clim_comp/results_rank_number_suitable_islandgroups_clim_native_comp.RData")

# Create one results data frame per blacklist definition to store blacklist ranking
# results based on the different algorithms
blacklists_algorithms_total_fraction_native_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_total_fraction_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_mean_fraction_native_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_mean_fraction_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_number_suitable_native_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_number_suitable_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Solely retain the results based on the certain algorithm
  results_rank_suitable_habitat_fraction_clim_native_comp_alg <- subset(results_rank_suitable_habitat_fraction_clim_native_comp, results_rank_suitable_habitat_fraction_clim_native_comp$algorithm == a)
  results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg <- subset(results_rank_mean_suitable_habitat_fraction_clim_native_comp, results_rank_mean_suitable_habitat_fraction_clim_native_comp$algorithm == a)
  results_rank_number_suitable_islandgroups_clim_native_comp_alg <- subset(results_rank_number_suitable_islandgroups_clim_native_comp, results_rank_number_suitable_islandgroups_clim_native_comp$algorithm == a)
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_native_comp_alg$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_native_comp_alg$blacklist <- "number_suitable"
  
  # Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
  # (when 10 species take ranking position one, the 11th species takes ranking position eleven)
  results_rank_suitable_habitat_fraction_clim_native_comp_alg$rank_2 <- results_rank_suitable_habitat_fraction_clim_native_comp_alg$rank
  results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg$rank
  results_rank_number_suitable_islandgroups_clim_native_comp_alg <- results_rank_number_suitable_islandgroups_clim_native_comp_alg[order(results_rank_number_suitable_islandgroups_clim_native_comp_alg$rank), ]
  results_rank_number_suitable_islandgroups_clim_native_comp_alg$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_native_comp_alg$rank)
  
  # Order the first data frame of each blacklist by rank based on the GLM algorithm 
  # of the very first blacklist ranking, functioning as reference blacklist in 
  # species positions
  if (a == "GLM") {
    # Order the first blacklist based on GLM algorithm by ranking positions as
    # reference blacklist
    results_rank_suitable_habitat_fraction_clim_native_comp_alg <- results_rank_suitable_habitat_fraction_clim_native_comp_alg[order(results_rank_suitable_habitat_fraction_clim_native_comp_alg$rank), ]
    # Order blacklist based on the mean suitable habitat fraction according to
    # species order in reference blacklist
    order_indices_GLM_mean_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg$species)
    results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg <- results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg[order_indices_GLM_mean_suitable, ]
    # Order blacklist based on the number of suitable island groups according to
    # species order in reference blacklist
    order_indices_GLM_number_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_number_suitable_islandgroups_clim_native_comp_alg$species)
    results_rank_number_suitable_islandgroups_clim_native_comp_alg <- results_rank_number_suitable_islandgroups_clim_native_comp_alg[order_indices_GLM_number_suitable, ]
  }
  
  # Bind the data frames to the corresponding results data frames
  blacklists_algorithms_total_fraction_native_clim_comp <- rbind(blacklists_algorithms_total_fraction_native_clim_comp, results_rank_suitable_habitat_fraction_clim_native_comp_alg[,c(2, 5:8)])
  blacklists_algorithms_mean_fraction_native_clim_comp <- rbind(blacklists_algorithms_mean_fraction_native_clim_comp, results_rank_mean_suitable_habitat_fraction_clim_native_comp_alg[,c(2, 5:8)])
  blacklists_algorithms_number_suitable_native_clim_comp <- rbind(blacklists_algorithms_number_suitable_native_clim_comp, results_rank_number_suitable_islandgroups_clim_native_comp_alg[,c(2, 5:8)])
  
  # Make sure the column names are correct
  colnames(blacklists_algorithms_total_fraction_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_mean_fraction_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_number_suitable_native_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  
}

# Save the data frames containing final blacklists based on the different algorithms
save(blacklists_algorithms_total_fraction_native_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_native_clim_comp.RData")
save(blacklists_algorithms_mean_fraction_native_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_native_clim_comp.RData")
save(blacklists_algorithms_number_suitable_native_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_native_clim_comp.RData")



# (b) corresponding plots ------------------------------------------------------

# Total suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_total_fraction_native_clim_comp$algorithm <- as.factor(blacklists_algorithms_total_fraction_native_clim_comp$algorithm)
blacklists_algorithms_total_fraction_native_clim_comp$species <- as.factor(blacklists_algorithms_total_fraction_native_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_total_fraction_native_clim_comp$algorithm <- factor(blacklists_algorithms_total_fraction_native_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_total_fraction_native_clim_comp$species[1:82]
blacklists_algorithms_total_fraction_native_clim_comp$species <- factor(blacklists_algorithms_total_fraction_native_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_total_fraction_native_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")


ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_native_clim_comp.svg", width = 14, height = 4)


# Mean suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_mean_fraction_native_clim_comp$algorithm <- as.factor(blacklists_algorithms_mean_fraction_native_clim_comp$algorithm)
blacklists_algorithms_mean_fraction_native_clim_comp$species <- as.factor(blacklists_algorithms_mean_fraction_native_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_mean_fraction_native_clim_comp$algorithm <- factor(blacklists_algorithms_mean_fraction_native_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_mean_fraction_native_clim_comp$species[1:82]
blacklists_algorithms_mean_fraction_native_clim_comp$species <- factor(blacklists_algorithms_mean_fraction_native_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_mean_fraction_native_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")
  
  
ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_native_clim_comp.svg", width = 14, height = 4)


# Number of suitable island groups
# Make sure algorithms and species are factors
blacklists_algorithms_number_suitable_native_clim_comp$algorithm <- as.factor(blacklists_algorithms_number_suitable_native_clim_comp$algorithm)
blacklists_algorithms_number_suitable_native_clim_comp$species <- as.factor(blacklists_algorithms_number_suitable_native_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_number_suitable_native_clim_comp$algorithm <- factor(blacklists_algorithms_number_suitable_native_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_number_suitable_native_clim_comp$species[1:82]
blacklists_algorithms_number_suitable_native_clim_comp$species <- factor(blacklists_algorithms_number_suitable_native_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_number_suitable_native_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_native_clim_comp.svg", width = 14, height = 4)



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

# Create one results data frame per blacklist definition to store blacklist ranking
# results based on the different algorithms
blacklists_algorithms_total_fraction_native_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_total_fraction_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_mean_fraction_native_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_mean_fraction_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_number_suitable_native_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_number_suitable_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Solely retain the results based on the certain algorithm
  results_rank_suitable_habitat_fraction_edaclim_native_alg <- subset(results_rank_suitable_habitat_fraction_edaclim_native, results_rank_suitable_habitat_fraction_edaclim_native$algorithm == a)
  results_rank_mean_suitable_habitat_fraction_edaclim_native_alg <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_native, results_rank_mean_suitable_habitat_fraction_edaclim_native$algorithm == a)
  results_rank_number_suitable_islandgroups_edaclim_native_alg <- subset(results_rank_number_suitable_islandgroups_edaclim_native, results_rank_number_suitable_islandgroups_edaclim_native$algorithm == a)
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_edaclim_native_alg$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_edaclim_native_alg$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_edaclim_native_alg$blacklist <- "number_suitable"
  
  # Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
  # (when 10 species take ranking position one, the 11th species takes ranking position eleven)
  results_rank_suitable_habitat_fraction_edaclim_native_alg$rank_2 <- results_rank_suitable_habitat_fraction_edaclim_native_alg$rank
  results_rank_mean_suitable_habitat_fraction_edaclim_native_alg$rank_2 <- results_rank_mean_suitable_habitat_fraction_edaclim_native_alg$rank
  results_rank_number_suitable_islandgroups_edaclim_native_alg <- results_rank_number_suitable_islandgroups_edaclim_native_alg[order(results_rank_number_suitable_islandgroups_edaclim_native_alg$rank), ]
  results_rank_number_suitable_islandgroups_edaclim_native_alg$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_edaclim_native_alg$rank)
  
  # Order the first data frame of each blacklist by rank based on the GLM algorithm 
  # of the very first blacklist ranking, functioning as reference blacklist in 
  # species positions
  if (a == "GLM") {
    # Order blacklist based on the total suitable habitat fraction
    order_indices_GLM_total_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_suitable_habitat_fraction_edaclim_native_alg$species)
    results_rank_suitable_habitat_fraction_edaclim_native_alg <- results_rank_suitable_habitat_fraction_edaclim_native_alg[order_indices_GLM_total_suitable, ]
    # Order blacklist based on the mean suitable habitat fraction
    order_indices_GLM_mean_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_mean_suitable_habitat_fraction_edaclim_native_alg$species)
    results_rank_mean_suitable_habitat_fraction_edaclim_native_alg <- results_rank_mean_suitable_habitat_fraction_edaclim_native_alg[order_indices_GLM_mean_suitable, ]
    # Order blacklist based on the number of suitable island groups
    order_indices_GLM_number_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_number_suitable_islandgroups_edaclim_native_alg$species)
    results_rank_number_suitable_islandgroups_edaclim_native_alg <- results_rank_number_suitable_islandgroups_edaclim_native_alg[order_indices_GLM_number_suitable, ]
  }
  
  # Bind the data frames to the corresponding results data frames
  blacklists_algorithms_total_fraction_native_edaclim <- rbind(blacklists_algorithms_total_fraction_native_edaclim, results_rank_suitable_habitat_fraction_edaclim_native_alg[,c(2, 5:8)])
  blacklists_algorithms_mean_fraction_native_edaclim <- rbind(blacklists_algorithms_mean_fraction_native_edaclim, results_rank_mean_suitable_habitat_fraction_edaclim_native_alg[,c(2, 5:8)])
  blacklists_algorithms_number_suitable_native_edaclim <- rbind(blacklists_algorithms_number_suitable_native_edaclim, results_rank_number_suitable_islandgroups_edaclim_native_alg[,c(2, 5:8)])
  
  # Make sure the column names are correct
  colnames(blacklists_algorithms_total_fraction_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_mean_fraction_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_number_suitable_native_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  
}

# Save the data frames containing final blacklists based on the different algorithms
save(blacklists_algorithms_total_fraction_native_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_native_edaclim.RData")
save(blacklists_algorithms_mean_fraction_native_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_native_edaclim.RData")
save(blacklists_algorithms_number_suitable_native_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_native_edaclim.RData")




# (b) corresponding plots ------------------------------------------------------

# Total suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_total_fraction_native_edaclim$algorithm <- as.factor(blacklists_algorithms_total_fraction_native_edaclim$algorithm)
blacklists_algorithms_total_fraction_native_edaclim$species <- as.factor(blacklists_algorithms_total_fraction_native_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_total_fraction_native_edaclim$algorithm <- factor(blacklists_algorithms_total_fraction_native_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_total_fraction_native_edaclim$species[1:82]
blacklists_algorithms_total_fraction_native_edaclim$species <- factor(blacklists_algorithms_total_fraction_native_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_total_fraction_native_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Niche: native, Predictor type: climate + edaphic") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_native_edaclim.svg", width = 14, height = 4)


# Mean suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_mean_fraction_native_edaclim$algorithm <- as.factor(blacklists_algorithms_mean_fraction_native_edaclim$algorithm)
blacklists_algorithms_mean_fraction_native_edaclim$species <- as.factor(blacklists_algorithms_mean_fraction_native_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_mean_fraction_native_edaclim$algorithm <- factor(blacklists_algorithms_mean_fraction_native_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_mean_fraction_native_edaclim$species[1:82]
blacklists_algorithms_mean_fraction_native_edaclim$species <- factor(blacklists_algorithms_mean_fraction_native_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_mean_fraction_native_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")


ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_native_edaclim.svg", width = 14, height = 4)


# Number of suitable island groups
# Make sure algorithms and species are factors
blacklists_algorithms_number_suitable_native_edaclim$algorithm <- as.factor(blacklists_algorithms_number_suitable_native_edaclim$algorithm)
blacklists_algorithms_number_suitable_native_edaclim$species <- as.factor(blacklists_algorithms_number_suitable_native_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_number_suitable_native_edaclim$algorithm <- factor(blacklists_algorithms_number_suitable_native_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_number_suitable_native_edaclim$species[1:82]
blacklists_algorithms_number_suitable_native_edaclim$species <- factor(blacklists_algorithms_number_suitable_native_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_number_suitable_native_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_native_edaclim.svg", width = 14, height = 4)




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

# Create one results data frame per blacklist definition to store blacklist ranking
# results based on the different algorithms
blacklists_algorithms_total_fraction_global_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_total_fraction_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_mean_fraction_global_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_mean_fraction_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_number_suitable_global_clim_comp <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_number_suitable_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Solely retain the results based on the certain algorithm
  results_rank_suitable_habitat_fraction_clim_global_comp_alg <- subset(results_rank_suitable_habitat_fraction_clim_global_comp, results_rank_suitable_habitat_fraction_clim_global_comp$algorithm == a)
  results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg <- subset(results_rank_mean_suitable_habitat_fraction_clim_global_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp$algorithm == a)
  results_rank_number_suitable_islandgroups_clim_global_comp_alg <- subset(results_rank_number_suitable_islandgroups_clim_global_comp, results_rank_number_suitable_islandgroups_clim_global_comp$algorithm == a)
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_clim_global_comp_alg$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_clim_global_comp_alg$blacklist <- "number_suitable"
  
  # Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
  # (when 10 species take ranking position one, the 11th species takes ranking position eleven)
  results_rank_suitable_habitat_fraction_clim_global_comp_alg$rank_2 <- results_rank_suitable_habitat_fraction_clim_global_comp_alg$rank
  results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg$rank_2 <- results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg$rank
  results_rank_number_suitable_islandgroups_clim_global_comp_alg <- results_rank_number_suitable_islandgroups_clim_global_comp_alg[order(results_rank_number_suitable_islandgroups_clim_global_comp_alg$rank), ]
  results_rank_number_suitable_islandgroups_clim_global_comp_alg$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_clim_global_comp_alg$rank)
  
  # Order the first data frame of each blacklist by rank based on the GLM algorithm 
  # of the very first blacklist ranking, functioning as reference blacklist in 
  # species positions
  if (a == "GLM") {
    # Order blacklist based on the total suitable habitat fraction
    order_indices_GLM_total_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_suitable_habitat_fraction_clim_global_comp_alg$species)
    results_rank_suitable_habitat_fraction_clim_global_comp_alg <- results_rank_suitable_habitat_fraction_clim_global_comp_alg[order_indices_GLM_total_suitable, ]
    # Order blacklist based on the mean suitable habitat fraction
    order_indices_GLM_mean_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg$species)
    results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg <- results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg[order_indices_GLM_mean_suitable, ]
    # Order blacklist based on the number of suitable island groups
    order_indices_GLM_number_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_number_suitable_islandgroups_clim_global_comp_alg$species)
    results_rank_number_suitable_islandgroups_clim_global_comp_alg <- results_rank_number_suitable_islandgroups_clim_global_comp_alg[order_indices_GLM_number_suitable, ]
  }
  
  # Bind the data frames to the corresponding results data frames
  blacklists_algorithms_total_fraction_global_clim_comp <- rbind(blacklists_algorithms_total_fraction_global_clim_comp, results_rank_suitable_habitat_fraction_clim_global_comp_alg[,c(2, 5:8)])
  blacklists_algorithms_mean_fraction_global_clim_comp <- rbind(blacklists_algorithms_mean_fraction_global_clim_comp, results_rank_mean_suitable_habitat_fraction_clim_global_comp_alg[,c(2, 5:8)])
  blacklists_algorithms_number_suitable_global_clim_comp <- rbind(blacklists_algorithms_number_suitable_global_clim_comp, results_rank_number_suitable_islandgroups_clim_global_comp_alg[,c(2, 5:8)])
  
  # Make sure the column names are correct
  colnames(blacklists_algorithms_total_fraction_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_mean_fraction_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_number_suitable_global_clim_comp) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  
}

# Save the data frames containing final blacklists based on the different algorithms
save(blacklists_algorithms_total_fraction_global_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_global_clim_comp.RData")
save(blacklists_algorithms_mean_fraction_global_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_global_clim_comp.RData")
save(blacklists_algorithms_number_suitable_global_clim_comp, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_global_clim_comp.RData")


# (b) corresponding plots ------------------------------------------------------

# Total suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_total_fraction_global_clim_comp$algorithm <- as.factor(blacklists_algorithms_total_fraction_global_clim_comp$algorithm)
blacklists_algorithms_total_fraction_global_clim_comp$species <- as.factor(blacklists_algorithms_total_fraction_global_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_total_fraction_global_clim_comp$algorithm <- factor(blacklists_algorithms_total_fraction_global_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_total_fraction_global_clim_comp$species[1:82]
blacklists_algorithms_total_fraction_global_clim_comp$species <- factor(blacklists_algorithms_total_fraction_global_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_total_fraction_global_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Niche: global, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")


ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_global_clim_comp.svg", width = 14, height = 4)


# Mean suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_mean_fraction_global_clim_comp$algorithm <- as.factor(blacklists_algorithms_mean_fraction_global_clim_comp$algorithm)
blacklists_algorithms_mean_fraction_global_clim_comp$species <- as.factor(blacklists_algorithms_mean_fraction_global_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_mean_fraction_global_clim_comp$algorithm <- factor(blacklists_algorithms_mean_fraction_global_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_mean_fraction_global_clim_comp$species[1:82]
blacklists_algorithms_mean_fraction_global_clim_comp$species <- factor(blacklists_algorithms_mean_fraction_global_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_mean_fraction_global_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")


ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_global_clim_comp.svg", width = 14, height = 4)


# Number of suitable island groups
# Make sure algorithms and species are factors
blacklists_algorithms_number_suitable_global_clim_comp$algorithm <- as.factor(blacklists_algorithms_number_suitable_global_clim_comp$algorithm)
blacklists_algorithms_number_suitable_global_clim_comp$species <- as.factor(blacklists_algorithms_number_suitable_global_clim_comp$species)

# Fix the order of algorithms
blacklists_algorithms_number_suitable_global_clim_comp$algorithm <- factor(blacklists_algorithms_number_suitable_global_clim_comp$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_number_suitable_global_clim_comp$species[1:82]
blacklists_algorithms_number_suitable_global_clim_comp$species <- factor(blacklists_algorithms_number_suitable_global_clim_comp$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_number_suitable_global_clim_comp, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_global_clim_comp.svg", width = 14, height = 4)

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

# Create one results data frame per blacklist definition to store blacklist ranking
# results based on the different algorithms
blacklists_algorithms_total_fraction_global_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_total_fraction_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_mean_fraction_global_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_mean_fraction_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
blacklists_algorithms_number_suitable_global_edaclim <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(blacklists_algorithms_number_suitable_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")


for (a in algorithm) { # Start the loop over all algorithms
  
  # Solely retain the results based on the certain algorithm
  results_rank_suitable_habitat_fraction_edaclim_global_alg <- subset(results_rank_suitable_habitat_fraction_edaclim_global, results_rank_suitable_habitat_fraction_edaclim_global$algorithm == a)
  results_rank_mean_suitable_habitat_fraction_edaclim_global_alg <- subset(results_rank_mean_suitable_habitat_fraction_edaclim_global, results_rank_mean_suitable_habitat_fraction_edaclim_global$algorithm == a)
  results_rank_number_suitable_islandgroups_edaclim_global_alg <- subset(results_rank_number_suitable_islandgroups_edaclim_global, results_rank_number_suitable_islandgroups_edaclim_global$algorithm == a)
  
  # Add a column that indicates the blacklist definition
  results_rank_suitable_habitat_fraction_edaclim_global_alg$blacklist <- "total_suitable_fraction"
  results_rank_mean_suitable_habitat_fraction_edaclim_global_alg$blacklist <- "mean_suitable_fraction"
  results_rank_number_suitable_islandgroups_edaclim_global_alg$blacklist <- "number_suitable"
  
  # Add a column that contains modified ranks for the blacklist defined by the number of suitable island groups
  # (when 10 species take ranking position one, the 11th species takes ranking position eleven)
  results_rank_suitable_habitat_fraction_edaclim_global_alg$rank_2 <- results_rank_suitable_habitat_fraction_edaclim_global_alg$rank
  results_rank_mean_suitable_habitat_fraction_edaclim_global_alg$rank_2 <- results_rank_mean_suitable_habitat_fraction_edaclim_global_alg$rank
  results_rank_number_suitable_islandgroups_edaclim_global_alg <- results_rank_number_suitable_islandgroups_edaclim_global_alg[order(results_rank_number_suitable_islandgroups_edaclim_global_alg$rank), ]
  results_rank_number_suitable_islandgroups_edaclim_global_alg$rank_2 <- min_rank(results_rank_number_suitable_islandgroups_edaclim_global_alg$rank)
  
  # Order the first data frame of each blacklist by rank based on the GLM algorithm 
  # of the very first blacklist ranking, functioning as reference blacklist in 
  # species positions
  if (a == "GLM") {
    # Order blacklist based on the total suitable habitat fraction
    order_indices_GLM_total_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_suitable_habitat_fraction_edaclim_global_alg$species)
    results_rank_suitable_habitat_fraction_edaclim_global_alg <- results_rank_suitable_habitat_fraction_edaclim_global_alg[order_indices_GLM_total_suitable, ]
    # Order blacklist based on the mean suitable habitat fraction
    order_indices_GLM_mean_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_mean_suitable_habitat_fraction_edaclim_global_alg$species)
    results_rank_mean_suitable_habitat_fraction_edaclim_global_alg <- results_rank_mean_suitable_habitat_fraction_edaclim_global_alg[order_indices_GLM_mean_suitable, ]
    # Order blacklist based on the number of suitable island groups
    order_indices_GLM_number_suitable <- match(results_rank_suitable_habitat_fraction_clim_native_comp_alg$species, results_rank_number_suitable_islandgroups_edaclim_global_alg$species)
    results_rank_number_suitable_islandgroups_edaclim_global_alg <- results_rank_number_suitable_islandgroups_edaclim_global_alg[order_indices_GLM_number_suitable, ]
  }
  
  # Bind the data frames to the corresponding results data frames
  blacklists_algorithms_total_fraction_global_edaclim <- rbind(blacklists_algorithms_total_fraction_global_edaclim, results_rank_suitable_habitat_fraction_edaclim_global_alg[,c(2, 5:8)])
  blacklists_algorithms_mean_fraction_global_edaclim <- rbind(blacklists_algorithms_mean_fraction_global_edaclim, results_rank_mean_suitable_habitat_fraction_edaclim_global_alg[,c(2, 5:8)])
  blacklists_algorithms_number_suitable_global_edaclim <- rbind(blacklists_algorithms_number_suitable_global_edaclim, results_rank_number_suitable_islandgroups_edaclim_global_alg[,c(2, 5:8)])
  
  # Make sure the column names are correct
  colnames(blacklists_algorithms_total_fraction_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_mean_fraction_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  colnames(blacklists_algorithms_number_suitable_global_edaclim) <- c("algorithm", "species", "rank", "blacklist", "rank_2")
  
}

# Save the data frames containing final blacklists based on the different algorithms
save(blacklists_algorithms_total_fraction_global_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_global_edaclim.RData")
save(blacklists_algorithms_mean_fraction_global_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_global_edaclim.RData")
save(blacklists_algorithms_number_suitable_global_edaclim, file = "output_data/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_global_edaclim.RData")




# (b) corresponding plots ------------------------------------------------------

# Total suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_total_fraction_global_edaclim$algorithm <- as.factor(blacklists_algorithms_total_fraction_global_edaclim$algorithm)
blacklists_algorithms_total_fraction_global_edaclim$species <- as.factor(blacklists_algorithms_total_fraction_global_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_total_fraction_global_edaclim$algorithm <- factor(blacklists_algorithms_total_fraction_global_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_total_fraction_global_edaclim$species[1:82]
blacklists_algorithms_total_fraction_global_edaclim$species <- factor(blacklists_algorithms_total_fraction_global_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_total_fraction_global_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Niche: global, Predictor type: climate + edaphic") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_total_fraction_global_edaclim.svg", width = 14, height = 4)


# Mean suitable habitat fraction
# Make sure algorithms and species are factors
blacklists_algorithms_mean_fraction_global_edaclim$algorithm <- as.factor(blacklists_algorithms_mean_fraction_global_edaclim$algorithm)
blacklists_algorithms_mean_fraction_global_edaclim$species <- as.factor(blacklists_algorithms_mean_fraction_global_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_mean_fraction_global_edaclim$algorithm <- factor(blacklists_algorithms_mean_fraction_global_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_mean_fraction_global_edaclim$species[1:82]
blacklists_algorithms_mean_fraction_global_edaclim$species <- factor(blacklists_algorithms_mean_fraction_global_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_mean_fraction_global_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 13), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none")


ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_mean_fraction_global_edaclim.svg", width = 14, height = 4)


# Number of suitable island groups
# Make sure algorithms and species are factors
blacklists_algorithms_number_suitable_global_edaclim$algorithm <- as.factor(blacklists_algorithms_number_suitable_global_edaclim$algorithm)
blacklists_algorithms_number_suitable_global_edaclim$species <- as.factor(blacklists_algorithms_number_suitable_global_edaclim$species)

# Fix the order of algorithms
blacklists_algorithms_number_suitable_global_edaclim$algorithm <- factor(blacklists_algorithms_number_suitable_global_edaclim$algorithm, levels = c("BRT", "RF", "GAM", "GLM"))

# Fix the order of species
species_order <- blacklists_algorithms_number_suitable_global_edaclim$species[1:82]
blacklists_algorithms_number_suitable_global_edaclim$species <- factor(blacklists_algorithms_number_suitable_global_edaclim$species, levels = species_order)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_algorithms_number_suitable_global_edaclim, aes(x = species, y = algorithm, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  #ggtitle("Niche: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("GLM", "GAM", "RF", "BRT"),
                   labels=c("GLM", "GAM", "RF", "BRT")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 22), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15)) +
  guides(fill = "none")



ggsave("output_data/plots/final_blacklisting/algorithm_blacklisting/blacklists_algorithms_number_suitable_global_edaclim.svg", width = 14, height = 4)
