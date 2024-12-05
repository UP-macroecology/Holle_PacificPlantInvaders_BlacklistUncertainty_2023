# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    10b. Final blacklisting plots                       #
# ---------------------------------------------------------------------- #


# Load needed packages
library(ggplot2)
library(dplyr)
library(showtext)



#-------------------------------------------------------------------------------

# 1. Final blacklisting plot based on the ensemble predictions based on native
# occurrences and purely climatic data -----------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_native_clim_comp.RData")

# Make sure blacklists and species are factors
blacklists_final_native_clim_comp$blacklist <- as.factor(blacklists_final_native_clim_comp$blacklist)
blacklists_final_native_clim_comp$species <- as.factor(blacklists_final_native_clim_comp$species)

# Fix the order of blacklists
blacklists_final_native_clim_comp$blacklist <- factor(blacklists_final_native_clim_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_native_clim_comp$species[1:82]
blacklists_final_native_clim_comp$species <- factor(blacklists_final_native_clim_comp$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_native_clim_comp$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_clim_native[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "number_suitable"] <- validation
  
    } else { validation <- "*"
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "number_suitable"] <- validation
  }
    
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_native_clim_comp, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none") +
  geom_text(label = blacklists_final_native_clim_comp$validation)


ggsave("output_data/plots/final_blacklisting/blacklists_final_native_clim_comp.svg", width = 19, height = 3, unit = "cm")



# (b) based on 49 island groups covered by climatic data -----------------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_native_clim.RData")

# Make sure blacklists and species are factors
blacklists_final_native_clim$blacklist <- as.factor(blacklists_final_native_clim$blacklist)
blacklists_final_native_clim$species <- as.factor(blacklists_final_native_clim$species)

# Fix the order of blacklists
blacklists_final_native_clim$blacklist <- factor(blacklists_final_native_clim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_native_clim$species[1:82]
blacklists_final_native_clim$species <- factor(blacklists_final_native_clim$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_native_clim$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_clim_native[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "number_suitable"] <- validation
  
  } else { validation <- "*"
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "number_suitable"] <- validation
  }
  
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_native_clim, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: native, Predictor type: climate") +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = "none") +
  geom_text(label = blacklists_final_native_clim$validation)


ggsave("output_data/plots/final_blacklisting/blacklists_final_native_clim.svg",  width = 19, height = 3, unit = "cm")



#-------------------------------------------------------------------------------

# 2. Final blacklisting plot based on the ensemble predictions based on native
# occurrences and combined climatic and edaphic data ---------------------------

# based on 25 island groups covered by climatic and edaphic data ---------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_native_edaclim.RData")

# Make sure blacklists and species are factors
blacklists_final_native_edaclim$blacklist <- as.factor(blacklists_final_native_edaclim$blacklist)
blacklists_final_native_edaclim$species <- as.factor(blacklists_final_native_edaclim$species)

# Fix the order of blacklists
blacklists_final_native_edaclim$blacklist <- factor(blacklists_final_native_edaclim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_native_edaclim$species[1:82]
blacklists_final_native_edaclim$species <- factor(blacklists_final_native_edaclim$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_native_edaclim$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_edaclim_native[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "number_suitable"] <- validation
  
  } else { validation <- "*"
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "number_suitable"] <- validation
  }
  
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_native_edaclim, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: native, Predictor type: climate + edaphic") +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = FALSE) +
  geom_text(label = blacklists_final_native_edaclim$validation)
  


ggsave("output_data/plots/final_blacklisting/blacklists_final_native_edaclim.svg", width = 19, height = 3, unit = "cm")




#-------------------------------------------------------------------------------

# 3. Final blacklisting plot based on the ensemble predictions based on global
# occurrences and purely climatic data -----------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_global_clim_comp.RData")

# Make sure blacklists and species are factors
blacklists_final_global_clim_comp$blacklist <- as.factor(blacklists_final_global_clim_comp$blacklist)
blacklists_final_global_clim_comp$species <- as.factor(blacklists_final_global_clim_comp$species)

# Fix the order of blacklists
blacklists_final_global_clim_comp$blacklist <- factor(blacklists_final_global_clim_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_global_clim_comp$species[1:82]
blacklists_final_global_clim_comp$species <- factor(blacklists_final_global_clim_comp$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_global_clim_comp$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_clim_global[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "number_suitable"] <- validation
  
  } else { validation <- "*"
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "number_suitable"] <- validation
  }
  
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_global_clim_comp, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: global, Predictor type: climate") +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = FALSE) +
  geom_text(label = blacklists_final_global_clim_comp$validation)


ggsave("output_data/plots/final_blacklisting/blacklists_final_global_clim_comp.svg", width = 19, height = 3, unit = "cm")



# (b) based on 49 island groups covered by climatic data -----------------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_global_clim.RData")

# Make sure blacklists and species are factors
blacklists_final_global_clim$blacklist <- as.factor(blacklists_final_global_clim$blacklist)
blacklists_final_global_clim$species <- as.factor(blacklists_final_global_clim$species)

# Fix the order of blacklists
blacklists_final_global_clim$blacklist <- factor(blacklists_final_global_clim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_global_clim$species[1:82]
blacklists_final_global_clim$species <- factor(blacklists_final_global_clim$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_global_clim$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_clim_global[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "number_suitable"] <- validation
  
  } else { validation <- "*"
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "number_suitable"] <- validation
  }
  
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_global_clim, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: global, Predictor type: climate") +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(),
        axis.title.x = element_blank(), axis.text.y = element_blank()) +
  guides(fill = FALSE) +
  geom_text(label = blacklists_final_global_clim$validation)


ggsave("output_data/plots/final_blacklisting/blacklists_final_global_clim.svg", width = 19, height = 3, unit = "cm")




#-------------------------------------------------------------------------------

# 4. Final blacklisting plot based on the ensemble predictions based on native
# occurrences and combined climatic and edaphic data ---------------------------

# based on 25 island groups covered by climatic and edaphic data ---------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_global_edaclim.RData")

# Make sure blacklists and species are factors
blacklists_final_global_edaclim$blacklist <- as.factor(blacklists_final_global_edaclim$blacklist)
blacklists_final_global_edaclim$species <- as.factor(blacklists_final_global_edaclim$species)

# Fix the order of blacklists
blacklists_final_global_edaclim$blacklist <- factor(blacklists_final_global_edaclim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Fix the order of species
species_order <- blacklists_final_global_edaclim$species[1:82]
blacklists_final_global_edaclim$species <- factor(blacklists_final_global_edaclim$species, levels = species_order)

# Add an asterisk for species that have ensemble validation values of TSS < 0.5
blacklists_final_global_edaclim$validation <- NA # Create a new column
study_species <- as.character(species_order) # Turn species into character vector

for (sp in study_species) { # Start the loop over all species
  
  # Load in the ensemble validation outcomes
  load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
  
  # Check the TSS value
  check_TSS <- ensemble_perf_edaclim_global[1,3]
  
  # TSS values above and below 0.5 are noted
  if (check_TSS >= 0.5) { validation <- ""
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "number_suitable"] <- validation
  
  } else { validation <- "*"
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "total_suitable_fraction"] <- validation
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "mean_suitable_fraction"] <- validation
  blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "number_suitable"] <- validation
  }
  
} # Close the loop over all species 

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_global_edaclim, aes(x = species, y = blacklist, fill = rank_2)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "firebrick4", high = "snow", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  ggtitle("Species data: global, Predictor type: climate + edaphic") +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), legend.key.size = unit(0.8, "lines"), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.8, "cm"), text = element_text(family = "Calibri"), axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), legend.direction = "horizontal", legend.position = c("none"), legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        legend.justification = "center") +
  #guides(fill = FALSE) +
  geom_text(label = blacklists_final_global_edaclim$validation)


ggsave("output_data/plots/final_blacklisting/blacklists_final_global_edaclim.svg", width = 19, height = 3, unit = "cm")










  
  

