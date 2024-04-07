# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    10b. Final blacklisting plots                       #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)



#-------------------------------------------------------------------------------

# 1. Final blacklisting plot based on the ensemble predictions based on native
# occurrences and purely climatic data -----------------------------------------

# based on 25 island groups covered by climatic and edaphic data ---------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_native_clim_comp.RData")

# Make sure blacklists and species are factors
blacklists_final_native_clim_comp$blacklist <- as.factor(blacklists_final_native_clim_comp$blacklist)
blacklists_final_native_clim_comp$species <- as.factor(blacklists_final_native_clim_comp$species)

# Plot
ggplot(blacklists_final_native_clim_comp, aes(x = species, y = blacklist, fill = rank)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "black", high = "white", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("total suitable fraction", "mean suitable fraction", "total suitable number")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black"), legend.key.size = unit(0.8, "lines"),
        legend.key.width = unit(0.6, "cm"))


ggsave("output_data/plots/final_blacklisting/blacklists_final_native_clim_comp.svg", width = 15, height = 5)






#-------------------------------------------------------------------------------

# 1. Final blacklisting plot total ensemble ------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Load needed data
load("output_data/final_blacklisting/blacklists_final_comp.RData")

# Make sure blacklists and species are factors
blacklists_final_comp$blacklist <- as.factor(blacklists_final_comp$blacklist)
blacklists_final_comp$species <- as.factor(blacklists_final_comp$species)

# Plot
ggplot(blacklists_final_comp, aes(x = species, y = blacklist, fill = final_rank)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "black", high = "white", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("total suitable fraction", "mean suitable fraction", "total suitable number")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black"), legend.key.size = unit(0.8, "lines"),
        legend.key.width = unit(0.6, "cm"))


ggsave("output_data/plots/final_blacklisting/blacklists_final_comp.svg", width = 15, height = 5)



# (b) based on 49 island groups covered by climatic data -----------------------

# Load needed data
load("output_data/final_blacklisting/blacklists_final.RData")

# Make sure blacklists and species are factors
blacklists_final$blacklist <- as.factor(blacklists_final$blacklist)
blacklists_final$species <- as.factor(blacklists_final$species)

# Plot
ggplot(blacklists_final, aes(x = species, y = blacklist, fill = final_rank)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "black", high = "white", name = "Ranking\nposition") +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species", y = "Blacklist") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("total suitable fraction", "mean suitable fraction", "total suitable number")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black"), legend.key.size = unit(0.8, "lines"),
        legend.key.width = unit(0.6, "cm"))


ggsave("output_data/plots/final_blacklisting/blacklists_final.svg", width = 15, height = 5)


  
  

