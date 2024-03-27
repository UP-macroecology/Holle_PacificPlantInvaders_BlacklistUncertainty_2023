# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    10b. Final blacklisting plot                        #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)



#-------------------------------------------------------------------------------

# 1. Final blacklisting plot ---------------------------------------------------

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
  scale_y_discrete(breaks=c("suitable_fraction", "number_suitable", "mean_rank"),
                   labels=c("suitable fraction", "suitable number", "mean rank")) +
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
  scale_y_discrete(breaks=c("suitable_fraction", "number_suitable", "mean_rank"),
                   labels=c("suitable fraction", "suitable number", "mean rank")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black"), legend.key.size = unit(0.8, "lines"),
        legend.key.width = unit(0.6, "cm"))


ggsave("output_data/plots/final_blacklisting/blacklists_final.svg", width = 15, height = 5)


  
  

