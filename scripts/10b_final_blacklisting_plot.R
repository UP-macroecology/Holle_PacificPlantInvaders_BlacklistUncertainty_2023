# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                     10. Final blacklisting plot                        #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)


#-------------------------------------------------------------------------------

# 1. Test ----------------------------------------------------------------------

# Create data
df1 <- data.frame(species = 1:82, rank = 1:82)
df2 <- data.frame(species = 1:82, rank = sample(1:82, 82, replace = FALSE))
df3 <- data.frame(species = 1:82, rank = sample(1:82, 82, replace = FALSE))

df1$blacklist <- "suitable_fraction"
df2$blacklist <- "number_suitable"
df3$blacklist <- "mean_rank"

df_ranks <- rbind(df1, df2)
df_ranks <- rbind(df_ranks, df3)

df_ranks$blacklist <- as.factor(df_ranks$blacklist)
df_ranks$species <- as.factor(df_ranks$species)


ggplot(df_ranks, aes(x = species, y = blacklist, fill = rank)) +
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


ggsave("output_data/plots/final_blacklisting/final_blacklisting.svg", width = 15, height = 5)


  
  

