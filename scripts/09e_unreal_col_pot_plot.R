# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             09e. Unrealized colonization potential plot                #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)



#-------------------------------------------------------------------------------

# 1. Boxplot of unrealized colonization potential ------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Needed objects (results of unrealized colonization potential of all four predictor sets)
load("output_data/unrealized_col_pot/native/clim_comp/unreal_col_pot_clim_native_comp.RData")
load("output_data/unrealized_col_pot/native/edaclim/unreal_col_pot_edaclim_native.RData")
load("output_data/unrealized_col_pot/global/clim_comp/unreal_col_pot_clim_global_comp.RData")
load("output_data/unrealized_col_pot/global/edaclim/unreal_col_pot_edaclim_global.RData")


# Bind the four data frames into one data frame
unreal_col_pot_25 <- rbind(unreal_col_pot_clim_native_comp, unreal_col_pot_edaclim_native,
                           unreal_col_pot_clim_global_comp, unreal_col_pot_edaclim_global)

# Convert unrealized colonization potential values into numeric values
unreal_col_pot_25$unrealized_col_pot <- as.numeric(as.character(unreal_col_pot_25$unrealized_col_pot))

# Construct boxplot
ggplot(unreal_col_pot_25, aes(x = predictor_set, y = unrealized_col_pot, fill = predictor_set)) + 
  stat_boxplot(geom = "errorbar", width=0.4) +
  geom_boxplot(width = 0.3, outlier.colour="black", outlier.shape=16, outlier.size=2) +
  labs(y = "Predicted unrealized colonization potential", x = "Predictor set") +
  #xlab("") +
  scale_x_discrete(breaks=c(1, 2, 3, 4),
                   labels=c("natclim", "natclim+eda", "globclim", "globclim+eda"))+
  scale_fill_manual(values = c("#6699CC", "#CC6677", "#27408B", "firebrick4")) +
  theme(legend.position = "none", axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), text = element_text(family = "Calibri"))

# Save the plot
ggsave("output_data/plots/unrealized_col_pot/unreal_col_pot_25.svg", width = 12, height = 7)




# (b) based on 49 island groups covered by climatic data -----------------------

# Needed objects (results of unrealized colonization potential of all four predictor sets)
load("output_data/unrealized_col_pot/native/clim/unreal_col_pot_clim_native.RData")
load("output_data/unrealized_col_pot/global/clim/unreal_col_pot_clim_global.RData")

# Bind the two data frame into one data frame
unreal_col_pot_49 <- rbind(unreal_col_pot_clim_native, unreal_col_pot_clim_global)

# Convert unrealized colonization potential values into numeric values
unreal_col_pot_49$unrealized_col_pot <- as.numeric(as.character(unreal_col_pot_49$unrealized_col_pot))

# Construct boxplot
ggplot(unreal_col_pot_49, aes(x = predictor_set, y = unrealized_col_pot, fill = predictor_set)) + 
  stat_boxplot(geom = "errorbar", width=0.4) +
  geom_boxplot(width = 0.3, outlier.colour="black", outlier.shape=16, outlier.size=2) +
  labs(y = "Predicted unrealized colonization potential", x = "Predictor set") +
  #xlab("") +
  scale_x_discrete(breaks=c(1, 3),
                   labels=c("natclim", "globclim"))+
  scale_fill_manual(values = c("#6699CC", "#27408B")) +
  theme(legend.position = "none", axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), text = element_text(family = "Calibri")) + 
  scale_y_continuous(limits = c(0,1))

# Save the plot
ggsave("output_data/plots/unrealized_col_pot/unreal_col_pot_49.svg", width = 12, height = 7)
