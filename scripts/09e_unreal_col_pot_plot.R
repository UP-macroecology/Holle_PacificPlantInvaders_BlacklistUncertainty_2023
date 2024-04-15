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
library(showtext)



#-------------------------------------------------------------------------------

# 1. Boxplot of unrealized colonization potential ------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Needed objects (results of unrealized colonization potential based on all four predictor sets
# and the algorithms)
load("output_data/unrealized_col_pot/native/clim_comp/unreal_col_pot_clim_native_comp.RData")
load("output_data/unrealized_col_pot/native/edaclim/unreal_col_pot_edaclim_native.RData")
load("output_data/unrealized_col_pot/global/clim_comp/unreal_col_pot_clim_global_comp.RData")
load("output_data/unrealized_col_pot/global/edaclim/unreal_col_pot_edaclim_global.RData")


# Bind the four data frames into one data frame
unreal_col_pot_25 <- rbind(unreal_col_pot_clim_native_comp, unreal_col_pot_edaclim_native,
                           unreal_col_pot_clim_global_comp, unreal_col_pot_edaclim_global)

# Solely retain the unrealized colonization potential of ensemble models
unreal_col_pot_25_ens <- subset(unreal_col_pot_25, unreal_col_pot_25$algorithm == "Ensemble")

# Convert unrealized colonization potential values into numeric values
unreal_col_pot_25_ens$unrealized_col_pot <- as.numeric(as.character(unreal_col_pot_25_ens$unrealized_col_pot))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Construct boxplot
ggplot(unreal_col_pot_25_ens, aes(x = predictor_set, y = unrealized_col_pot, fill = predictor_set)) + 
  stat_boxplot(geom = "errorbar", width=0.4) +
  geom_boxplot(width = 0.4, outlier.colour="black", outlier.shape=16, outlier.size=1.6) +
  theme_minimal() +
  labs(y = "Predicted unrealised colonisation potential", x = "") +
  scale_x_discrete(breaks=c(1, 2, 3, 4),
                   labels=c("natclim", "natclim+eda", "globclim", "globclim+eda"))+
  scale_fill_manual(values = c("lightgrey", "lightgrey", "lightgrey", "lightgrey")) +
  theme(legend.position = "none", axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"), text = element_text(family = "Calibri"))

# Save the plot
ggsave("output_data/plots/unrealized_col_pot/unreal_col_pot_25.svg", width = 5, height = 4)

#scale_fill_manual(values = c("#6699CC", "#CC6677", "#27408B", "firebrick4")) +




# (b) based on 49 island groups covered by climatic data -----------------------

# Needed objects (results of unrealized colonization potential based on the two predictor sets
# including solely climatic data)
load("output_data/unrealized_col_pot/native/clim/unreal_col_pot_clim_native.RData")
load("output_data/unrealized_col_pot/global/clim/unreal_col_pot_clim_global.RData")

# Bind the two data frame into one data frame
unreal_col_pot_49 <- rbind(unreal_col_pot_clim_native, unreal_col_pot_clim_global)

# Solely retain the unrealized colonization potential of ensemble models
unreal_col_pot_49_ens <- subset(unreal_col_pot_49, unreal_col_pot_49$algorithm == "Ensemble")

# Convert unrealized colonization potential values into numeric values
unreal_col_pot_49_ens$unrealized_col_pot <- as.numeric(as.character(unreal_col_pot_49_ens$unrealized_col_pot))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Construct boxplot
ggplot(unreal_col_pot_49_ens, aes(x = predictor_set, y = unrealized_col_pot, fill = predictor_set)) + 
  stat_boxplot(geom = "errorbar", width=0.4) +
  geom_boxplot(width = 0.4, outlier.colour="black", outlier.shape=16, outlier.size=1.6) +
  theme_minimal() +
  labs(y = "Predicted unrealised colonisation potential", x = "") +
  #xlab("") +
  scale_x_discrete(breaks=c(1, 3),
                   labels=c("natclim", "globclim"))+
  scale_fill_manual(values = c("lightgrey", "lightgrey")) +
  theme(legend.position = "none", axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"), text = element_text(family = "Calibri")) +
  scale_y_continuous(limits = c(0,1))

# Save the plot
ggsave("output_data/plots/unrealized_col_pot/unreal_col_pot_49.svg", width = 5, height = 4)

# scale_fill_manual(values = c("#6699CC", "#27408B")) +

#-------------------------------------------------------------------------------

# 2. Calculate component uncertainties -----------------------------------------

library(randomForest)

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Needed objects (results of unrealized colonization potential of all four predictor sets)
load("output_data/unrealized_col_pot/native/clim_comp/unreal_col_pot_clim_native_comp.RData")
load("output_data/unrealized_col_pot/native/edaclim/unreal_col_pot_edaclim_native.RData")
load("output_data/unrealized_col_pot/global/clim_comp/unreal_col_pot_clim_global_comp.RData")
load("output_data/unrealized_col_pot/global/edaclim/unreal_col_pot_edaclim_global.RData")

# Bind the four data frames into one data frame
unreal_col_pot_25 <- rbind(unreal_col_pot_clim_native_comp, unreal_col_pot_edaclim_native,
                           unreal_col_pot_clim_global_comp, unreal_col_pot_edaclim_global)

# Remove the unrealized colonization potential of ensemble models
unreal_col_pot_25 <- subset(unreal_col_pot_25, unreal_col_pot_25$algorithm != "Ensemble")

# Make sure the independent variables are factors
unreal_col_pot_25$algorithm <- as.factor(unreal_col_pot_25$algorithm)
unreal_col_pot_25$predictor_type <- as.factor(unreal_col_pot_25$predictor_type)
unreal_col_pot_25$niche <- as.factor(unreal_col_pot_25$niche)

# Make sure the dependent variable is numeric
unreal_col_pot_25$unrealized_col_pot <- as.numeric(unreal_col_pot_25$unrealized_col_pot)

# Build the random forest model
model_RF_unrealized_col_pot_comp <- randomForest(x = unreal_col_pot_25[, c("algorithm", "predictor_type", "niche")],
                                                 y = unreal_col_pot_25$unrealized_col_pot,
                                                 ntree = 1000, importance = TRUE)

# Look at variable importance of model variables
importance(model_RF_unrealized_col_pot_comp)




# (b) based on 49 island groups covered by climatic data -----------------------

# Needed objects (results of unrealized colonization potential based on the two predictor sets
# including solely climatic data)
load("output_data/unrealized_col_pot/native/clim/unreal_col_pot_clim_native.RData")
load("output_data/unrealized_col_pot/global/clim/unreal_col_pot_clim_global.RData")

# Bind the two data frame into one data frame
unreal_col_pot_49 <- rbind(unreal_col_pot_clim_native, unreal_col_pot_clim_global)

# Remove the unrealized colonization potential of ensemble models
unreal_col_pot_49 <- subset(unreal_col_pot_49, unreal_col_pot_49$algorithm != "Ensemble")

# Make sure the independent variables are factors
unreal_col_pot_49$algorithm <- as.factor(unreal_col_pot_49$algorithm)
unreal_col_pot_49$niche <- as.factor(unreal_col_pot_49$niche)

# Make sure the dependent variable is numeric
unreal_col_pot_49$unrealized_col_pot <- as.numeric(unreal_col_pot_49$unrealized_col_pot)

# Build the random forest model
model_RF_unrealized_col_pot <- randomForest(x = unreal_col_pot_49[, c("algorithm", "niche")],
                                            y = unreal_col_pot_49$unrealized_col_pot,
                                            ntree = 1000, importance = TRUE)

# Look at variable importance of model variables
importance(model_RF_unrealized_col_pot)



