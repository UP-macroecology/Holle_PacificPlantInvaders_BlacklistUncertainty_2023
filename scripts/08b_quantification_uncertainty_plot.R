# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                 8b. Quantification of uncertainty plot                 #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)
library(viridis)
library(showtext)
library(randomForest)



#-------------------------------------------------------------------------------

# 1. Plot uncertainty of the three factors of uncertainty ----------------------
# Based on 25 island groups

# Load needed objects
load("output_data/uncertainty_quantification/models_uncertainty_RF_comp.RData") # Random forest models


# (a) Total suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction_comp <- as.data.frame(importance(model_suitable_fraction_RF_comp))
imp_suitable_fraction_comp <- cbind(factor = rownames(imp_suitable_fraction_comp), imp_suitable_fraction_comp)
imp_suitable_fraction_comp$factor <- factor(imp_suitable_fraction_comp$factor, levels = unique(imp_suitable_fraction_comp$factor))
imp_suitable_fraction_comp$blacklist <- "total_suitable_fraction"

# Set negative variable importance values to 0
imp_suitable_fraction_comp$`%IncMSE` <- ifelse(imp_suitable_fraction_comp$`%IncMSE` < 0, 0, imp_suitable_fraction_comp$`%IncMSE`)
 
# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction_comp <- as.numeric(imp_suitable_fraction_comp[1,2]) +
                                       as.numeric(imp_suitable_fraction_comp[2,2]) + 
                                       as.numeric(imp_suitable_fraction_comp[3,2])
imp_suitable_fraction_comp$standardized <- NA 
imp_suitable_fraction_comp[1,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[1,2])
imp_suitable_fraction_comp[2,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[2,2])
imp_suitable_fraction_comp[3,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[3,2])


# (b) Mean suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_suitable_fraction_comp <- as.data.frame(importance(model_mean_suitable_fraction_RF_comp))
imp_mean_suitable_fraction_comp <- cbind(factor = rownames(imp_mean_suitable_fraction_comp), imp_mean_suitable_fraction_comp)
imp_mean_suitable_fraction_comp$factor <- factor(imp_mean_suitable_fraction_comp$factor, levels = unique(imp_mean_suitable_fraction_comp$factor))
imp_mean_suitable_fraction_comp$blacklist <- "mean_suitable_fraction"

# Set negative variable importance values to 0
imp_mean_suitable_fraction_comp$`%IncMSE` <- ifelse(imp_mean_suitable_fraction_comp$`%IncMSE` < 0, 0, imp_mean_suitable_fraction_comp$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_mean_suitable_fraction_comp <- as.numeric(imp_mean_suitable_fraction_comp[1,2]) +
                                            as.numeric(imp_mean_suitable_fraction_comp[2,2]) + 
                                            as.numeric(imp_mean_suitable_fraction_comp[3,2])
imp_mean_suitable_fraction_comp$standardized <- NA 
imp_mean_suitable_fraction_comp[1,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[1,2])
imp_mean_suitable_fraction_comp[2,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[2,2])
imp_mean_suitable_fraction_comp[3,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[3,2])


# (c) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable_comp <- as.data.frame(importance(model_number_suitable_RF_comp))
imp_number_suitable_comp <- cbind(factor = rownames(imp_number_suitable_comp), imp_number_suitable_comp)
imp_number_suitable_comp$factor <- factor(imp_number_suitable_comp$factor, levels = unique(imp_number_suitable_comp$factor))
imp_number_suitable_comp$blacklist <- "number_suitable"

# Set negative variable importance values to 0
imp_number_suitable_comp$`%IncMSE` <- ifelse(imp_number_suitable_comp$`%IncMSE` < 0, 0, imp_number_suitable_comp$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable_comp <- as.numeric(imp_number_suitable_comp[1,2]) +
                                     as.numeric(imp_number_suitable_comp[2,2]) + 
                                     as.numeric(imp_number_suitable_comp[3,2])
imp_number_suitable_comp$standardized <- NA 
imp_number_suitable_comp[1,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[1,2])
imp_number_suitable_comp[2,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[2,2])
imp_number_suitable_comp[3,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[3,2])





# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together and decide the factor order for the three blacklists
imp_blacklists_comp <- rbind(imp_suitable_fraction_comp, imp_mean_suitable_fraction_comp, imp_number_suitable_comp)
imp_blacklists_comp$blacklist <- factor(imp_blacklists_comp$blacklist, levels = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot the variable importance
ggplot(data = imp_blacklists_comp, aes(x = factor, y = standardized, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.5) +
  theme_minimal() +
  xlab("") +
  ylab('% of variable importance') +
  ggtitle("") +
  scale_fill_manual(values=c("aquamarine4", "#44AA99", "darkseagreen3"), name = "Blacklisting approach:", labels = c("total fraction (D² = 0.043)", "mean fraction (D² = 0.054)", "island groups (D² = 0.122)")) +
  ylim(c(0, 100)) +
  #geom_text(label = round((imp_blacklists_comp$standardized), 2), position=position_dodge(width=0.5), hjust= imp_blacklists_comp$hjust) +
  scale_x_discrete(breaks=c("algorithm", "niche", "predictor_type"),
                   labels=c("Algorithm", "Niche", "Predictor type"))+
  theme(legend.position = c(0.73,0.73), axis.text = element_text(color = "black", size = 13), legend.box.background = element_rect(color="black", linewidth = 0.3),
        axis.title = element_text(size = 14, color = "black"), legend.text = element_text(size = 10), legend.title = element_text(size = 11),
        text = element_text(family = "Calibri"), axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  #labs(tag = "(a)") +
  coord_flip()


# Save the plot
ggsave("output_data/plots/uncertainty_quantification/uncertainty_quantification_comp.svg", width = 5, height = 4)





#-------------------------------------------------------------------------------

# 2. Plot uncertainty of two factors of uncertainty ----------------------------
# Based on 49 island groups

# Load needed objects
load("output_data/uncertainty_quantification/models_uncertainty_RF.RData") # Random forest models


# (a) Total Suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction <- as.data.frame(importance(model_suitable_fraction_RF))
imp_suitable_fraction <- cbind(factor = rownames(imp_suitable_fraction), imp_suitable_fraction)
imp_suitable_fraction$factor <- factor(imp_suitable_fraction$factor, levels = unique(imp_suitable_fraction$factor))
imp_suitable_fraction$blacklist <- "total_suitable_fraction"

# Set negative variable importance values to 0
imp_suitable_fraction$`%IncMSE` <- ifelse(imp_suitable_fraction$`%IncMSE` < 0, 0, imp_suitable_fraction$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction <- as.numeric(imp_suitable_fraction[1,2]) +
                                  as.numeric(imp_suitable_fraction[2,2]) 
imp_suitable_fraction$standardized <- NA 
imp_suitable_fraction[1,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[1,2])
imp_suitable_fraction[2,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[2,2])


# (b) Mean suitable habitat fraction -------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_suitable_fraction <- as.data.frame(importance(model_mean_suitable_fraction_RF))
imp_mean_suitable_fraction <- cbind(factor = rownames(imp_mean_suitable_fraction), imp_mean_suitable_fraction)
imp_mean_suitable_fraction$factor <- factor(imp_mean_suitable_fraction$factor, levels = unique(imp_mean_suitable_fraction$factor))
imp_mean_suitable_fraction$blacklist <- "mean_suitable_fraction"

# Set negative variable importance values to 0
imp_mean_suitable_fraction$`%IncMSE` <- ifelse(imp_mean_suitable_fraction$`%IncMSE` < 0, 0, imp_mean_suitable_fraction$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_mean_suitable_fraction <- as.numeric(imp_mean_suitable_fraction[1,2]) +
                                       as.numeric(imp_mean_suitable_fraction[2,2]) 
imp_mean_suitable_fraction$standardized <- NA 
imp_mean_suitable_fraction[1,5] <- (100/sum_accuracy_mean_suitable_fraction) * as.numeric(imp_mean_suitable_fraction[1,2])
imp_mean_suitable_fraction[2,5] <- (100/sum_accuracy_mean_suitable_fraction) * as.numeric(imp_mean_suitable_fraction[2,2])


# (c) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable <- as.data.frame(importance(model_number_suitable_RF))
imp_number_suitable <- cbind(factor = rownames(imp_number_suitable), imp_number_suitable)
imp_number_suitable$factor <- factor(imp_number_suitable$factor, levels = unique(imp_number_suitable$factor))
imp_number_suitable$blacklist <- "number_suitable"

# Set negative variable importance values to 0
imp_number_suitable$blacklist$`%IncMSE` <- ifelse(imp_number_suitable$blacklist$`%IncMSE` < 0, 0, imp_number_suitable$blacklist$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable <- as.numeric(imp_number_suitable[1,2]) +
                                as.numeric(imp_number_suitable[2,2])
imp_number_suitable$standardized <- NA 
imp_number_suitable[1,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[1,2])
imp_number_suitable[2,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[2,2])




# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together
imp_blacklists <- rbind(imp_suitable_fraction, imp_mean_suitable_fraction, imp_number_suitable)
imp_blacklists$blacklist <- factor(imp_blacklists$blacklist, levels = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot the variable importance
ggplot(data = imp_blacklists, aes(x = factor, y = standardized, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.5) +
  theme_minimal() +
  xlab("") +
  ylab('% of variable importance') +
  ggtitle("") +
  scale_fill_manual(values=c("aquamarine4", "#44AA99", "darkseagreen3"), name = "Blacklisting approach:", labels = c("total fraction (D² = 0.014)", "mean fraction (D² = 0.033)", "island groups (D² = 0.175)")) +
  ylim(c(0, 105)) +
  #geom_text(label = round((imp_blacklists$standardized), 2), position=position_dodge(width=0.5), hjust= imp_blacklists$hjust) +
  scale_x_discrete(breaks=c("algorithm", "niche"),
                   labels=c("Algorithm", "Niche"))+
  theme(legend.position = c(0.71,0.67), axis.text = element_text(color = "black", size = 13), legend.box.background = element_rect(color="black", linewidth = 0.3),
        axis.title = element_text(size = 14, color = "black"), legend.text = element_text(size = 9), legend.title = element_text(size = 10),
        text = element_text(family = "Calibri"), axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  #labs(tag = "(b)") +
  coord_flip()


# Save the plot
ggsave("output_data/plots/uncertainty_quantification/uncertainty_quantification.svg", width = 5, height = 4)


