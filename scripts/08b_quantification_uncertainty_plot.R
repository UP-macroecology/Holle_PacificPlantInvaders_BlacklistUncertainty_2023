# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                 8b. Quantification of uncertainty plot                 #
# ---------------------------------------------------------------------- #

# Load needed packages
library(ggplot2)
library(viridis)
library(showtext)
library(randomForest)




#-------------------------------------------------------------------------------

# 1. Plot uncertainty of the four factors of uncertainty ----------------------
# Based on 25 island groups

print("Quantify blacklist uncertainty based on 25 island groups")

# Load needed objects
load("output_data/uncertainty_quantification_rev/models_uncertainty_RF_comp.RData") # Random forest models


# (a) Total suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction_comp <- as.data.frame(importance(model_suitable_fraction_RF_comp))
imp_suitable_fraction_comp <- cbind(factor = rownames(imp_suitable_fraction_comp), imp_suitable_fraction_comp)
imp_suitable_fraction_comp$factor <- factor(imp_suitable_fraction_comp$factor, levels = unique(imp_suitable_fraction_comp$factor))
imp_suitable_fraction_comp$blacklist <- "total_suitable_fraction"

# Extract the D²) of the random forest model
var_explained_suitable_fraction_comp <- model_suitable_fraction_RF_comp$rsq[length(model_suitable_fraction_RF_comp$rsq)]
var_explained_suitable_fraction_comp <- sprintf("%.3f", var_explained_suitable_fraction_comp)

# Set negative variable importance values to 0
imp_suitable_fraction_comp$`%IncMSE` <- ifelse(imp_suitable_fraction_comp$`%IncMSE` < 0, 0, imp_suitable_fraction_comp$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction_comp <- as.numeric(imp_suitable_fraction_comp[1,2]) +
  as.numeric(imp_suitable_fraction_comp[2,2]) + 
  as.numeric(imp_suitable_fraction_comp[3,2]) +
  as.numeric(imp_suitable_fraction_comp[4,2])
imp_suitable_fraction_comp$standardised <- NA 
imp_suitable_fraction_comp[1,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[1,2])
imp_suitable_fraction_comp[2,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[2,2])
imp_suitable_fraction_comp[3,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[3,2])
imp_suitable_fraction_comp[4,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[4,2])


# (b) Mean suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_suitable_fraction_comp <- as.data.frame(importance(model_mean_suitable_fraction_RF_comp))
imp_mean_suitable_fraction_comp <- cbind(factor = rownames(imp_mean_suitable_fraction_comp), imp_mean_suitable_fraction_comp)
imp_mean_suitable_fraction_comp$factor <- factor(imp_mean_suitable_fraction_comp$factor, levels = unique(imp_mean_suitable_fraction_comp$factor))
imp_mean_suitable_fraction_comp$blacklist <- "mean_suitable_fraction"

# Extract the D² of the random forest model
var_explained_mean_suitable_fraction_comp <- model_mean_suitable_fraction_RF_comp$rsq[length(model_mean_suitable_fraction_RF_comp$rsq)]
var_explained_mean_suitable_fraction_comp <- sprintf("%.3f", var_explained_mean_suitable_fraction_comp)

# Set negative variable importance values to 0
imp_mean_suitable_fraction_comp$`%IncMSE` <- ifelse(imp_mean_suitable_fraction_comp$`%IncMSE` < 0, 0, imp_mean_suitable_fraction_comp$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_mean_suitable_fraction_comp <- as.numeric(imp_mean_suitable_fraction_comp[1,2]) +
  as.numeric(imp_mean_suitable_fraction_comp[2,2]) + 
  as.numeric(imp_mean_suitable_fraction_comp[3,2]) +
  as.numeric(imp_mean_suitable_fraction_comp[4,2])
imp_mean_suitable_fraction_comp$standardised <- NA 
imp_mean_suitable_fraction_comp[1,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[1,2])
imp_mean_suitable_fraction_comp[2,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[2,2])
imp_mean_suitable_fraction_comp[3,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[3,2])
imp_mean_suitable_fraction_comp[4,5] <- (100/sum_accuracy_mean_suitable_fraction_comp) * as.numeric(imp_mean_suitable_fraction_comp[4,2])


# (c) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable_comp <- as.data.frame(importance(model_number_suitable_RF_comp))
imp_number_suitable_comp <- cbind(factor = rownames(imp_number_suitable_comp), imp_number_suitable_comp)
imp_number_suitable_comp$factor <- factor(imp_number_suitable_comp$factor, levels = unique(imp_number_suitable_comp$factor))
imp_number_suitable_comp$blacklist <- "number_suitable"

# Extract the D² of the random forest model
var_explained_number_suitable_comp <- model_number_suitable_RF_comp$rsq[length(model_number_suitable_RF_comp$rsq)]
var_explained_number_suitable_comp <- sprintf("%.3f", var_explained_number_suitable_comp)


# Set negative variable importance values to 0
imp_number_suitable_comp$`%IncMSE` <- ifelse(imp_number_suitable_comp$`%IncMSE` < 0, 0, imp_number_suitable_comp$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable_comp <- as.numeric(imp_number_suitable_comp[1,2]) +
  as.numeric(imp_number_suitable_comp[2,2]) + 
  as.numeric(imp_number_suitable_comp[3,2]) +
  as.numeric(imp_number_suitable_comp[4,2])
imp_number_suitable_comp$standardised <- NA 
imp_number_suitable_comp[1,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[1,2])
imp_number_suitable_comp[2,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[2,2])
imp_number_suitable_comp[3,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[3,2])
imp_number_suitable_comp[4,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[4,2])





# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together and decide the factor order for the three blacklists
imp_blacklists_comp <- rbind(imp_suitable_fraction_comp, imp_mean_suitable_fraction_comp, imp_number_suitable_comp)
imp_blacklists_comp$blacklist <- factor(imp_blacklists_comp$blacklist, levels = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot the variable importance
ggplot(data = imp_blacklists_comp, aes(x = factor, y = standardised, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.7) +
  theme_bw() +
  xlab("") +
  ylab('% of variable importance') +
  scale_fill_manual(values=c("aquamarine4", "#44AA99", "darkseagreen3"), name = "Blacklisting approach:", labels = c(paste0("Total fraction (D² = ", var_explained_suitable_fraction_comp, ")"),
                                                                                                                     paste0("Mean fraction (D² = ", var_explained_mean_suitable_fraction_comp, ")"), 
                                                                                                                     paste0("Island groups (D² = ", var_explained_number_suitable_comp, ")"))) +
  ylim(c(min(imp_blacklists_comp$`%IncMSE`), 100)) +
  #geom_text(label = round((imp_blacklists_comp$standardized), 2), position=position_dodge(width=0.5), hjust= imp_blacklists_comp$hjust) +
  scale_x_discrete(breaks=c("algorithm", "niche", "predictor_type", "thresh_method"),
                   labels=c("Algorithm", "Species data", "Predictor type", "Threshold"))+
  theme(legend.position = c(0.75,0.3), axis.text = element_text(color = "black", size = 8), legend.box.background = element_rect(color="black", linewidth = 0.3),
        axis.title = element_text(size = 9, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        text = element_text(family = "Calibri"), axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  #labs(tag = "(a)") +
  coord_flip()


# Save the plot
ggsave("output_data/plots/uncertainty_quantification_rev/uncertainty_quantification_comp.svg", width = 12.0, height = 8.5, unit = "cm")






#-------------------------------------------------------------------------------

# 2. Plot uncertainty of three factors of uncertainty ----------------------------
# Based on 49 island groups

print("Quantify blacklist uncertainty based on 49 island groups")

# Load needed objects
load("output_data/uncertainty_quantification_rev/models_uncertainty_RF.RData") # Random forest models


# (a) Total Suitable habitat fraction ------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction <- as.data.frame(importance(model_suitable_fraction_RF))
imp_suitable_fraction <- cbind(factor = rownames(imp_suitable_fraction), imp_suitable_fraction)
imp_suitable_fraction$factor <- factor(imp_suitable_fraction$factor, levels = unique(imp_suitable_fraction$factor))
imp_suitable_fraction$blacklist <- "total_suitable_fraction"

# Extract the D² of the random forest model
var_explained_suitable_fraction <- model_suitable_fraction_RF$rsq[length(model_suitable_fraction_RF$rsq)]
var_explained_suitable_fraction <- sprintf("%.3f", var_explained_suitable_fraction)

# Set negative variable importance values to 0
imp_suitable_fraction$`%IncMSE` <- ifelse(imp_suitable_fraction$`%IncMSE` < 0, 0, imp_suitable_fraction$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction <- as.numeric(imp_suitable_fraction[1,2]) +
  as.numeric(imp_suitable_fraction[2,2]) +
  as.numeric(imp_suitable_fraction[3,2])
imp_suitable_fraction$standardised <- NA 
imp_suitable_fraction[1,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[1,2])
imp_suitable_fraction[2,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[2,2])
imp_suitable_fraction[3,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[3,2])


# (b) Mean suitable habitat fraction -------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_suitable_fraction <- as.data.frame(importance(model_mean_suitable_fraction_RF))
imp_mean_suitable_fraction <- cbind(factor = rownames(imp_mean_suitable_fraction), imp_mean_suitable_fraction)
imp_mean_suitable_fraction$factor <- factor(imp_mean_suitable_fraction$factor, levels = unique(imp_mean_suitable_fraction$factor))
imp_mean_suitable_fraction$blacklist <- "mean_suitable_fraction"

# Extract the D² of the random forest model
var_explained_mean_suitable_fraction <- model_mean_suitable_fraction_RF$rsq[length(model_mean_suitable_fraction_RF$rsq)]
var_explained_mean_suitable_fraction <- sprintf("%.3f", var_explained_mean_suitable_fraction)

# Set negative variable importance values to 0
imp_mean_suitable_fraction$`%IncMSE` <- ifelse(imp_mean_suitable_fraction$`%IncMSE` < 0, 0, imp_mean_suitable_fraction$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_mean_suitable_fraction <- as.numeric(imp_mean_suitable_fraction[1,2]) +
  as.numeric(imp_mean_suitable_fraction[2,2]) +
  as.numeric(imp_mean_suitable_fraction[3,2]) 
imp_mean_suitable_fraction$standardised <- NA 
imp_mean_suitable_fraction[1,5] <- (100/sum_accuracy_mean_suitable_fraction) * as.numeric(imp_mean_suitable_fraction[1,2])
imp_mean_suitable_fraction[2,5] <- (100/sum_accuracy_mean_suitable_fraction) * as.numeric(imp_mean_suitable_fraction[2,2])
imp_mean_suitable_fraction[3,5] <- (100/sum_accuracy_mean_suitable_fraction) * as.numeric(imp_mean_suitable_fraction[3,2])


# (c) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable <- as.data.frame(importance(model_number_suitable_RF))
imp_number_suitable <- cbind(factor = rownames(imp_number_suitable), imp_number_suitable)
imp_number_suitable$factor <- factor(imp_number_suitable$factor, levels = unique(imp_number_suitable$factor))
imp_number_suitable$blacklist <- "number_suitable"

# Extract the D² of the random forest model
var_explained_number_suitable <- model_number_suitable_RF$rsq[length(model_number_suitable_RF$rsq)]
var_explained_number_suitable <- sprintf("%.3f", var_explained_number_suitable)

# Set negative variable importance values to 0
imp_number_suitable$blacklist$`%IncMSE` <- ifelse(imp_number_suitable$blacklist$`%IncMSE` < 0, 0, imp_number_suitable$blacklist$`%IncMSE`)

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable <- as.numeric(imp_number_suitable[1,2]) +
  as.numeric(imp_number_suitable[2,2]) +
  as.numeric(imp_number_suitable[3,2])
imp_number_suitable$standardised <- NA 
imp_number_suitable[1,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[1,2])
imp_number_suitable[2,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[2,2])
imp_number_suitable[3,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[3,2])




# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together
imp_blacklists <- rbind(imp_suitable_fraction, imp_mean_suitable_fraction, imp_number_suitable)
imp_blacklists$blacklist <- factor(imp_blacklists$blacklist, levels = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot the variable importance
ggplot(data = imp_blacklists, aes(x = factor, y = standardised, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.7) +
  theme_bw() +
  xlab("") +
  ylab('% of variable importance') +
  scale_fill_manual(values=c("aquamarine4", "#44AA99", "darkseagreen3"), name = "Blacklisting approach:", labels = c(paste0("Total fraction (D² = ", var_explained_suitable_fraction, ")"),
                                                                                                                     paste0("Mean fraction (D² = ", var_explained_mean_suitable_fraction, ")"), 
                                                                                                                     paste0("Island groups (D² = ", var_explained_number_suitable, ")"))) +
  ylim(c(min(imp_blacklists$`%IncMSE`), 100)) +
  #geom_text(label = round((imp_blacklists$standardized), 2), position=position_dodge(width=0.5), hjust= imp_blacklists$hjust) +
  scale_x_discrete(breaks=c("algorithm", "niche", "thresh_method"),
                   labels=c("Algorithm", "Species data", "Threshold"))+
  theme(legend.position = c(0.75,0.3), axis.text = element_text(color = "black", size = 9), legend.box.background = element_rect(color="black", linewidth = 0.3),
        axis.title = element_text(size = 9, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        text = element_text(family = "Calibri"), axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  #labs(tag = "(b)") +
  coord_flip()


# Save the plot
ggsave("output_data/plots/uncertainty_quantification_rev/uncertainty_quantification.svg", width = 12, height = 6.8, unit = "cm")
  
  

