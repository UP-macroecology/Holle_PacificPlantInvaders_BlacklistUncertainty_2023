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



#-------------------------------------------------------------------------------

# 1. Plot uncertainty of the three factors of uncertainty ----------------------
# Based on 25 island groups

# Load needed objects
load("output_data/uncertainty_quantification/models_uncertainty_RF_comp.RData") # Random forest models


# (a) Suitable habitat fraction ------------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction_comp <- as.data.frame(importance(model_suitable_fraction_RF_comp))
imp_suitable_fraction_comp <- cbind(factor = rownames(imp_suitable_fraction_comp), imp_suitable_fraction_comp)
imp_suitable_fraction_comp$factor <- factor(imp_suitable_fraction_comp$factor, levels = unique(imp_suitable_fraction_comp$factor))
imp_suitable_fraction_comp$blacklist <- "suitable_fraction"
 
# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction_comp <- as.numeric(imp_suitable_fraction_comp[1,2]) +
                                       as.numeric(imp_suitable_fraction_comp[2,2]) + 
                                       as.numeric(imp_suitable_fraction_comp[3,2])
imp_suitable_fraction_comp$standardized <- NA 
imp_suitable_fraction_comp[1,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[1,2])
imp_suitable_fraction_comp[2,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[2,2])
imp_suitable_fraction_comp[3,5] <- (100/sum_accuracy_suitable_fraction_comp) * as.numeric(imp_suitable_fraction_comp[3,2])


# (b) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable_comp <- as.data.frame(importance(model_number_suitable_RF_comp))
imp_number_suitable_comp <- cbind(factor = rownames(imp_number_suitable_comp), imp_number_suitable_comp)
imp_number_suitable_comp$factor <- factor(imp_number_suitable_comp$factor, levels = unique(imp_number_suitable_comp$factor))
imp_number_suitable_comp$blacklist <- "number_suitable"

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable_comp <- as.numeric(imp_number_suitable_comp[1,2]) +
                                     as.numeric(imp_number_suitable_comp[2,2]) + 
                                     as.numeric(imp_number_suitable_comp[3,2])
imp_number_suitable_comp$standardized <- NA 
imp_number_suitable_comp[1,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[1,2])
imp_number_suitable_comp[2,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[2,2])
imp_number_suitable_comp[3,5] <- (100/sum_accuracy_number_suitable_comp) * as.numeric(imp_number_suitable_comp[3,2])


# (c) Mean rank over all island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_rank_comp <- as.data.frame(importance(model_mean_rank_RF_comp))
imp_mean_rank_comp <- cbind(factor = rownames(imp_mean_rank_comp), imp_mean_rank_comp)
imp_mean_rank_comp$factor <- factor(imp_mean_rank_comp$factor, levels = unique(imp_mean_rank_comp$factor))
imp_mean_rank_comp$blacklist <- "mean_rank"

# Standardize these values to sum up to 100 %
sum_accuracy_mean_rank_comp <- as.numeric(imp_mean_rank_comp[1,2]) +
                               as.numeric(imp_mean_rank_comp[2,2]) + 
                               as.numeric(imp_mean_rank_comp[3,2])
imp_mean_rank_comp$standardized <- NA 
imp_mean_rank_comp[1,5] <- (100/sum_accuracy_mean_rank_comp) * as.numeric(imp_mean_rank_comp[1,2])
imp_mean_rank_comp[2,5] <- (100/sum_accuracy_mean_rank_comp) * as.numeric(imp_mean_rank_comp[2,2])
imp_mean_rank_comp[3,5] <- (100/sum_accuracy_mean_rank_comp) * as.numeric(imp_mean_rank_comp[3,2])


# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together
imp_blacklists_comp <- rbind(imp_suitable_fraction_comp, imp_number_suitable_comp, imp_mean_rank_comp)

# Plot the variable importance
ggplot(data = imp_blacklists_comp, aes(x = factor, y = standardized, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.5) +
  xlab("") +
  ylab('% of variable importance') +
  ggtitle("") +
  scale_fill_manual(values=c("darkseagreen3", "#44AA99", "aquamarine4"), name = "Blacklist ranking established by", labels = c("mean rank over all island groups", "number of suitable island groups", "suitable habitat fraction over all island groups"))+
  ylim(c(-5, 100)) +
  geom_text(label = round((imp_blacklists_comp$standardized), 2), position=position_dodge(width=0.5), hjust= -0.5) +
  scale_x_discrete(breaks=c("algorithm", "niche", "predictor_type"),
                   labels=c("Algorithm", "Niche", "Predictor type"))+
  theme(legend.position = c(0.778,0.865), axis.text = element_text(color = "black", size = 12), legend.box.background = element_rect(color="black", size=0.5),
        axis.title = element_text(size = 13, color = "black"), legend.text = element_text(size = 11), legend.title = element_text(size = 13),
        text = element_text(family = "Calibri")) +
  #labs(tag = "(a)") +
  coord_flip()

# Save the plot
ggsave("output_data/plots/uncertainty_quantification/uncertainty_quantification_comp.svg", width = 10, height = 6)





#-------------------------------------------------------------------------------

# 2. Plot uncertainty of two factors of uncertainty ----------------------------
# Based on 49 island groups

# Load needed objects
load("output_data/uncertainty_quantification/models_uncertainty_RF.RData") # Random forest models


# (a) Suitable habitat fraction ------------------------------------------------

# Get the variable importance values of the uncertainty factors
imp_suitable_fraction <- as.data.frame(importance(model_suitable_fraction_RF))
imp_suitable_fraction <- cbind(factor = rownames(imp_suitable_fraction), imp_suitable_fraction)
imp_suitable_fraction$factor <- factor(imp_suitable_fraction$factor, levels = unique(imp_suitable_fraction$factor))
imp_suitable_fraction$blacklist <- "suitable_fraction"

# Standardize these values to sum up to 100 %
sum_accuracy_suitable_fraction <- as.numeric(imp_suitable_fraction[1,2]) +
                                  as.numeric(imp_suitable_fraction_comp[2,2]) 
imp_suitable_fraction$standardized <- NA 
imp_suitable_fraction[1,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[1,2])
imp_suitable_fraction[2,5] <- (100/sum_accuracy_suitable_fraction) * as.numeric(imp_suitable_fraction[2,2])


# (b) Number of suitable island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_number_suitable <- as.data.frame(importance(model_number_suitable_RF))
imp_number_suitable <- cbind(factor = rownames(imp_number_suitable), imp_number_suitable)
imp_number_suitable$factor <- factor(imp_number_suitable$factor, levels = unique(imp_number_suitable$factor))
imp_number_suitable$blacklist <- "number_suitable"

# Standardize these values to sum up to 100 %
sum_accuracy_number_suitable <- as.numeric(imp_number_suitable[1,2]) +
                                as.numeric(imp_number_suitable[2,2])
imp_number_suitable$standardized <- NA 
imp_number_suitable[1,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[1,2])
imp_number_suitable[2,5] <- (100/sum_accuracy_number_suitable) * as.numeric(imp_number_suitable[2,2])


# (c) Mean rank over all island groups  ----------------------------------------

# Get the variable importance values of the uncertainty factors
imp_mean_rank <- as.data.frame(importance(model_mean_rank_RF))
imp_mean_rank <- cbind(factor = rownames(imp_mean_rank), imp_mean_rank)
imp_mean_rank$factor <- factor(imp_mean_rank$factor, levels = unique(imp_mean_rank$factor))
imp_mean_rank$blacklist <- "mean_rank"

# Standardize these values to sum up to 100 %
sum_accuracy_mean_rank <- as.numeric(imp_mean_rank[1,2]) +
                          as.numeric(imp_mean_rank[2,2])
imp_mean_rank$standardized <- NA 
imp_mean_rank[1,5] <- (100/sum_accuracy_mean_rank) * as.numeric(imp_mean_rank[1,2])
imp_mean_rank[2,5] <- (100/sum_accuracy_mean_rank) * as.numeric(imp_mean_rank[2,2])


# (d) Plot the uncertainty factors as bars  ------------------------------------

# Bind the data frames together
imp_blacklists <- rbind(imp_suitable_fraction, imp_number_suitable, imp_mean_rank)

# Plot the variable importance
ggplot(data = imp_blacklists, aes(x = factor, y = standardized, fill = blacklist)) +
  geom_bar(position= position_dodge2(preserve = "single"), stat='identity', width = 0.5) +
  xlab("") +
  ylab('% of variable importance') +
  ggtitle("") +
  scale_fill_manual(values=c("darkseagreen3", "#44AA99", "aquamarine4"), name = "Blacklist ranking established by", labels = c("mean rank over all island groups", "number of suitable island groups", "suitable habitat fraction over all island groups"))+
  ylim(c(-5, 105)) +
  geom_text(label = round((imp_blacklists$standardized), 2), position=position_dodge(width=0.5), hjust= -0.5) +
  scale_x_discrete(breaks=c("algorithm", "niche"),
                   labels=c("Algorithm", "Niche"))+
  theme(legend.position = c(0.785,0.865), axis.text = element_text(color = "black", size = 12), legend.box.background = element_rect(color="black", size=0.5),
        axis.title = element_text(size = 13, color = "black"), legend.text = element_text(size = 11), legend.title = element_text(size = 13),
        text = element_text(family = "Calibri")) +
  #labs(tag = "(b)") +
  coord_flip()


# Save the plot
ggsave("output_data/plots/uncertainty_quantification/uncertainty_quantification.svg", width = 10, height = 6)
