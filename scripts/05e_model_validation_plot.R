# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                      05e. Model validation plot                        #
# ---------------------------------------------------------------------- #

# Load needed packages
library(ggplot2)
library(showtext)

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Create a vector with applied algorithms and their ensemble
algorithm <- c("glm", "gam", "rf", "brt", "ensemble")

# Create a vector with the different predictor set
predictor_set <- c("natclim", "natclim+eda", "globclim", "globclim+eda")

# Create a vector with the different thresholding methods
thresh_method <- c("maxTSS", "meanProb", "tenthPer")

# Create a vector with the different performace measures
perf_measure <- c("AUC", "TSS", "Sens", "Spec", "Boyce")



#-------------------------------------------------------------------------------

# 1. Performance measures ------------------------------------------------------


# Value extractions  
# Create a data frame to store the performance values of all models based
# on the different algorithms, predictor sets and thresholding methods
performance_algorithm_comparison <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(performance_algorithm_comparison) <- c("Species", "Perf_value", "Perf_method", "Algorithm", "Predictor_set", "Thresh_method")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    print(pred)
    
    for (t in thresh_method) { # Start of the loop over the three different thresholding methods
      
      print(t)
      
      # Load in the performance measures
      if (pred == "natclim") { load(paste0("output_data/validation_rev/native/clim/validation_clim_native_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_clim_native_", t))
        validation_ens <- ensemble_perf_clim_native
      } else if (pred == "natclim+eda") { load(paste0("output_data/validation_rev/native/edaclim/validation_edaclim_native_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_edaclim_native_", t))
        validation_ens <- ensemble_perf_edaclim_native
      } else if (pred == "globclim") { load(paste0("output_data/validation_rev/global/clim/validation_clim_global_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_clim_global_", t))
        validation_ens <- ensemble_perf_clim_global
      } else if (pred == "globclim+eda") { load(paste0("output_data/validation_rev/global/edaclim/validation_edaclim_global_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_edaclim_global_", t))
        validation_ens <- ensemble_perf_edaclim_global
      }                                    
      
      
      
      for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
        
        print(alg)
        
        for (m in perf_measure) { # Start of the loop over the different performance measures
          
          print(m)
          
          # Extract the considered performance value
          if (alg %in% c("glm", "gam", "rf", "brt")) { performance_value <- validation_alg[alg, m]
          } else if (alg == "ensemble") { performance_value <- validation_ens[t, m]
          }
          
          # Write a vector with extracted information
          performance_results <- c(sp, performance_value, m, alg, pred, t)
          
          # Add the vector to the results data frame
          performance_algorithm_comparison <- rbind(performance_algorithm_comparison, performance_results) 
          
        } # End of the loop over the different performance measures
        
      } # End of the loop over the algorithms
      
    } # End of the loop over the thresholding methods
    
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(performance_algorithm_comparison) <- c("Species", "Perf_value", "Perf_method", "Algorithm", "Predictor_set", "Thresh_method")

# Save the resulting data frame
save(performance_algorithm_comparison, file = "output_data/validation_rev/performance_algorithm_comparison.RData")



#-------------------------------------------------------------------------------

# 2. Visualisation -------------------------------------------------------------

# (a) Grouped boxplot based on maxTSS threshold  -------------------------------

# Subset the data frame containing the performance measures to only contain 
# values based on the respective threshold
performance_algorithm_comparison_maxTSS <- performance_algorithm_comparison[performance_algorithm_comparison$Thresh_method == "maxTSS", ]
  
# Convert performance values to numeric
performance_algorithm_comparison_maxTSS$Perf_value <- as.numeric(as.character(performance_algorithm_comparison_maxTSS$Perf_value))

# Bring the algorithm groups in the right order
performance_algorithm_comparison_maxTSS$Algorithm <- factor(performance_algorithm_comparison_maxTSS$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Change the entry Sens to Sensitivity and Spec to Specificity
performance_algorithm_comparison_maxTSS$Perf_method[performance_algorithm_comparison_maxTSS$Perf_method == "Sens"] <- "Sensitivity"
performance_algorithm_comparison_maxTSS$Perf_method[performance_algorithm_comparison_maxTSS$Perf_method == "Spec"] <- "Specificity"

# Bring the performance measure groups in the right order
performance_algorithm_comparison_maxTSS$Perf_method <- factor(performance_algorithm_comparison_maxTSS$Perf_method, levels=c("AUC", "Boyce", "TSS", "Sensitivity", "Specificity"))

# Bring the predictor sets in the right order
performance_algorithm_comparison_maxTSS$Predictor_set <- factor(performance_algorithm_comparison_maxTSS$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Rename facet labels for plot in main manuscript text
facet_labels <- c("Boyce" = "(a) Boyce Index", "TSS" = "(b) TSS")


# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot TSS and Boyce values for the main manuscript text
ggplot(performance_algorithm_comparison_maxTSS[performance_algorithm_comparison_maxTSS$Perf_method %in% c("TSS", "Boyce"), ], aes(x = Predictor_set, y = Perf_value, fill = Algorithm)) + 
  stat_boxplot(geom = "errorbar", width = 0.8) +
  geom_boxplot(width = 0.8, outlier.colour = "black", outlier.shape = 16,
               outlier.size = 1.4) +
  facet_wrap(~ Perf_method, nrow = 2, ncol = 1, scales = "free_x", labeller = as_labeller(facet_labels)) +
  ylab("Performance value") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("") +
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 9),
    text = element_text(family = "Calibri"),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightsteelblue3", color = "black")) +
  scale_fill_brewer(
    palette = "BuPu",
    labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble")
  ) +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))


ggsave("output_data/plots/validation_rev/performance_algorithm_comparison_maxTSS_Boyce_TSS.svg", width = 9.5, height = 14, unit = "cm")

# Plot AUC, Sensitivity, and Specificity values for the appendix
ggplot(performance_algorithm_comparison_maxTSS[performance_algorithm_comparison_maxTSS$Perf_method %in% c("AUC", "Sensitivity", "Specificity"), ], aes(x = Predictor_set, y = Perf_value, fill = Algorithm)) + 
  stat_boxplot(geom = "errorbar", width = 0.8) +
  geom_boxplot(width = 0.8, outlier.colour = "black", outlier.shape = 16,
               outlier.size = 1.4) +
  facet_wrap(~ Perf_method, nrow = 2, ncol = 2, scales = "free_x") +
  ylab("Performance value") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("") +
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 9),
    text = element_text(family = "Calibri"),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightsteelblue3", color = "black")) +
  scale_fill_brewer(
    palette = "BuPu",
    labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble")
  ) +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))


ggsave("output_data/plots/validation_rev/performance_algorithm_comparison_maxTSS_AUC_Sens_Spec.svg", width = 18, height = 14, unit = "cm")



#-------------------------------------------------------------------------------

# 3. Descriptive statistics ----------------------------------------------------




# (b) Grouped boxplot based on meanProb threshold  -----------------------------

# Subset the data frame containing the performance measures to only contain 
# values based on the respective threshold
performance_algorithm_comparison_meanProb <- performance_algorithm_comparison[performance_algorithm_comparison$Thresh_method == "meanProb", ]

# Convert performance values to numeric
performance_algorithm_comparison_meanProb$Perf_value <- as.numeric(as.character(performance_algorithm_comparison_meanProb$Perf_value))

# Bring the algorithm groups in the right order
performance_algorithm_comparison_meanProb$Algorithm <- factor(performance_algorithm_comparison_meanProb$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Change the entry Sens to Sensitivity and Spec to Specificity
performance_algorithm_comparison_meanProb$Perf_method[performance_algorithm_comparison_meanProb$Perf_method == "Sens"] <- "Sensitivity"
performance_algorithm_comparison_meanProb$Perf_method[performance_algorithm_comparison_meanProb$Perf_method == "Spec"] <- "Specificity"

# Bring the performance measure groups in the right order
performance_algorithm_comparison_meanProb$Perf_method <- factor(performance_algorithm_comparison_meanProb$Perf_method, levels=c("AUC", "Boyce", "TSS", "Sensitivity", "Specificity"))

# Bring the predictor sets in the right order
performance_algorithm_comparison_meanProb$Predictor_set <- factor(performance_algorithm_comparison_meanProb$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)


# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot performance values
ggplot(performance_algorithm_comparison_meanProb, aes(x = Predictor_set, y = Perf_value, fill = Algorithm)) + 
  stat_boxplot(geom = "errorbar", width = 0.8) +
  geom_boxplot(width = 0.8, outlier.colour = "black", outlier.shape = 16,
               outlier.size = 1.4) +
  facet_wrap(~ Perf_method, nrow = 3, ncol = 2, scales = "free_x") +
  ylab("Performance value") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("") +
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 9),
    text = element_text(family = "Calibri"),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightsteelblue3", color = "black")) +
  scale_fill_brewer(
    palette = "BuPu",
    labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble")
  ) +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))


ggsave("output_data/plots/validation_rev/performance_algorithm_comparison_meanProb.svg", width = 18, height = 19, unit = "cm")



# (c) Grouped boxplot based on tenthPer threshold  -----------------------------

# Subset the data frame containing the performance measures to only contain 
# values based on the respective threshold
performance_algorithm_comparison_tenthPer <- performance_algorithm_comparison[performance_algorithm_comparison$Thresh_method == "tenthPer", ]

# Convert performance values to numeric
performance_algorithm_comparison_tenthPer$Perf_value <- as.numeric(as.character(performance_algorithm_comparison_tenthPer$Perf_value))

# Bring the algorithm groups in the right order
performance_algorithm_comparison_tenthPer$Algorithm <- factor(performance_algorithm_comparison_tenthPer$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Change the entry Sens to Sensitivity and Spec to Specificity
performance_algorithm_comparison_tenthPer$Perf_method[performance_algorithm_comparison_tenthPer$Perf_method == "Sens"] <- "Sensitivity"
performance_algorithm_comparison_tenthPer$Perf_method[performance_algorithm_comparison_tenthPer$Perf_method == "Spec"] <- "Specificity"

# Bring the performance measure groups in the right order
performance_algorithm_comparison_tenthPer$Perf_method <- factor(performance_algorithm_comparison_tenthPer$Perf_method, levels=c("AUC", "Boyce", "TSS", "Sensitivity", "Specificity"))

# Bring the predictor sets in the right order
performance_algorithm_comparison_tenthPer$Predictor_set <- factor(performance_algorithm_comparison_tenthPer$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)



# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot performance values
ggplot(performance_algorithm_comparison_tenthPer, aes(x = Predictor_set, y = Perf_value, fill = Algorithm)) + 
  stat_boxplot(geom = "errorbar", width = 0.8) +
  geom_boxplot(width = 0.8, outlier.colour = "black", outlier.shape = 16,
               outlier.size = 1.4) +
  facet_wrap(~ Perf_method, nrow = 3, ncol = 2, scales = "free_x") +
  ylab("Performance value") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("") +
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.subtitle = element_text(hjust = 0.5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 9),
    text = element_text(family = "Calibri"),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightsteelblue3", color = "black")) +
  scale_fill_brewer(
    palette = "BuPu",
    labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble")
  ) +
  guides(
    fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))


ggsave("output_data/plots/validation_rev/performance_algorithm_comparison_tenthPer.svg", width = 18, height = 19, unit = "cm")











# (b) Grouped boxplot ----------------------------------------------------------

# Convert AUC values to numeric
AUC_algorithm_comparison$AUC <- as.numeric(as.character(AUC_algorithm_comparison$AUC))

# Bring the algorithm groups in the right order
AUC_algorithm_comparison$Algorithm <- factor(AUC_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
AUC_algorithm_comparison$Predictor_set <- factor(AUC_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(AUC_algorithm_comparison, aes(x= Predictor_set, y=AUC, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.8) +
  geom_boxplot(width = 0.8, outlier.colour="black", outlier.shape=16,
               outlier.size=1.4) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1)) +
  #labs(title = "(a) AUC") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.direction = "horizontal", legend.position = c(0.5, 0.13),
        legend.key.size = unit(0.5, "cm"), axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        text = element_text(family = "Calibri")) +
        scale_fill_brewer(palette="BuPu", labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble"))

ggsave("output_data/plots/validation/AUC_algorithm_comparison.svg", width = 9.5, height = 8, unit = "cm")





# (c) Descriptive statistics ---------------------------------------------------

# Average value across all predictor sets of the algorithms and their ensemble
# GLM
AUC_algorithm_comparison_glm <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm == "glm")
AUC_algorithm_comparison_glm$AUC <- as.numeric(AUC_algorithm_comparison_glm$AUC)
mean(AUC_algorithm_comparison_glm$AUC) # 0.79
sd(AUC_algorithm_comparison_glm$AUC) # 0.07

# GAM
AUC_algorithm_comparison_gam <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm == "gam")
AUC_algorithm_comparison_gam$AUC <- as.numeric(AUC_algorithm_comparison_gam$AUC)
mean(AUC_algorithm_comparison_gam$AUC) # 0.80
sd(AUC_algorithm_comparison_gam$AUC) # 0.06

# RF
AUC_algorithm_comparison_rf <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm == "rf")
AUC_algorithm_comparison_rf$AUC <- as.numeric(AUC_algorithm_comparison_rf$AUC)
mean(AUC_algorithm_comparison_rf$AUC) # 0.93
sd(AUC_algorithm_comparison_rf$AUC) # 0.03

# BRT
AUC_algorithm_comparison_brt <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm == "brt")
AUC_algorithm_comparison_brt$AUC <- as.numeric(AUC_algorithm_comparison_brt$AUC)
mean(AUC_algorithm_comparison_brt$AUC) # 0.83
sd(AUC_algorithm_comparison_brt$AUC) # 0.06

# Ensemble
AUC_algorithm_comparison_ens <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm == "ensemble")
AUC_algorithm_comparison_ens$AUC <- as.numeric(AUC_algorithm_comparison_ens$AUC)
mean(AUC_algorithm_comparison_ens$AUC) # 0.87
sd(AUC_algorithm_comparison_ens$AUC) # 0.04

# Generally without ensemble
AUC_algorithm_comparison_no_ens <- subset(AUC_algorithm_comparison, AUC_algorithm_comparison$Algorithm != "ensemble")
AUC_algorithm_comparison_no_ens$AUC <- as.numeric(AUC_algorithm_comparison_no_ens$AUC)
mean(AUC_algorithm_comparison_no_ens$AUC) # 0.84
sd(AUC_algorithm_comparison_no_ens$AUC) # 0.08

# All algorithms + ensemble
AUC_algorithm_comparison$AUC <- as.numeric(AUC_algorithm_comparison$AUC)
mean(AUC_algorithm_comparison$AUC) # 0.84
sd(AUC_algorithm_comparison$AUC) # 0.07






#-------------------------------------------------------------------------------

# 2. TSS -----------------------------------------------------------------------


# (a) Value extraction  --------------------------------------------------------

# Create a data frame to store the TSS values of all models
TSS_algorithm_comparison <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(TSS_algorithm_comparison) <- c("Species", "TSS", "Algorithm", "Predictor_set")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    # Load in the performance measures
    if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
                             validation_alg_TSS <- comp_perf_clim_native
                             validation_ens_TSS <- ensemble_perf_clim_native
    } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
                                        validation_alg_TSS <- comp_perf_edaclim_native
                                        validation_ens_TSS <- ensemble_perf_edaclim_native
    } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
                                     validation_alg_TSS <- comp_perf_clim_global
                                     validation_ens_TSS <- ensemble_perf_clim_global
    } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
                                         validation_alg_TSS <- comp_perf_edaclim_global
                                         validation_ens_TSS <- ensemble_perf_edaclim_global
    }
    
    for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
      
      # Extract the considered performance value
      if (alg %in% c("glm", "gam", "rf", "brt")) { TSS_alg <- validation_alg_TSS[alg, "TSS"]
      } else if (alg == "ensemble") { TSS_alg <- validation_ens_TSS["mean_prob", "TSS"]
      }
      
      # Write a vector with extracted information
      TSS_info <- c(sp, TSS_alg, alg, pred)
      
      # Add the vector to the results data frame
      TSS_algorithm_comparison <- rbind(TSS_algorithm_comparison, TSS_info) 
      
    } # End of the loop over the algorithms
    
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(TSS_algorithm_comparison) <- c("Species", "TSS", "Algorithm", "Predictor_set")

# Save the resulting data frame
save(TSS_algorithm_comparison, file = "output_data/validation/TSS_algorithm_comparison.RData")




# (b) Grouped boxplot ----------------------------------------------------------

# Convert TSS values to numeric
TSS_algorithm_comparison$TSS <- as.numeric(as.character(TSS_algorithm_comparison$TSS))

# Bring the algorithm groups in the right order
TSS_algorithm_comparison$Algorithm <- factor(TSS_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
TSS_algorithm_comparison$Predictor_set <- factor(TSS_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(TSS_algorithm_comparison, aes(x= Predictor_set, y=TSS, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.8) +
  geom_boxplot(width = 0.8, outlier.colour="black", outlier.shape=16,
               outlier.size=1.4) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1)) +
  #labs(title = "(a) TSS") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.direction = "horizontal", legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"), axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu", labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble"))


ggsave("output_data/plots/validation/TSS_algorithm_comparison.svg", width = 9.5, height = 8, unit = "cm")



# (c) Descriptive statistics ---------------------------------------------------

# Average value across all predictor sets of the algorithms and their ensemble
# GLM
TSS_algorithm_comparison_glm <- subset(TSS_algorithm_comparison, TSS_algorithm_comparison$Algorithm == "glm")
TSS_algorithm_comparison_glm$TSS <- as.numeric(TSS_algorithm_comparison_glm$TSS)
mean(TSS_algorithm_comparison_glm$TSS) # 0.47
sd(TSS_algorithm_comparison_glm$TSS) # 0.12

# GAM
TSS_algorithm_comparison_gam <- subset(TSS_algorithm_comparison, TSS_algorithm_comparison$Algorithm == "gam")
TSS_algorithm_comparison_gam$TSS <- as.numeric(TSS_algorithm_comparison_gam$TSS)
mean(TSS_algorithm_comparison_gam$TSS) # 0.48
sd(TSS_algorithm_comparison_gam$TSS) # 0.12

# RF
TSS_algorithm_comparison_rf <- subset(TSS_algorithm_comparison, TSS_algorithm_comparison$Algorithm == "rf")
TSS_algorithm_comparison_rf$TSS <- as.numeric(TSS_algorithm_comparison_rf$TSS)
mean(TSS_algorithm_comparison_rf$TSS) # 0.70
sd(TSS_algorithm_comparison_rf$TSS) # 0.06

# BRT
TSS_algorithm_comparison_brt <- subset(TSS_algorithm_comparison, TSS_algorithm_comparison$Algorithm == "brt")
TSS_algorithm_comparison_brt$TSS <- as.numeric(TSS_algorithm_comparison_brt$TSS)
mean(TSS_algorithm_comparison_brt$TSS) # 0.52
sd(TSS_algorithm_comparison_brt$TSS) # 0.12

# Ensemble
TSS_algorithm_comparison_ens <- subset(TSS_algorithm_comparison, TSS_algorithm_comparison$Algorithm == "ensemble")
TSS_algorithm_comparison_ens$TSS <- as.numeric(TSS_algorithm_comparison_ens$TSS)
mean(TSS_algorithm_comparison_ens$TSS) # 0.58
sd(TSS_algorithm_comparison_ens$TSS) # 0.09

# All algorithms + ensemble
TSS_algorithm_comparison$TSS <- as.numeric(TSS_algorithm_comparison$TSS)
mean(TSS_algorithm_comparison$TSS) # 0.55
sd(TSS_algorithm_comparison$TSS) # 0.14




#-------------------------------------------------------------------------------

# 3. Boyce ---------------------------------------------------------------------


# (a) Value extraction  --------------------------------------------------------

# Create a data frame to store the Boyce values of all models
Boyce_algorithm_comparison <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Boyce_algorithm_comparison) <- c("Species", "Boyce", "Algorithm", "Predictor_set")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    # Load in the performance measures
    if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
                             validation_alg_Boyce <- comp_perf_clim_native
                             validation_ens_Boyce <- ensemble_perf_clim_native
    } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
                                        validation_alg_Boyce <- comp_perf_edaclim_native
                                        validation_ens_Boyce <- ensemble_perf_edaclim_native
    } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
                                     validation_alg_Boyce <- comp_perf_clim_global
                                     validation_ens_Boyce <- ensemble_perf_clim_global
    } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
                                         validation_alg_Boyce <- comp_perf_edaclim_global
                                         validation_ens_Boyce <- ensemble_perf_edaclim_global
    }
    
    for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
      
      # Extract the considered performance value
      if (alg %in% c("glm", "gam", "rf", "brt")) { Boyce_alg <- validation_alg_Boyce[alg, "Boyce"]
      } else if (alg == "ensemble") { Boyce_alg <- validation_ens_Boyce["mean_prob", "Boyce"]
      }
      
      # Write a vector with extracted information
      Boyce_info <- c(sp, Boyce_alg, alg, pred)
      
      # Add the vector to the results data frame
      Boyce_algorithm_comparison <- rbind(Boyce_algorithm_comparison, Boyce_info) 
      
    } # End of the loop over the algorithms
    
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(Boyce_algorithm_comparison) <- c("Species", "Boyce", "Algorithm", "Predictor_set")

# Save the resulting data frame
save(Boyce_algorithm_comparison, file = "output_data/validation/Boyce_algorithm_comparison.RData")



# (b) Grouped boxplot ----------------------------------------------------------

# Convert Boyce values to numeric
Boyce_algorithm_comparison$Boyce <- as.numeric(as.character(Boyce_algorithm_comparison$Boyce))

# Bring the algorithm groups in the right order
Boyce_algorithm_comparison$Algorithm <- factor(Boyce_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
Boyce_algorithm_comparison$Predictor_set <- factor(Boyce_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(Boyce_algorithm_comparison, aes(x= Predictor_set, y=Boyce, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.8) +
  geom_boxplot(width = 0.8, outlier.colour="black", outlier.shape=16,
               outlier.size=1.4) +
  theme_minimal() +
  scale_y_continuous(limits=c(-0.1,1)) +
  #labs(title = "(b) Boyce index") +
  xlab("") +
  ylab("Boyce index") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.direction = "horizontal", legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"), axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu", labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble"))

ggsave("output_data/plots/validation/Boyce_algorithm_comparison.svg", width = 9.5, height = 8, unit = "cm")



# (c) Descriptive statistics ---------------------------------------------------

# Average value across all predictor sets of the algorithms and their ensemble
# + average value across predictor sets
# GLM
Boyce_algorithm_comparison_glm <- subset(Boyce_algorithm_comparison, Boyce_algorithm_comparison$Algorithm == "glm")
Boyce_algorithm_comparison_glm$Boyce <- as.numeric(Boyce_algorithm_comparison_glm$Boyce)
mean(Boyce_algorithm_comparison_glm$Boyce) # 0.92
sd(Boyce_algorithm_comparison_glm$Boyce) # 0.1

# GAM
Boyce_algorithm_comparison_gam <- subset(Boyce_algorithm_comparison, Boyce_algorithm_comparison$Algorithm == "gam")
Boyce_algorithm_comparison_gam$Boyce <- as.numeric(Boyce_algorithm_comparison_gam$Boyce)
mean(Boyce_algorithm_comparison_gam$Boyce) # 0.93
sd(Boyce_algorithm_comparison_gam$Boyce) # 0.09

# RF
Boyce_algorithm_comparison_rf <- subset(Boyce_algorithm_comparison, Boyce_algorithm_comparison$Algorithm == "rf")
Boyce_algorithm_comparison_rf$Boyce <- as.numeric(Boyce_algorithm_comparison_rf$Boyce)
mean(Boyce_algorithm_comparison_rf$Boyce) # 0.98
sd(Boyce_algorithm_comparison_rf$Boyce) # 0.04

# BRT
Boyce_algorithm_comparison_brt <- subset(Boyce_algorithm_comparison, Boyce_algorithm_comparison$Algorithm == "brt")
Boyce_algorithm_comparison_brt$Boyce <- as.numeric(Boyce_algorithm_comparison_brt$Boyce)
mean(Boyce_algorithm_comparison_brt$Boyce) # 0.86
sd(Boyce_algorithm_comparison_brt$Boyce) # 0.12

# Ensemble
Boyce_algorithm_comparison_ens <- subset(Boyce_algorithm_comparison, Boyce_algorithm_comparison$Algorithm == "ensemble")
Boyce_algorithm_comparison_ens$Boyce <- as.numeric(Boyce_algorithm_comparison_ens$Boyce)
mean(Boyce_algorithm_comparison_ens$Boyce) # 0.97
sd(Boyce_algorithm_comparison_ens$Boyce) # 0.05

# All algorithms + ensemble
Boyce_algorithm_comparison$Boyce <- as.numeric(Boyce_algorithm_comparison$Boyce)
mean(Boyce_algorithm_comparison$Boyce) # 0.93
sd(Boyce_algorithm_comparison$Boyce) # 0.09

# natclim & natclim+eda
Boyce_algorithm_comparison_natclim <- subset(Boyce_algorithm_comparison_no_ens, Boyce_algorithm_comparison_no_ens$Predictor_set == "natclim")
Boyce_algorithm_comparison_natclimeda <- subset(Boyce_algorithm_comparison_no_ens, Boyce_algorithm_comparison_no_ens$Predictor_set == "natclim+eda")
Boyce_algorithm_comparison_nat <- rbind(Boyce_algorithm_comparison_natclim, Boyce_algorithm_comparison_natclimeda)
Boyce_algorithm_comparison_nat$Boyce <- as.numeric(Boyce_algorithm_comparison_nat$Boyce)
mean(Boyce_algorithm_comparison_nat$Boyce) # 0.90
sd(Boyce_algorithm_comparison_nat$Boyce) # 0.12

# globclim & globclim+eda
Boyce_algorithm_comparison_globclim <- subset(Boyce_algorithm_comparison_no_ens, Boyce_algorithm_comparison_no_ens$Predictor_set == "globclim")
Boyce_algorithm_comparison_globclimeda <- subset(Boyce_algorithm_comparison_no_ens, Boyce_algorithm_comparison_no_ens$Predictor_set == "globclim+eda")
Boyce_algorithm_comparison_glob <- rbind(Boyce_algorithm_comparison_globclim, Boyce_algorithm_comparison_globclimeda)
Boyce_algorithm_comparison_glob$Boyce <- as.numeric(Boyce_algorithm_comparison_glob$Boyce)
mean(Boyce_algorithm_comparison_glob$Boyce) # 0.95
sd(Boyce_algorithm_comparison_glob$Boyce) # 0.07



#-------------------------------------------------------------------------------

# 4. Sensitivity ---------------------------------------------------------------


# (a) Value extraction  --------------------------------------------------------

# Create a data frame to store the Sensitivity values of all models
Sensitivity_algorithm_comparison <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Sensitivity_algorithm_comparison) <- c("Species", "Sensitivity", "Algorithm", "Predictor_set")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    # Load in the performance measures
    if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
                             validation_alg_Sensitivity <- comp_perf_clim_native
                             validation_ens_Sensitivity <- ensemble_perf_clim_native
    } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
                                        validation_alg_Sensitivity <- comp_perf_edaclim_native
                                        validation_ens_Sensitivity <- ensemble_perf_edaclim_native
    } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
                                     validation_alg_Sensitivity <- comp_perf_clim_global
                                     validation_ens_Sensitivity <- ensemble_perf_clim_global
    } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
                                         validation_alg_Sensitivity <- comp_perf_edaclim_global
                                         validation_ens_Sensitivity <- ensemble_perf_edaclim_global
    }
    
    for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
      
      # Extract the considered performance value
      if (alg %in% c("glm", "gam", "rf", "brt")) { Sensitivity_alg <- validation_alg_Sensitivity[alg, "Sens"]
      } else if (alg == "ensemble") { Sensitivity_alg <- validation_ens_Sensitivity["mean_prob", "Sens"]
      }
      
      # Write a vector with extracted information
      Sensitivity_info <- c(sp, Sensitivity_alg, alg, pred)
      
      # Add the vector to the results data frame
      Sensitivity_algorithm_comparison <- rbind(Sensitivity_algorithm_comparison, Sensitivity_info) 
      
    } # End of the loop over the algorithms
    
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(Sensitivity_algorithm_comparison) <- c("Species", "Sensitivity", "Algorithm", "Predictor_set")

# Save the resulting data frame
save(Sensitivity_algorithm_comparison, file = "output_data/validation/Sensitivity_algorithm_comparison.RData")



# (b) Grouped boxplot ----------------------------------------------------------

# Convert Sensitivity values to numeric
Sensitivity_algorithm_comparison$Sensitivity <- as.numeric(as.character(Sensitivity_algorithm_comparison$Sensitivity))

# Bring the algorithm groups in the right order
Sensitivity_algorithm_comparison$Algorithm <- factor(Sensitivity_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
Sensitivity_algorithm_comparison$Predictor_set <- factor(Sensitivity_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(Sensitivity_algorithm_comparison, aes(x= Predictor_set, y=Sensitivity, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.8) +
  geom_boxplot(width = 0.8, outlier.colour="black", outlier.shape=16,
               outlier.size=1.4) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1)) +
  #labs(title = "(b) Sensitivity") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.direction = "horizontal", legend.position = c(0.5, 0.13),
        legend.key.size = unit(0.5, "cm"), axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu", labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble"))

ggsave("output_data/plots/validation/Sensitivity_algorithm_comparison.svg", width = 9.5, height = 8, unit = "cm")





#-------------------------------------------------------------------------------

# 5. Specificity ---------------------------------------------------------------


# (a) Value extraction  --------------------------------------------------------

# Create a data frame to store the Specificity values of all models
Specificity_algorithm_comparison <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Specificity_algorithm_comparison) <- c("Species", "Specificity", "Algorithm", "Predictor_set")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    # Load in the performance measures
    if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
                             validation_alg_Specificity <- comp_perf_clim_native
                             validation_ens_Specificity <- ensemble_perf_clim_native
    } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
                                        validation_alg_Specificity <- comp_perf_edaclim_native
                                        validation_ens_Specificity <- ensemble_perf_edaclim_native
    } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
                                     validation_alg_Specificity <- comp_perf_clim_global
                                     validation_ens_Specificity <- ensemble_perf_clim_global
    } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
                                         validation_alg_Specificity <- comp_perf_edaclim_global
                                         validation_ens_Specificity <- ensemble_perf_edaclim_global
    }
    
    for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
      
      # Extract the considered performance value
      if (alg %in% c("glm", "gam", "rf", "brt")) { Specificity_alg <- validation_alg_Specificity[alg, "Spec"]
      } else if (alg == "ensemble") { Specificity_alg <- validation_ens_Specificity["mean_prob", "Spec"]
      }
      
      # Write a vector with extracted information
      Specificity_info <- c(sp, Specificity_alg, alg, pred)
      
      # Add the vector to the results data frame
      Specificity_algorithm_comparison <- rbind(Specificity_algorithm_comparison, Specificity_info) 
      
    } # End of the loop over the algorithms
    
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(Specificity_algorithm_comparison) <- c("Species", "Specificity", "Algorithm", "Predictor_set")

# Save the resulting data frame
save(Specificity_algorithm_comparison, file = "output_data/validation/Specificity_algorithm_comparison.RData")


# (b) Grouped boxplot ----------------------------------------------------------

# Convert Specificity values to numeric
Specificity_algorithm_comparison$Specificity <- as.numeric(as.character(Specificity_algorithm_comparison$Specificity))

# Bring the algorithm groups in the right order
Specificity_algorithm_comparison$Algorithm <- factor(Specificity_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
Specificity_algorithm_comparison$Predictor_set <- factor(Specificity_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(Specificity_algorithm_comparison, aes(x= Predictor_set, y=Specificity, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.8) +
  geom_boxplot(width = 0.8, outlier.colour="black", outlier.shape=16,
               outlier.size=1.4) +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1)) +
  #labs(title = "(c) Specificity") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.direction = "horizontal", legend.position = c(0.5, 0.13),
        legend.key.size = unit(0.5, "cm"), axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10, color = "black"), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu", labels = c("glm" = "GLM", "gam" = "GAM", "rf" = "RF", "brt" = "BRT", "ensemble" = "Ensemble"))




ggsave("output_data/plots/validation/Specificity_algorithm_comparison.svg", width = 9.5, height = 8, unit = "cm")



#-------------------------------------------------------------------------------

# 6. Calculate component uncertainties -----------------------------------------

library(randomForest)

# (a) AUC ----------------------------------------------------------------------

# Loop through the different rows of the data frame and identify the predictor sets
# as single predictor type and niche

# Load in the data frame
load("output_data/validation/AUC_algorithm_comparison.RData")

# Create new columns for the data frame
AUC_algorithm_comparison_uncertainty <- AUC_algorithm_comparison
AUC_algorithm_comparison_uncertainty$Predictor_type <- NA
AUC_algorithm_comparison_uncertainty$Niche <- NA

for (i in 1:nrow(AUC_algorithm_comparison_uncertainty)) {
  
  # identify the predictor set
  AUC_predictor_set <- AUC_algorithm_comparison_uncertainty[i,"Predictor_set"]
  
  if (AUC_predictor_set == "natclim") { AUC_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                        AUC_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
    
  } else if (AUC_predictor_set == "natclim+eda") { AUC_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                   AUC_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
                                                   
  } else if (AUC_predictor_set == "globclim") { AUC_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                                AUC_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
                                                
  } else if (AUC_predictor_set == "globclim+eda") { AUC_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                    AUC_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  }
                                                   
                                               
} # End of loop over the rows

# Remove the values of ensembles
AUC_algorithm_comparison_uncertainty <- subset(AUC_algorithm_comparison_uncertainty, AUC_algorithm_comparison_uncertainty$Algorithm != "ensemble")

# Make sure the uncertainty components Algorithm, predictor type and niche are factors
AUC_algorithm_comparison_uncertainty$Algorithm <- as.factor(AUC_algorithm_comparison_uncertainty$Algorithm)
AUC_algorithm_comparison_uncertainty$Predictor_type <- as.factor(AUC_algorithm_comparison_uncertainty$Predictor_type)
AUC_algorithm_comparison_uncertainty$Niche <- as.factor(AUC_algorithm_comparison_uncertainty$Niche)

# Make sure the dependent variable (validation measure) is numeric
AUC_algorithm_comparison_uncertainty$AUC <- as.numeric(AUC_algorithm_comparison_uncertainty$AUC)

# Build a random forest model
model_RF_AUC <- randomForest(x = AUC_algorithm_comparison_uncertainty[, c("Algorithm", "Predictor_type", "Niche")],
                             y = AUC_algorithm_comparison_uncertainty$AUC,
                             ntree = 1000, importance = TRUE)

# Show variable importance
importance(model_RF_AUC)





# (b) TSS ----------------------------------------------------------------------


# Loop through the different rows of the data frame and identify the predictor sets
# as single predictor type and niche

# Load in the data frame
load("output_data/validation/TSS_algorithm_comparison.RData")

# Create new columns for the data frame
TSS_algorithm_comparison_uncertainty <- TSS_algorithm_comparison
TSS_algorithm_comparison_uncertainty$Predictor_type <- NA
TSS_algorithm_comparison_uncertainty$Niche <- NA

for (i in 1:nrow(TSS_algorithm_comparison_uncertainty)) {
  
  # identify the predictor set
  TSS_predictor_set <- TSS_algorithm_comparison_uncertainty[i,"Predictor_set"]
  
  if (TSS_predictor_set == "natclim") { TSS_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                        TSS_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (TSS_predictor_set == "natclim+eda") { TSS_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                   TSS_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (TSS_predictor_set == "globclim") { TSS_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                                TSS_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  
  } else if (TSS_predictor_set == "globclim+eda") { TSS_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                    TSS_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  }
  
  
} # End of loop over the rows

# Remove the values of ensembles
TSS_algorithm_comparison_uncertainty <- subset(TSS_algorithm_comparison_uncertainty, TSS_algorithm_comparison_uncertainty$Algorithm != "ensemble")

# Make sure the uncertainty components Algorithm, predictor type and niche are factors
TSS_algorithm_comparison_uncertainty$Algorithm <- as.factor(TSS_algorithm_comparison_uncertainty$Algorithm)
TSS_algorithm_comparison_uncertainty$Predictor_type <- as.factor(TSS_algorithm_comparison_uncertainty$Predictor_type)
TSS_algorithm_comparison_uncertainty$Niche <- as.factor(TSS_algorithm_comparison_uncertainty$Niche)

# Make sure the dependent variable (validation measure) is numeric
TSS_algorithm_comparison_uncertainty$TSS <- as.numeric(TSS_algorithm_comparison_uncertainty$TSS)

# Build a random forest model
model_RF_TSS <- randomForest(x = TSS_algorithm_comparison_uncertainty[, c("Algorithm", "Predictor_type", "Niche")],
                             y = TSS_algorithm_comparison_uncertainty$TSS,
                             ntree = 1000, importance = TRUE)

# Show variable importance
importance(model_RF_TSS)



# (c) Boyce --------------------------------------------------------------------

# Loop through the different rows of the data frame and identify the predictor sets
# as single predictor type and niche

# Load in the data frame
load("output_data/validation/Boyce_algorithm_comparison.RData")

# Create new columns for the data frame
Boyce_algorithm_comparison_uncertainty <- Boyce_algorithm_comparison
Boyce_algorithm_comparison_uncertainty$Predictor_type <- NA
Boyce_algorithm_comparison_uncertainty$Niche <- NA

for (i in 1:nrow(Boyce_algorithm_comparison_uncertainty)) {
  
  # identify the predictor set
  Boyce_predictor_set <- Boyce_algorithm_comparison_uncertainty[i,"Predictor_set"]
  
  if (Boyce_predictor_set == "natclim") { Boyce_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                          Boyce_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Boyce_predictor_set == "natclim+eda") { Boyce_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                     Boyce_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Boyce_predictor_set == "globclim") { Boyce_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                                  Boyce_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  
  } else if (Boyce_predictor_set == "globclim+eda") { Boyce_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                      Boyce_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  }
  
  
} # End of loop over the rows

# Remove the values of ensembles
Boyce_algorithm_comparison_uncertainty <- subset(Boyce_algorithm_comparison_uncertainty, Boyce_algorithm_comparison_uncertainty$Algorithm != "ensemble")

# Make sure the uncertainty components Algorithm, predictor type and niche are factors
Boyce_algorithm_comparison_uncertainty$Algorithm <- as.factor(Boyce_algorithm_comparison_uncertainty$Algorithm)
Boyce_algorithm_comparison_uncertainty$Predictor_type <- as.factor(Boyce_algorithm_comparison_uncertainty$Predictor_type)
Boyce_algorithm_comparison_uncertainty$Niche <- as.factor(Boyce_algorithm_comparison_uncertainty$Niche)

# Make sure the dependent variable (validation measure) is numeric
Boyce_algorithm_comparison_uncertainty$Boyce <- as.numeric(Boyce_algorithm_comparison_uncertainty$Boyce)

# Build a random forest model
model_RF_Boyce <- randomForest(x = Boyce_algorithm_comparison_uncertainty[, c("Algorithm", "Predictor_type", "Niche")],
                             y = Boyce_algorithm_comparison_uncertainty$Boyce,
                             ntree = 1000, importance = TRUE)

# Show variable importance
importance(model_RF_Boyce)




# (d) Sensitivity --------------------------------------------------------------

# Loop through the different rows of the data frame and identify the predictor sets
# as single predictor type and niche

# Load in the data frame
load("output_data/validation/Sensitivity_algorithm_comparison.RData")

# Create new columns for the data frame
Sens_algorithm_comparison_uncertainty <- Sensitivity_algorithm_comparison
Sens_algorithm_comparison_uncertainty$Predictor_type <- NA
Sens_algorithm_comparison_uncertainty$Niche <- NA

for (i in 1:nrow(Sens_algorithm_comparison_uncertainty)) {
  
  # identify the predictor set
  Sens_predictor_set <- Sens_algorithm_comparison_uncertainty[i,"Predictor_set"]
  
  if (Sens_predictor_set == "natclim") { Sens_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                         Sens_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Sens_predictor_set == "natclim+eda") { Sens_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                    Sens_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Sens_predictor_set == "globclim") { Sens_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                                 Sens_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  
  } else if (Sens_predictor_set == "globclim+eda") { Sens_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                     Sens_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  }
  
  
} # End of loop over the rows

# Remove the values of ensembles
Sens_algorithm_comparison_uncertainty <- subset(Sens_algorithm_comparison_uncertainty, Sens_algorithm_comparison_uncertainty$Algorithm != "ensemble")

# Make sure the uncertainty components Algorithm, predictor type and niche are factors
Sens_algorithm_comparison_uncertainty$Algorithm <- as.factor(Sens_algorithm_comparison_uncertainty$Algorithm)
Sens_algorithm_comparison_uncertainty$Predictor_type <- as.factor(Sens_algorithm_comparison_uncertainty$Predictor_type)
Sens_algorithm_comparison_uncertainty$Niche <- as.factor(Sens_algorithm_comparison_uncertainty$Niche)

# Make sure the dependent variable (validation measure) is numeric
Sens_algorithm_comparison_uncertainty$Sensitivity <- as.numeric(Sens_algorithm_comparison_uncertainty$Sensitivity)

# Build a random forest model
model_RF_Sens <- randomForest(x = Sens_algorithm_comparison_uncertainty[, c("Algorithm", "Predictor_type", "Niche")],
                               y = Sens_algorithm_comparison_uncertainty$Sensitivity,
                               ntree = 1000, importance = TRUE)

# Show variable importance
importance(model_RF_Sens)



# (e) Specificity --------------------------------------------------------------

# Loop through the different rows of the data frame and identify the predictor sets
# as single predictor type and niche

# Load in the data frame
load("output_data/validation/Specificity_algorithm_comparison.RData")

# Create new columns for the data frame
Spec_algorithm_comparison_uncertainty <- Specificity_algorithm_comparison
Spec_algorithm_comparison_uncertainty$Predictor_type <- NA
Spec_algorithm_comparison_uncertainty$Niche <- NA

for (i in 1:nrow(Spec_algorithm_comparison_uncertainty)) {
  
  # identify the predictor set
  Spec_predictor_set <- Spec_algorithm_comparison_uncertainty[i,"Predictor_set"]
  
  if (Spec_predictor_set == "natclim") { Spec_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                         Spec_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Spec_predictor_set == "natclim+eda") { Spec_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                    Spec_algorithm_comparison_uncertainty[i, "Niche"] <- "native"
  
  } else if (Spec_predictor_set == "globclim") { Spec_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "clim"
                                                 Spec_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  
  } else if (Spec_predictor_set == "globclim+eda") { Spec_algorithm_comparison_uncertainty[i, "Predictor_type"] <- "edaclim"
                                                     Spec_algorithm_comparison_uncertainty[i, "Niche"] <- "global"
  }
  
  
} # End of loop over the rows

# Remove the values of ensembles
Spec_algorithm_comparison_uncertainty <- subset(Spec_algorithm_comparison_uncertainty, Spec_algorithm_comparison_uncertainty$Algorithm != "ensemble")

# Make sure the uncertainty components Algorithm, predictor type and niche are factors
Spec_algorithm_comparison_uncertainty$Algorithm <- as.factor(Spec_algorithm_comparison_uncertainty$Algorithm)
Spec_algorithm_comparison_uncertainty$Predictor_type <- as.factor(Spec_algorithm_comparison_uncertainty$Predictor_type)
Spec_algorithm_comparison_uncertainty$Niche <- as.factor(Spec_algorithm_comparison_uncertainty$Niche)

# Make sure the dependent variable (validation measure) is numeric
Spec_algorithm_comparison_uncertainty$Specificity <- as.numeric(Spec_algorithm_comparison_uncertainty$Specificity)

# Build a random forest model
model_RF_Spec <- randomForest(x = Spec_algorithm_comparison_uncertainty[, c("Algorithm", "Predictor_type", "Niche")],
                              y = Spec_algorithm_comparison_uncertainty$Specificity,
                              ntree = 1000, importance = TRUE)

# Show variable importance
importance(model_RF_Spec)

