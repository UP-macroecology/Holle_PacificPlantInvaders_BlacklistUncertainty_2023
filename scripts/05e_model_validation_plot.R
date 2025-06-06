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
      if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_clim_native_", t))
        validation_ens <- ensemble_perf_clim_native
      } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_edaclim_native_", t))
        validation_ens <- ensemble_perf_edaclim_native
      } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
        validation_alg <- get(paste0("comp_perf_clim_global_", t))
        validation_ens <- ensemble_perf_clim_global
      } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
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
save(performance_algorithm_comparison, file = "output_data/validation/performance_algorithm_comparison.RData")




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


ggsave("output_data/plots/validation/performance_algorithm_comparison_maxTSS_Boyce_TSS.svg", width = 9.5, height = 14, unit = "cm")


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


ggsave("output_data/plots/validation/performance_algorithm_comparison_maxTSS_AUC_Sens_Spec.svg", width = 18, height = 14, unit = "cm")





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


ggsave("output_data/plots/validation/performance_algorithm_comparison_meanProb.svg", width = 18, height = 19, unit = "cm")



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


ggsave("output_data/plots/validation/performance_algorithm_comparison_tenthPer.svg", width = 18, height = 19, unit = "cm")






#-------------------------------------------------------------------------------

# 3. Descriptive statistics based on maxTSS threshold --------------------------

# Extract the validation results based on maxTSS threshold
performance_algorithm_comparison_maxTSS <- performance_algorithm_comparison[performance_algorithm_comparison$Thresh_method == "maxTSS", ]


# (a) AUC ----------------------------------------------------------------------
# Extract validation results for respective validation measure
performance_algorithm_comparison_maxTSS_AUC <- performance_algorithm_comparison_maxTSS[performance_algorithm_comparison_maxTSS$Perf_method == "AUC", ]

# GLM
performance_algorithm_comparison_maxTSS_AUC_GLM <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm == "glm", ]
performance_algorithm_comparison_maxTSS_AUC_GLM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_GLM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_GLM$Perf_value) # 0.79
sd(performance_algorithm_comparison_maxTSS_AUC_GLM$Perf_value) # 0.065

# GAM
performance_algorithm_comparison_maxTSS_AUC_GAM <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm == "gam", ]
performance_algorithm_comparison_maxTSS_AUC_GAM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_GAM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_GAM$Perf_value) # 0.80
sd(performance_algorithm_comparison_maxTSS_AUC_GAM$Perf_value) # 0.065

# RF
performance_algorithm_comparison_maxTSS_AUC_RF <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm == "rf", ]
performance_algorithm_comparison_maxTSS_AUC_RF$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_RF$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_RF$Perf_value) # 0.85
sd(performance_algorithm_comparison_maxTSS_AUC_RF$Perf_value) # 0.056

# BRT
performance_algorithm_comparison_maxTSS_AUC_BRT <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm == "brt", ]
performance_algorithm_comparison_maxTSS_AUC_BRT$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_BRT$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_BRT$Perf_value) # 0.84
sd(performance_algorithm_comparison_maxTSS_AUC_BRT$Perf_value) # 0.06

# Ensemble
performance_algorithm_comparison_maxTSS_AUC_Ens <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm == "ensemble", ]
performance_algorithm_comparison_maxTSS_AUC_Ens$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_Ens$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_Ens$Perf_value) # 0.84
sd(performance_algorithm_comparison_maxTSS_AUC_Ens$Perf_value) # 0.057

# Generally without ensemble
performance_algorithm_comparison_maxTSS_AUC_noEns <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Algorithm != "ensemble", ]
performance_algorithm_comparison_maxTSS_AUC_noEns$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_noEns$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_noEns$Perf_value) # 0.82
sd(performance_algorithm_comparison_maxTSS_AUC_noEns$Perf_value) # 0.066

# All algorithms + ensemble
performance_algorithm_comparison_maxTSS_AUC$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC$Perf_value) # 0.82
sd(performance_algorithm_comparison_maxTSS_AUC$Perf_value) # 0.065

# natclim & natclim+eda
performance_algorithm_comparison_maxTSS_AUC_native <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Predictor_set %in% c("natclim", "natclim+eda"), ]
performance_algorithm_comparison_maxTSS_AUC_native$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_native$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_native$Perf_value) # 0.82
sd(performance_algorithm_comparison_maxTSS_AUC_native$Perf_value) # 0.068

# globclim & globclim+eda
performance_algorithm_comparison_maxTSS_AUC_global <- performance_algorithm_comparison_maxTSS_AUC[performance_algorithm_comparison_maxTSS_AUC$Predictor_set %in% c("globclim", "globclim+eda"), ]
performance_algorithm_comparison_maxTSS_AUC_global$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_AUC_global$Perf_value)
mean(performance_algorithm_comparison_maxTSS_AUC_global$Perf_value) # 0.83
sd(performance_algorithm_comparison_maxTSS_AUC_global$Perf_value) # 0.061






# (b) TSS ----------------------------------------------------------------------
# Extract validation results for respective validation measure
performance_algorithm_comparison_maxTSS_TSS <- performance_algorithm_comparison_maxTSS[performance_algorithm_comparison_maxTSS$Perf_method == "TSS", ]

# GLM
performance_algorithm_comparison_maxTSS_TSS_GLM <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm == "glm", ]
performance_algorithm_comparison_maxTSS_TSS_GLM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_GLM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_GLM$Perf_value) # 0.47
sd(performance_algorithm_comparison_maxTSS_TSS_GLM$Perf_value) # 0.12

# GAM
performance_algorithm_comparison_maxTSS_TSS_GAM <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm == "gam", ]
performance_algorithm_comparison_maxTSS_TSS_GAM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_GAM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_GAM$Perf_value) # 0.48
sd(performance_algorithm_comparison_maxTSS_TSS_GAM$Perf_value) # 0.12

# RF
performance_algorithm_comparison_maxTSS_TSS_RF <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm == "rf", ]
performance_algorithm_comparison_maxTSS_TSS_RF$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_RF$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_RF$Perf_value) # 0.57
sd(performance_algorithm_comparison_maxTSS_TSS_RF$Perf_value) # 0.11

# BRT
performance_algorithm_comparison_maxTSS_TSS_BRT <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm == "brt", ]
performance_algorithm_comparison_maxTSS_TSS_BRT$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_BRT$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_BRT$Perf_value) # 0.54
sd(performance_algorithm_comparison_maxTSS_TSS_BRT$Perf_value) # 0.11

# Ensemble
performance_algorithm_comparison_maxTSS_TSS_Ens <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm == "ensemble", ]
performance_algorithm_comparison_maxTSS_TSS_Ens$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_Ens$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_Ens$Perf_value) # 0.54
sd(performance_algorithm_comparison_maxTSS_TSS_Ens$Perf_value) # 0.11

# Generally without ensemble
performance_algorithm_comparison_maxTSS_TSS_noEns <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Algorithm != "ensemble", ]
performance_algorithm_comparison_maxTSS_TSS_noEns$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_noEns$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_noEns$Perf_value) # 0.51
sd(performance_algorithm_comparison_maxTSS_TSS_noEns$Perf_value) # 0.12

# All algorithms + ensemble
performance_algorithm_comparison_maxTSS_TSS$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS$Perf_value) # 0.52
sd(performance_algorithm_comparison_maxTSS_TSS$Perf_value) # 0.12

# natclim & natclim+eda
performance_algorithm_comparison_maxTSS_TSS_native <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Predictor_set %in% c("natclim", "natclim+eda"), ]
performance_algorithm_comparison_maxTSS_TSS_native$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_native$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_native$Perf_value) # 0.51
sd(performance_algorithm_comparison_maxTSS_TSS_native$Perf_value) # 0.12

# globclim & globclim+eda
performance_algorithm_comparison_maxTSS_TSS_global <- performance_algorithm_comparison_maxTSS_TSS[performance_algorithm_comparison_maxTSS_TSS$Predictor_set %in% c("globclim", "globclim+eda"), ]
performance_algorithm_comparison_maxTSS_TSS_global$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_TSS_global$Perf_value)
mean(performance_algorithm_comparison_maxTSS_TSS_global$Perf_value) # 0.53
sd(performance_algorithm_comparison_maxTSS_TSS_global$Perf_value) # 0.12




# (b) Boyce --------------------------------------------------------------------
# Extract validation results for respective validation measure
performance_algorithm_comparison_maxTSS_Boyce <- performance_algorithm_comparison_maxTSS[performance_algorithm_comparison_maxTSS$Perf_method == "Boyce", ]

# GLM
performance_algorithm_comparison_maxTSS_Boyce_GLM <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm == "glm", ]
performance_algorithm_comparison_maxTSS_Boyce_GLM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_GLM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_GLM$Perf_value) # 0.92
sd(performance_algorithm_comparison_maxTSS_Boyce_GLM$Perf_value) # 0.094

# GAM
performance_algorithm_comparison_maxTSS_Boyce_GAM <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm == "gam", ]
performance_algorithm_comparison_maxTSS_Boyce_GAM$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_GAM$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_GAM$Perf_value) # 0.93
sd(performance_algorithm_comparison_maxTSS_Boyce_GAM$Perf_value) # 0.084

# RF
performance_algorithm_comparison_maxTSS_Boyce_RF <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm == "rf", ]
performance_algorithm_comparison_maxTSS_Boyce_RF$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_RF$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_RF$Perf_value) # 0.96
sd(performance_algorithm_comparison_maxTSS_Boyce_RF$Perf_value) # 0.069

# BRT
performance_algorithm_comparison_maxTSS_Boyce_BRT <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm == "brt", ]
performance_algorithm_comparison_maxTSS_Boyce_BRT$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_BRT$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_BRT$Perf_value) # 0.95
sd(performance_algorithm_comparison_maxTSS_Boyce_BRT$Perf_value) # 0.076

# Ensemble
performance_algorithm_comparison_maxTSS_Boyce_Ens <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm == "ensemble", ]
performance_algorithm_comparison_maxTSS_Boyce_Ens$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_Ens$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_Ens$Perf_value) # 0.95
sd(performance_algorithm_comparison_maxTSS_Boyce_Ens$Perf_value) # 0.068

# Generally without ensemble
performance_algorithm_comparison_maxTSS_Boyce_noEns <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Algorithm != "ensemble", ]
performance_algorithm_comparison_maxTSS_Boyce_noEns$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_noEns$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_noEns$Perf_value) # 0.94
sd(performance_algorithm_comparison_maxTSS_Boyce_noEns$Perf_value) # 0.083

# All algorithms + ensemble
performance_algorithm_comparison_maxTSS_Boyce$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce$Perf_value) # 0.94
sd(performance_algorithm_comparison_maxTSS_Boyce$Perf_value) # 0.08

# natclim & natclim+eda
performance_algorithm_comparison_maxTSS_Boyce_native <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Predictor_set %in% c("natclim", "natclim+eda"), ]
performance_algorithm_comparison_maxTSS_Boyce_native$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_native$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_native$Perf_value) # 0.91
sd(performance_algorithm_comparison_maxTSS_Boyce_native$Perf_value) # 0.099

# globclim & globclim+eda
performance_algorithm_comparison_maxTSS_Boyce_global <- performance_algorithm_comparison_maxTSS_Boyce[performance_algorithm_comparison_maxTSS_Boyce$Predictor_set %in% c("globclim", "globclim+eda"), ]
performance_algorithm_comparison_maxTSS_Boyce_global$Perf_value <- as.numeric(performance_algorithm_comparison_maxTSS_Boyce_global$Perf_value)
mean(performance_algorithm_comparison_maxTSS_Boyce_global$Perf_value) # 0.97
sd(performance_algorithm_comparison_maxTSS_Boyce_global$Perf_value) # 0.045


