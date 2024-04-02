# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                      05e. Model validation plot                        #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(ggplot2)

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 

# Create a vector with applied algorithms and their ensemble
algorithm <- c("glm", "gam", "rf", "brt", "ensemble")

# Create a vector with the different predictor set
predictor_set <- c("natclim", "natclim+eda", "globclim", "globclim+eda")



#-------------------------------------------------------------------------------

# 1. AUC -----------------------------------------------------------------------


# (a) Value extraction  --------------------------------------------------------

# Create a data frame to store the AUC values of all models
AUC_algorithm_comparison <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(AUC_algorithm_comparison) <- c("Species", "AUC", "Algorithm", "Predictor_set")


for (sp in study_species) { # Start of the loop over all species
  
  print(sp)
  
  for (pred in predictor_set) { # Start of the loop over the four predictor sets
    
    # Load in the performance measures
    if (pred == "natclim") { load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
                             validation_alg_AUC <- comp_perf_clim_native
                             validation_ens_AUC <- ensemble_perf_clim_native
    } else if (pred == "natclim+eda") { load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData"))
                                        validation_alg_AUC <- comp_perf_edaclim_native
                                        validation_ens_AUC <- ensemble_perf_edaclim_native
    } else if (pred == "globclim") { load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
                                     validation_alg_AUC <- comp_perf_clim_global
                                     validation_ens_AUC <- ensemble_perf_clim_global
    } else if (pred == "globclim+eda") { load(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
                                         validation_alg_AUC <- comp_perf_edaclim_global
                                         validation_ens_AUC <- ensemble_perf_edaclim_global
                                        
    }                                    
    

    
    for (alg in algorithm) { # Start of the loop over the algorithms and ensemble
      
      # Extract the considered performance value
      if (alg %in% c("glm", "gam", "rf", "brt")) { AUC_alg <- validation_alg_AUC[alg, "AUC"]
      } else if (alg == "ensemble") { AUC_alg <- validation_ens_AUC["mean_prob", "AUC"]
      }
      
      # Write a vector with extracted information
      AUC_info <- c(sp, AUC_alg, alg, pred)
      
      # Add the vector to the results data frame
      AUC_algorithm_comparison <- rbind(AUC_algorithm_comparison, AUC_info) 
      
    } # End of the loop over the algorithms
      
  } # End of the loop over the predictor sets
  
} # End of the loop over the species

# Make sure the column names are correct
colnames(AUC_algorithm_comparison) <- c("Species", "AUC", "Algorithm", "Predictor_set")

# Save the resulting data frame
save(AUC_algorithm_comparison, file = "output_data/validation/AUC_algorithm_comparison.RData")



# (b) Grouped boxplot ----------------------------------------------------------

# Convert AUC values to numeric
AUC_algorithm_comparison$AUC <- as.numeric(as.character(AUC_algorithm_comparison$AUC))

# Bring the algorithm groups in the right order
AUC_algorithm_comparison$Algorithm <- factor(AUC_algorithm_comparison$Algorithm, levels=c("glm", "gam", "rf", "brt", "ensemble"))

# Bring the predictor sets in the right order
AUC_algorithm_comparison$Predictor_set <- factor(AUC_algorithm_comparison$Predictor_set, levels=c("natclim", "natclim+eda", "globclim", "globclim+eda"), ordered = TRUE)

# Plot
ggplot(AUC_algorithm_comparison, aes(x= Predictor_set, y=AUC, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.6) +
  geom_boxplot(width = 0.6, outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title = "(a) AUC") +
  xlab("Predictor set") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.925, 0.18),
        legend.key.size = unit(0.8, "cm"), legend.box.background = element_rect(color="black", size=0.9), axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu")

ggsave("output_data/plots/validation/AUC_algorithm_comparison.svg", width = 11, height = 7)





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

# Plot
ggplot(TSS_algorithm_comparison, aes(x= Predictor_set, y=TSS, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.6) +
  geom_boxplot(width = 0.6, outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title = "(b) TSS") +
  xlab("Predictor set") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.925, 0.18),
        legend.key.size = unit(0.8, "cm"), legend.box.background = element_rect(color="black", size=0.9), axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu")

ggsave("output_data/plots/validation/TSS_algorithm_comparison.svg", width = 11, height = 7)




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

# Plot
ggplot(Boyce_algorithm_comparison, aes(x= Predictor_set, y=Boyce, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.6) +
  geom_boxplot(width = 0.6, outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  scale_y_continuous(limits=c(-0.1,1)) +
  labs(title = "(c) Boyce") +
  xlab("Predictor set") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.925, 0.18),
        legend.key.size = unit(0.8, "cm"), legend.box.background = element_rect(color="black", size=0.9), axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu")

ggsave("output_data/plots/validation/Boyce_algorithm_comparison.svg", width = 11, height = 7)



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

# Plot
ggplot(Sensitivity_algorithm_comparison, aes(x= Predictor_set, y=Sensitivity, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.6) +
  geom_boxplot(width = 0.6, outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title = "(d) Sensitivity") +
  xlab("Predictor set") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.925, 0.18),
        legend.key.size = unit(0.8, "cm"), legend.box.background = element_rect(color="black", size=0.9), axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu")

ggsave("output_data/plots/validation/Sensitivity_algorithm_comparison.svg", width = 11, height = 7)





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

# Plot
ggplot(Specificity_algorithm_comparison, aes(x= Predictor_set, y=Specificity, fill=Algorithm)) + 
  stat_boxplot(geom = "errorbar", width=0.6) +
  geom_boxplot(width = 0.6, outlier.colour="black", outlier.shape=16,
               outlier.size=2) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title = "(e) Specificity") +
  xlab("Predictor set") +
  theme(plot.title = element_text(hjust = 0, size = 20), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.925, 0.18),
        legend.key.size = unit(0.8, "cm"), legend.box.background = element_rect(color="black", size=0.9), axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, color = "black"), legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        text = element_text(family = "Calibri")) +
  scale_fill_brewer(palette="BuPu")

ggsave("output_data/plots/validation/Specificity_algorithm_comparison.svg", width = 11, height = 7)
