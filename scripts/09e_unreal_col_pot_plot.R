# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             09e. Unrealized colonization potential plot                #
# ---------------------------------------------------------------------- #


# Load needed packages
library(ggplot2)
library(showtext)


# Create a vector containing the different thresholding methods for binarising predictions
threshold_methods <- c("maxTSS", "meanProb", "tenthPer")


for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  
  
#-------------------------------------------------------------------------------
  
# 1. Boxplot of unrealized colonization potential ------------------------------
  
# (a) based on 25 island groups covered by climatic and edaphic data -----------
  
  # Needed objects (results of unrealized colonization potential based on all four predictor sets
  # and the algorithms)
  load(paste0("output_data/unrealized_col_pot_rev/native/clim_comp/",t,"/unreal_col_pot_clim_native_comp.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/native/edaclim/",t,"/unreal_col_pot_edaclim_native.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/clim_comp/",t,"/unreal_col_pot_clim_global_comp.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/edaclim/",t,"/unreal_col_pot_edaclim_global.RData"))
  
  
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
    theme_bw() +
    labs(y = "Predicted unrealised colonisation potential", x = "") +
    scale_x_discrete(breaks=c(1, 2, 3, 4),
                     labels=c("natclim", "natclim+eda", "globclim", "globclim+eda"))+
    scale_fill_manual(values = c("lightsteelblue3", "lightsteelblue3", "lightsteelblue3", "lightsteelblue3")) +
    theme(legend.position = "none", axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"), text = element_text(family = "Calibri"))
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/unreal_col_pot_25_",t,".svg"), width = 7, height = 7, unit = "cm")
  
  #scale_fill_manual(values = c("#6699CC", "#CC6677", "#27408B", "firebrick4")) +
  
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  # Needed objects (results of unrealized colonization potential based on the two predictor sets
  # including solely climatic data)
  load(paste0("output_data/unrealized_col_pot_rev/native/clim/",t,"/unreal_col_pot_clim_native.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/clim/",t,"/unreal_col_pot_clim_global.RData"))
  
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
    theme_bw() +
    labs(y = "Predicted unrealised colonisation potential", x = "") +
    #xlab("") +
    scale_x_discrete(breaks=c(1, 3),
                     labels=c("natclim", "globclim"))+
    scale_fill_manual(values = c("lightsteelblue3", "lightsteelblue3")) +
    theme(legend.position = "none", axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"), text = element_text(family = "Calibri")) +
    scale_y_continuous(limits = c(0,1))
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/unreal_col_pot_49_",t,".svg"), width = 6, height = 7, unit = "cm")
  
  
  
  
  
#-------------------------------------------------------------------------------
  
# 2. Density plots of suitable number of island groups (FP, TN, TP+FP) ---------
  
  # (a) based on 25 island groups covered by climatic and edaphic data -----------
  
  # Needed objects (results of FP, TN, and TP+FP based on all four predictor sets
  # and the algorithms)
  load(paste0("output_data/unrealized_col_pot_rev/native/clim_comp/",t,"/unreal_col_pot_clim_native_comp.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/native/edaclim/",t,"/unreal_col_pot_edaclim_native.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/clim_comp/",t,"/unreal_col_pot_clim_global_comp.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/edaclim/",t,"/unreal_col_pot_edaclim_global.RData"))
  
  # Adjust ylim of plots depending on the threshold
  if(t == "maxTSS") { ymax <- 0.125
  } else if (t == "meanProb") { ymax <- 0.2
  } else if (t == "tenthPer") { ymax <- 0.2
  }
  
  # 1. natclim
  # Solely retain values based on ensemble models
  unreal_col_pot_clim_native_comp_ens <- subset(unreal_col_pot_clim_native_comp, unreal_col_pot_clim_native_comp$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_clim_native_comp_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_clim_native_comp_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_clim_native_comp_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_clim_native_comp <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_clim_native_comp$value <- as.numeric(as.character(metric_comparison_clim_native_comp$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_clim_native_comp$metric <- factor(metric_comparison_clim_native_comp$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_clim_native_comp, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none") +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_clim_native_comp_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  
  
  # 2. natclim+eda
  # Solely retain values based on ensemble models
  unreal_col_pot_edaclim_native_ens <- subset(unreal_col_pot_edaclim_native, unreal_col_pot_edaclim_native$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_edaclim_native_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_edaclim_native_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_edaclim_native_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_edaclim_native <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_edaclim_native$value <- as.numeric(as.character(metric_comparison_edaclim_native$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_edaclim_native$metric <- factor(metric_comparison_edaclim_native$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_edaclim_native, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none") +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_edaclim_native_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  
  
  # 3. globclim
  # Solely retain values based on ensemble models
  unreal_col_pot_clim_global_comp_ens <- subset(unreal_col_pot_clim_global_comp, unreal_col_pot_clim_global_comp$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_clim_global_comp_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_clim_global_comp_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_clim_global_comp_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_clim_global_comp <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_clim_global_comp$value <- as.numeric(as.character(metric_comparison_clim_global_comp$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_clim_global_comp$metric <- factor(metric_comparison_clim_global_comp$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_clim_global_comp, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none") +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_clim_global_comp_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  # 4. globclim+eda
  # Solely retain values based on ensemble models
  unreal_col_pot_edaclim_global_ens <- subset(unreal_col_pot_edaclim_global, unreal_col_pot_edaclim_global$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_edaclim_global_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_edaclim_global_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_edaclim_global_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_edaclim_global <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_edaclim_global$value <- as.numeric(as.character(metric_comparison_edaclim_global$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_edaclim_global$metric <- factor(metric_comparison_edaclim_global$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_edaclim_global, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none", legend.text = element_text(size = 8)) +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_edaclim_global_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  
  
  
# (b) based on 49 island groups covered by climatic data -----------------------
  
  # Needed objects (results of TP and FP based on the two predictor sets
  # including solely climatic data)
  load(paste0("output_data/unrealized_col_pot_rev/native/clim/",t,"/unreal_col_pot_clim_native.RData"))
  load(paste0("output_data/unrealized_col_pot_rev/global/clim/",t,"/unreal_col_pot_clim_global.RData"))
  
  # Adjust ylim of plots depending on the threshold
  if(t == "maxTSS") { ymax <- 0.04
  } else if (t == "meanProb") { ymax <- 0.07
  } else if (t == "tenthPer") { ymax <- 0.05
  }
  
  # 1. natclim
  # Solely retain values based on ensemble models
  unreal_col_pot_clim_native_ens <- subset(unreal_col_pot_clim_native, unreal_col_pot_clim_native$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_clim_native_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_clim_native_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_clim_native_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_clim_native <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_clim_native$value <- as.numeric(as.character(metric_comparison_clim_native$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_clim_native$metric <- factor(metric_comparison_clim_native$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_clim_native, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    xlim(0, 49) +
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none") +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_clim_native_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  
  
  # 2. globclim
  # Solely retain values based on ensemble models
  unreal_col_pot_clim_global_ens <- subset(unreal_col_pot_clim_global, unreal_col_pot_clim_global$algorithm == "Ensemble")
  
  # Extract single columns of FP, TN, and TP_and_FP
  FP_column <- data.frame(unreal_col_pot_clim_global_ens[, "FP"])
  colnames(FP_column) <- "value"
  TN_column <- data.frame(unreal_col_pot_clim_global_ens[, "TN"])
  colnames(TN_column) <- "value"
  TP_and_FP_column <- data.frame(unreal_col_pot_clim_global_ens[, "TP_and_FP"])
  colnames(TP_and_FP_column) <- "value"
  
  # Add a column that indicates their metric
  FP_column$metric <- "FP"
  TN_column$metric <- "TN"
  TP_and_FP_column$metric <- "TP_and_FP"
  
  # Bind the three data frames
  metric_comparison_clim_global <- rbind(FP_column, TN_column, TP_and_FP_column)
  
  # Make sure the values are numeric
  metric_comparison_clim_global$value <- as.numeric(as.character(metric_comparison_clim_global$value))
  
  # Make sure the order of metrices is correct
  metric_comparison_clim_global$metric <- factor(metric_comparison_clim_global$metric, levels = c("FP", "TN", "TP_and_FP"))
  
  # Load the Calibri font
  font_add(family = "Calibri", regular = "Calibri.ttf")
  showtext_auto()
  
  # Plot density curve of all three metrics
  ggplot(metric_comparison_clim_global, aes(x = value, color = metric)) +
    geom_density(linewidth = 0.8) +
    labs(x = "Island groups", y = "Density") +
    theme_bw() +
    ylim(0, ymax) + 
    xlim(0, 49) +
    theme(text = element_text(family = "Calibri"), axis.title = element_blank(), axis.text = element_text(size = 8, color = "black"), 
          legend.position = "none") +
    scale_color_manual(labels = c("FP" = "False Positives", "TN" = "True Negatives", "TP_and_FP" = "True Positives + False Positives"),
                       values = c("#F8766D", "#619CFF", "black"),
                       guide = guide_legend(title = NULL))
  
  
  
  # Save the plot
  ggsave(paste0("output_data/plots/unrealized_col_pot_rev/metrices_clim_global_",t,".svg"), width = 4.45, height = 2.8, unit = "cm")
  
  
  
} # Close the loop over the different thresholding methods


#-------------------------------------------------------------------------------

# 3. Calculate component uncertainties -----------------------------------------

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



#-------------------------------------------------------------------------------

# 3. Descriptive statistics ----------------------------------------------------

# (a) based on 25 island groups covered by climatic and edaphic data -----------

# Extract average values of the different predictor sets and per niche
# natclim
unreal_col_pot_25_ens_natclim <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_set == 1)
mean(unreal_col_pot_25_ens_natclim$unrealized_col_pot) # 0.73
sd(unreal_col_pot_25_ens_natclim$unrealized_col_pot) # 0.31

# natclim+eda
unreal_col_pot_25_ens_natclimeda <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_set == 2)
mean(unreal_col_pot_25_ens_natclimeda$unrealized_col_pot) # 0.73
sd(unreal_col_pot_25_ens_natclimeda$unrealized_col_pot) # 0.33

# globclim
unreal_col_pot_25_ens_globclim <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_set == 3)
mean(unreal_col_pot_25_ens_globclim$unrealized_col_pot) # 0.8
sd(unreal_col_pot_25_ens_globclim$unrealized_col_pot) # 0.29

# globclim+eda
unreal_col_pot_25_ens_globclimeda <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_set == 4)
mean(unreal_col_pot_25_ens_globclimeda$unrealized_col_pot) # 0.77
sd(unreal_col_pot_25_ens_globclimeda$unrealized_col_pot) # 0.34

# native
unreal_col_pot_25_ens_native <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$niche == "native")
mean(unreal_col_pot_25_ens_native$unrealized_col_pot) # 0.73
sd(unreal_col_pot_25_ens_native$unrealized_col_pot) # 0.32

# global
unreal_col_pot_25_ens_global <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$niche == "global")
mean(unreal_col_pot_25_ens_global$unrealized_col_pot) # 0.79
sd(unreal_col_pot_25_ens_global$unrealized_col_pot) # 0.32

# climatic
unreal_col_pot_25_ens_clim <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_type == "clim")
mean(unreal_col_pot_25_ens_clim$unrealized_col_pot) # 0.77
sd(unreal_col_pot_25_ens_clim$unrealized_col_pot) # 0.3

# climatic + edaphic 
unreal_col_pot_25_ens_edaclim <- subset(unreal_col_pot_25_ens, unreal_col_pot_25_ens$predictor_type == "edaclim")
mean(unreal_col_pot_25_ens_edaclim$unrealized_col_pot) # 0.75
sd(unreal_col_pot_25_ens_edaclim$unrealized_col_pot) # 0.34


# all
mean(unreal_col_pot_25_ens$unrealized_col_pot) # 0.76
sd(unreal_col_pot_25_ens$unrealized_col_pot) # 0.32
min(unreal_col_pot_25_ens$unrealized_col_pot) # 0
max(unreal_col_pot_25_ens$unrealized_col_pot) # 1



# (b) based on 49 island groups covered by climatic data -----------------------

# Extract average values of the different predictor sets and per niche
# natclim
unreal_col_pot_49_ens_natclim <- subset(unreal_col_pot_49_ens, unreal_col_pot_49_ens$predictor_set == 1)
mean(unreal_col_pot_49_ens_natclim$unrealized_col_pot) # 0.68
sd(unreal_col_pot_49_ens_natclim$unrealized_col_pot) # 0.33

# globclim
unreal_col_pot_49_ens_globclim <- subset(unreal_col_pot_49_ens, unreal_col_pot_49_ens$predictor_set == 3)
mean(unreal_col_pot_49_ens_globclim$unrealized_col_pot) # 0.75
sd(unreal_col_pot_49_ens_globclim$unrealized_col_pot) # 0.32

# all
mean(unreal_col_pot_49_ens$unrealized_col_pot) # 0.71
sd(unreal_col_pot_49_ens$unrealized_col_pot) # 0.32






