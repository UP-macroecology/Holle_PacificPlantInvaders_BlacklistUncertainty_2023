# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                    10b. Final blacklisting plots                       #
# ---------------------------------------------------------------------- #


# Load needed packages
library(ggplot2)
library(dplyr)
library(showtext)
library(ggtext)

# Load data frame that contains names of study species
load("input_data/occ_numbers_thinned_env_filtered.RData") 

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species))


# Create a vector containing the different thresholding methods for binarising predictions
threshold_methods <- c("maxTSS", "meanProb", "tenthPer")



  
  
#-------------------------------------------------------------------------------
  
# 1. Prepare data for final blacklisting plot for each studied thresholding method
# based on the ensemble predictions including 25 Pacific island groups covered by 
# climatic and edaphic data ----------------------------------------------------
  
  
for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  
# (a) based on native occurrences and purely climatic data ---------------------
  
  print("native; climate")
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_native_clim_comp_",t,".RData"))
  
  # Assign ranking groups based on the ranks
  blacklists_final_native_clim_comp <-  blacklists_final_native_clim_comp %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_native_clim_comp$blacklist <- as.factor(blacklists_final_native_clim_comp$blacklist)
  blacklists_final_native_clim_comp$species <- as.factor(blacklists_final_native_clim_comp$species)
  blacklists_final_native_clim_comp$rank_group <- as.factor(blacklists_final_native_clim_comp$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_native_clim_comp$blacklist <- factor(blacklists_final_native_clim_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_native_clim_comp$predictor_set <- "Species data: native, Predictor type: climate"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_native_clim_comp$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/native/clim/validation_clim_native_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_clim_native[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim_comp$validation[blacklists_final_native_clim_comp$species == sp & blacklists_final_native_clim_comp$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species 
  

  
  
  
  
# (b) based on native occurrences and climatic-edaphic data --------------------
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_native_edaclim_",t,".RData"))
  
  print("native; climate + edaphic")
  
  # Assign ranking groups based on the ranks
  blacklists_final_native_edaclim <-  blacklists_final_native_edaclim %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_native_edaclim$blacklist <- as.factor(blacklists_final_native_edaclim$blacklist)
  blacklists_final_native_edaclim$species <- as.factor(blacklists_final_native_edaclim$species)
  blacklists_final_native_edaclim$rank_group <- as.factor(blacklists_final_native_edaclim$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_native_edaclim$blacklist <- factor(blacklists_final_native_edaclim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_native_edaclim$predictor_set <- "Species data: native, Predictor type: climate + edaphic"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_native_edaclim$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/native/edaclim/validation_edaclim_native_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_edaclim_native[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_edaclim$validation[blacklists_final_native_edaclim$species == sp & blacklists_final_native_edaclim$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species 
  

  
  
  
  
# (c) based on global occurrences and purely climatic data ---------------------
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_global_clim_comp_",t,".RData"))
  
  print("global; climate")
  
  # Assign ranking groups based on the ranks
  blacklists_final_global_clim_comp <-  blacklists_final_global_clim_comp %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_global_clim_comp$blacklist <- as.factor(blacklists_final_global_clim_comp$blacklist)
  blacklists_final_global_clim_comp$species <- as.factor(blacklists_final_global_clim_comp$species)
  blacklists_final_global_clim_comp$rank_group <- as.factor(blacklists_final_global_clim_comp$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_global_clim_comp$blacklist <- factor(blacklists_final_global_clim_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_global_clim_comp$predictor_set <- "Species data: global, Predictor type: climate"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_global_clim_comp$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/global/clim/validation_clim_global_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_clim_global[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_clim_comp$validation[blacklists_final_global_clim_comp$species == sp & blacklists_final_global_clim_comp$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species 
  
  
  

# (d) based on global occurrences and climatic-edaphic data --------------------
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_global_edaclim_",t,".RData"))
  
  print("global; climate + edaphic")
  
  # Assign ranking groups based on the ranks
  blacklists_final_global_edaclim <-  blacklists_final_global_edaclim %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_global_edaclim$blacklist <- as.factor(blacklists_final_global_edaclim$blacklist)
  blacklists_final_global_edaclim$species <- as.factor(blacklists_final_global_edaclim$species)
  blacklists_final_global_edaclim$rank_group <- as.factor(blacklists_final_global_edaclim$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_global_edaclim$blacklist <- factor(blacklists_final_global_edaclim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_global_edaclim$predictor_set <- "Species data: global, Predictor type: climate + edaphic"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_global_edaclim$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/global/edaclim/validation_edaclim_global_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_edaclim_global[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_edaclim$validation[blacklists_final_global_edaclim$species == sp & blacklists_final_global_edaclim$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species 
  
  
  if (t == "maxTSS") { blacklists_final_maxTSS_comp <- rbind(blacklists_final_native_clim_comp, blacklists_final_native_edaclim,
                                                             blacklists_final_global_clim_comp, blacklists_final_global_edaclim)
  } else if (t == "meanProb") { blacklists_final_meanProb_comp <- rbind(blacklists_final_native_clim_comp, blacklists_final_native_edaclim,
                                                                        blacklists_final_global_clim_comp, blacklists_final_global_edaclim)
  } else if (t == "tenthPer") { blacklists_final_tenthPer_comp <- rbind(blacklists_final_native_clim_comp, blacklists_final_native_edaclim,
                                                                        blacklists_final_global_clim_comp, blacklists_final_global_edaclim)
  }

  

  
} # Close the loop over the three different thresholding methods




#-------------------------------------------------------------------------------

# 2.Visualise final ensemble blacklists ----------------------------------------
# Based on the three studied thresholds

# (a) Create figure based on maxTSS threshold for main manuscript --------------

# Make sure predictor sets occurr in the correct order
blacklists_final_maxTSS_comp$predictor_set <- factor(blacklists_final_maxTSS_comp$predictor_set, 
                                                     levels = c("Species data: global, Predictor type: climate", 
                                                                "Species data: global, Predictor type: climate + edaphic",
                                                                "Species data: native, Predictor type: climate", 
                                                                "Species data: native, Predictor type: climate + edaphic"))

# Make sure the ranking groups occurr in the correct order in the legend
blacklists_final_maxTSS_comp$rank_group <- factor(blacklists_final_maxTSS_comp$rank_group, 
                                                 levels = c("Top 10", "Top 20", "Top 30", "Remaining"))

# Make sure that the first appearing blacklist and predictor set determines the
# fixed species order per plot
load("output_data/final_blacklisting_rev/blacklists_final_global_clim_comp_maxTSS.RData")
blacklist_df <- blacklists_final_global_clim_comp[blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction", ]
species_order <- blacklist_df$species[order(blacklist_df$rank)]

# Make sure the species occurr in the correct order
blacklists_final_maxTSS_comp$species <- factor(blacklists_final_maxTSS_comp$species, 
                                               levels = species_order)
# Make sure the blacklists appear in the correct order
blacklists_final_maxTSS_comp$blacklist <- factor(blacklists_final_maxTSS_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Visualise data
ggplot(blacklists_final_maxTSS_comp, aes(x = species, y = blacklist, fill = rank_group)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("Top 10" = "brown3", 
                               "Top 20" = "goldenrod3", 
                               "Top 30" = "steelblue", 
                               "Remaining" = "seagreen"), 
                    name = "Ranking group") +
  theme_bw() +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks=c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels=c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(axis.text = element_text(color = "black", size = 10), 
        legend.key.size = unit(0.8, "lines"), 
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.key.width = unit(0.6, "cm"), 
        text = element_text(family = "Calibri"), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 11, face = "bold", color = "black"),
        strip.background = element_rect(fill = "lightsteelblue3", color = "black")) +
  guides(fill = "legend") +
  geom_text(label = blacklists_final_maxTSS_comp$validation) +
  facet_wrap(~predictor_set, nrow = 4)

# Save the plot
ggsave("output_data/plots/final_blacklisting_rev/blacklists_final_maxTSS_comp.svg", width = 19, height = 12, unit = "cm")





# (b) Create one figure including blacklisting results based on all three
# thresholding methods ---------------------------------------------------------

# Add another column to the three data frames indicating their thresholding method
blacklists_final_maxTSS_comp$thresh_method <- "maxTSS"
blacklists_final_meanProb_comp$thresh_method <- "meanProb"
blacklists_final_tenthPer_comp$thresh_method <- "tenthPer"

# Bind the data frames containing results based on all three thresholding methods
blacklists_final_all_comp <- rbind(blacklists_final_maxTSS_comp,
                                   blacklists_final_meanProb_comp,
                                   blacklists_final_tenthPer_comp)


# Make sure predictor sets occurr in the correct order
blacklists_final_all_comp$predictor_set <- factor(blacklists_final_all_comp$predictor_set, 
                                                  levels = c("Species data: global, Predictor type: climate", 
                                                             "Species data: global, Predictor type: climate + edaphic",
                                                             "Species data: native, Predictor type: climate", 
                                                             "Species data: native, Predictor type: climate + edaphic"))


# Make sure the ranking groups occurr in the correct order in the legend
blacklists_final_all_comp$rank_group <- factor(blacklists_final_all_comp$rank_group, 
                                               levels = c("Top 10", "Top 20", "Top 30", "Remaining"))

# Make sure the thresholding methods appear in the correct order
blacklists_final_all_comp$thresh_method <- factor(blacklists_final_all_comp$thresh_method, 
                                                  levels = c("maxTSS", "meanProb", "tenthPer"))

# Make sure that the first appearing blacklist and predictor set determines the
# fixed species order per plot
load("output_data/final_blacklisting_rev/blacklists_final_global_clim_comp_maxTSS.RData")
blacklist_df <- blacklists_final_global_clim_comp[blacklists_final_global_clim_comp$blacklist == "total_suitable_fraction", ]
species_order <- blacklist_df$species[order(blacklist_df$rank)]

# Make sure the species occurr in the correct order
blacklists_final_all_comp$species <- factor(blacklists_final_all_comp$species, 
                                            levels = species_order)
# Make sure the blacklists appear in the correct order
blacklists_final_all_comp$blacklist <- factor(blacklists_final_all_comp$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Create facet labels
blacklists_final_all_comp$facet_label <- paste0(
  "<b>", blacklists_final_all_comp$predictor_set, "</b><br><span style='font-size:9pt;'>", 
  blacklists_final_all_comp$thresh_method, "</span>"
)

# Make sure the labels occurr in the correct order
blacklists_final_all_comp$facet_label <- factor(
  blacklists_final_all_comp$facet_label,
  levels = c(
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>maxTSS</span>",
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>meanProb</span>",
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>tenthPer</span>", 
    "<b>Species data: global, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>maxTSS</span>",
    "<b>Species data: global, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>meanProb</span>",
    "<b>Species data: global, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>tenthPer</span>",
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>maxTSS</span>",
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>meanProb</span>", 
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>tenthPer</span>",
    "<b>Species data: native, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>maxTSS</span>", 
    "<b>Species data: native, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>meanProb</span>",
    "<b>Species data: native, Predictor type: climate + edaphic</b><br><span style='font-size:9pt;'>tenthPer</span>"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_all_comp, aes(x = species, y = blacklist, fill = rank_group)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("Top 10" = "brown3", 
                               "Top 20" = "goldenrod3", 
                               "Top 30" = "steelblue", 
                               "Remaining" = "seagreen"), 
                    name = "Ranking group") +
  theme_bw() +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels = c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(
    axis.text = element_text(color = "black", size = 10), 
    legend.key.size = unit(0.8, "lines"), 
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.key.width = unit(0.6, "cm"), 
    text = element_text(family = "Calibri"), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_markdown(size = 10, face = "plain", color = "black")) +
  guides(fill = "legend") +
  geom_text(label = blacklists_final_all_comp$validation) +
  facet_wrap(~ facet_label, ncol = 1)

# Save the plot
ggsave("output_data/plots/final_blacklisting_rev/blacklists_final_allthresh_comp.svg", width = 20, height = 24, unit = "cm")






#-------------------------------------------------------------------------------

# 3. Prepare data for final blacklisting plot for each studied thresholding method
# based on the ensemble predictions including 49 Pacific island groups solely 
# covered by climatic data -----------------------------------------------------


for (t in threshold_methods) { # Start of the loop over the three different thresholding methods
  
  print(t)
  
  
# (a) based on native occurrences and purely climatic data ---------------------
  
  print("native; climate")
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_native_clim_",t,".RData"))
  
  # Assign ranking groups based on the ranks
  blacklists_final_native_clim <-  blacklists_final_native_clim %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_native_clim$blacklist <- as.factor(blacklists_final_native_clim$blacklist)
  blacklists_final_native_clim$species <- as.factor(blacklists_final_native_clim$species)
  blacklists_final_native_clim$rank_group <- as.factor(blacklists_final_native_clim$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_native_clim$blacklist <- factor(blacklists_final_native_clim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_native_clim$predictor_set <- "Species data: native, Predictor type: climate"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_native_clim$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/native/clim/validation_clim_native_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_clim_native[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_native_clim$validation[blacklists_final_native_clim$species == sp & blacklists_final_native_clim$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species 
  
  

# (b) based on global occurrences and purely climatic data ---------------------
  
  # Load needed data
  load(paste0("output_data/final_blacklisting_rev/blacklists_final_global_clim_",t,".RData"))
  
  print("global; climate")
  
  # Assign ranking groups based on the ranks
  blacklists_final_global_clim <-  blacklists_final_global_clim %>%
    mutate(rank_group = case_when(
      rank >= 1 & rank <= 10 ~ "Top 10",
      rank >= 11 & rank <= 20 ~ "Top 20",
      rank >= 21 & rank <= 30 ~ "Top 30",
      rank >= 31 & rank <= 82 ~ "Remaining"))
  
  # Make sure blacklists and species are factors
  blacklists_final_global_clim$blacklist <- as.factor(blacklists_final_global_clim$blacklist)
  blacklists_final_global_clim$species <- as.factor(blacklists_final_global_clim$species)
  blacklists_final_global_clim$rank_group <- as.factor(blacklists_final_global_clim$rank_group)
  
  # Fix the order of blacklists
  blacklists_final_global_clim$blacklist <- factor(blacklists_final_global_clim$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))
  
  # Add a column indicating the used predictor set
  blacklists_final_global_clim$predictor_set <- "Species data: global, Predictor type: climate"
  
  # Add an asterisk for species that have ensemble validation values of Boyce < 0.7
  blacklists_final_global_clim$validation <- NA # Create a new column
  
  for (sp in study_species) { # Start the loop over all species
    
    # Load in the ensemble validation outcomes
    load(paste0("output_data/validation_rev/global/clim/validation_clim_global_",sp,".RData"))
    
    # Check the Boyce value
    check_Boyce <- ensemble_perf_clim_global[t,"Boyce"]
    
    # Boyce values above and below 0.7 are noted
    if (check_Boyce > 0.7) { validation <- ""
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "number_suitable"] <- validation
    
    } else { validation <- "*"
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "total_suitable_fraction"] <- validation
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "mean_suitable_fraction"] <- validation
    blacklists_final_global_clim$validation[blacklists_final_global_clim$species == sp & blacklists_final_global_clim$blacklist == "number_suitable"] <- validation
    }
    
  } # Close the loop over all species
  
  
  if (t == "maxTSS") { blacklists_final_maxTSS <- rbind(blacklists_final_native_clim,
                                                        blacklists_final_global_clim)
  } else if (t == "meanProb") { blacklists_final_meanProb <- rbind(blacklists_final_native_clim,
                                                                   blacklists_final_global_clim)
  } else if (t == "tenthPer") { blacklists_final_tenthPer <- rbind(blacklists_final_native_clim,
                                                                   blacklists_final_global_clim)
  }

  
} # Close the loop over the three different thresholding methods



#-------------------------------------------------------------------------------

# 4.Visualise final ensemble blacklists ----------------------------------------
# Based on the three studied thresholds
  

# (a) Create one figure including blacklisting results based on all three
# thresholding methods ---------------------------------------------------------

# Add another column to the three data frames indicating their thresholding method
blacklists_final_maxTSS$thresh_method <- "maxTSS"
blacklists_final_meanProb$thresh_method <- "meanProb"
blacklists_final_tenthPer$thresh_method <- "tenthPer"

# Bind the data frames containing results based on all three thresholding methods
blacklists_final_all <- rbind(blacklists_final_maxTSS,
                              blacklists_final_meanProb,
                              blacklists_final_tenthPer)


# Make sure predictor sets occurr in the correct order
blacklists_final_all$predictor_set <- factor(blacklists_final_all$predictor_set, 
                                             levels = c("Species data: global, Predictor type: climate", 
                                                        "Species data: native, Predictor type: climate"))


# Make sure the ranking groups occurr in the correct order in the legend
blacklists_final_all$rank_group <- factor(blacklists_final_all$rank_group, 
                                          levels = c("Top 10", "Top 20", "Top 30", "Remaining"))

# Make sure the thresholding methods appear in the correct order
blacklists_final_all$thresh_method <- factor(blacklists_final_all$thresh_method, 
                                             levels = c("maxTSS", "meanProb", "tenthPer"))

# Make sure that the first appearing blacklist and predictor set determines the
# fixed species order per plot
load("output_data/final_blacklisting_rev/blacklists_final_global_clim_maxTSS.RData")
blacklist_df <- blacklists_final_global_clim[blacklists_final_global_clim$blacklist == "total_suitable_fraction", ]
species_order <- blacklist_df$species[order(blacklist_df$rank)]

# Make sure the species occurr in the correct order
blacklists_final_all$species <- factor(blacklists_final_all$species, 
                                       levels = species_order)
# Make sure the blacklists appear in the correct order
blacklists_final_all$blacklist <- factor(blacklists_final_all$blacklist, levels = c("number_suitable", "mean_suitable_fraction", "total_suitable_fraction"))

# Create facet labels
blacklists_final_all$facet_label <- paste0(
  "<b>", blacklists_final_all$predictor_set, "</b><br><span style='font-size:9pt;'>", 
  blacklists_final_all$thresh_method, "</span>"
)

# Make sure the labels occurr in the correct order
blacklists_final_all$facet_label <- factor(
  blacklists_final_all$facet_label,
  levels = c(
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>maxTSS</span>",
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>meanProb</span>",
    "<b>Species data: global, Predictor type: climate</b><br><span style='font-size:9pt;'>tenthPer</span>", 
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>maxTSS</span>",
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>meanProb</span>", 
    "<b>Species data: native, Predictor type: climate</b><br><span style='font-size:9pt;'>tenthPer</span>"))

# Load the Calibri font
font_add(family = "Calibri", regular = "Calibri.ttf")
showtext_auto()

# Plot
ggplot(blacklists_final_all, aes(x = species, y = blacklist, fill = rank_group)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("Top 10" = "brown3", 
                               "Top 20" = "goldenrod3", 
                               "Top 30" = "steelblue", 
                               "Remaining" = "seagreen"), 
                    name = "Ranking group") +
  theme_bw() +
  coord_fixed(ratio = 1.5) +
  labs(x = "Non-native species") +
  scale_y_discrete(breaks = c("total_suitable_fraction", "mean_suitable_fraction", "number_suitable"),
                   labels = c("Total fraction", "Mean fraction", "Island groups")) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30", "35", "40", 
                              "45", "50", "55", "60", "65", "70", "75", "80")) +
  theme(
    axis.text = element_text(color = "black", size = 10), 
    legend.key.size = unit(0.8, "lines"), 
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.key.width = unit(0.6, "cm"), 
    text = element_text(family = "Calibri"), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_markdown(size = 10, face = "plain", color = "black")  # Renders HTML
  ) +
  guides(fill = "legend") +
  geom_text(label = blacklists_final_all$validation) +
  facet_wrap(~ facet_label, ncol = 1)

# Save the plot
ggsave("output_data/plots/final_blacklisting_rev/blacklists_final_allthresh.svg", width = 19, height = 18, unit = "cm")
