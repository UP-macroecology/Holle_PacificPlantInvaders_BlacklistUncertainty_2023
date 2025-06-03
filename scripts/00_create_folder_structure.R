# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                         00. Create folder structure                    #
# ---------------------------------------------------------------------- #


#-------------------------------------------------------------------------------


# A folder structure is created as we need it to read in data and store outputs
# for the analyses (Define full folder structure)
folders <- c(
  # Input
  "input_data/environmental_data/Chelsa_V2",
  "input_data/environmental_data/SoilGrids_V2_raw",
  "input_data/environmental_data/SoilGrids_V2",
  
  "input_data/spatial_data",
  
  # Output
  "output_data/presences_thinned",
  
  "output_data/absences_thinned/native",
  "output_data/absences_thinned/global",
  
  "output_data/distribution_data/native",
  "output_data/distribution_data/global",
  
  "output_data/distribution_env_data/native/clim",
  "output_data/distribution_env_data/native/edaclim",
  "output_data/distribution_env_data/global/clim",
  "output_data/distribution_env_data/global/edaclim",
  
  "output_data/distribution_env_data_subset/native/clim",
  "output_data/distribution_env_data_subset/native/edaclim",
  "output_data/distribution_env_data_subset/global/clim",
  "output_data/distribution_env_data_subset/global/edaclim",
  
  "output_data/variable_selection/native/clim",
  "output_data/variable_selection/native/edaclim",
  "output_data/variable_selection/global/clim",
  "output_data/variable_selection/global/edaclim",
  
  "output_data/models/native/clim",
  "output_data/models/native/edaclim",
  "output_data/models/global/clim",
  "output_data/models/global/edaclim",
  
  "output_data/validation/native/clim",
  "output_data/validation/native/edaclim",
  "output_data/validation/global/clim",
  "output_data/validation/global/edaclim",
  
  "output_data/model_predictions/native/clim/maxTSS",
  "output_data/model_predictions/native/clim/meanProb",
  "output_data/model_predictions/native/clim/tenthPer",
  "output_data/model_predictions/native/clim_comp/maxTSS",
  "output_data/model_predictions/native/clim_comp/meanProb",
  "output_data/model_predictions/native/clim_comp/tenthPer",
  "output_data/model_predictions/native/edaclim/maxTSS",
  "output_data/model_predictions/native/edaclim/meanProb",
  "output_data/model_predictions/native/edaclim/tenthPer",
  "output_data/model_predictions/global/clim/maxTSS",
  "output_data/model_predictions/global/clim/meanProb",
  "output_data/model_predictions/global/clim/tenthPer",
  "output_data/model_predictions/global/clim_comp/maxTSS",
  "output_data/model_predictions/global/clim_comp/meanProb",
  "output_data/model_predictions/global/clim_comp/tenthPer",
  "output_data/model_predictions/global/edaclim/maxTSS",
  "output_data/model_predictions/global/edaclim/meanProb",
  "output_data/model_predictions/global/edaclim/tenthPer",
  
  "output_data/blacklists/native/clim/maxTSS",
  "output_data/blacklists/native/clim/meanProb",
  "output_data/blacklists/native/clim/tenthPer",
  "output_data/blacklists/native/clim_comp/maxTSS",
  "output_data/blacklists/native/clim_comp/meanProb",
  "output_data/blacklists/native/clim_comp/tenthPer",
  "output_data/blacklists/native/edaclim/maxTSS",
  "output_data/blacklists/native/edaclim/meanProb",
  "output_data/blacklists/native/edaclim/tenthPer",
  "output_data/blacklists/global/clim/maxTSS",
  "output_data/blacklists/global/clim/meanProb",
  "output_data/blacklists/global/clim/tenthPer",
  "output_data/blacklists/global/clim_comp/maxTSS",
  "output_data/blacklists/global/clim_comp/meanProb",
  "output_data/blacklists/global/clim_comp/tenthPer",
  "output_data/blacklists/global/edaclim/maxTSS",
  "output_data/blacklists/global/edaclim/meanProb",
  "output_data/blacklists/global/edaclim/tenthPer",
  
  "output_data/uncertainty_quantification",
  
  "output_data/unrealized_col_pot/native/clim/maxTSS",
  "output_data/unrealized_col_pot/native/clim/meanProb",
  "output_data/unrealized_col_pot/native/clim/tenthPer",
  "output_data/unrealized_col_pot/native/clim_comp/maxTSS",
  "output_data/unrealized_col_pot/native/clim_comp/meanProb",
  "output_data/unrealized_col_pot/native/clim_comp/tenthPer",
  "output_data/unrealized_col_pot/native/edaclim/maxTSS",
  "output_data/unrealized_col_pot/native/edaclim/meanProb",
  "output_data/unrealized_col_pot/native/edaclim/tenthPer",
  "output_data/unrealized_col_pot/global/clim/maxTSS",
  "output_data/unrealized_col_pot/global/clim/meanProb",
  "output_data/unrealized_col_pot/global/clim/tenthPer",
  "output_data/unrealized_col_pot/global/clim_comp/maxTSS",
  "output_data/unrealized_col_pot/global/clim_comp/meanProb",
  "output_data/unrealized_col_pot/global/clim_comp/tenthPer",
  "output_data/unrealized_col_pot/global/edaclim/maxTSS",
  "output_data/unrealized_col_pot/global/edaclim/meanProb",
  "output_data/unrealized_col_pot/global/edaclim/tenthPer",
  
  "output_data/final_blacklisting",
  
  "output_data/plots/presence_absence_plots",
  "output_data/plots/validation",
  "output_data/plots/uncertainty_quantification",
  "output_data/plots/unrealized_col_pot",
  "output_data/plots/final_blacklisting",
  "output_data/plots/study_region"
)

# Create directories if not present
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message("Created: ", folder)
  }
}

