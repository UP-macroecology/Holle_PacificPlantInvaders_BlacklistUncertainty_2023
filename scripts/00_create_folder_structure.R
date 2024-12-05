# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                         00. Create folder structure                    #
# ---------------------------------------------------------------------- #


#-------------------------------------------------------------------------------


# A folder structure is created as we need it to read in data and store outputs
# for the analyses
folders <- c(
  "input_data/environmental_data/Chelsa_V",
  "input_data/environmental_data/SoilGrids_V2",
  "input_data/environmental_data/SoilGrids_V2_raw",
  "input_data/spatial_data",
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
  "output_data/model_predictions/native/clim",
  "output_data/model_predictions/native/clim_comp",
  "output_data/model_predictions/native/edaclim",
  "output_data/model_predictions/global/clim",
  "output_data/model_predictions/global/clim_comp",
  "output_data/model_predictions/global/edaclim",
  "output_data/blacklists/native/clim",
  "output_data/blacklists/native/clim_comp",
  "output_data/blacklists/native/edaclim",
  "output_data/blacklists/global/clim",
  "output_data/blacklists/global/clim_comp",
  "output_data/blacklists/global/edaclim",
  "output_data/uncertainty_quantification",
  "output_data/unrealized_col_pot/native/clim",
  "output_data/unrealized_col_pot/native/clim_comp",
  "output_data/unrealized_col_pot/native/edaclim",
  "output_data/unrealized_col_pot/global/clim",
  "output_data/unrealized_col_pot/global/clim_comp",
  "output_data/unrealized_col_pot/global/edaclim",
  "output_data/final_blacklisting/algorithm_blacklisting",
  "output_data/plots/presence_absence_plots",
  "output_data/plots/validation",
  "output_data/plots/response_plots",
  "output_data/plots/uncertainty_quantification",
  "output_data/plots/unrealized_col_pot",
  "output_data/plots/final_blacklisting",
  "output_data/plots/study_region",
  "scripts"
)

# Create each folder if it does not exist yet
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}