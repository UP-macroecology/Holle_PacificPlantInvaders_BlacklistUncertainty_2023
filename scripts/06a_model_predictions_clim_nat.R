# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                06a. Model predictions based on native                  #
#                occurrences and purely climatic data                    #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(terra)
library(mgcv)
library(randomForest)
library(gbm)
library(dplyr)
library(tidyr)
library(stringr)
library(dismo)

# Load needed objects
load("input_data/occ_numbers_thinned_env_nat_filtered.RData") # Contains names of study species
load("input_data/spatial_data/islandgroups_clim.RData") # Contains names of island groups suitable for the analysis using purely climatic data
load("input_data/spatial_data/pacific_islands_extent.RData") # Contains the spatial extents of the island groups
Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable rasters
load("input_data/spatial_data/geoentities_plus_newname.RData") # Island group shapefiles
fiji_1 <- terra::vect("input_data/spatial_data/fiji_1.shp") # Modified shapefile of Fiji - first part
fiji_2 <- terra::vect("input_data/spatial_data/fiji_2.shp") # Modified shapefile of Fiji - second part



#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_nat_filtered$species))

# Retrieve island group names
islandgroup_climate <- as.character(islandgroups_clim$island_group)

# Change climate variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_'))

# Transform to terra SpatVect object
geoentities_SpatVect <- vect(geoentities, crs = "EPSG:4326")

# Create a data frame to store the results of area calculations for all species
islandgroups_results_clim_native <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(islandgroups_results_clim_native) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")


#-------------------------------------------------------------------------------

# 2. Model predictions ---------------------------------------------------------

for (sp in study_species) { # Start of the loop over all species
  
  # Load the needed objects for each species
  load(paste0("output_data/models/native/clim/models_clim_native_",sp,".RData")) # the four different models
  load(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData")) # validation outcomes
  load(paste0("output_data/variable_selection/native/clim/pred_sel_clim_native_",sp,".RData")) # predictor variables
  
  
  
  # Create a data frame to store the results for each species
  islandgroups_results_clim_native_spec <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(islandgroups_results_clim_native_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
  
  
  
  # (a) Prepare island group data for predictors -------------------------------
  
  for (i in islandgroup_climate) {
    
    # Get the extent of each island group
    single_islandgroup <- subset(pacific_islands_extent, pacific_islands_extent$island_group == i)
    single_islandgroup_extent <- c(single_islandgroup$xmin, single_islandgroup$xmax, single_islandgroup$ymin, single_islandgroup$ymax)
    
    # Crop the variables to the island group extent
    islandgroup_var <- crop(Chelsa, single_islandgroup_extent)
    
    # Create a mask of the Fiji Islands using the modified shapefiles in two parts
    if (i == "Fiji_1") { islandgroup_raster <- terra::rasterize(fiji_1, islandgroup_var)
                         islandgroup_mask <- mask(islandgroup_var, islandgroup_raster)
    } else if (i == "Fiji_2") { islandgroup_raster <- terra::rasterize(fiji_2, islandgroup_var)
                              islandgroup_mask <- mask(islandgroup_var, islandgroup_raster)
      
    } else { # Get the shapefile of the island group
             islandgroup_shape <- subset(geoentities_SpatVect, geoentities_SpatVect$new_archipelago_name == i)
      
             # Rasterize the island shape based on the cropped variable raster
             islandgroup_raster <- terra::rasterize(islandgroup_shape, islandgroup_var)
      
             # Create a mask of the island group
             islandgroup_mask <- mask(islandgroup_var, islandgroup_raster)
             
    }
      
    # Create a data frame with the coordinates and their environmental predictor values
    bio_curr_df_islandgroup <- data.frame(crds(islandgroup_mask[[pred_sel_clim_native]]),as.points(islandgroup_mask[[pred_sel_clim_native]]))
    
    
    
    # (b) Predictions of GLM ---------------------------------------------------
    
    # Make predictions
    env_preds_glm <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                glm = predict(m_glm_clim_native, bio_curr_df_islandgroup[, pred_sel_clim_native], type='response'))
    
    # Binarise predictions
    env_preds_glm_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                    glm = ifelse(env_preds_glm[,"glm"]>=comp_perf_clim_native[comp_perf_clim_native$alg=="glm",'thresh'],1,0))
    
    
    # Make a raster from binarised predictions
    r_env_preds_glm_bin <- terra::rast(env_preds_glm_bin, crs=crs(Chelsa$bio_1))
    
    
    
    # (c) Predictions of GAM ---------------------------------------------------
    

    
    
    
    
    
  }
  
  
}




