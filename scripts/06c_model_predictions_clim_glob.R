# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                06c. Model predictions based on global                  #
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
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
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
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species))

# Retrieve island group names (change Fiji Islands for the two divided parts)
islandgroup_climate <- islandgroups_clim[islandgroups_clim$island_group != "Fiji",]
islandgroup_climate <- as.character(islandgroup_climate$island_group)
islandgroup_climate <- c(islandgroup_climate, "Fiji_1", "Fiji_2")

# Change climate variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_'))

# Transform to terra SpatVect object
geoentities_SpatVect <- vect(geoentities, crs = "EPSG:4326")

# Give a coordinate reference system to Fiji shapefiles
crs(fiji_1) <- "EPSG:4326"
crs(fiji_2) <- "EPSG:4326"


#-------------------------------------------------------------------------------

# 2. Model predictions ---------------------------------------------------------

for (sp in study_species) { # Start of the loop over all species
  try({
    
    print(sp)
    
    print("clim")
    
    # Check if prediction results already exist
    file_exists <- file.exists(paste0("output_data/model_predictions/global/clim/islandgroups_results_clim_global_spec_",sp,".RData"))
    
    if (file_exists == FALSE) { # just continue with model predictions if output 
      # with prediction results does not exist yet
      
      print("start of model predictions")
      
      # Load the needed objects for each species
      load(paste0("output_data/models/global/clim/models_clim_global_",sp,".RData")) # the four different models
      load(paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData")) # validation outcomes
      load(paste0("output_data/variable_selection/global/clim/pred_sel_clim_global_",sp,".RData")) # predictor variables
      
      
      
      # Create a data frame to store the results for each species
      islandgroups_results_clim_global_spec <- data.frame(matrix(ncol = 8, nrow = 0))
      colnames(islandgroups_results_clim_global_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
      
      
      
      # (a) Prepare island group data for predictors ---------------------------
      
      for (i in islandgroup_climate) {
        try({
          
          print(i)
          
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
          bio_curr_df_islandgroup <- data.frame(crds(islandgroup_mask[[pred_sel_clim_global]]),as.points(islandgroup_mask[[pred_sel_clim_global]]))
          
          
          
          # (b) Predictions of GLM ---------------------------------------------
          
          # Make predictions
          env_preds_glm <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                      glm = predict(m_glm_clim_global, bio_curr_df_islandgroup[, pred_sel_clim_global], type='response'))
          
          # Binarise predictions
          env_preds_glm_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                          glm = ifelse(env_preds_glm[,"glm"]>=comp_perf_clim_global[comp_perf_clim_global$alg=="glm",'thresh'],1,0))
          
          
          # Make a raster from binarised predictions - For the smallest island group Hunter and Matthew
          # an alternative approach needs to be taken as it only consists of two data points (wrong resolution is estimated)
          if (i == "Hunter and Matthew") {
            extent_preds_glm <- c(min(env_preds_glm_bin$x), max(env_preds_glm_bin$x), min(env_preds_glm_bin$y), max(env_preds_glm_bin$y))
            r_env_preds_glm_bin_temp <- terra::rast(ext = extent_preds_glm, res = res(Chelsa$bio_1), crs = crs(Chelsa$bio_1))
            vec_preds_glm <- vect(env_preds_glm_bin, geom = c("x", "y"))
            r_env_preds_glm_bin <- rasterize(vec_preds_glm, r_env_preds_glm_bin_temp, field = "glm")
            
          } else { r_env_preds_glm_bin <- terra::rast(env_preds_glm_bin, crs=crs(Chelsa$bio_1))
          }
          
          
          
          # (c) Predictions of GAM ---------------------------------------------
          
          # Make predictions
          env_preds_gam <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                      gam = predict(m_gam_clim_global, bio_curr_df_islandgroup[, pred_sel_clim_global], type='response'))
          
          # Binarise predictions
          env_preds_gam_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                          gam = ifelse(env_preds_gam[,"gam"]>=comp_perf_clim_global[comp_perf_clim_global$alg=="gam",'thresh'],1,0))
          
          # Make a raster from binarised predictions - For the smallest island group Hunter and Matthew
          # an alternative approach needs to be taken as it only consists of two data points (wrong resolution is estimated)
          if (i == "Hunter and Matthew") {
            extent_preds_gam <- c(min(env_preds_gam_bin$x), max(env_preds_gam_bin$x), min(env_preds_gam_bin$y), max(env_preds_gam_bin$y))
            r_env_preds_gam_bin_temp <- terra::rast(ext = extent_preds_gam, res = res(Chelsa$bio_1), crs = crs(Chelsa$bio_1))
            vec_preds_gam <- vect(env_preds_gam_bin, geom = c("x", "y"))
            r_env_preds_gam_bin <- rasterize(vec_preds_gam, r_env_preds_gam_bin_temp, field = "gam")
            
          } else { r_env_preds_gam_bin <- terra::rast(env_preds_gam_bin, crs=crs(Chelsa$bio_1))
          }
          
          
          
          # (d) Predictions of RF ----------------------------------------------
          
          # Make predictions of all 10 RF models
          env_preds_rf_all <- data.frame(bio_curr_df_islandgroup[, 1:2], 
                                         lapply(1:10, FUN=function(m) {rf = predict(m_rf_clim_global[[m]], bio_curr_df_islandgroup[, pred_sel_clim_global], type='response')}))
          
          # Average the predictions into one RF prediction
          env_preds_rf <- data.frame(bio_curr_df_islandgroup[,1:2], rf = rowMeans(env_preds_rf_all[,-c(1:2)]))
          
          # Binarise predictions
          env_preds_rf_bin <- data.frame(bio_curr_df_islandgroup[,1:2],
                                         rf = ifelse(env_preds_rf[,"rf"]>=comp_perf_clim_global[comp_perf_clim_global$alg=="rf",'thresh'],1,0))
          
          # Make a raster from binarised predictions - For the smallest island group Hunter and Matthew
          # an alternative approach needs to be taken as it only consists of two data points (wrong resolution is estimated)
          if (i == "Hunter and Matthew") {
            extent_preds_rf <- c(min(env_preds_rf_bin$x), max(env_preds_rf_bin$x), min(env_preds_rf_bin$y), max(env_preds_rf_bin$y))
            r_env_preds_rf_bin_temp <- terra::rast(ext = extent_preds_rf, res = res(Chelsa$bio_1), crs = crs(Chelsa$bio_1))
            vec_preds_rf <- vect(env_preds_rf_bin, geom = c("x", "y"))
            r_env_preds_rf_bin <- rasterize(vec_preds_rf, r_env_preds_rf_bin_temp, field = "rf")
            
          } else { r_env_preds_rf_bin <- terra::rast(env_preds_rf_bin, crs=crs(Chelsa$bio_1))
          }
          
          
          
          # (e) Predictions of BRT ---------------------------------------------
          
          # Make predictions of all 10 BRT models
          env_preds_brt_all <- data.frame(bio_curr_df_islandgroup[, 1:2],
                                          lapply(1:10, FUN=function(m) {brt = predict.gbm(m_brt_clim_global[[m]], bio_curr_df_islandgroup[, pred_sel_clim_global],
                                                                                          n.trees=m_brt_clim_global$gbm.call$best.trees, type='response')}))
          
          
          # Average the predictions into one RF prediction
          env_preds_brt <- data.frame(bio_curr_df_islandgroup[,1:2], brt = rowMeans(env_preds_brt_all[,-c(1:2)]))
          
          # Binarise predictions
          env_preds_brt_bin <- data.frame(bio_curr_df_islandgroup[,1:2],
                                          brt = ifelse(env_preds_brt[,"brt"]>=comp_perf_clim_global[comp_perf_clim_global$alg=="brt",'thresh'],1,0))
          
          # Make a raster from binarised predictions - For the smallest island group Hunter and Matthew
          # an alternative approach needs to be taken as it only consists of two data points (wrong resolution is estimated)
          if (i == "Hunter and Matthew") {
            extent_preds_brt <- c(min(env_preds_brt_bin$x), max(env_preds_brt_bin$x), min(env_preds_brt_bin$y), max(env_preds_brt_bin$y))
            r_env_preds_brt_bin_temp <- terra::rast(ext = extent_preds_brt, res = res(Chelsa$bio_1), crs = crs(Chelsa$bio_1))
            vec_preds_brt <- vect(env_preds_brt_bin, geom = c("x", "y"))
            r_env_preds_brt_bin <- rasterize(vec_preds_brt, r_env_preds_brt_bin_temp, field = "brt")
            
          } else { r_env_preds_brt_bin <- terra::rast(env_preds_brt_bin, crs=crs(Chelsa$bio_1))
          }
          
          
          
          # (f) Predictions of Ensemble ----------------------------------------
          
          # Bind all continuous predictions of the algorithms into one data frame
          env_preds_all <- cbind(env_preds_glm, env_preds_gam$gam, env_preds_rf$rf, env_preds_brt$brt)
          colnames(env_preds_all) <- c("x", "y", "glm", "gam", "rf", "brt")
          
          # Average model predictions of the four different algorithms
          env_preds_ensemble <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                           mean_prob = rowMeans(env_preds_all[,-c(1:2)]))
          
          # Binarise predictions
          env_preds_ensemble_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                               ensemble = ifelse(env_preds_ensemble[,"mean_prob"]>= ensemble_perf_clim_global["mean_prob",'thresh'],1,0))
          
          
          # Make a raster from binarised predictions - For the smallest island group Hunter and Matthew
          # an alternative approach needs to be taken as it only consists of two data points (wrong resolution is estimated)
          if (i == "Hunter and Matthew") {
            extent_preds_ensemble <- c(min(env_preds_ensemble_bin$x), max(env_preds_ensemble_bin$x), min(env_preds_ensemble_bin$y), max(env_preds_ensemble_bin$y))
            r_env_preds_ensemble_bin_temp <- terra::rast(ext = extent_preds_ensemble, res = res(Chelsa$bio_1), crs = crs(Chelsa$bio_1))
            vec_preds_ensemble <- vect(env_preds_ensemble_bin, geom = c("x", "y"))
            r_env_preds_ensemble_bin <- rasterize(vec_preds_ensemble, r_env_preds_ensemble_bin_temp, field = "ensemble")
            
          } else { r_env_preds_ensemble_bin <- terra::rast(env_preds_ensemble_bin, crs=crs(Chelsa$bio_1))
          }
          
          
          
          #-------------------------------------------------------------------------------
          
          # 3. Area calculations ---------------------------------------------------------   
          
          # (a) Area of each island group covered by data ----------------------
          
          # GLM
          area_islandgroup_raster_glm <- expanse(r_env_preds_glm_bin, unit = "km")
          area_islandgroup_raster_glm <- round(area_islandgroup_raster_glm$area, 2)
          
          # GAM
          area_islandgroup_raster_gam <- expanse(r_env_preds_gam_bin, unit = "km")
          area_islandgroup_raster_gam <- round(area_islandgroup_raster_gam$area, 2)
          
          # RF
          area_islandgroup_raster_rf <- expanse(r_env_preds_rf_bin, unit = "km")
          area_islandgroup_raster_rf <- round(area_islandgroup_raster_rf$area, 2)
          
          # BRT
          area_islandgroup_raster_brt <- expanse(r_env_preds_brt_bin, unit = "km")
          area_islandgroup_raster_brt <- round(area_islandgroup_raster_brt$area, 2)
          
          # Ensemble
          area_islandgroup_raster_ensemble <- expanse(r_env_preds_ensemble_bin, unit = "km")
          area_islandgroup_raster_ensemble <- round(area_islandgroup_raster_ensemble$area, 2)
          
          
          
          # (b) Suitable area of each island group -----------------------------
          
          # GLM
          cells_islandgroup_suitable_glm  <- clamp(r_env_preds_glm_bin, lower = 1, values = FALSE) # Just retain cells with a value of 1 ("suitable")
          area_islandgroup_suitable_glm <- expanse(cells_islandgroup_suitable_glm, unit = "km")
          area_islandgroup_suitable_glm <- round(area_islandgroup_suitable_glm$area, 2)
          
          # GAM
          cells_islandgroup_suitable_gam  <- clamp(r_env_preds_gam_bin, lower = 1, values = FALSE) # Just retain cells with a value of 1 ("suitable")
          area_islandgroup_suitable_gam <- expanse(cells_islandgroup_suitable_gam, unit = "km")
          area_islandgroup_suitable_gam <- round(area_islandgroup_suitable_gam$area, 2)
          
          # RF
          cells_islandgroup_suitable_rf  <- clamp(r_env_preds_rf_bin, lower = 1, values = FALSE) # Just retain cells with a value of 1 ("suitable")
          area_islandgroup_suitable_rf <- expanse(cells_islandgroup_suitable_rf, unit = "km")
          area_islandgroup_suitable_rf <- round(area_islandgroup_suitable_rf$area, 2)
          
          # BRT
          cells_islandgroup_suitable_brt  <- clamp(r_env_preds_brt_bin, lower = 1, values = FALSE) # Just retain cells with a value of 1 ("suitable")
          area_islandgroup_suitable_brt <- expanse(cells_islandgroup_suitable_brt, unit = "km")
          area_islandgroup_suitable_brt <- round(area_islandgroup_suitable_brt$area, 2)
          
          # Ensemble
          cells_islandgroup_suitable_ensemble  <- clamp(r_env_preds_ensemble_bin, lower = 1, values = FALSE) # Just retain cells with a value of 1 ("suitable")
          area_islandgroup_suitable_ensemble <- expanse(cells_islandgroup_suitable_ensemble, unit = "km")
          area_islandgroup_suitable_ensemble <- round(area_islandgroup_suitable_ensemble$area, 2)
          
          
          
          # (c) Suitable habitat fraction in % per island group ----------------
          
          # GLM
          suitable_islandgroup_fraction_glm <- (area_islandgroup_suitable_glm/area_islandgroup_raster_glm)*100
          suitable_islandgroup_fraction_glm <- round(suitable_islandgroup_fraction_glm, 2)
          
          # GAM
          suitable_islandgroup_fraction_gam <- (area_islandgroup_suitable_gam/area_islandgroup_raster_gam)*100
          suitable_islandgroup_fraction_gam <- round(suitable_islandgroup_fraction_gam, 2)
          
          # RF
          suitable_islandgroup_fraction_rf <- (area_islandgroup_suitable_rf/area_islandgroup_raster_rf)*100
          suitable_islandgroup_fraction_rf <- round(suitable_islandgroup_fraction_rf, 2)
          
          # BRT
          suitable_islandgroup_fraction_brt <- (area_islandgroup_suitable_brt/area_islandgroup_raster_brt)*100
          suitable_islandgroup_fraction_brt <- round(suitable_islandgroup_fraction_brt, 2)
          
          # Ensemble
          suitable_islandgroup_fraction_ensemble <- (area_islandgroup_suitable_ensemble/area_islandgroup_raster_ensemble)*100
          suitable_islandgroup_fraction_ensemble <- round(suitable_islandgroup_fraction_ensemble, 2)
          
          
          # Special case of the Fiji Islands
          # Divided results due to two shapefiles need to be brought together
          # Rename results for Fiji 1
          if (i == "Fiji_1") {
            
            # GLM
            area_islandgroup_raster_glm_Fiji1 <- area_islandgroup_raster_glm
            area_islandgroup_suitable_glm_Fiji1 <- area_islandgroup_suitable_glm
            
            # GAM
            area_islandgroup_raster_gam_Fiji1 <- area_islandgroup_raster_gam
            area_islandgroup_suitable_gam_Fiji1 <- area_islandgroup_suitable_gam
            
            # RF
            area_islandgroup_raster_rf_Fiji1 <- area_islandgroup_raster_rf
            area_islandgroup_suitable_rf_Fiji1 <- area_islandgroup_suitable_rf
            
            # BRT
            area_islandgroup_raster_brt_Fiji1 <- area_islandgroup_raster_brt
            area_islandgroup_suitable_brt_Fiji1 <- area_islandgroup_suitable_brt
            
            # Ensemble
            area_islandgroup_raster_ensemble_Fiji1 <- area_islandgroup_raster_ensemble
            area_islandgroup_suitable_ensemble_Fiji1 <- area_islandgroup_suitable_ensemble
            
            # Sum up the results from the first Fiji Islands part with the second one  
          } else if (i == "Fiji_2") {
            
            # GLM
            area_islandgroup_raster_glm <- area_islandgroup_raster_glm + area_islandgroup_raster_glm_Fiji1
            area_islandgroup_suitable_glm <- area_islandgroup_suitable_glm + area_islandgroup_suitable_glm_Fiji1
            suitable_islandgroup_fraction_glm <- round(((area_islandgroup_suitable_glm/area_islandgroup_raster_glm)*100), 2)
            
            # GAM
            area_islandgroup_raster_gam <- area_islandgroup_raster_gam + area_islandgroup_raster_gam_Fiji1
            area_islandgroup_suitable_gam <- area_islandgroup_suitable_gam + area_islandgroup_suitable_gam_Fiji1
            suitable_islandgroup_fraction_gam <- round(((area_islandgroup_suitable_gam/area_islandgroup_raster_gam)*100), 2)
            
            # RF
            area_islandgroup_raster_rf <- area_islandgroup_raster_rf + area_islandgroup_raster_rf_Fiji1
            area_islandgroup_suitable_rf <- area_islandgroup_suitable_rf + area_islandgroup_suitable_rf_Fiji1
            suitable_islandgroup_fraction_rf <- round(((area_islandgroup_suitable_rf/area_islandgroup_raster_rf)*100), 2)
            
            # BRT
            area_islandgroup_raster_brt <- area_islandgroup_raster_brt + area_islandgroup_raster_brt_Fiji1
            area_islandgroup_suitable_brt <- area_islandgroup_suitable_brt + area_islandgroup_suitable_brt_Fiji1
            suitable_islandgroup_fraction_brt <- round(((area_islandgroup_suitable_brt/area_islandgroup_raster_brt)*100), 2)
            
            # Ensemble
            area_islandgroup_raster_ensemble <- area_islandgroup_raster_ensemble + area_islandgroup_raster_ensemble_Fiji1
            area_islandgroup_suitable_ensemble <- area_islandgroup_suitable_ensemble + area_islandgroup_suitable_ensemble_Fiji1
            suitable_islandgroup_fraction_ensemble <- round(((area_islandgroup_suitable_ensemble/area_islandgroup_raster_ensemble)*100), 2)
            
            # Write a vector with the results
            results_glm_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_glm), as.numeric(area_islandgroup_suitable_glm), as.numeric(suitable_islandgroup_fraction_glm), "GLM", "clim", "global", sp)
            results_gam_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_gam), as.numeric(area_islandgroup_suitable_gam), as.numeric(suitable_islandgroup_fraction_gam), "GAM", "clim", "global", sp)
            results_rf_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_rf), as.numeric(area_islandgroup_suitable_rf), as.numeric(suitable_islandgroup_fraction_rf), "RF", "clim", "global", sp)
            results_brt_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_brt), as.numeric(area_islandgroup_suitable_brt), as.numeric(suitable_islandgroup_fraction_brt), "BRT", "clim", "global", sp)
            results_ensemble_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_ensemble), as.numeric(area_islandgroup_suitable_ensemble), as.numeric(suitable_islandgroup_fraction_ensemble), "Ensemble", "clim", "global", sp) 
            
            # Add results to the results data frame
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_glm_Fiji)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_gam_Fiji)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_rf_Fiji)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_brt_Fiji)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_ensemble_Fiji)
            
            
          } else {
            
            # Write a vector with the results
            results_glm <- c(i, as.numeric(area_islandgroup_raster_glm), as.numeric(area_islandgroup_suitable_glm), as.numeric(suitable_islandgroup_fraction_glm), "GLM", "clim", "global", sp)
            results_gam <- c(i, as.numeric(area_islandgroup_raster_gam), as.numeric(area_islandgroup_suitable_gam), as.numeric(suitable_islandgroup_fraction_gam), "GAM", "clim", "global", sp)
            results_rf <- c(i, as.numeric(area_islandgroup_raster_rf), as.numeric(area_islandgroup_suitable_rf), as.numeric(suitable_islandgroup_fraction_rf), "RF", "clim", "global", sp)
            results_brt <- c(i, as.numeric(area_islandgroup_raster_brt), as.numeric(area_islandgroup_suitable_brt), as.numeric(suitable_islandgroup_fraction_brt), "BRT", "clim", "global", sp)
            results_ensemble <- c(i, as.numeric(area_islandgroup_raster_ensemble), as.numeric(area_islandgroup_suitable_ensemble), as.numeric(suitable_islandgroup_fraction_ensemble), "Ensemble", "clim", "global", sp) 
            
            # Add results to the results data frame
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_glm)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_gam)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_rf)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_brt)
            islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, results_ensemble)
            
            
            
            
          } # End the if conditions due to Fiji Islands
          
          # Make sure the data frame contains the correct column names 
          colnames(islandgroups_results_clim_global_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
          
          # Make sure that the column containing calculations are numeric
          islandgroups_results_clim_global_spec[, 2:4] <- apply(islandgroups_results_clim_global_spec[, 2:4], 2, as.numeric)
          
        })} # end of try and for loop over all island groups
      
      
      
      # (d) Pacific-wide suitable habitat fraction in % ------------------------
      
      # GLM
      glm_alg_subset <- subset(islandgroups_results_clim_global_spec, islandgroups_results_clim_global_spec$algorithm == "GLM") # subset GLM algorithm results
      pacific_total_area_glm <- round(sum(glm_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_glm <- round(sum(glm_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_glm <- round(((pacific_suitable_area_glm/pacific_total_area_glm)*100), 2)
      
      total_results_glm <- c("Pacific", as.numeric(pacific_total_area_glm), as.numeric(pacific_suitable_area_glm), as.numeric(pacific_suitable_fraction_glm), "GLM", "clim", "global", sp) # Write results in a vector
      
      # GAM
      gam_alg_subset <- subset(islandgroups_results_clim_global_spec, islandgroups_results_clim_global_spec$algorithm == "GAM") # subset GAM algorithm results
      pacific_total_area_gam <- round(sum(gam_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_gam <- round(sum(gam_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_gam <- round(((pacific_suitable_area_gam/pacific_total_area_gam)*100), 2)
      
      total_results_gam <- c("Pacific", as.numeric(pacific_total_area_gam), as.numeric(pacific_suitable_area_gam), as.numeric(pacific_suitable_fraction_gam), "GAM", "clim", "global", sp) # Write results in a vector
      
      # RF
      rf_alg_subset <- subset(islandgroups_results_clim_global_spec, islandgroups_results_clim_global_spec$algorithm == "RF") # subset RF algorithm results
      pacific_total_area_rf <- round(sum(rf_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_rf <- round(sum(rf_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_rf <- round(((pacific_suitable_area_rf/pacific_total_area_rf)*100), 2)
      
      total_results_rf <- c("Pacific", as.numeric(pacific_total_area_rf), as.numeric(pacific_suitable_area_rf), as.numeric(pacific_suitable_fraction_rf), "RF", "clim", "global", sp) # Write results in a vector
      
      # BRT
      brt_alg_subset <- subset(islandgroups_results_clim_global_spec, islandgroups_results_clim_global_spec$algorithm == "BRT") # subset BRT algorithm results
      pacific_total_area_brt <- round(sum(brt_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_brt <- round(sum(brt_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_brt <- round(((pacific_suitable_area_brt/pacific_total_area_brt)*100), 2)
      
      total_results_brt <- c("Pacific", as.numeric(pacific_total_area_brt), as.numeric(pacific_suitable_area_brt), as.numeric(pacific_suitable_fraction_brt), "BRT", "clim", "global", sp) # Write results in a vector
      
      # Ensemble
      ensemble_alg_subset <- subset(islandgroups_results_clim_global_spec, islandgroups_results_clim_global_spec$algorithm == "Ensemble") # subset ensemble algorithm results
      pacific_total_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_ensemble <- round(((pacific_suitable_area_ensemble/pacific_total_area_ensemble)*100), 2)
      
      total_results_ensemble <- c("Pacific", as.numeric(pacific_total_area_ensemble), as.numeric(pacific_suitable_area_ensemble), as.numeric(pacific_suitable_fraction_ensemble), "Ensemble", "clim", "global", sp) # Write results in a vector
      
      # Bind the result vectors to the existing results data frame
      islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, total_results_glm)
      islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, total_results_gam)
      islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, total_results_rf)
      islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, total_results_brt)
      islandgroups_results_clim_global_spec <- rbind(islandgroups_results_clim_global_spec, total_results_ensemble)
      
      
      # (e) Save results for each species --------------------------------------
      
      # Make sure the data frame contains the correct column names 
      colnames(islandgroups_results_clim_global_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
      
      # Make sure that the column containing calculations are numeric
      islandgroups_results_clim_global_spec[, 2:4] <- apply(islandgroups_results_clim_global_spec[, 2:4], 2, as.numeric)
      
      # Save the results data frame per species
      save(islandgroups_results_clim_global_spec, file = paste0("output_data/model_predictions/global/clim/islandgroups_results_clim_global_spec_",sp,".RData"))
      
      
      
    } else if (file_exists == TRUE) { print("already done model predictions")
    } # End of if condition
    
    
  })} # end of try and for loop over species


# (f) Save results of all species together -------------------------------------

# Create a data frame to store the results of area calculations for all species
islandgroups_results_clim_global <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(islandgroups_results_clim_global) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")

for (sp in study_species) {
  try({
    
    # Check if prediction results already exist
    file_exists <- file.exists(paste0("output_data/model_predictions/global/clim/islandgroups_results_clim_global_spec_",sp,".RData"))
    
    if (file_exists == TRUE) { # just continue with model predictions if output 
      # with prediction results exists
      
      # Load the file with predictions results
      load(paste0("output_data/model_predictions/global/clim/islandgroups_results_clim_global_spec_",sp,".RData"))
      
      # Add the data frame for each species to the data frame containing all results for all species
      islandgroups_results_clim_global <- rbind(islandgroups_results_clim_global, islandgroups_results_clim_global_spec)
      
    } else if (file_exists == FALSE) { next
    } # End of if condition
    
    
    
  })} # end of try and for loop over species

# Make sure the data frame contains the correct column names
colnames(islandgroups_results_clim_global) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")

# Make sure that the column containing calculations are numeric
islandgroups_results_clim_global[, 2:4] <- apply(islandgroups_results_clim_global[, 2:4], 2, as.numeric)

# Save the data frame containing all prediction results
save(islandgroups_results_clim_global, file = "output_data/model_predictions/global/clim/islandgroups_results_clim_global.RData")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                06c. Model predictions based on global                  #
#                occurrences and purely climatic data                    #
# (only considering island groups that are covered by edaphic data       #
# for a reasonable comparison)                                           #
# ---------------------------------------------------------------------- #

# Load needed objects
load("input_data/spatial_data/islandgroups_edaclim.RData") # Contains names of island groups suitable for the analysis using combined climatic data and edaphic data
load("input_data/occ_numbers_thinned_env_nat_filtered.RData") # Contains names of study species


#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_nat_filtered$species))

# Retrieve island group names (change Fiji Islands for the two divided parts)
islandgroup_climate_soil <- as.character(islandgroups_edaclim$island_group)


#-------------------------------------------------------------------------------

# 2. New area calculations -----------------------------------------------------

for (sp in study_species) { # Start of the loop over all species
  try({
    
    # Check if prediction results based on only 25 island groups already exist
    file_exists <- file.exists(paste0("output_data/model_predictions/global/clim_comp/islandgroups_results_clim_global_spec_comp_",sp,".RData"))
    
    if (file_exists == FALSE) { # just continue with new area calculations if output 
      # with prediction results based on 25 island groups does not exist yet
      
      # Load the data file containing the area calculations of predictions
      load(paste0("output_data/model_predictions/global/clim/islandgroups_results_clim_global_spec_",sp,".RData"))
      
      # Only retain the entries of island groups that are included in the analysis
      # including edaphic data
      islandgroups_results_clim_global_spec_comp <- islandgroups_results_clim_global_spec[islandgroups_results_clim_global_spec$islandgroup %in% islandgroup_climate_soil, ]
      
      # Make sure that the column containing calculations are numeric
      islandgroups_results_clim_global_spec_comp[, 2:4] <- apply(islandgroups_results_clim_global_spec_comp[, 2:4], 2, as.numeric)
      
      # Make new Pacific-wide area calculations of suitable habitat as a lower
      # number of island groups is considered now
      # GLM
      glm_alg_subset <- subset(islandgroups_results_clim_global_spec_comp, islandgroups_results_clim_global_spec_comp$algorithm == "GLM") # subset GLM algorithm results
      pacific_total_area_glm <- round(sum(glm_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_glm <- round(sum(glm_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_glm <- round(((pacific_suitable_area_glm/pacific_total_area_glm)*100), 2)
      
      total_results_glm <- c("Pacific", as.numeric(pacific_total_area_glm), as.numeric(pacific_suitable_area_glm), as.numeric(pacific_suitable_fraction_glm), "GLM", "clim", "global", sp) # Write results in a vector
      
      # GAM
      gam_alg_subset <- subset(islandgroups_results_clim_global_spec_comp, islandgroups_results_clim_global_spec_comp$algorithm == "GAM") # subset GAM algorithm results
      pacific_total_area_gam <- round(sum(gam_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_gam <- round(sum(gam_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_gam <- round(((pacific_suitable_area_gam/pacific_total_area_gam)*100), 2)
      
      total_results_gam <- c("Pacific", as.numeric(pacific_total_area_gam), as.numeric(pacific_suitable_area_gam), as.numeric(pacific_suitable_fraction_gam), "GAM", "clim", "global", sp) # Write results in a vector
      
      # RF
      rf_alg_subset <- subset(islandgroups_results_clim_global_spec_comp, islandgroups_results_clim_global_spec_comp$algorithm == "RF") # subset RF algorithm results
      pacific_total_area_rf <- round(sum(rf_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_rf <- round(sum(rf_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_rf <- round(((pacific_suitable_area_rf/pacific_total_area_rf)*100), 2)
      
      total_results_rf <- c("Pacific", as.numeric(pacific_total_area_rf), as.numeric(pacific_suitable_area_rf), as.numeric(pacific_suitable_fraction_rf), "RF", "clim", "global", sp) # Write results in a vector
      
      # BRT
      brt_alg_subset <- subset(islandgroups_results_clim_global_spec_comp, islandgroups_results_clim_global_spec_comp$algorithm == "BRT") # subset BRT algorithm results
      pacific_total_area_brt <- round(sum(brt_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_brt <- round(sum(brt_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_brt <- round(((pacific_suitable_area_brt/pacific_total_area_brt)*100), 2)
      
      total_results_brt <- c("Pacific", as.numeric(pacific_total_area_brt), as.numeric(pacific_suitable_area_brt), as.numeric(pacific_suitable_fraction_brt), "BRT", "clim", "global", sp) # Write results in a vector
      
      # Ensemble
      ensemble_alg_subset <- subset(islandgroups_results_clim_global_spec_comp, islandgroups_results_clim_global_spec_comp$algorithm == "Ensemble") # subset ensemble algorithm results
      pacific_total_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
      pacific_suitable_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
      pacific_suitable_fraction_ensemble <- round(((pacific_suitable_area_ensemble/pacific_total_area_ensemble)*100), 2)
      
      total_results_ensemble <- c("Pacific", as.numeric(pacific_total_area_ensemble), as.numeric(pacific_suitable_area_ensemble), as.numeric(pacific_suitable_fraction_ensemble), "Ensemble", "clim", "global", sp) # Write results in a vector
      
      # Bind the result vectors to the existing results data frame
      islandgroups_results_clim_global_spec_comp <- rbind(islandgroups_results_clim_global_spec_comp, total_results_glm)
      islandgroups_results_clim_global_spec_comp <- rbind(islandgroups_results_clim_global_spec_comp, total_results_gam)
      islandgroups_results_clim_global_spec_comp <- rbind(islandgroups_results_clim_global_spec_comp, total_results_rf)
      islandgroups_results_clim_global_spec_comp <- rbind(islandgroups_results_clim_global_spec_comp, total_results_brt)
      islandgroups_results_clim_global_spec_comp <- rbind(islandgroups_results_clim_global_spec_comp, total_results_ensemble)
      
      # Make sure the data frame contains the correct column names 
      colnames(islandgroups_results_clim_global_spec_comp) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
      
      # Make sure that the column containing calculations are numeric
      islandgroups_results_clim_global_spec_comp[, 2:4] <- apply(islandgroups_results_clim_global_spec_comp[, 2:4], 2, as.numeric)
      
      # Save the results data frame per species
      save(islandgroups_results_clim_global_spec_comp, file = paste0("output_data/model_predictions/global/clim_comp/islandgroups_results_clim_global_spec_comp_",sp,".RData"))
      
      
    } else if (file_exists == TRUE) { next
    } # End of if condition
    
    
  })} # end of try and for loop over species





#-------------------------------------------------------------------------------

# 3. Save results of all species together --------------------------------------

# Create a data frame to store the results of area calculations for all species
islandgroups_results_clim_global_comp <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(islandgroups_results_clim_global_comp) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")

for (sp in study_species) {
  try({
    
    # Check if prediction results already exist
    file_exists <- file.exists(paste0("output_data/model_predictions/global/clim_comp/islandgroups_results_clim_global_spec_comp_",sp,".RData"))
    
    if (file_exists == TRUE) { # just continue with model predictions if output 
      # with prediction results exists
      
      # Load the file with predictions results
      load(paste0("output_data/model_predictions/global/clim_comp/islandgroups_results_clim_global_spec_comp_",sp,".RData"))
      
      # Add the data frame for each species to the data frame containing all results for all species
      islandgroups_results_clim_global_comp <- rbind(islandgroups_results_clim_global_comp, islandgroups_results_clim_global_spec_comp)
      
    } else if (file_exists == FALSE) { next
    } # End of if condition
    
    
    
  })} # end of try and for loop over species

# Make sure the data frame contains the correct column names
colnames(islandgroups_results_clim_global_comp) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")

# Make sure that the column containing calculations are numeric
islandgroups_results_clim_global_comp[, 2:4] <- apply(islandgroups_results_clim_global_comp[, 2:4], 2, as.numeric)

# Save the data frame containing all prediction results
save(islandgroups_results_clim_global_comp, file = "output_data/model_predictions/global/clim_comp/islandgroups_results_clim_global_comp.RData")


gc()
rm(list=ls())