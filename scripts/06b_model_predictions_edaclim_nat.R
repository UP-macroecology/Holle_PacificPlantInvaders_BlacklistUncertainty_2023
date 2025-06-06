# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#           06b. Model predictions based on native occurrences           #
#           and combined climatic and edaphic data                       #
# ---------------------------------------------------------------------- #

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
load("input_data/spatial_data/islandgroups_edaclim.RData") # Contains names of island groups suitable for the analysis using combined climatic data and edaphic data
load("/input_data/spatial_data/pacific_islands_extent.RData") # Contains the spatial extents of the island groups
Chelsa <- terra::rast(str_sort(list.files("input_data/environmental_data/Chelsa_V2", 
                                          pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Climate variable rasters
SoilGrids <- terra::rast(str_sort(list.files("input_data/environmental_data/SoilGrids_V2", 
                                             pattern = ".tif", full.names = TRUE), numeric = TRUE)) # Edaphic variable rasters
load("input_data/spatial_data/geoentities_plus_newname.RData") # Island group shapefiles
fiji_1 <- terra::vect("input_data/spatial_data/fiji_1.shp") # Modified shapefile of Fiji - first part
fiji_2 <- terra::vect("input_data/spatial_data/fiji_2.shp") # Modified shapefile of Fiji - second part

# Create a vector containing the different thresholding methods for binarising predictions
threshold_methods <- c("maxTSS", "meanProb", "tenthPer")

#-------------------------------------------------------------------------------

# 1. Data preparations ---------------------------------------------------------

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species))

# Retrieve island group names (change Fiji Islands for the two divided parts)
islandgroup_climate_soil <- islandgroups_edaclim[islandgroups_edaclim$island_group != "Fiji",]
islandgroup_climate_soil <- as.character(islandgroup_climate_soil$island_group)
islandgroup_climate_soil <- c(islandgroup_climate_soil, "Fiji_1", "Fiji_2")

# Change climate variable names
names(Chelsa) <- c(paste('bio',1:19, sep='_'))

# Stack climatic and edaphic rasters
Chelsa_SoilGrids <- c(Chelsa, SoilGrids)

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
    
    print("native + edaclim")
    
    
    for (t in threshold_methods) { # Start the loop over the different thresholding methods
      try({
        
        print(t)
        
        # Check if prediction results already exist
        file_exists <- file.exists(paste0("output_data/model_predictions/native/edaclim/",t,"/islandgroups_results_edaclim_native_spec_",sp,".RData"))
        
        if (file_exists == FALSE) { # just continue with model predictions if output 
          # with prediction results does not exist yet
          
          print("start of model predictions")
          
          # Load the needed objects for each species
          load(paste0("output_data/models/native/edaclim/models_edaclim_native_",sp,".RData")) # the four different models
          load(paste0("output_data/validation/native/edaclim/validation_edaclim_native_",sp,".RData")) # validation outcomes
          load(paste0("output_data/variable_selection/native/edaclim/pred_sel_edaclim_native_",sp,".RData")) # predictor variables
          
          # Dynamically get the validation data corresponding to the respective thresholding method
          comp_perf_edaclim_native <- get(paste0("comp_perf_edaclim_native_", t))
          
          
          
          # Create a data frame to store the results for each species
          islandgroups_results_edaclim_native_spec <- data.frame(matrix(ncol = 8, nrow = 0))
          colnames(islandgroups_results_edaclim_native_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
          
          
          
          # (a) Prepare island group data for predictors ---------------------------
          
          for (i in islandgroup_climate_soil) {
            try({
              
              print(i)
              
              # Get the extent of each island group
              single_islandgroup <- subset(pacific_islands_extent, pacific_islands_extent$island_group == i)
              single_islandgroup_extent <- c(single_islandgroup$xmin, single_islandgroup$xmax, single_islandgroup$ymin, single_islandgroup$ymax)
              
              # Crop the variables to the island group extent
              islandgroup_var <- crop(Chelsa_SoilGrids, single_islandgroup_extent)
              
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
              bio_curr_df_islandgroup <- data.frame(crds(islandgroup_mask[[pred_sel_edaclim_native]]),as.points(islandgroup_mask[[pred_sel_edaclim_native]]))
              
              
              
              # (b) Predictions of GLM ---------------------------------------------
              
              # Make predictions
              print("GLM")
              env_preds_glm <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                          glm = predict(m_glm_edaclim_native, bio_curr_df_islandgroup[, pred_sel_edaclim_native], type='response'))
              
              # Binarise predictions
              env_preds_glm_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                              glm = ifelse(env_preds_glm[,"glm"]>=comp_perf_edaclim_native[comp_perf_edaclim_native$alg=="glm",'thresh'],1,0))
              
              
              # Make a raster from binarised predictions
              r_env_preds_glm_bin <- terra::rast(env_preds_glm_bin, crs=crs(Chelsa_SoilGrids$bio_1))
              
              
              
              
              # (c) Predictions of GAM ---------------------------------------------
              
              # Make predictions
              print("GAM")
              env_preds_gam <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                          gam = predict(m_gam_edaclim_native, bio_curr_df_islandgroup[, pred_sel_edaclim_native], type='response'))
              
              # Binarise predictions
              env_preds_gam_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                              gam = ifelse(env_preds_gam[,"gam"]>=comp_perf_edaclim_native[comp_perf_edaclim_native$alg=="gam",'thresh'],1,0))
              
              # Make a raster from binarised predictions
              r_env_preds_gam_bin <- terra::rast(env_preds_gam_bin, crs=crs(Chelsa_SoilGrids$bio_1))
              
              
              
              
              # (d) Predictions of RF ----------------------------------------------
              
              # Make predictions of all 10 RF models
              print("RF")
              env_preds_rf <- data.frame(bio_curr_df_islandgroup[, 1:2], 
                                         rf = rowMeans(sapply(1:10, FUN=function(m) {print(m); predict(m_rf_edaclim_native[[m]], bio_curr_df_islandgroup[, pred_sel_edaclim_native], type='response')})))
              
              # Binarise predictions
              env_preds_rf_bin <- data.frame(bio_curr_df_islandgroup[,1:2],
                                             rf = ifelse(env_preds_rf[,"rf"]>=comp_perf_edaclim_native[comp_perf_edaclim_native$alg=="rf",'thresh'],1,0))
              
              # Make a raster from binarised predictions
              r_env_preds_rf_bin <- terra::rast(env_preds_rf_bin, crs=crs(Chelsa_SoilGrids$bio_1))
              
              
              
              
              # (e) Predictions of BRT ---------------------------------------------
              
              # Make predictions of all 10 BRT models
              print("BRT")
              env_preds_brt <- data.frame(bio_curr_df_islandgroup[, 1:2],
                                          brt = rowMeans(sapply(1:10, FUN=function(m) {print(m); predict.gbm(m_brt_edaclim_native[[m]], bio_curr_df_islandgroup[, pred_sel_edaclim_native],
                                                                                                             n.trees=m_brt_edaclim_native[[m]]$gbm.call$best.trees, type='response')})))
              
              
              # Binarise predictions
              env_preds_brt_bin <- data.frame(bio_curr_df_islandgroup[,1:2],
                                              brt = ifelse(env_preds_brt[,"brt"]>=comp_perf_edaclim_native[comp_perf_edaclim_native$alg=="brt",'thresh'],1,0))
              
              # Make a raster from binarised predictions
              r_env_preds_brt_bin <- terra::rast(env_preds_brt_bin, crs=crs(Chelsa_SoilGrids$bio_1))
              
              
              
              
              # (f) Predictions of Ensemble ----------------------------------------
              print("Ensemble")
              
              # Bind all continuous predictions of the algorithms into one data frame
              env_preds_all <- cbind(x = bio_curr_df_islandgroup$x, y = bio_curr_df_islandgroup$y, glm = env_preds_glm$glm, gam = env_preds_gam$gam, rf = env_preds_rf$rf, brt = env_preds_brt$brt)
              
              # Average model predictions of the four different algorithms
              env_preds_ensemble <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                               mean_prob = rowMeans(env_preds_all[,-c(1:2)]))
              
              # Binarise predictions
              env_preds_ensemble_bin <- data.frame(bio_curr_df_islandgroup[,1:2], 
                                                   ensemble = ifelse(env_preds_ensemble[,"mean_prob"]>= ensemble_perf_edaclim_native[t,'thresh'],1,0))
              
              
              # Make a raster from binarised predictions
              r_env_preds_ensemble_bin <- terra::rast(env_preds_ensemble_bin, crs=crs(Chelsa_SoilGrids$bio_1))
              
              
              
              
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
                results_glm_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_glm), as.numeric(area_islandgroup_suitable_glm), as.numeric(suitable_islandgroup_fraction_glm), "GLM", "edaclim", "native", sp)
                results_gam_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_gam), as.numeric(area_islandgroup_suitable_gam), as.numeric(suitable_islandgroup_fraction_gam), "GAM", "edaclim", "native", sp)
                results_rf_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_rf), as.numeric(area_islandgroup_suitable_rf), as.numeric(suitable_islandgroup_fraction_rf), "RF", "edaclim", "native", sp)
                results_brt_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_brt), as.numeric(area_islandgroup_suitable_brt), as.numeric(suitable_islandgroup_fraction_brt), "BRT", "edaclim", "native", sp)
                results_ensemble_Fiji <- c("Fiji", as.numeric(area_islandgroup_raster_ensemble), as.numeric(area_islandgroup_suitable_ensemble), as.numeric(suitable_islandgroup_fraction_ensemble), "Ensemble", "edaclim", "native", sp) 
                
                # Add results to the results data frame
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_glm_Fiji)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_gam_Fiji)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_rf_Fiji)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_brt_Fiji)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_ensemble_Fiji)
                
                
              } else {
                
                # Write a vector with the results
                results_glm <- c(i, as.numeric(area_islandgroup_raster_glm), as.numeric(area_islandgroup_suitable_glm), as.numeric(suitable_islandgroup_fraction_glm), "GLM", "edaclim", "native", sp)
                results_gam <- c(i, as.numeric(area_islandgroup_raster_gam), as.numeric(area_islandgroup_suitable_gam), as.numeric(suitable_islandgroup_fraction_gam), "GAM", "edaclim", "native", sp)
                results_rf <- c(i, as.numeric(area_islandgroup_raster_rf), as.numeric(area_islandgroup_suitable_rf), as.numeric(suitable_islandgroup_fraction_rf), "RF", "edaclim", "native", sp)
                results_brt <- c(i, as.numeric(area_islandgroup_raster_brt), as.numeric(area_islandgroup_suitable_brt), as.numeric(suitable_islandgroup_fraction_brt), "BRT", "edaclim", "native", sp)
                results_ensemble <- c(i, as.numeric(area_islandgroup_raster_ensemble), as.numeric(area_islandgroup_suitable_ensemble), as.numeric(suitable_islandgroup_fraction_ensemble), "Ensemble", "edaclim", "native", sp) 
                
                # Add results to the results data frame
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_glm)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_gam)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_rf)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_brt)
                islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, results_ensemble)
                
                
                
                
              } # End the if conditions due to Fiji Islands
              
              # Make sure the data frame contains the correct column names 
              colnames(islandgroups_results_edaclim_native_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
              
              # Make sure that the column containing calculations are numeric
              islandgroups_results_edaclim_native_spec[, 2:4] <- apply(islandgroups_results_edaclim_native_spec[, 2:4], 2, as.numeric)
              
            })} # end of try and for loop over all island groups
          
          
          
          # (d) Pacific-wide suitable habitat fraction in % ------------------------
          
          # GLM
          glm_alg_subset <- subset(islandgroups_results_edaclim_native_spec, islandgroups_results_edaclim_native_spec$algorithm == "GLM") # subset GLM algorithm results
          pacific_total_area_glm <- round(sum(glm_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
          pacific_suitable_area_glm <- round(sum(glm_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
          pacific_suitable_fraction_glm <- round(((pacific_suitable_area_glm/pacific_total_area_glm)*100), 2)
          
          total_results_glm <- c("Pacific", as.numeric(pacific_total_area_glm), as.numeric(pacific_suitable_area_glm), as.numeric(pacific_suitable_fraction_glm), "GLM", "edaclim", "native", sp) # Write results in a vector
          
          # GAM
          gam_alg_subset <- subset(islandgroups_results_edaclim_native_spec, islandgroups_results_edaclim_native_spec$algorithm == "GAM") # subset GAM algorithm results
          pacific_total_area_gam <- round(sum(gam_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
          pacific_suitable_area_gam <- round(sum(gam_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
          pacific_suitable_fraction_gam <- round(((pacific_suitable_area_gam/pacific_total_area_gam)*100), 2)
          
          total_results_gam <- c("Pacific", as.numeric(pacific_total_area_gam), as.numeric(pacific_suitable_area_gam), as.numeric(pacific_suitable_fraction_gam), "GAM", "edaclim", "native", sp) # Write results in a vector
          
          # RF
          rf_alg_subset <- subset(islandgroups_results_edaclim_native_spec, islandgroups_results_edaclim_native_spec$algorithm == "RF") # subset RF algorithm results
          pacific_total_area_rf <- round(sum(rf_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
          pacific_suitable_area_rf <- round(sum(rf_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
          pacific_suitable_fraction_rf <- round(((pacific_suitable_area_rf/pacific_total_area_rf)*100), 2)
          
          total_results_rf <- c("Pacific", as.numeric(pacific_total_area_rf), as.numeric(pacific_suitable_area_rf), as.numeric(pacific_suitable_fraction_rf), "RF", "edaclim", "native", sp) # Write results in a vector
          
          # BRT
          brt_alg_subset <- subset(islandgroups_results_edaclim_native_spec, islandgroups_results_edaclim_native_spec$algorithm == "BRT") # subset BRT algorithm results
          pacific_total_area_brt <- round(sum(brt_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
          pacific_suitable_area_brt <- round(sum(brt_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
          pacific_suitable_fraction_brt <- round(((pacific_suitable_area_brt/pacific_total_area_brt)*100), 2)
          
          total_results_brt <- c("Pacific", as.numeric(pacific_total_area_brt), as.numeric(pacific_suitable_area_brt), as.numeric(pacific_suitable_fraction_brt), "BRT", "edaclim", "native", sp) # Write results in a vector
          
          # Ensemble
          ensemble_alg_subset <- subset(islandgroups_results_edaclim_native_spec, islandgroups_results_edaclim_native_spec$algorithm == "Ensemble") # subset ensemble algorithm results
          pacific_total_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_raster, na.rm = TRUE), 2)
          pacific_suitable_area_ensemble <- round(sum(ensemble_alg_subset$area_islandgroup_suitable, na.rm = TRUE), 2)
          pacific_suitable_fraction_ensemble <- round(((pacific_suitable_area_ensemble/pacific_total_area_ensemble)*100), 2)
          
          total_results_ensemble <- c("Pacific", as.numeric(pacific_total_area_ensemble), as.numeric(pacific_suitable_area_ensemble), as.numeric(pacific_suitable_fraction_ensemble), "Ensemble", "edaclim", "native", sp) # Write results in a vector
          
          # Bind the result vectors to the existing results data frame
          islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, total_results_glm)
          islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, total_results_gam)
          islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, total_results_rf)
          islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, total_results_brt)
          islandgroups_results_edaclim_native_spec <- rbind(islandgroups_results_edaclim_native_spec, total_results_ensemble)
          
          
          # (e) Save results -------------------------------------------------------
          
          # Make sure the data frame contains the correct column names 
          colnames(islandgroups_results_edaclim_native_spec) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
          
          # Make sure that the column containing calculations are numeric
          islandgroups_results_edaclim_native_spec[, 2:4] <- apply(islandgroups_results_edaclim_native_spec[, 2:4], 2, as.numeric)
          
          # Save the results data frame per species
          save(islandgroups_results_edaclim_native_spec, file = paste0("output_data/model_predictions/native/edaclim/",t,"/islandgroups_results_edaclim_native_spec_",sp,".RData"))
          
          
          
        } else if (file_exists == TRUE) { print("already done model predictions")
        } # End of if condition
        
        
      })} # End of try and for loop over thresholding methods
  })} # end of try and for loop over species



# (f) Save results of all species together -------------------------------------

print("start putting species prediction results together")


for (t in threshold_methods) { # Start the loop over the three thresholding methods
  try({
    
    print(t)
    
    # Create a data frame to store the results of area calculations for all species
    islandgroups_results_edaclim_native <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(islandgroups_results_edaclim_native) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
    
    for (sp in study_species) {
      try({
        
        print(sp)
        
        # Check if prediction results already exist
        file_exists <- file.exists(paste0("output_data/model_predictions/native/edaclim/",t,"/islandgroups_results_edaclim_native_spec_",sp,".RData"))
        
        if (file_exists == TRUE) { # just continue with model predictions if output 
          # with prediction results exists
          
          # Load the file with predictions results
          load(paste0("output_data/model_predictions/native/edaclim/",t,"/islandgroups_results_edaclim_native_spec_",sp,".RData"))
          
          # Add the data frame for each species to the data frame containing all results for all species
          islandgroups_results_edaclim_native <- rbind(islandgroups_results_edaclim_native, islandgroups_results_edaclim_native_spec)
          
        } else if (file_exists == FALSE) { next
        } # End of if condition
        
        
        
      })} # end of try and for loop over species
    
    # Make sure the data frame contains the correct column names
    colnames(islandgroups_results_edaclim_native) <- c("islandgroup", "area_islandgroup_raster", "area_islandgroup_suitable", "suitable_habitat_fraction", "algorithm", "predictor_type", "niche", "species")
    
    # Make sure that the column containing calculations are numeric
    islandgroups_results_edaclim_native[, 2:4] <- apply(islandgroups_results_edaclim_native[, 2:4], 2, as.numeric)
    
    # Save the data frame containing all prediction results
    save(islandgroups_results_edaclim_native, file = paste0("output_data/model_predictions/native/edaclim/",t,"/islandgroups_results_edaclim_native.RData"))
    
  })} # end of try and for loop over thresholding methods

gc()
rm(list=ls())
