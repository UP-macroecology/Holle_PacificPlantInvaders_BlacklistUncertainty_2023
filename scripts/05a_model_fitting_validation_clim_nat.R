# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             05a. Model fitting and validation based on                 #
#             native occurrences and purely climatic data                #
# ---------------------------------------------------------------------- #

# Set working directory
setwd("/import/ecoc9z/data-zurell/holle/Holle_PacificPlantInvaders_BlacklistUncertainty_2023/")

# Required path 
# path_imp <- file.path("/import/ecoc9z/data-zurell/holle/uncertainty_paper")

# Load needed packages
library(mgcv)
library(randomForest)
library(gbm)
library(dismo)
library(PresenceAbsence)

# Load needed objects
load("input_data/occ_numbers_thinned_env_nat_filtered.RData") # Contains names of study species
source("scripts/functions.R") # partial_response,cross-validation and evaluation metrics function

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_nat_filtered$species)) 

#-------------------------------------------------------------------------------

# 1. Model fitting -------------------------------------------------------------

for (sp in study_species) { # Start the loop over all species
  try({
    
    print(sp)
    
    print(clim)
    
    # check if models already exist
    file_exists_models <- file.exists(paste0("output_data/models/native/clim/models_clim_native_",sp,".RData"))
    
    if (file_exists_models == FALSE) { # just continue with model fitting if output 
    # with models does not exist yet
      
    print("start of model building process")
    
    # Load needed objects of species and environmental data
    load(paste0("output_data/distribution_env_data/native/clim/species_occ_clim_native_",sp,".RData")) # distribution and environmental data
    load(paste0("output_data/variable_selection/native/clim/pred_sel_clim_native_",sp,".RData")) # predictor variables
    
    # Create an absence index for machine learning algorithm to achieve even 
    # presence and absence data sets for machine learning algorithms
    species_occ_clim_native$abs_index <- NA
    species_occ_clim_native$abs_index[species_occ_clim_native$occ!=1] <- sample(1:10,sum(species_occ_clim_native$occ!=1), replace=T)
    
    # Calculate same weights for presences and absences for regression based algorithms
    weights <- ifelse(species_occ_clim_native$occ==1, 1, sum(species_occ_clim_native$occ==1) / sum(species_occ_clim_native$occ==0))
    
    
    
    # (a) GLM -----------------------
    print("GLM")
    
    # Including linear and quadratic terms
    m_glm_clim_native <- glm(formula = as.formula(paste('occ ~',paste(pred_sel_clim_native,paste0('+ I(', pred_sel_clim_native ,'^2)'), collapse='+'))),
                         family = "binomial", 
                         weights = weights, 
                         data = species_occ_clim_native)
    
    
    
    # (b) GAM -----------------------
    print("GAM")
    
    # Including smoothing splines and until 4 degrees of freedom
    m_gam_clim_native <- mgcv::gam(formula = as.formula(paste(paste('occ ~', paste(paste0('s(',pred_sel_clim_native,',k=4)'),collapse=' + ')))), 
                               family = 'binomial', 
                               weights = weights,
                               data = species_occ_clim_native)
    

    
    
    # (c) RF -----------------------
    print("RF")
    
    # Including 1000 trees and resulting in 10 models based on the absence index
    m_rf_clim_native <- lapply(1:10, FUN=function(i) {randomForest(x = species_occ_clim_native[is.na(species_occ_clim_native$abs_index) | species_occ_clim_native$abs_index == i , pred_sel_clim_native],
                                                                   y = species_occ_clim_native[is.na(species_occ_clim_native$abs_index) | species_occ_clim_native$abs_index == i,]$occ, 
                                                                   ntree = 1000, nodesize = 20)})
    

    
    
    # (d) BRT -----------------------
    print("BRT")
    
    # Including a tree complexity of 2 and bag fraction of 0.75. Resulting in 10 models based on the absence index
    # Including automatic adaption of learning rate and tree numbers between 1000 and 5000
    m_brt_clim_native <- lapply(1:10, FUN=function(i) {
      opt.LR <- TRUE;
      LR <- 0.01;
      while(opt.LR){
        m.brt <- try(gbm.step(data = species_occ_clim_native[is.na(species_occ_clim_native$abs_index) | species_occ_clim_native$abs_index==i,], gbm.x = pred_sel_clim_native, gbm.y = "occ", family = 'bernoulli', tree.complexity = 2, bag.fraction = 0.75, learning.rate = LR, verbose=F, plot.main=F))
        if (class(m.brt) == "try-error" | class(m.brt) == "NULL"){
          LR <- LR/2
        } else
          if(m.brt$gbm.call$best.trees<1000){
            LR <- LR/2
          } else 
            if(m.brt$gbm.call$best.trees>5000){
              LR <- LR*2
            } else { 
              opt.LR <- FALSE}}; 
      return(m.brt)})
    

    
    
    # (e) ---------------------------
    
    # Save all models together
    save(m_glm_clim_native, m_gam_clim_native, m_rf_clim_native, m_brt_clim_native, file = paste0("output_data/models/native/clim/models_clim_native_",sp,".RData"))
    
    
    } else if (file_exists_models == TRUE) { print("already done model building")
    } # End of if condition
    
    
#-------------------------------------------------------------------------------
    
# 2. Model validation ---------------------------------------------------------- 
    
    # check if validation files already exist
    file_exists_validation <- file.exists(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
    
    if (file_exists_validation == FALSE) { # just continue with model validation if output 
    # with validation files does not exist yet
      
      print("start of model validation process")
      
      # Create a directory for each species to save R output plots
      dir.create(paste0("output_data/plots/response_plots/",sp))
      
      
      
      # (a) GLM -----------------------
      print("GLM")
      
      # 5-fold cross-validation
      preds_glm_cv_clim_native <- crossval_glm(m_glm_clim_native, traindat = species_occ_clim_native[,c('occ', pred_sel_clim_native)], colname_species = 'occ', colname_pred = pred_sel_clim_native, weights = weights)
      
      # Calculation of performance metrics
      perf_glm_clim_native <- evalSDM(species_occ_clim_native$occ, preds_glm_cv_clim_native)
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/GLM_clim_native_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_glm_clim_native, predictors = species_occ_clim_native[,pred_sel_clim_native], main='GLM')
      dev.off()
      
      
      
      # (b) GAM -----------------------
      print("GAM")
      
      # 5-fold cross-validation
      preds_gam_cv_clim_native <- crossvalSDM(m_gam_clim_native, traindat = species_occ_clim_native[,c('occ', pred_sel_clim_native)], colname_species = 'occ', colname_pred = pred_sel_clim_native, weights = weights)
      
      # Calculation of performance metrics
      perf_gam_clim_native <- evalSDM(species_occ_clim_native$occ, preds_gam_cv_clim_native)
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/GAM_clim_native_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_gam_clim_native, predictors = species_occ_clim_native[,pred_sel_clim_native], main='GAM')
      dev.off()
      
      
      
      # (c) RF -----------------------
      print("RF")
      
      # 5-fold cross-validation (run through each of the 10 resulted models)
      preds_rf_cv_clim_native_all <- lapply(1:10,FUN=function(i){crossvalSDM(m_rf_clim_native[[i]], traindat = species_occ_clim_native[,c('occ', pred_sel_clim_native)],
                                                                             colname_species = 'occ', colname_pred = pred_sel_clim_native)})
      
      # Calculation of performance metrics for each of the 10 cross-validated predictions
      perf_rf_clim_native <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_clim_native$occ, preds_rf_cv_clim_native_all[[i]])}))
      
      # Calculate the mean of the 10 model performance metrics
      perf_rf_clim_native <- colMeans(perf_rf_clim_native)
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/RF_clim_native_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_rf_clim_native[[1]], predictors = species_occ_clim_native[,pred_sel_clim_native], main='RF')
      dev.off()
      
      
      
      # (d) BRT -----------------------
      print("BRT")
      
      # 5-fold cross-validation (run through each of the 10 resulted models)
      preds_brt_cv_clim_native_all <- lapply(1:10,FUN=function(i){x <- crossvalSDM(m_brt_clim_native[[i]], traindat = species_occ_clim_native[,c('occ', pred_sel_clim_native)],
                                                                                   colname_species = 'occ', colname_pred = pred_sel_clim_native)})
      
      # Calculation of performance metrics for each of the 10 cross-validated predictions
      perf_brt_clim_native <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_clim_native$occ, preds_brt_cv_clim_native_all[[i]])}))
      
      # Calculate the mean of the 10 model performance metrics
      perf_brt_clim_native <- colMeans(perf_brt_clim_native)
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/BRT_clim_native_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_brt_clim_native[[1]], predictors = species_occ_clim_native[,pred_sel_clim_native], main='BRT')
      dev.off()
      
      
      
      # (e) Summarize performance metrics  -------------------------------------
      comp_perf_clim_native <- rbind(glm = perf_glm_clim_native, gam = perf_gam_clim_native, rf = perf_rf_clim_native, brt = perf_brt_clim_native)
      
      # Add a column containing the names of the algorithm
      comp_perf_clim_native <- data.frame(alg=row.names(comp_perf_clim_native),comp_perf_clim_native)
      
      
      
      # (f) Ensemble performance metrics  --------------------------------------
      
      # Calculate the mean of the cross-validated predictions all 10 rf models
      preds_rf_cv_clim_native_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_rf_cv_clim_native_all[[i]])}))
      preds_rf_cv_clim_native <- rowMeans(preds_rf_cv_clim_native_list)
      
      # Calculate the mean of the cross-validated predictions all 10 brt models
      preds_brt_cv_clim_native_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_brt_cv_clim_native_all[[i]])}))
      preds_brt_cv_clim_native <- rowMeans(preds_brt_cv_clim_native_list)
      
      # Combine predictions from all algorithms into one data frame
      preds_all_clim_native <- data.frame(glm = preds_glm_cv_clim_native, gam = preds_gam_cv_clim_native, rf = preds_rf_cv_clim_native, brt = preds_brt_cv_clim_native)
      
      # Get the binary predictions of all algorithms (using the "thresh" value for thresholding)
      binpred_all_clim_native <- sapply(names(preds_all_clim_native), 
                                        FUN=function(alg){
                                            ifelse(preds_all_clim_native[,alg] >= comp_perf_clim_native[comp_perf_clim_native$alg==alg,'thresh'],1,0)
                                            }
                                        )
      
      
      # Calculate the predictions for each row
      preds_mean_clim_native <- rowMeans(preds_all_clim_native) # using the mean
      preds_median_clim_native <- apply(preds_all_clim_native, 1, median) # using the median
      preds_wmean_clim_native <- apply(preds_all_clim_native, 1, weighted.mean, w=comp_perf_clim_native[, "TSS"]) # using the weighted mean
      preds_comav_clim_native <- rowSums(binpred_all_clim_native)/ncol(binpred_all_clim_native) # using the committee average
      
      # Calculate ensemble performance metrics
      ensemble_perf_mean_clim_native <- evalSDM(species_occ_clim_native$occ, preds_mean_clim_native)
      ensemble_perf_median_clim_native <- evalSDM(species_occ_clim_native$occ, preds_median_clim_native)
      ensemble_perf_wmean_clim_native <- evalSDM(species_occ_clim_native$occ, preds_wmean_clim_native)
      ensemble_perf_comav_clim_native <- evalSDM(species_occ_clim_native$occ, preds_comav_clim_native)
      
      # Combine ensemble performances into one data frame
      ensemble_perf_clim_native <- rbind(mean_prob = ensemble_perf_mean_clim_native, median_prob = ensemble_perf_median_clim_native, wmean_prob = ensemble_perf_wmean_clim_native,
                                         committee_av = ensemble_perf_comav_clim_native)
      
      # Add a column containing the names of the ensemble options
      ensemble_perf_clim_native <- data.frame(ens=row.names(ensemble_perf_clim_native),ensemble_perf_clim_native)
      
      
      
      # (g) Save validation outputs  -------------------------------------------
      
      save(comp_perf_clim_native, ensemble_perf_clim_native, file = paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
      
      
      
    } else if (file_exists_validation == TRUE) { print("already done model validation")
    } # End of if condition
    
    
})} # end of try and for loop over species
    


