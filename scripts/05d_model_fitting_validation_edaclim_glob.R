# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             05d. Model fitting and validation based on global          #
#             occurrences and combined climatic and edaphic data         #
# ---------------------------------------------------------------------- #

# Load needed packages
library(mgcv)
library(randomForest)
library(gbm)
library(dismo)
library(PresenceAbsence)
library(ecospat)

# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
source("scripts/functions.R") # partial_response,cross-validation and evaluation metrics function

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 



#-------------------------------------------------------------------------------

# 1. Model fitting -------------------------------------------------------------

for (sp in study_species) { # Start the loop over all species
  try({
    
    print(sp)
    
    print("edaclim")
    
    # check if models already exist
    file_exists_models <- file.exists(paste0("output_data/models/global/edaclim/models_edaclim_global_",sp,".RData"))
    
    if (file_exists_models == FALSE) { # just continue with model fitting if output 
    # with models does not exist yet
      
      print("start of model building process")
      
      # Load needed objects of species and environmental data
      load(paste0("output_data/distribution_env_data_subset/global/edaclim/species_occ_edaclim_global_",sp,".RData")) # distribution and environmental data
      load(paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData")) # predictor variables
      
      
      # Create an absence index for machine learning algorithm to achieve even 
      # presence and absence data sets for machine learning algorithms
      species_occ_edaclim_global$abs_index <- NA
      number_absences <- sum(species_occ_edaclim_global$occ != 1)
      values_absences <- rep(1:10, length.out = number_absences)
      values_sample <- sample(values_absences)
      species_occ_edaclim_global$abs_index[species_occ_edaclim_global$occ != 1] <- values_sample
      
      # Calculate same weights for presences and absences for regression based algorithms
      weights <- ifelse(species_occ_edaclim_global$occ==1, 1, sum(species_occ_edaclim_global$occ==1) / sum(species_occ_edaclim_global$occ==0))
      
      
      
      # (a) GLM -----------------------
      print("GLM")
      
      # Including linear and quadratic terms
      m_glm_edaclim_global <- glm(formula = as.formula(paste('occ ~',paste(pred_sel_edaclim_global,paste0('+ I(', pred_sel_edaclim_global ,'^2)'), collapse='+'))),
                                  family = "binomial", 
                                  weights = weights, 
                                  data = species_occ_edaclim_global)
      
      
      
      # (b) GAM -----------------------
      print("GAM")
      
      # Including smoothing splines and until 4 degrees of freedom
      m_gam_edaclim_global <- mgcv::gam(formula = as.formula(paste(paste('occ ~', paste(paste0('s(',pred_sel_edaclim_global,',k=4)'),collapse=' + ')))), 
                                        family = 'binomial', 
                                        weights = weights,
                                        data = species_occ_edaclim_global)
      
      
      
      
      # (c) RF -----------------------
      print("RF")
      
      # Including 1000 trees and resulting in 10 models based on the absence index
      m_rf_edaclim_global <- lapply(1:10, FUN=function(i) {randomForest(x = species_occ_edaclim_global[is.na(species_occ_edaclim_global$abs_index) | species_occ_edaclim_global$abs_index == i , pred_sel_edaclim_global],
                                                                        y = species_occ_edaclim_global[is.na(species_occ_edaclim_global$abs_index) | species_occ_edaclim_global$abs_index == i,]$occ, 
                                                                        ntree = 1000, nodesize = 20)})
      
      
      
      
      # (d) BRT -----------------------
      print("BRT")
      
      # Including a tree complexity of 2 and bag fraction of 0.75. Resulting in 10 models based on the absence index
      # Including automatic adaption of learning rate and tree numbers between 1000 and 5000
      m_brt_edaclim_global <- lapply(1:10, FUN=function(i) {
        opt.LR <- TRUE;
        LR <- 0.01;
        while(opt.LR){
          m.brt <- try(gbm.step(data = species_occ_edaclim_global[is.na(species_occ_edaclim_global$abs_index) | species_occ_edaclim_global$abs_index==i,], gbm.x = pred_sel_edaclim_global, gbm.y = "occ", family = 'bernoulli', tree.complexity = 2, bag.fraction = 0.75, learning.rate = LR, verbose=F, plot.main=F))
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
      save(m_glm_edaclim_global, m_gam_edaclim_global, m_rf_edaclim_global, m_brt_edaclim_global, file = paste0("output_data/models/global/edaclim/models_edaclim_global_",sp,".RData"))
      
      
    } else if (file_exists_models == TRUE) { print("already done model building")
    } # End of if condition
    
    
#-------------------------------------------------------------------------------
    
# 2. Model validation ---------------------------------------------------------- 
    
    # check if validation files already exist
    file_exists_validation <- file.exists(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
    
    if (file_exists_validation == FALSE) { # just continue with model validation if output 
    # with validation files does not exist yet
      
      print("start of model validation process")
      
      # Create a directory for each species to save R output plots
      dir.create(paste0("output_data/plots/response_plots/",sp))
      
      
      
      # (a) GLM -----------------------
      print("GLM")
      
      # 5-fold cross-validation
      preds_glm_cv_edaclim_global <- crossval_glm(m_glm_edaclim_global, traindat = species_occ_edaclim_global[,c('occ', pred_sel_edaclim_global)], colname_species = 'occ', colname_pred = pred_sel_edaclim_global, weights = weights)
      
      # Calculation of performance metrics
      perf_glm_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_glm_cv_edaclim_global)
      
      # Calculation of the Boyce index
      presences_indices <- which(species_occ_edaclim_global$occ == 1) # Extract the indices of the presences
      preds_glm_cv_edaclim_global_presences <- preds_glm_cv_edaclim_global[presences_indices] # Just retain the predictions of the presences based on indices
      
      boyce_index_glm_edaclim_global <- ecospat.boyce(fit = preds_glm_cv_edaclim_global, obs = preds_glm_cv_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_glm_edaclim_global <- boyce_index_glm_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      perf_glm_edaclim_global$Boyce <- boyce_index_glm_edaclim_global
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/GLM_edaclim_global_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_glm_edaclim_global, predictors = species_occ_edaclim_global[,pred_sel_edaclim_global], main='GLM')
      dev.off()
      
      
      
      # (b) GAM -----------------------
      print("GAM")
      
      # 5-fold cross-validation
      preds_gam_cv_edaclim_global <- crossvalSDM(m_gam_edaclim_global, traindat = species_occ_edaclim_global[,c('occ', pred_sel_edaclim_global)], colname_species = 'occ', colname_pred = pred_sel_edaclim_global, weights = weights)
      
      # Calculation of performance metrics
      perf_gam_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_gam_cv_edaclim_global)
      
      # Calculation of the Boyce index
      presences_indices <- which(species_occ_edaclim_global$occ == 1) # Extract the indices of the presences
      preds_gam_cv_edaclim_global_presences <- preds_gam_cv_edaclim_global[presences_indices] # Just retain the predictions of the presences based on indices
      
      boyce_index_gam_edaclim_global <- ecospat.boyce(fit = preds_gam_cv_edaclim_global, obs = preds_gam_cv_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_gam_edaclim_global <- boyce_index_gam_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      perf_gam_edaclim_global$Boyce <- boyce_index_gam_edaclim_global
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/GAM_edaclim_global_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_gam_edaclim_global, predictors = species_occ_edaclim_global[,pred_sel_edaclim_global], main='GAM')
      dev.off()
      
      
      
      # (c) RF -----------------------
      print("RF")
      
      # 5-fold cross-validation (run through each of the 10 resulted models)
      preds_rf_cv_edaclim_global_all <- lapply(1:10,FUN=function(i){crossvalSDM(m_rf_edaclim_global[[i]], traindat = species_occ_edaclim_global[,c('occ', pred_sel_edaclim_global)],
                                                                                colname_species = 'occ', colname_pred = pred_sel_edaclim_global)})
      
      # Calculation of performance metrics for each of the 10 cross-validated predictions
      perf_rf_edaclim_global <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_edaclim_global$occ, preds_rf_cv_edaclim_global_all[[i]])}))
      
      # Calculate the mean of the 10 model performance metrics
      perf_rf_edaclim_global <- colMeans(perf_rf_edaclim_global)
      
      # Calculation of the Boyce index
      presences_indices <- which(species_occ_edaclim_global$occ == 1)
      preds_rf_cv_edaclim_global_presences <- do.call("cbind", lapply(1:10,FUN=function(i){preds_rf_cv_edaclim_global_all[[i]][presences_indices]}))
      
      boyce_index_rf_edaclim_global <- do.call("rbind", lapply(1:10,FUN=function(i){boyce_index_rf_edaclim_global_all <- ecospat.boyce(fit = preds_rf_cv_edaclim_global_all[[i]], obs = preds_rf_cv_edaclim_global_presences[,i],
                                                                                                                                       nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                                                                                       rm.duplicate = TRUE, method = 'kendall')
                                                                                    boyce_index_rf_edaclim_global_all$cor }))
      boyce_index_rf_edaclim_global <- colMeans(boyce_index_rf_edaclim_global)
      
      # Add the Boyce index to the performance metrics data frame
      perf_rf_edaclim_global["Boyce"] <- boyce_index_rf_edaclim_global
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/RF_edaclim_global_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_rf_edaclim_global[[1]], predictors = species_occ_edaclim_global[,pred_sel_edaclim_global], main='RF')
      dev.off()
      
      
      
      # (d) BRT -----------------------
      print("BRT")
      
      # 5-fold cross-validation (run through each of the 10 resulted models)
      preds_brt_cv_edaclim_global_all <- lapply(1:10,FUN=function(i){x <- crossvalSDM(m_brt_edaclim_global[[i]], traindat = species_occ_edaclim_global[,c('occ', pred_sel_edaclim_global)],
                                                                                      colname_species = 'occ', colname_pred = pred_sel_edaclim_global)})
      
      # Calculation of performance metrics for each of the 10 cross-validated predictions
      perf_brt_edaclim_global <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_edaclim_global$occ, preds_brt_cv_edaclim_global_all[[i]])}))
      
      # Calculate the mean of the 10 model performance metrics
      perf_brt_edaclim_global <- colMeans(perf_brt_edaclim_global)
      
      # Calculation of the Boyce index
      presences_indices <- which(species_occ_edaclim_global$occ == 1)
      preds_brt_cv_edaclim_global_presences <- do.call("cbind", lapply(1:10,FUN=function(i){preds_brt_cv_edaclim_global_all[[i]][presences_indices]}))
      
      boyce_index_brt_edaclim_global <- do.call("rbind", lapply(1:10,FUN=function(i){boyce_index_brt_edaclim_global_all <- ecospat.boyce(fit = preds_brt_cv_edaclim_global_all[[i]], obs = preds_brt_cv_edaclim_global_presences[,i],
                                                                                                                                         nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                                                                                         rm.duplicate = TRUE, method = 'kendall')
                                                                                     boyce_index_brt_edaclim_global_all$cor }))
      boyce_index_brt_edaclim_global <- colMeans(boyce_index_brt_edaclim_global)
      
      # Add the Boyce index to the performance metrics data frame
      perf_brt_edaclim_global["Boyce"] <- boyce_index_brt_edaclim_global
      
      # Plot partial response curves and save them
      svg(paste0("output_data/plots/response_plots/",sp,"/BRT_edaclim_global_",sp,".svg"))
      par(mfrow=c(2,2)) 
      partial_response(m_brt_edaclim_global[[1]], predictors = species_occ_edaclim_global[,pred_sel_edaclim_global], main='BRT')
      dev.off()
      
      
      
      # (e) Summarize performance metrics  -------------------------------------
      comp_perf_edaclim_global <- rbind(glm = perf_glm_edaclim_global, gam = perf_gam_edaclim_global, rf = perf_rf_edaclim_global, brt = perf_brt_edaclim_global)
      
      # Add a column containing the names of the algorithm
      comp_perf_edaclim_global <- data.frame(alg=row.names(comp_perf_edaclim_global),comp_perf_edaclim_global)
      
      
      
      # (f) Ensemble performance metrics  --------------------------------------
      
      # Calculate the mean of the cross-validated predictions all 10 rf models
      preds_rf_cv_edaclim_global_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_rf_cv_edaclim_global_all[[i]])}))
      preds_rf_cv_edaclim_global <- rowMeans(preds_rf_cv_edaclim_global_list)
      
      # Calculate the mean of the cross-validated predictions all 10 brt models
      preds_brt_cv_edaclim_global_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_brt_cv_edaclim_global_all[[i]])}))
      preds_brt_cv_edaclim_global <- rowMeans(preds_brt_cv_edaclim_global_list)
      
      # Combine predictions from all algorithms into one data frame
      preds_all_edaclim_global <- data.frame(glm = preds_glm_cv_edaclim_global, gam = preds_gam_cv_edaclim_global, rf = preds_rf_cv_edaclim_global, brt = preds_brt_cv_edaclim_global)
      
      # Get the binary predictions of all algorithms (using the MaxSSS "thresh" value for thresholding)
      binpred_all_edaclim_global <- sapply(names(preds_all_edaclim_global), 
                                           FUN=function(alg){
                                             ifelse(preds_all_edaclim_global[,alg] >= comp_perf_edaclim_global[comp_perf_edaclim_global$alg==alg,'thresh'],1,0)
                                           }
                                    )
      
      # Calculate the predictions for each row
      preds_mean_edaclim_global <- rowMeans(preds_all_edaclim_global) # using the mean
      preds_median_edaclim_global <- apply(preds_all_edaclim_global, 1, median) # using the median
      preds_wmean_edaclim_global <- apply(preds_all_edaclim_global, 1, weighted.mean, w=comp_perf_edaclim_global[names(preds_all_edaclim_global), "TSS"]) # using the weighted mean
      preds_comav_edaclim_global <- rowSums(binpred_all_edaclim_global)/ncol(binpred_all_edaclim_global) # using the committee average
      
      # Calculate ensemble performance metrics
      ensemble_perf_mean_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_mean_edaclim_global)
      ensemble_perf_median_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_median_edaclim_global)
      ensemble_perf_wmean_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_wmean_edaclim_global)
      ensemble_perf_comav_edaclim_global <- evalSDM(species_occ_edaclim_global$occ, preds_comav_edaclim_global)
      
      # Combine ensemble performances into one data frame
      ensemble_perf_edaclim_global <- rbind(mean_prob = ensemble_perf_mean_edaclim_global, median_prob = ensemble_perf_median_edaclim_global, wmean_prob = ensemble_perf_wmean_edaclim_global,
                                            committee_av = ensemble_perf_comav_edaclim_global)
      
      # Calculate the Boyce index for the ensemble
      presences_indices <- which(species_occ_edaclim_global$occ == 1)
      preds_mean_edaclim_global_presences <- preds_mean_edaclim_global[presences_indices]
      preds_median_edaclim_global_presences <- preds_median_edaclim_global[presences_indices]
      preds_wmean_edaclim_global_presences <- preds_wmean_edaclim_global[presences_indices]
      preds_comav_edaclim_global_presences <- preds_comav_edaclim_global[presences_indices]
      
      boyce_index_ensemble_edaclim_global_mean <- ecospat.boyce(fit = preds_mean_edaclim_global, obs = preds_mean_edaclim_global_presences,
                                                                nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ensemble_edaclim_global_median <- ecospat.boyce(fit = preds_median_edaclim_global, obs = preds_median_edaclim_global_presences,
                                                                  nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                  rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ensemble_edaclim_global_wmean <- ecospat.boyce(fit = preds_wmean_edaclim_global, obs = preds_wmean_edaclim_global_presences,
                                                                 nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                 rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ensemble_edaclim_global_comav <- ecospat.boyce(fit = preds_comav_edaclim_global, obs = preds_comav_edaclim_global_presences,
                                                                 nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                                 rm.duplicate = TRUE, method = 'kendall')
      
      
      ensemble_perf_boyce_edaclim_global <- data.frame(Boyce = c(boyce_index_ensemble_edaclim_global_mean$cor, boyce_index_ensemble_edaclim_global_median$cor,
                                                                 boyce_index_ensemble_edaclim_global_wmean$cor, boyce_index_ensemble_edaclim_global_comav$cor))
      
      
      # Add the Boyce index to the other performance metrics
      ensemble_perf_edaclim_global <- cbind(ensemble_perf_edaclim_global, ensemble_perf_boyce_edaclim_global)
      
      # Add a column containing the names of the ensemble options
      ensemble_perf_edaclim_global <- data.frame(ens=row.names(ensemble_perf_edaclim_global),ensemble_perf_edaclim_global)
      
      
      
      # (g) Save validation outputs  -------------------------------------------
      
      save(comp_perf_edaclim_global, ensemble_perf_edaclim_global, file = paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
      
      
      
    } else if (file_exists_validation == TRUE) { print("already done model validation")
    } # End of if condition
    
    
})} # end of try and for loop over species

gc()
rm(list=ls())