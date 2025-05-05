# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             05c. Model fitting and validation based on                 #
#             global occurrences and purely climatic data                #
# ---------------------------------------------------------------------- #

# Load needed packages
library(mgcv)
library(randomForest)
library(gbm)
library(dismo)
library(PresenceAbsence)
library(ecospat)
library(doParallel)
library(foreach)

# register cores for parallel computation:
no_cores <- 3
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# Define directory path for cluster usage
dir <- file.path("/import", "ecoc9z", "data-zurell", "holle", "Holle_PacificPlantInvaders_BlacklistUncertainty_2023")


# Load needed objects
load(paste0(dir, "/input_data/occ_numbers_thinned_env_filtered.RData")) # Contains names of study species
source(paste0(dir, "/scripts/00_functions.R")) # Evaluation metrics function

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 



#-------------------------------------------------------------------------------

# 1. Model fitting -------------------------------------------------------------

foreach(sp = study_species, .packages = c("mgcv", "randomForest", "gbm", "dismo", "PresenceAbsence", "ecospat")) %dopar% { # Start the loop over all species
  try({
    
    prog_log_file <- file(file.path(dir, "output_data", "models_rev", "model_fitting_validation_progress_clim_global.txt"), open = "at") # write console output here
    
    # check if models already exist
    file_exists_models <- file.exists(paste0(dir, "/output_data/models_rev/global/clim/models_clim_global_",sp,".RData"))
    
    if (file_exists_models == FALSE) { # just continue with model fitting if output 
    # with models does not exist yet
      
      cat(paste0(Sys.time(), " - Starting model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Load needed objects of species and environmental data
      load(paste0(dir, "/output_data/distribution_env_data_subset/global/clim/species_occ_clim_global_",sp,".RData")) # distribution and environmental data
      load(paste0(dir, "/output_data/variable_selection/global/clim/pred_sel_clim_global_",sp,".RData")) # predictor variables
      
      # Create an absence index for machine learning algorithm to achieve even 
      # presence and absence data sets for machine learning algorithms
      species_occ_clim_global$abs_index <- NA
      number_absences <- sum(species_occ_clim_global$occ != 1)
      values_absences <- rep(1:10, length.out = number_absences)
      values_sample <- sample(values_absences)
      species_occ_clim_global$abs_index[species_occ_clim_global$occ != 1] <- values_sample
      
      # Calculate same weights for presences and absences for regression based algorithms
      weights <- ifelse(species_occ_clim_global$occ==1, 1, sum(species_occ_clim_global$occ==1) / sum(species_occ_clim_global$occ==0))
      
      # Create a vector with indices containing presence and absences in occurrence
      # data frame
      presences_indices <- which(species_occ_clim_global$occ == 1)
      absences_indices <- which(species_occ_clim_global$occ == 0)
      
      
      
      # (a) GLM -----------------------
      cat(paste0(Sys.time(), " - Starting to fit GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including linear and quadratic terms
      m_glm_clim_global <- glm(formula = as.formula(paste('occ ~',paste(pred_sel_clim_global,paste0('+ I(', pred_sel_clim_global ,'^2)'), collapse='+'))),
                               family = "binomial", 
                               weights = weights, 
                               data = species_occ_clim_global)
      
      
      
      # (b) GAM -----------------------
      cat(paste0(Sys.time(), " - Starting to fit GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including smoothing splines and until 4 degrees of freedom
      m_gam_clim_global <- mgcv::gam(formula = as.formula(paste(paste('occ ~', paste(paste0('s(',pred_sel_clim_global,',k=4)'),collapse=' + ')))), 
                                     family = 'binomial', 
                                     weights = weights,
                                     data = species_occ_clim_global)
      
      
      
      
      # (c) RF -----------------------
      cat(paste0(Sys.time(), " - Starting to fit RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including 1000 trees and resulting in 10 models based on the absence index
      m_rf_clim_global <- lapply(1:10, FUN=function(i){sp_train <- species_occ_clim_global[c(presences_indices, which(species_occ_clim_global$abs_index == i)),]; print(i); randomForest(as.formula(paste('occ~',paste(pred_sel_clim_global, collapse='+'))), 
                                                                                                                                                                                         data = sp_train, ntree = 1000, nodesize = 20, importance = T)})
      
      
      
      
      
      # (d) BRT -----------------------
      cat(paste0(Sys.time(), " - Starting to fit BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including a tree complexity of 2 and bag fraction of 0.75. Resulting in 10 models based on the absence index
      # Including automatic adaption of learning rate and tree numbers between 1000 and 5000
      m_brt_clim_global <- lapply(1:10, FUN=function(i) {
        print(i);
        opt.LR <- TRUE;
        LR <- 0.01;
        while(opt.LR){
          m.brt <- try(gbm.step(data = species_occ_clim_global[c(presences_indices, which(species_occ_clim_global$abs_index == i)),], gbm.x = pred_sel_clim_global, gbm.y = "occ", family = 'bernoulli', tree.complexity = 2, bag.fraction = 0.75, learning.rate = LR, verbose=F, plot.main=F))
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
      save(m_glm_clim_global, m_gam_clim_global, m_rf_clim_global, m_brt_clim_global, species_occ_clim_global, weights, file = paste0(dir, "/output_data/models_rev/global/clim/models_clim_global_",sp,".RData"))
      
      
    } else if (file_exists_models == TRUE) { cat(paste0(Sys.time(), " - Already done model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
#-------------------------------------------------------------------------------
    
# 2. Model validation ---------------------------------------------------------- 
    
    # check if validation files already exist
    file_exists_validation <- file.exists(paste0(dir, "/output_data/validation_rev/global/clim/validation_clim_global_",sp,".RData"))
    
    if (file_exists_validation == FALSE) { # just continue with model validation if output 
    # with validation files does not exist yet
      
      cat(paste0(Sys.time(), " - Starting the model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Load the fitted models for the species, their predictors, and distribution dataset
      load(paste0(dir, "/output_data/models_rev/global/clim/models_clim_global_",sp,".RData"))
      load(paste0(dir, "/output_data/variable_selection/global/clim/pred_sel_clim_global_",sp,".RData"))
      
      # Model validation - using a 5-fold random cross-validation
      kfolds <- 5
      
      # Make folds for entire data set with thinned species occurrences
      crossval_folds <- dismo::kfold(seq_len(nrow(species_occ_clim_global)), k = kfolds)
      
      # Create a vector with indices containing presence and absences in occurrence
      # data frame
      presences_indices <- which(species_occ_clim_global$occ == 1)
      absences_indices <- which(species_occ_clim_global$occ == 0)
      
      
      # (a) GLM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_glm_preds_cv <- rep(NA, nrow(species_occ_clim_global))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_clim_global[crossval_folds!=i,]
        cv_test <- species_occ_clim_global[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_glm <- update(m_glm_clim_global, data=cv_train, weights=cv_weights)
        m_glm_preds_cv[crossval_folds==i] <- predict(cv_glm, cv_test, type='response')
      }
      
      # Start preparations for model validation
      m_glm_preds_cv_clim_global_presences <- m_glm_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_glm_preds_cv_clim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_glm_perf_cv_maxTSS <- evalSDM(species_occ_clim_global$occ, m_glm_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_glm_perf_cv_meanProb <- evalSDM(species_occ_clim_global$occ, m_glm_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_glm_perf_cv_tenthPer <- evalSDM(species_occ_clim_global$occ, m_glm_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_glm_clim_global <- ecospat.boyce(fit = m_glm_preds_cv, obs = m_glm_preds_cv_clim_global_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_glm_clim_global <- boyce_index_glm_clim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_glm_perf_cv_maxTSS$Boyce <- boyce_index_glm_clim_global
      m_glm_perf_cv_meanProb$Boyce <- boyce_index_glm_clim_global
      m_glm_perf_cv_tenthPer$Boyce <- boyce_index_glm_clim_global
      
      
      
      
      # (b) GAM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_gam_preds_cv <- rep(NA, nrow(species_occ_clim_global))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_clim_global[crossval_folds!=i,]
        cv_test <- species_occ_clim_global[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_gam <- update(m_gam_clim_global, data=cv_train, weights=cv_weights)
        m_gam_preds_cv[crossval_folds==i] <- predict(cv_gam, cv_test, type='response')
      }
      
      # Start preparations for model validation
      m_gam_preds_cv_clim_global_presences <- m_gam_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_gam_preds_cv_clim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_gam_perf_cv_maxTSS <- evalSDM(species_occ_clim_global$occ, m_gam_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_gam_perf_cv_meanProb <- evalSDM(species_occ_clim_global$occ, m_gam_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_gam_perf_cv_tenthPer <- evalSDM(species_occ_clim_global$occ, m_gam_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_gam_clim_global <- ecospat.boyce(fit = m_gam_preds_cv, obs = m_gam_preds_cv_clim_global_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_gam_clim_global <- boyce_index_gam_clim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_gam_perf_cv_maxTSS$Boyce <- boyce_index_gam_clim_global
      m_gam_perf_cv_meanProb$Boyce <- boyce_index_gam_clim_global
      m_gam_perf_cv_tenthPer$Boyce <- boyce_index_gam_clim_global
      
      
      
      # (c) RF -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_rf_predmat_cv <- matrix(nrow=nrow(species_occ_clim_global), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_clim_global$abs_index == m))
          
          cv_train <- species_occ_clim_global[hold_in[hold_in %in% dat_tenth],]
          cv_test <- species_occ_clim_global[hold_out,]
          
          cv_rf <- update(m_rf_clim_global[[m]], data=cv_train)
          m_rf_predmat_cv[hold_out,m] <- predict(cv_rf, cv_test, type='response')
        }
      }
      
      m_rf_preds_cv <- rowMeans(m_rf_predmat_cv)
      
      # Start preparations for model validation
      m_rf_preds_cv_clim_global_presences <- m_rf_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_rf_preds_cv_clim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_rf_perf_cv_maxTSS <- evalSDM(species_occ_clim_global$occ, m_rf_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_rf_perf_cv_meanProb <- evalSDM(species_occ_clim_global$occ, m_rf_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_rf_perf_cv_tenthPer <- evalSDM(species_occ_clim_global$occ, m_rf_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_rf_clim_global <- ecospat.boyce(fit = m_rf_preds_cv, obs = m_rf_preds_cv_clim_global_presences,
                                                  nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                  rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_rf_clim_global <- boyce_index_rf_clim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_rf_perf_cv_maxTSS$Boyce <- boyce_index_rf_clim_global
      m_rf_perf_cv_meanProb$Boyce <- boyce_index_rf_clim_global
      m_rf_perf_cv_tenthPer$Boyce <- boyce_index_rf_clim_global
      
      
      
      # (d) BRT -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_brt_predmat_cv <- matrix(nrow=nrow(species_occ_clim_global), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_clim_global$abs_index == m))
          
          cv_train <- species_occ_clim_global[hold_in[hold_in %in% dat_tenth],]
          names(cv_train)[names(cv_train)=='occ'] <- m_brt_clim_global[[m]]$response.name
          cv_test <- species_occ_clim_global[hold_out,]
          
          cv_brt <- gbm::gbm(m_brt_clim_global[[m]]$call, 'bernoulli', data = cv_train[,c( m_brt_clim_global[[m]]$response.name, pred_sel_clim_global)], 
                             n.trees=m_brt_clim_global[[m]]$gbm.call$best.trees, 
                             shrinkage=m_brt_clim_global[[m]]$gbm.call$learning.rate, 
                             bag.fraction=m_brt_clim_global[[m]]$gbm.call$bag.fraction, 
                             interaction.depth=m_brt_clim_global[[m]]$gbm.call$tree.complexity)
          m_brt_predmat_cv[hold_out,m] <- predict(cv_brt, cv_test, type='response', n.trees=m_brt_clim_global[[m]]$gbm.call$best.trees)
        }
      }
      
      m_brt_preds_cv <- rowMeans(m_brt_predmat_cv)
      
      # Start preparations for model validation
      m_brt_preds_cv_clim_global_presences <- m_brt_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_brt_preds_cv_clim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_brt_perf_cv_maxTSS <- evalSDM(species_occ_clim_global$occ, m_brt_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_brt_perf_cv_meanProb <- evalSDM(species_occ_clim_global$occ, m_brt_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_brt_perf_cv_tenthPer <- evalSDM(species_occ_clim_global$occ, m_brt_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_brt_clim_global <- ecospat.boyce(fit = m_brt_preds_cv, obs = m_brt_preds_cv_clim_global_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_brt_clim_global <- boyce_index_brt_clim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_brt_perf_cv_maxTSS$Boyce <- boyce_index_brt_clim_global
      m_brt_perf_cv_meanProb$Boyce <- boyce_index_brt_clim_global
      m_brt_perf_cv_tenthPer$Boyce <- boyce_index_brt_clim_global
      
      
      
      # (e) Summarize performance metrics  -------------------------------------
      # for each algorithm and threshold
      comp_perf_clim_global_maxTSS <- rbind(glm = m_glm_perf_cv_maxTSS, gam = m_gam_perf_cv_maxTSS, rf = m_rf_perf_cv_maxTSS, brt = m_brt_perf_cv_maxTSS)
      comp_perf_clim_global_maxTSS <- data.frame(alg=row.names(comp_perf_clim_global_maxTSS),comp_perf_clim_global_maxTSS) # Add a column containing the names of the algorithm
      
      comp_perf_clim_global_meanProb <- rbind(glm = m_glm_perf_cv_meanProb, gam = m_gam_perf_cv_meanProb, rf = m_rf_perf_cv_meanProb, brt = m_brt_perf_cv_meanProb)
      comp_perf_clim_global_meanProb <- data.frame(alg=row.names(comp_perf_clim_global_meanProb),comp_perf_clim_global_meanProb)
      
      comp_perf_clim_global_tenthPer <- rbind(glm = m_glm_perf_cv_tenthPer, gam = m_gam_perf_cv_tenthPer, rf = m_rf_perf_cv_tenthPer, brt = m_brt_perf_cv_tenthPer)
      comp_perf_clim_global_tenthPer <- data.frame(alg=row.names(comp_perf_clim_global_tenthPer),comp_perf_clim_global_tenthPer)
      
      
      
      # (f) Ensemble performance metrics  --------------------------------------
      cat(paste0(Sys.time(), " - Starting to validate ensemble for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Combine predictions from all algorithms into one data frame
      preds_cv_all_clim_global <- data.frame(glm = m_glm_preds_cv, gam = m_gam_preds_cv,  rf = m_rf_preds_cv, brt = m_brt_preds_cv)
      
      # Calculate cross-validated ensemble predictions (mean)
      preds_cv_ens_clim_global <- rowMeans(preds_cv_all_clim_global) # using the mean
      
      # Start preparations for model validation
      preds_cv_ens_clim_global_presences <- preds_cv_ens_clim_global[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(preds_cv_ens_clim_global_presences, probs = 0.10)
      
      # Calculate ensemble performance metrics based on different thresholds
      ensemble_perf_clim_global_maxTSS <- evalSDM(species_occ_clim_global$occ, preds_cv_ens_clim_global, thresh.method = 'MaxSens+Spec', weigths = weights)
      ensemble_perf_clim_global_meanProb <- evalSDM(species_occ_clim_global$occ, preds_cv_ens_clim_global, thresh.method = 'MeanProb', weigths = weights)
      ensemble_perf_clim_global_tenthPer <- evalSDM(species_occ_clim_global$occ, preds_cv_ens_clim_global, thresh = thresh_tenthPer, weigths = weights)
      
      
      # Calculate the Boyce index of the ensemble
      boyce_index_ens_clim_global <- ecospat.boyce(fit = preds_cv_ens_clim_global, obs = preds_cv_ens_clim_global_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ens_clim_global <- boyce_index_ens_clim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      ensemble_perf_clim_global_maxTSS$Boyce <- boyce_index_ens_clim_global
      ensemble_perf_clim_global_meanProb$Boyce <- boyce_index_ens_clim_global
      ensemble_perf_clim_global_tenthPer$Boyce <- boyce_index_ens_clim_global
      
      # Comparing cross-validated ensemble model performance across thresholds:
      ensemble_perf_clim_global <- rbind(maxTSS = ensemble_perf_clim_global_maxTSS,
                                         meanProb = ensemble_perf_clim_global_meanProb,
                                         tenthPer = ensemble_perf_clim_global_tenthPer)
      
      
      
      
      
      
      
      # (g) Save validation outputs  -------------------------------------------
      
      save(m_glm_preds_cv, m_gam_preds_cv, m_brt_preds_cv, m_rf_preds_cv, preds_cv_ens_clim_global, crossval_folds, 
           comp_perf_clim_global_maxTSS, comp_perf_clim_global_meanProb, comp_perf_clim_global_tenthPer, 
           ensemble_perf_clim_global,  file = paste0(dir, "/output_data/validation_rev/global/clim/validation_clim_global_",sp,".RData"))
      
      
      
    } else if (file_exists_validation == TRUE) { cat(paste0(Sys.time(), " - Already done model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
    
  }) # End of try
} # End of foreach


stopCluster(cl)
gc()
rm(list=ls())



      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      # Create a directory for each species to save R output plots
#       dir.create(paste0("output_data/plots/response_plots/",sp))
#       
#       
#       
#       # (a) GLM -----------------------
#       print("GLM")
#       
#       # 5-fold cross-validation
#       preds_glm_cv_clim_global <- crossval_glm(m_glm_clim_global, traindat = species_occ_clim_global[,c('occ', pred_sel_clim_global)], colname_species = 'occ', colname_pred = pred_sel_clim_global, weights = weights)
#       
#       # Calculation of performance metrics
#       perf_glm_clim_global <- evalSDM(species_occ_clim_global$occ, preds_glm_cv_clim_global)
#       
#       # Calculation of the Boyce index
#       presences_indices <- which(species_occ_clim_global$occ == 1) # Extract the indices of the presences
#       preds_glm_cv_clim_global_presences <- preds_glm_cv_clim_global[presences_indices] # Just retain the predictions of the presences based on indices
#       
#       boyce_index_glm_clim_global <- ecospat.boyce(fit = preds_glm_cv_clim_global, obs = preds_glm_cv_clim_global_presences,
#                                                    nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                    rm.duplicate = TRUE, method = 'kendall')
#       
#       boyce_index_glm_clim_global <- boyce_index_glm_clim_global$cor # Extract correlation value (Boyce index)
#       
#       # Add the Boyce index to the performance metrics data frame
#       perf_glm_clim_global$Boyce <- boyce_index_glm_clim_global
#       
#       # Plot partial response curves and save them
#       svg(paste0("output_data/plots/response_plots/",sp,"/GLM_clim_global_",sp,".svg"))
#       par(mfrow=c(2,2)) 
#       partial_response(m_glm_clim_global, predictors = species_occ_clim_global[,pred_sel_clim_global], main='GLM')
#       dev.off()
#       
#       
#       
#       # (b) GAM -----------------------
#       print("GAM")
#       
#       # 5-fold cross-validation
#       preds_gam_cv_clim_global <- crossvalSDM(m_gam_clim_global, traindat = species_occ_clim_global[,c('occ', pred_sel_clim_global)], colname_species = 'occ', colname_pred = pred_sel_clim_global, weights = weights)
#       
#       # Calculation of performance metrics
#       perf_gam_clim_global <- evalSDM(species_occ_clim_global$occ, preds_gam_cv_clim_global)
#       
#       # Calculation of the Boyce index
#       presences_indices <- which(species_occ_clim_global$occ == 1) # Extract the indices of the presences
#       preds_gam_cv_clim_global_presences <- preds_gam_cv_clim_global[presences_indices] # Just retain the predictions of the presences based on indices
#       
#       boyce_index_gam_clim_global <- ecospat.boyce(fit = preds_gam_cv_clim_global, obs = preds_gam_cv_clim_global_presences,
#                                                    nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                    rm.duplicate = TRUE, method = 'kendall')
#       
#       boyce_index_gam_clim_global <- boyce_index_gam_clim_global$cor # Extract correlation value (Boyce index)
#       
#       # Add the Boyce index to the performance metrics data frame
#       perf_gam_clim_global$Boyce <- boyce_index_gam_clim_global
#       
#       # Plot partial response curves and save them
#       svg(paste0("output_data/plots/response_plots/",sp,"/GAM_clim_global_",sp,".svg"))
#       par(mfrow=c(2,2)) 
#       partial_response(m_gam_clim_global, predictors = species_occ_clim_global[,pred_sel_clim_global], main='GAM')
#       dev.off()
#       
#       
#       
#       # (c) RF -----------------------
#       print("RF")
#       
#       # 5-fold cross-validation (run through each of the 10 resulted models)
#       preds_rf_cv_clim_global_all <- lapply(1:10,FUN=function(i){crossvalSDM(m_rf_clim_global[[i]], traindat = species_occ_clim_global[,c('occ', pred_sel_clim_global)],
#                                                                              colname_species = 'occ', colname_pred = pred_sel_clim_global)})
#       
#       # Calculation of performance metrics for each of the 10 cross-validated predictions
#       perf_rf_clim_global <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_clim_global$occ, preds_rf_cv_clim_global_all[[i]])}))
#       
#       # Calculate the mean of the 10 model performance metrics
#       perf_rf_clim_global <- colMeans(perf_rf_clim_global)
#       
#       # Calculation of the Boyce index
#       presences_indices <- which(species_occ_clim_global$occ == 1)
#       preds_rf_cv_clim_global_presences <- do.call("cbind", lapply(1:10,FUN=function(i){preds_rf_cv_clim_global_all[[i]][presences_indices]}))
#       
#       boyce_index_rf_clim_global <- do.call("rbind", lapply(1:10,FUN=function(i){boyce_index_rf_clim_global_all <- ecospat.boyce(fit = preds_rf_cv_clim_global_all[[i]], obs = preds_rf_cv_clim_global_presences[,i],
#                                                                                                                                  nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                                                                                                  rm.duplicate = TRUE, method = 'kendall')
#                                                                                  boyce_index_rf_clim_global_all$cor }))
#       boyce_index_rf_clim_global <- colMeans(boyce_index_rf_clim_global)
#       
#       # Add the Boyce index to the performance metrics data frame
#       perf_rf_clim_global["Boyce"] <- boyce_index_rf_clim_global
#       
#       # Plot partial response curves and save them
#       svg(paste0("output_data/plots/response_plots/",sp,"/RF_clim_global_",sp,".svg"))
#       par(mfrow=c(2,2)) 
#       partial_response(m_rf_clim_global[[1]], predictors = species_occ_clim_global[,pred_sel_clim_global], main='RF')
#       dev.off()
#       
#       
#       
#       # (d) BRT -----------------------
#       print("BRT")
#       
#       # 5-fold cross-validation (run through each of the 10 resulted models)
#       preds_brt_cv_clim_global_all <- lapply(1:10,FUN=function(i){x <- crossvalSDM(m_brt_clim_global[[i]], traindat = species_occ_clim_global[,c('occ', pred_sel_clim_global)],
#                                                                                    colname_species = 'occ', colname_pred = pred_sel_clim_global)})
#       
#       # Calculation of performance metrics for each of the 10 cross-validated predictions
#       perf_brt_clim_global <- do.call("rbind", lapply(1:10,FUN=function(i){evalSDM(species_occ_clim_global$occ, preds_brt_cv_clim_global_all[[i]])}))
#       
#       # Calculate the mean of the 10 model performance metrics
#       perf_brt_clim_global <- colMeans(perf_brt_clim_global)
#       
#       # Calculation of the Boyce index
#       presences_indices <- which(species_occ_clim_global$occ == 1)
#       preds_brt_cv_clim_global_presences <- do.call("cbind", lapply(1:10,FUN=function(i){preds_brt_cv_clim_global_all[[i]][presences_indices]}))
#       
#       boyce_index_brt_clim_global <- do.call("rbind", lapply(1:10,FUN=function(i){boyce_index_brt_clim_global_all <- ecospat.boyce(fit = preds_brt_cv_clim_global_all[[i]], obs = preds_brt_cv_clim_global_presences[,i],
#                                                                                                                                    nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                                                                                                    rm.duplicate = TRUE, method = 'kendall')
#                                                                                   boyce_index_brt_clim_global_all$cor }))
#       boyce_index_brt_clim_global <- colMeans(boyce_index_brt_clim_global)
#       
#       # Add the Boyce index to the performance metrics data frame
#       perf_brt_clim_global["Boyce"] <- boyce_index_brt_clim_global
#       
#       # Plot partial response curves and save them
#       svg(paste0("output_data/plots/response_plots/",sp,"/BRT_clim_global_",sp,".svg"))
#       par(mfrow=c(2,2)) 
#       partial_response(m_brt_clim_global[[1]], predictors = species_occ_clim_global[,pred_sel_clim_global], main='BRT')
#       dev.off()
#       
#       
#       
#       # (e) Summarize performance metrics  -------------------------------------
#       comp_perf_clim_global <- rbind(glm = perf_glm_clim_global, gam = perf_gam_clim_global, rf = perf_rf_clim_global, brt = perf_brt_clim_global)
#       
#       # Add a column containing the names of the algorithm
#       comp_perf_clim_global <- data.frame(alg=row.names(comp_perf_clim_global),comp_perf_clim_global)
#       
#       
#       
#       # (f) Ensemble performance metrics  --------------------------------------
#       
#       # Calculate the mean of the cross-validated predictions all 10 rf models
#       preds_rf_cv_clim_global_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_rf_cv_clim_global_all[[i]])}))
#       preds_rf_cv_clim_global <- rowMeans(preds_rf_cv_clim_global_list)
#       
#       # Calculate the mean of the cross-validated predictions all 10 brt models
#       preds_brt_cv_clim_global_list <- do.call("cbind", lapply(1:10, FUN=function(i){unlist(preds_brt_cv_clim_global_all[[i]])}))
#       preds_brt_cv_clim_global <- rowMeans(preds_brt_cv_clim_global_list)
#       
#       # Combine predictions from all algorithms into one data frame
#       preds_all_clim_global <- data.frame(glm = preds_glm_cv_clim_global, gam = preds_gam_cv_clim_global, rf = preds_rf_cv_clim_global, brt = preds_brt_cv_clim_global)
#       
#       # Get the binary predictions of all algorithms (using the MaxSSS "thresh" value for thresholding)
#       binpred_all_clim_global <- sapply(names(preds_all_clim_global), 
#                                         FUN=function(alg){
#                                           ifelse(preds_all_clim_global[,alg] >= comp_perf_clim_global[comp_perf_clim_global$alg==alg,'thresh'],1,0)
#                                         }
#                                   )
#       
#       # Calculate the predictions for each row
#       preds_mean_clim_global <- rowMeans(preds_all_clim_global) # using the mean
#       preds_median_clim_global <- apply(preds_all_clim_global, 1, median) # using the median
#       preds_wmean_clim_global <- apply(preds_all_clim_global, 1, weighted.mean, w=comp_perf_clim_global[names(preds_all_clim_global), "TSS"]) # using the weighted mean
#       preds_comav_clim_global <- rowSums(binpred_all_clim_global)/ncol(binpred_all_clim_global) # using the committee average
#       
#       # Calculate ensemble performance metrics
#       ensemble_perf_mean_clim_global <- evalSDM(species_occ_clim_global$occ, preds_mean_clim_global)
#       ensemble_perf_median_clim_global <- evalSDM(species_occ_clim_global$occ, preds_median_clim_global)
#       ensemble_perf_wmean_clim_global <- evalSDM(species_occ_clim_global$occ, preds_wmean_clim_global)
#       ensemble_perf_comav_clim_global <- evalSDM(species_occ_clim_global$occ, preds_comav_clim_global)
#       
#       # Combine ensemble performances into one data frame
#       ensemble_perf_clim_global <- rbind(mean_prob = ensemble_perf_mean_clim_global, median_prob = ensemble_perf_median_clim_global, wmean_prob = ensemble_perf_wmean_clim_global,
#                                          committee_av = ensemble_perf_comav_clim_global)
#       
#       # Calculate the Boyce index for the ensemble
#       presences_indices <- which(species_occ_clim_global$occ == 1)
#       preds_mean_clim_global_presences <- preds_mean_clim_global[presences_indices]
#       preds_median_clim_global_presences <- preds_median_clim_global[presences_indices]
#       preds_wmean_clim_global_presences <- preds_wmean_clim_global[presences_indices]
#       preds_comav_clim_global_presences <- preds_comav_clim_global[presences_indices]
#       
#       boyce_index_ensemble_clim_global_mean <- ecospat.boyce(fit = preds_mean_clim_global, obs = preds_mean_clim_global_presences,
#                                                              nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                              rm.duplicate = TRUE, method = 'kendall')
#       
#       boyce_index_ensemble_clim_global_median <- ecospat.boyce(fit = preds_median_clim_global, obs = preds_median_clim_global_presences,
#                                                                nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                                rm.duplicate = TRUE, method = 'kendall')
#       
#       boyce_index_ensemble_clim_global_wmean <- ecospat.boyce(fit = preds_wmean_clim_global, obs = preds_wmean_clim_global_presences,
#                                                               nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                               rm.duplicate = TRUE, method = 'kendall')
#       
#       boyce_index_ensemble_clim_global_comav <- ecospat.boyce(fit = preds_comav_clim_global, obs = preds_comav_clim_global_presences,
#                                                               nclass=0, window.w="default", res=100, PEplot = FALSE, 
#                                                               rm.duplicate = TRUE, method = 'kendall')
#       
#       
#       ensemble_perf_boyce_clim_global <- data.frame(Boyce = c(boyce_index_ensemble_clim_global_mean$cor, boyce_index_ensemble_clim_global_median$cor,
#                                                               boyce_index_ensemble_clim_global_wmean$cor, boyce_index_ensemble_clim_global_comav$cor))
#       
#       
#       # Add the Boyce index to the other performance metrics
#       ensemble_perf_clim_global <- cbind(ensemble_perf_clim_global, ensemble_perf_boyce_clim_global)
#       
#       # Add a column containing the names of the ensemble options
#       ensemble_perf_clim_global <- data.frame(ens=row.names(ensemble_perf_clim_global),ensemble_perf_clim_global)
#       
#       
#       
#       # (g) Save validation outputs  -------------------------------------------
#       
#       save(comp_perf_clim_global, ensemble_perf_clim_global, file = paste0("output_data/validation/global/clim/validation_clim_global_",sp,".RData"))
#       
#       
#       
#     } else if (file_exists_validation == TRUE) { print("already done model validation")
#     } # End of if condition
#     
#     
# })} # end of try and for loop over species
# 
# gc()
# rm(list=ls())
