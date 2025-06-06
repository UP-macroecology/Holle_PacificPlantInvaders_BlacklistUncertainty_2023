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
library(doParallel)
library(foreach)

# register cores for parallel computation:
no_cores <- 3
cl <- makeCluster(no_cores)
registerDoParallel(cl)


# Load needed objects
load("input_data/occ_numbers_thinned_env_filtered.RData") # Contains names of study species
source("scripts/00_functions.R") # Evaluation metrics function

# Retrieve species names
study_species <- unique(as.character(occ_numbers_thinned_env_filtered$species)) 



#-------------------------------------------------------------------------------

# 1. Model fitting -------------------------------------------------------------

foreach(sp = study_species, .packages = c("mgcv", "randomForest", "gbm", "dismo", "PresenceAbsence", "ecospat")) %dopar% { # Start the loop over all species
  try({
    
    prog_log_file <- file(file.path("output_data", "models", "model_fitting_validation_progress_edaclim_global.txt"), open = "at") # write console output here
    
    # check if models already exist
    file_exists_models <- file.exists(paste0("output_data/models/global/edaclim/models_edaclim_global_",sp,".RData"))
    
    if (file_exists_models == FALSE) { # just continue with model fitting if output 
    # with models does not exist yet
      
      cat(paste0(Sys.time(), " - Starting model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Load needed objects of species and environmental data
      load(paste0(dir, "/output_data/distribution_env_data_subset/global/edaclim/species_occ_edaclim_global_",sp,".RData")) # distribution and environmental data
      load(paste0(dir, "/output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData")) # predictor variables
      
      
      # Create an absence index for machine learning algorithm to achieve even 
      # presence and absence data sets for machine learning algorithms
      species_occ_edaclim_global$abs_index <- NA
      number_absences <- sum(species_occ_edaclim_global$occ != 1)
      values_absences <- rep(1:10, length.out = number_absences)
      values_sample <- sample(values_absences)
      species_occ_edaclim_global$abs_index[species_occ_edaclim_global$occ != 1] <- values_sample
      
      # Calculate same weights for presences and absences for regression based algorithms
      weights <- ifelse(species_occ_edaclim_global$occ==1, 1, sum(species_occ_edaclim_global$occ==1) / sum(species_occ_edaclim_global$occ==0))
      
      # Create a vector with indices containing presence and absences in occurrence
      # data frame
      presences_indices <- which(species_occ_edaclim_global$occ == 1)
      absences_indices <- which(species_occ_edaclim_global$occ == 0)
      
      
      
      # (a) GLM -----------------------
      cat(paste0(Sys.time(), " - Starting to fit GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including linear and quadratic terms
      m_glm_edaclim_global <- glm(formula = as.formula(paste('occ ~',paste(pred_sel_edaclim_global,paste0('+ I(', pred_sel_edaclim_global ,'^2)'), collapse='+'))),
                                  family = "binomial", 
                                  weights = weights, 
                                  data = species_occ_edaclim_global)
      
      
      
      # (b) GAM -----------------------
      cat(paste0(Sys.time(), " - Starting to fit GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including smoothing splines and until 4 degrees of freedom
      m_gam_edaclim_global <- mgcv::gam(formula = as.formula(paste(paste('occ ~', paste(paste0('s(',pred_sel_edaclim_global,',k=4)'),collapse=' + ')))), 
                                        family = 'binomial', 
                                        weights = weights,
                                        data = species_occ_edaclim_global)
      
      
      
      
      # (c) RF -----------------------
      cat(paste0(Sys.time(), " - Starting to fit RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including 1000 trees and resulting in 10 models based on the absence index
      m_rf_edaclim_global <- lapply(1:10, FUN=function(i){sp_train <- species_occ_edaclim_global[c(presences_indices, which(species_occ_edaclim_global$abs_index == i)),]; print(i); randomForest(as.formula(paste('occ~',paste(pred_sel_edaclim_global, collapse='+'))), 
                                                                                                                                                                                                  data = sp_train, ntree = 1000, nodesize = 20, importance = T)})
      
      
      
      
      
      # (d) BRT -----------------------
      cat(paste0(Sys.time(), " - Starting to fit BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Including a tree complexity of 2 and bag fraction of 0.75. Resulting in 10 models based on the absence index
      # Including automatic adaption of learning rate and tree numbers between 1000 and 5000
      m_brt_edaclim_global <- lapply(1:10, FUN=function(i) {
        print(i);
        opt.LR <- TRUE;
        LR <- 0.01;
        while(opt.LR){
          m.brt <- try(gbm.step(data = species_occ_edaclim_global[c(presences_indices, which(species_occ_edaclim_global$abs_index == i)),], gbm.x = pred_sel_edaclim_global, gbm.y = "occ", family = 'bernoulli', tree.complexity = 2, bag.fraction = 0.75, learning.rate = LR, verbose=F, plot.main=F))
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
      save(m_glm_edaclim_global, m_gam_edaclim_global, m_rf_edaclim_global, m_brt_edaclim_global, species_occ_edaclim_global, weights,  file = paste0("output_data/models/global/edaclim/models_edaclim_global_",sp,".RData"))
      
      
    } else if (file_exists_models == TRUE) { cat(paste0(Sys.time(), " - Already done model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
#-------------------------------------------------------------------------------
    
# 2. Model validation ---------------------------------------------------------- 
    
    # check if validation files already exist
    file_exists_validation <- file.exists(paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
    
    if (file_exists_validation == FALSE) { # just continue with model validation if output 
    # with validation files does not exist yet
      
      cat(paste0(Sys.time(), " - Starting the model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Load the fitted models for the species, their predictors, and distribution dataset
      load(paste0("output_data/models/global/edaclim/models_edaclim_global_",sp,".RData"))
      load(paste0("output_data/variable_selection/global/edaclim/pred_sel_edaclim_global_",sp,".RData"))
      
      
      # Model validation - using a 5-fold random cross-validation
      kfolds <- 5
      
      # Make folds for entire data set with thinned species occurrences
      crossval_folds <- dismo::kfold(seq_len(nrow(species_occ_edaclim_global)), k = kfolds)
      
      # Create a vector with indices containing presence and absences in occurrence
      # data frame
      presences_indices <- which(species_occ_edaclim_global$occ == 1)
      absences_indices <- which(species_occ_edaclim_global$occ == 0)
      
      
      
      
      # (a) GLM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_glm_preds_cv <- rep(NA, nrow(species_occ_edaclim_global))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_edaclim_global[crossval_folds!=i,]
        cv_test <- species_occ_edaclim_global[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_glm <- update(m_glm_edaclim_global, data=cv_train, weights=cv_weights)
        m_glm_preds_cv[crossval_folds==i] <- predict(cv_glm, cv_test, type='response')
      }
      
      # Start preparations for model validation
      m_glm_preds_cv_edaclim_global_presences <- m_glm_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_glm_preds_cv_edaclim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_glm_perf_cv_maxTSS <- evalSDM(species_occ_edaclim_global$occ, m_glm_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_glm_perf_cv_meanProb <- evalSDM(species_occ_edaclim_global$occ, m_glm_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_glm_perf_cv_tenthPer <- evalSDM(species_occ_edaclim_global$occ, m_glm_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_glm_edaclim_global <- ecospat.boyce(fit = m_glm_preds_cv, obs = m_glm_preds_cv_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_glm_edaclim_global <- boyce_index_glm_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_glm_perf_cv_maxTSS$Boyce <- boyce_index_glm_edaclim_global
      m_glm_perf_cv_meanProb$Boyce <- boyce_index_glm_edaclim_global
      m_glm_perf_cv_tenthPer$Boyce <- boyce_index_glm_edaclim_global
      
      
      
      
      # (b) GAM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_gam_preds_cv <- rep(NA, nrow(species_occ_edaclim_global))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_edaclim_global[crossval_folds!=i,]
        cv_test <- species_occ_edaclim_global[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_gam <- update(m_gam_edaclim_global, data=cv_train, weights=cv_weights)
        m_gam_preds_cv[crossval_folds==i] <- predict(cv_gam, cv_test, type='response')
      }
      
      
      # Start preparations for model validation
      m_gam_preds_cv_edaclim_global_presences <- m_gam_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_gam_preds_cv_edaclim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_gam_perf_cv_maxTSS <- evalSDM(species_occ_edaclim_global$occ, m_gam_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_gam_perf_cv_meanProb <- evalSDM(species_occ_edaclim_global$occ, m_gam_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_gam_perf_cv_tenthPer <- evalSDM(species_occ_edaclim_global$occ, m_gam_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_gam_edaclim_global <- ecospat.boyce(fit = m_gam_preds_cv, obs = m_gam_preds_cv_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_gam_edaclim_global <- boyce_index_gam_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_gam_perf_cv_maxTSS$Boyce <- boyce_index_gam_edaclim_global
      m_gam_perf_cv_meanProb$Boyce <- boyce_index_gam_edaclim_global
      m_gam_perf_cv_tenthPer$Boyce <- boyce_index_gam_edaclim_global
      
      
      
      # (c) RF -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_rf_predmat_cv <- matrix(nrow=nrow(species_occ_edaclim_global), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_edaclim_global$abs_index == m))
          
          cv_train <- species_occ_edaclim_global[hold_in[hold_in %in% dat_tenth],]
          cv_test <- species_occ_edaclim_global[hold_out,]
          
          cv_rf <- update(m_rf_edaclim_global[[m]], data=cv_train)
          m_rf_predmat_cv[hold_out,m] <- predict(cv_rf, cv_test, type='response')
        }
      }
      
      m_rf_preds_cv <- rowMeans(m_rf_predmat_cv)
      
      # Start preparations for model validation
      m_rf_preds_cv_edaclim_global_presences <- m_rf_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_rf_preds_cv_edaclim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_rf_perf_cv_maxTSS <- evalSDM(species_occ_edaclim_global$occ, m_rf_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_rf_perf_cv_meanProb <- evalSDM(species_occ_edaclim_global$occ, m_rf_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_rf_perf_cv_tenthPer <- evalSDM(species_occ_edaclim_global$occ, m_rf_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_rf_edaclim_global <- ecospat.boyce(fit = m_rf_preds_cv, obs = m_rf_preds_cv_edaclim_global_presences,
                                                     nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                     rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_rf_edaclim_global <- boyce_index_rf_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_rf_perf_cv_maxTSS$Boyce <- boyce_index_rf_edaclim_global
      m_rf_perf_cv_meanProb$Boyce <- boyce_index_rf_edaclim_global
      m_rf_perf_cv_tenthPer$Boyce <- boyce_index_rf_edaclim_global
      
      
      
      # (d) BRT -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_brt_predmat_cv <- matrix(nrow=nrow(species_occ_edaclim_global), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_edaclim_global$abs_index == m))
          
          cv_train <- species_occ_edaclim_global[hold_in[hold_in %in% dat_tenth],]
          names(cv_train)[names(cv_train)=='occ'] <- m_brt_edaclim_global[[m]]$response.name
          cv_test <- species_occ_edaclim_global[hold_out,]
          
          cv_brt <- gbm::gbm(m_brt_edaclim_global[[m]]$call, 'bernoulli', data = cv_train[,c( m_brt_edaclim_global[[m]]$response.name, pred_sel_edaclim_global)], 
                             n.trees=m_brt_edaclim_global[[m]]$gbm.call$best.trees, 
                             shrinkage=m_brt_edaclim_global[[m]]$gbm.call$learning.rate, 
                             bag.fraction=m_brt_edaclim_global[[m]]$gbm.call$bag.fraction, 
                             interaction.depth=m_brt_edaclim_global[[m]]$gbm.call$tree.complexity)
          m_brt_predmat_cv[hold_out,m] <- predict(cv_brt, cv_test, type='response', n.trees=m_brt_edaclim_global[[m]]$gbm.call$best.trees)
        }
      }
      
      m_brt_preds_cv <- rowMeans(m_brt_predmat_cv)
      
      # Start preparations for model validation
      m_brt_preds_cv_edaclim_global_presences <- m_brt_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_brt_preds_cv_edaclim_global_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_brt_perf_cv_maxTSS <- evalSDM(species_occ_edaclim_global$occ, m_brt_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_brt_perf_cv_meanProb <- evalSDM(species_occ_edaclim_global$occ, m_brt_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_brt_perf_cv_tenthPer <- evalSDM(species_occ_edaclim_global$occ, m_brt_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_brt_edaclim_global <- ecospat.boyce(fit = m_brt_preds_cv, obs = m_brt_preds_cv_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_brt_edaclim_global <- boyce_index_brt_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_brt_perf_cv_maxTSS$Boyce <- boyce_index_brt_edaclim_global
      m_brt_perf_cv_meanProb$Boyce <- boyce_index_brt_edaclim_global
      m_brt_perf_cv_tenthPer$Boyce <- boyce_index_brt_edaclim_global
      
      
      
      # (e) Summarize performance metrics  -------------------------------------
      # for each algorithm and threshold
      comp_perf_edaclim_global_maxTSS <- rbind(glm = m_glm_perf_cv_maxTSS, gam = m_gam_perf_cv_maxTSS, rf = m_rf_perf_cv_maxTSS, brt = m_brt_perf_cv_maxTSS)
      comp_perf_edaclim_global_maxTSS <- data.frame(alg=row.names(comp_perf_edaclim_global_maxTSS),comp_perf_edaclim_global_maxTSS) # Add a column containing the names of the algorithm
      
      comp_perf_edaclim_global_meanProb <- rbind(glm = m_glm_perf_cv_meanProb, gam = m_gam_perf_cv_meanProb, rf = m_rf_perf_cv_meanProb, brt = m_brt_perf_cv_meanProb)
      comp_perf_edaclim_global_meanProb <- data.frame(alg=row.names(comp_perf_edaclim_global_meanProb),comp_perf_edaclim_global_meanProb)
      
      comp_perf_edaclim_global_tenthPer <- rbind(glm = m_glm_perf_cv_tenthPer, gam = m_gam_perf_cv_tenthPer, rf = m_rf_perf_cv_tenthPer, brt = m_brt_perf_cv_tenthPer)
      comp_perf_edaclim_global_tenthPer <- data.frame(alg=row.names(comp_perf_edaclim_global_tenthPer),comp_perf_edaclim_global_tenthPer)
      
      
      
      # (f) Ensemble performance metrics  --------------------------------------
      cat(paste0(Sys.time(), " - Starting to validate ensemble for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Combine predictions from all algorithms into one data frame
      preds_cv_all_edaclim_global <- data.frame(glm = m_glm_preds_cv, gam = m_gam_preds_cv,  rf = m_rf_preds_cv, brt = m_brt_preds_cv)
      
      # Calculate cross-validated ensemble predictions (mean)
      preds_cv_ens_edaclim_global <- rowMeans(preds_cv_all_edaclim_global) # using the mean
      
      # Start preparations for model validation
      preds_cv_ens_edaclim_global_presences <- preds_cv_ens_edaclim_global[presences_indices] # Just retain the predictions of the presences based on indices
      
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(preds_cv_ens_edaclim_global_presences, probs = 0.10)
      
      # Calculate ensemble performance metrics based on different thresholds
      ensemble_perf_edaclim_global_maxTSS <- evalSDM(species_occ_edaclim_global$occ, preds_cv_ens_edaclim_global, thresh.method = 'MaxSens+Spec', weigths = weights)
      ensemble_perf_edaclim_global_meanProb <- evalSDM(species_occ_edaclim_global$occ, preds_cv_ens_edaclim_global, thresh.method = 'MeanProb', weigths = weights)
      ensemble_perf_edaclim_global_tenthPer <- evalSDM(species_occ_edaclim_global$occ, preds_cv_ens_edaclim_global, thresh = thresh_tenthPer, weigths = weights)
      
      
      # Calculate the Boyce index of the ensemble
      boyce_index_ens_edaclim_global <- ecospat.boyce(fit = preds_cv_ens_edaclim_global, obs = preds_cv_ens_edaclim_global_presences,
                                                      nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                      rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ens_edaclim_global <- boyce_index_ens_edaclim_global$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      ensemble_perf_edaclim_global_maxTSS$Boyce <- boyce_index_ens_edaclim_global
      ensemble_perf_edaclim_global_meanProb$Boyce <- boyce_index_ens_edaclim_global
      ensemble_perf_edaclim_global_tenthPer$Boyce <- boyce_index_ens_edaclim_global
      
      # Comparing cross-validated ensemble model performance across thresholds:
      ensemble_perf_edaclim_global <- rbind(maxTSS = ensemble_perf_edaclim_global_maxTSS,
                                            meanProb = ensemble_perf_edaclim_global_meanProb,
                                            tenthPer = ensemble_perf_edaclim_global_tenthPer)
      
      
      
      
      
      
      
      # (g) Save validation outputs  -------------------------------------------
      
      save(m_glm_preds_cv, m_gam_preds_cv, m_brt_preds_cv, m_rf_preds_cv, preds_cv_ens_edaclim_global, crossval_folds, 
           comp_perf_edaclim_global_maxTSS, comp_perf_edaclim_global_meanProb, comp_perf_edaclim_global_tenthPer, 
           ensemble_perf_edaclim_global,  file = paste0("output_data/validation/global/edaclim/validation_edaclim_global_",sp,".RData"))
      
      
      
    } else if (file_exists_validation == TRUE) { cat(paste0(Sys.time(), " - Already done model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
    
  }) # End of try
} # End of foreach


stopCluster(cl)
gc()
rm(list=ls())



