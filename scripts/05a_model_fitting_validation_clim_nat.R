# Uncertainty in blacklisting potential Pacific plant invaders 
# using species distribution models


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#             05a. Model fitting and validation based on                 #
#             native occurrences and purely climatic data                #
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
    
    # Log overall progress
    prog_log_file <- file(file.path("output_data", "models", "model_fitting_validation_progress_clim_native.txt"), open = "at") # write console output here
    
    
    # check if models already exist
    file_exists_models <- file.exists(paste0("output_data/models/native/clim/models_clim_native_",sp,".RData"))
    
    if (file_exists_models == FALSE) { # just continue with model fitting if output 
    # with models does not exist yet
      
      cat(paste0(Sys.time(), " - Starting model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
    # Load needed objects of species and environmental data
    load(paste0("output_data/distribution_env_data_subset/native/clim/species_occ_clim_native_",sp,".RData")) # distribution and environmental data
    load(paste0("output_data/variable_selection/native/clim/pred_sel_clim_native_",sp,".RData")) # predictor variables
    
    # Create an absence index for machine learning algorithm to achieve even 
    # presence and absence data sets for machine learning algorithms
    species_occ_clim_native$abs_index <- NA
    number_absences <- sum(species_occ_clim_native$occ != 1)
    values_absences <- rep(1:10, length.out = number_absences)
    values_sample <- sample(values_absences)
    species_occ_clim_native$abs_index[species_occ_clim_native$occ != 1] <- values_sample
    
    # Calculate same weights for presences and absences for regression based algorithms
    weights <- ifelse(species_occ_clim_native$occ==1, 1, sum(species_occ_clim_native$occ==1) / sum(species_occ_clim_native$occ==0))
    
    # Create a vector with indices containing presence and absences in occurrence
    # data frame
    presences_indices <- which(species_occ_clim_native$occ == 1)
    absences_indices <- which(species_occ_clim_native$occ == 0)
    
    
    
    # (a) GLM -----------------------
    cat(paste0(Sys.time(), " - Starting to fit GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    
    # Including linear and quadratic terms
    m_glm_clim_native <- glm(formula = as.formula(paste('occ ~',paste(pred_sel_clim_native,paste0('+ I(', pred_sel_clim_native ,'^2)'), collapse='+'))),
                             family = "binomial", 
                             weights = weights, 
                             data = species_occ_clim_native)
    
    
    
    # (b) GAM -----------------------
    cat(paste0(Sys.time(), " - Starting to fit GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    
    # Including smoothing splines and until 4 degrees of freedom
    m_gam_clim_native <- mgcv::gam(formula = as.formula(paste(paste('occ ~', paste(paste0('s(',pred_sel_clim_native,',k=4)'),collapse=' + ')))), 
                                   family = 'binomial', 
                                   weights = weights,
                                   data = species_occ_clim_native)
    

    
    
    # (c) RF -----------------------
    cat(paste0(Sys.time(), " - Starting to fit RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    
    # Including 1000 trees and resulting in 10 models based on the absence index
    m_rf_clim_native <- lapply(1:10, FUN=function(i){sp_train <- species_occ_clim_native[c(presences_indices, which(species_occ_clim_native$abs_index == i)),]; print(i); randomForest(as.formula(paste('occ~',paste(pred_sel_clim_native, collapse='+'))), 
                                                                                                                                                                                       data = sp_train, ntree = 1000, nodesize = 20, importance = T)})

    

    
    
    # (d) BRT -----------------------
    cat(paste0(Sys.time(), " - Starting to fit BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    
    # Including a tree complexity of 2 and bag fraction of 0.75. Resulting in 10 models based on the absence index
    # Including automatic adaption of learning rate and tree numbers between 1000 and 5000
    m_brt_clim_native <- lapply(1:10, FUN=function(i) {
      print(i);
      opt.LR <- TRUE;
      LR <- 0.01;
      while(opt.LR){
        m.brt <- try(gbm.step(data = species_occ_clim_native[c(presences_indices, which(species_occ_clim_native$abs_index == i)),], gbm.x = pred_sel_clim_native, gbm.y = "occ", family = 'bernoulli', tree.complexity = 2, bag.fraction = 0.75, learning.rate = LR, verbose=F, plot.main=F))
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
    save(m_glm_clim_native, m_gam_clim_native, m_rf_clim_native, m_brt_clim_native, species_occ_clim_native, weights, file = paste0("output_data/models/native/clim/models_clim_native_",sp,".RData"))
    
    
    } else if (file_exists_models == TRUE) { cat(paste0(Sys.time(), " - Already done model fitting for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
    
    
    
#-------------------------------------------------------------------------------
    
# 2. Model validation ---------------------------------------------------------- 
    
    # check if validation files already exist
    file_exists_validation <- file.exists(paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
    
    if (file_exists_validation == FALSE) { # just continue with model validation if output 
    # with validation files does not exist yet
      
      cat(paste0(Sys.time(), " - Starting the model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Load the fitted models for the species, their predictors, and distribution dataset
      load(paste0("output_data/models/native/clim/models_clim_native_",sp,".RData"))
      load(paste0("output_data/variable_selection/native/clim/pred_sel_clim_native_",sp,".RData"))
      
      # Model validation - using a 5-fold random cross-validation
      kfolds <- 5
      
      # Make folds for entire data set with thinned species occurrences
      crossval_folds <- dismo::kfold(seq_len(nrow(species_occ_clim_native)), k = kfolds)
      
      # Create a vector with indices containing presence and absences in occurrence
      # data frame
      presences_indices <- which(species_occ_clim_native$occ == 1)
      absences_indices <- which(species_occ_clim_native$occ == 0)
      
      
      # (a) GLM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GLMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_glm_preds_cv <- rep(NA, nrow(species_occ_clim_native))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_clim_native[crossval_folds!=i,]
        cv_test <- species_occ_clim_native[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_glm <- update(m_glm_clim_native, data=cv_train, weights=cv_weights)
        m_glm_preds_cv[crossval_folds==i] <- predict(cv_glm, cv_test, type='response')
      }
      
      # Start preparations for model validation
      m_glm_preds_cv_clim_native_presences <- m_glm_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_glm_preds_cv_clim_native_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_glm_perf_cv_maxTSS <- evalSDM(species_occ_clim_native$occ, m_glm_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_glm_perf_cv_meanProb <- evalSDM(species_occ_clim_native$occ, m_glm_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_glm_perf_cv_tenthPer <- evalSDM(species_occ_clim_native$occ, m_glm_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_glm_clim_native <- ecospat.boyce(fit = m_glm_preds_cv, obs = m_glm_preds_cv_clim_native_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_glm_clim_native <- boyce_index_glm_clim_native$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_glm_perf_cv_maxTSS$Boyce <- boyce_index_glm_clim_native
      m_glm_perf_cv_meanProb$Boyce <- boyce_index_glm_clim_native
      m_glm_perf_cv_tenthPer$Boyce <- boyce_index_glm_clim_native
      
      
      
      
      # (b) GAM -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate GAMs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_gam_preds_cv <- rep(NA, nrow(species_occ_clim_native))
      for(i in seq_len(kfolds)){
        print(i)
        cv_train <- species_occ_clim_native[crossval_folds!=i,]
        cv_test <- species_occ_clim_native[crossval_folds==i,]
        cv_weights <- weights[crossval_folds!=i]
        
        cv_gam <- update(m_gam_clim_native, data=cv_train, weights=cv_weights)
        m_gam_preds_cv[crossval_folds==i] <- predict(cv_gam, cv_test, type='response')
      }
      
      # Start preparations for model validation
      m_gam_preds_cv_clim_native_presences <- m_gam_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_gam_preds_cv_clim_native_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_gam_perf_cv_maxTSS <- evalSDM(species_occ_clim_native$occ, m_gam_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_gam_perf_cv_meanProb <- evalSDM(species_occ_clim_native$occ, m_gam_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_gam_perf_cv_tenthPer <- evalSDM(species_occ_clim_native$occ, m_gam_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_gam_clim_native <- ecospat.boyce(fit = m_gam_preds_cv, obs = m_gam_preds_cv_clim_native_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_gam_clim_native <- boyce_index_gam_clim_native$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_gam_perf_cv_maxTSS$Boyce <- boyce_index_gam_clim_native
      m_gam_perf_cv_meanProb$Boyce <- boyce_index_gam_clim_native
      m_gam_perf_cv_tenthPer$Boyce <- boyce_index_gam_clim_native
      
      
      
      # (c) RF -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate RFs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_rf_predmat_cv <- matrix(nrow=nrow(species_occ_clim_native), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_clim_native$abs_index == m))
          
          cv_train <- species_occ_clim_native[hold_in[hold_in %in% dat_tenth],]
          cv_test <- species_occ_clim_native[hold_out,]
          
          cv_rf <- update(m_rf_clim_native[[m]], data=cv_train)
          m_rf_predmat_cv[hold_out,m] <- predict(cv_rf, cv_test, type='response')
        }
      }
      
      m_rf_preds_cv <- rowMeans(m_rf_predmat_cv)
      
      # Start preparations for model validation
      m_rf_preds_cv_clim_native_presences <- m_rf_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_rf_preds_cv_clim_native_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_rf_perf_cv_maxTSS <- evalSDM(species_occ_clim_native$occ, m_rf_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_rf_perf_cv_meanProb <- evalSDM(species_occ_clim_native$occ, m_rf_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_rf_perf_cv_tenthPer <- evalSDM(species_occ_clim_native$occ, m_rf_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_rf_clim_native <- ecospat.boyce(fit = m_rf_preds_cv, obs = m_rf_preds_cv_clim_native_presences,
                                                  nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                  rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_rf_clim_native <- boyce_index_rf_clim_native$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_rf_perf_cv_maxTSS$Boyce <- boyce_index_rf_clim_native
      m_rf_perf_cv_meanProb$Boyce <- boyce_index_rf_clim_native
      m_rf_perf_cv_tenthPer$Boyce <- boyce_index_rf_clim_native
      
      
      
      # (d) BRT -----------------------
      # Make cross-validated predictions and calculate accuracy measures based on different thresholds
      cat(paste0(Sys.time(), " - Starting to validate BRTs for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      m_brt_predmat_cv <- matrix(nrow=nrow(species_occ_clim_native), ncol=10)
      for(i in seq_len(kfolds)){ # loop over k folds
        
        print(i)
        
        hold_in <- which(crossval_folds!=i)
        hold_out <- which(crossval_folds==i)
        
        for (m in 1:10) { # loop over 10 models
          
          print(m)
          
          dat_tenth <- c(presences_indices, which(species_occ_clim_native$abs_index == m))
          
          cv_train <- species_occ_clim_native[hold_in[hold_in %in% dat_tenth],]
          names(cv_train)[names(cv_train)=='occ'] <- m_brt_clim_native[[m]]$response.name
          cv_test <- species_occ_clim_native[hold_out,]
          
          cv_brt <- gbm::gbm(m_brt_clim_native[[m]]$call, 'bernoulli', data = cv_train[,c( m_brt_clim_native[[m]]$response.name, pred_sel_clim_native)], 
                             n.trees=m_brt_clim_native[[m]]$gbm.call$best.trees, 
                             shrinkage=m_brt_clim_native[[m]]$gbm.call$learning.rate, 
                             bag.fraction=m_brt_clim_native[[m]]$gbm.call$bag.fraction, 
                             interaction.depth=m_brt_clim_native[[m]]$gbm.call$tree.complexity)
          m_brt_predmat_cv[hold_out,m] <- predict(cv_brt, cv_test, type='response', n.trees=m_brt_clim_native[[m]]$gbm.call$best.trees)
        }
      }
      
      m_brt_preds_cv <- rowMeans(m_brt_predmat_cv)
      
      # Start preparations for model validation
      m_brt_preds_cv_clim_native_presences <- m_brt_preds_cv[presences_indices] # Just retain the predictions of the presences based on indices
      

      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(m_brt_preds_cv_clim_native_presences, probs = 0.10)
      
      # Calculate performance measures based on different thresholds
      m_brt_perf_cv_maxTSS <- evalSDM(species_occ_clim_native$occ, m_brt_preds_cv, thresh.method='MaxSens+Spec', weigths = weights)
      m_brt_perf_cv_meanProb <- evalSDM(species_occ_clim_native$occ, m_brt_preds_cv, thresh.method='MeanProb', weigths = weights)
      m_brt_perf_cv_tenthPer <- evalSDM(species_occ_clim_native$occ, m_brt_preds_cv, thresh = thresh_tenthPer, weigths = weights)
      
      # Calculate the Boyce index additionally
      boyce_index_brt_clim_native <- ecospat.boyce(fit = m_brt_preds_cv, obs = m_brt_preds_cv_clim_native_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_brt_clim_native <- boyce_index_brt_clim_native$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      m_brt_perf_cv_maxTSS$Boyce <- boyce_index_brt_clim_native
      m_brt_perf_cv_meanProb$Boyce <- boyce_index_brt_clim_native
      m_brt_perf_cv_tenthPer$Boyce <- boyce_index_brt_clim_native
      
      
      
      # (e) Summarize performance metrics  -------------------------------------
      # for each algorithm and threshold
      comp_perf_clim_native_maxTSS <- rbind(glm = m_glm_perf_cv_maxTSS, gam = m_gam_perf_cv_maxTSS, rf = m_rf_perf_cv_maxTSS, brt = m_brt_perf_cv_maxTSS)
      comp_perf_clim_native_maxTSS <- data.frame(alg=row.names(comp_perf_clim_native_maxTSS),comp_perf_clim_native_maxTSS) # Add a column containing the names of the algorithm
      
      comp_perf_clim_native_meanProb <- rbind(glm = m_glm_perf_cv_meanProb, gam = m_gam_perf_cv_meanProb, rf = m_rf_perf_cv_meanProb, brt = m_brt_perf_cv_meanProb)
      comp_perf_clim_native_meanProb <- data.frame(alg=row.names(comp_perf_clim_native_meanProb),comp_perf_clim_native_meanProb)
      
      comp_perf_clim_native_tenthPer <- rbind(glm = m_glm_perf_cv_tenthPer, gam = m_gam_perf_cv_tenthPer, rf = m_rf_perf_cv_tenthPer, brt = m_brt_perf_cv_tenthPer)
      comp_perf_clim_native_tenthPer <- data.frame(alg=row.names(comp_perf_clim_native_tenthPer),comp_perf_clim_native_tenthPer)
      
      
      
      # (f) Ensemble performance metrics  --------------------------------------
      cat(paste0(Sys.time(), " - Starting to validate ensemble for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
      
      # Combine predictions from all algorithms into one data frame
      preds_cv_all_clim_native <- data.frame(glm = m_glm_preds_cv, gam = m_gam_preds_cv,  rf = m_rf_preds_cv, brt = m_brt_preds_cv)
      
      # Calculate cross-validated ensemble predictions (mean)
      preds_cv_ens_clim_native <- rowMeans(preds_cv_all_clim_native) # using the mean
      
      # Start preparations for model validation
      preds_cv_ens_clim_native_presences <- preds_cv_ens_clim_native[presences_indices] # Just retain the predictions of the presences based on indices
      
      # Calculate the 10th percentile of cross-validated prediction probabilities
      # of presence locations (10th percentile of training presences)
      thresh_tenthPer <- quantile(preds_cv_ens_clim_native_presences, probs = 0.10)
      
      # Calculate ensemble performance metrics based on different thresholds
      ensemble_perf_clim_native_maxTSS <- evalSDM(species_occ_clim_native$occ, preds_cv_ens_clim_native, thresh.method = 'MaxSens+Spec', weigths = weights)
      ensemble_perf_clim_native_meanProb <- evalSDM(species_occ_clim_native$occ, preds_cv_ens_clim_native, thresh.method = 'MeanProb', weigths = weights)
      ensemble_perf_clim_native_tenthPer <- evalSDM(species_occ_clim_native$occ, preds_cv_ens_clim_native, thresh = thresh_tenthPer, weigths = weights)
      
      
      # Calculate the Boyce index of the ensemble
      boyce_index_ens_clim_native <- ecospat.boyce(fit = preds_cv_ens_clim_native, obs = preds_cv_ens_clim_native_presences,
                                                   nclass=0, window.w="default", res=100, PEplot = FALSE, 
                                                   rm.duplicate = TRUE, method = 'kendall')
      
      boyce_index_ens_clim_native <- boyce_index_ens_clim_native$cor # Extract correlation value (Boyce index)
      
      # Add the Boyce index to the performance metrics data frame
      ensemble_perf_clim_native_maxTSS$Boyce <- boyce_index_ens_clim_native
      ensemble_perf_clim_native_meanProb$Boyce <- boyce_index_ens_clim_native
      ensemble_perf_clim_native_tenthPer$Boyce <- boyce_index_ens_clim_native
      
      # Comparing cross-validated ensemble model performance across thresholds:
      ensemble_perf_clim_native <- rbind(maxTSS = ensemble_perf_clim_native_maxTSS,
                                         meanProb = ensemble_perf_clim_native_meanProb,
                                         tenthPer = ensemble_perf_clim_native_tenthPer)
      
      
      
      
      
      
      
      # (g) Save validation outputs  -------------------------------------------
      
      save(m_glm_preds_cv, m_gam_preds_cv, m_brt_preds_cv, m_rf_preds_cv, preds_cv_ens_clim_native, crossval_folds, 
           comp_perf_clim_native_maxTSS, comp_perf_clim_native_meanProb, comp_perf_clim_native_tenthPer, 
           ensemble_perf_clim_native,  file = paste0("output_data/validation/native/clim/validation_clim_native_",sp,".RData"))
      
      
      
    } else if (file_exists_validation == TRUE) { cat(paste0(Sys.time(), " - Already done model validation for species: ", sp, "\n"), file = prog_log_file, append = TRUE)
    } # End of if condition
    
    
}) # End of try
} # End of foreach



    
stopCluster(cl)
gc()
rm(list=ls())






