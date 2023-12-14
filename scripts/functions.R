# Uncertainty paper


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                            Functions                                   #
# ---------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# 1. Function for spatial thinning ---------------------------------------------
thin <- function(sf, thin_dist = 3000, runs = 10, ncores = 10){
  
  require(sf, quietly = TRUE)
  require(purrr, quietly = TRUE)
  require(furrr, quietly = TRUE)
  
  sample.vec <- function(x, ...) x[sample(length(x), ...)]
  
  sf_buffer <- st_buffer(sf, thin_dist)
  buff_int <- st_intersects(sf, sf_buffer) 
  buff_int <- setNames(buff_int, 1:length(buff_int))
  
  n_int <- map_dbl(buff_int, length)
  
  plan(multisession, workers = ncores)
  
  seeds <- sample.int(n = runs)
  results_runs <- future_map(seeds, function(i){
    
    set.seed(i)
    while (max(n_int) > 1) {
      max_neighbors <- names(which(n_int == max(n_int)))
      
      # remove point with max neighbors
      sampled_id <- sample.vec(max_neighbors, 1)
      
      pluck(buff_int, sampled_id) <- NULL
      buff_int <- map(buff_int, function(x) setdiff(x, as.numeric(sampled_id)))
      n_int <- map_dbl(buff_int, length)
    }
    
    unlist(buff_int) %>% unique()
    
  })
  
  lengths <- map_dbl(results_runs, length)
  
  selected_run <- results_runs[[sample.vec(which(lengths == max(lengths)), 1)]]
  
  out <- sf[selected_run,]
  
  out <- sf_to_df(out)[,3:4] %>%
    rename("lon" = "x", "lat" = "y")
  
  out
}

#-------------------------------------------------------------------------------

# 2. Function for variable selection -------------------------------------------

#' select07
#'
#' Select weakly correlated variables based on univariate importance based on \insertCite{Dormann2013}{mecofun}. Univariate variable importance is based on AIC. Variable importance can also be pre-defined by hand.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param X Matrix or data.frame containing the predictor variables
#' @param y vector of response variable
#' @param family a description of the error distribution and link function to be used in the model.
#' @param univar a character string indicating the regression method to be used for estimating univariate importance. Must be one of the strings "glm1", "glm2" (default), or "gam". "glm1" will estimate a generalised linear model (GLM) with a linear predictor, "glm2" a GLM with a second order polynomial, and "gam" a generalised additive model (GAM) with smooting splines 
#' @param threshold a numeric value indicating the absolute value of the correlation coefficient above which the paired correlation are judged as problematic.
#' @param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of "spearman" (default), "kendall", or "pearson".
#' @param sequence an optional character vector providing the order of importance of the predictors. This overrides the univar method.
#' @param weights an optional vector of prior weights to be used in univariate GLMs or GAMs
#' 
#' @return A list with three objects: "AIC" a numeric vector containing the AIC for the univariate models, "cor_mat" containing the correlation matrix, and "pred_sel" a character vector with the names of the remaining, weakly correlated variables. The variables are ordered according to their univariate variable importance (starting with most important variable).
#' 
#' @examples 
#' data(Anguilla_train)
#' select07(X=Anguilla_train[,3:10], y=Anguilla_train[,2])
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
select07 <- function(X, y, family="binomial",univar="glm2", threshold=0.7, method="spearman",sequence=NULL, weights=NULL)
{
  # selects variables based on removing correlations > 0.7, retaining those
  # variables more important with respect to y
  # Order of importance can be provided by the character vector 'sequence'
  
  # 1. step: cor-matrix
  # 2. step: importance vector
  # 3. step: identify correlated pairs
  # 4. step: in order of importance: remove collinear less important variable,
  #           recalculate correlation matrix a.s.f.
  
  var.imp <- function (variable, response, univar,family, weights)
  {
    # calculates the univariate (=marginal) importance of a variable for a response
    # if(univar=="glm1")
    # {
    #   fm.glm <- glm(response ~ variable, family=family, weights=weights)
    #   summary(fm.glm)$aic
    # } else
    # if(univar=="glm2")
    # {
    #   fm.glm <- glm(response ~ poly(variable,2), family=family, weights=weights)
    #   summary(fm.glm)$aic
    # } else  
    # if(univar=="gam")
    # {
    #   fm.gam <- mgcv::gam(response ~ s(variable,k=4), family=family, weights=weights)
    #   AIC(fm.gam)
    # } else return(F)
    m1 <- switch(univar,
                 glm1 = glm(response ~ variable, family=family, weights=weights),
                 glm2 = glm(response ~ poly(variable,2), family=family, weights=weights),
                 gam = mgcv::gam(response ~ s(variable,k=4), family=family, weights=weights))
    AIC(m1)
  }
  
  cm <- cor(X, method=method)
  
  if (is.null(sequence)) {
    a<-try(var.imp(X[,1],y,univar=univar, family=family, weights=weights))
    if (is.numeric(a)!=1) {stop("invalid univar method")}
    
    imp <- apply(X, 2, var.imp, response=y, family=family,univar=univar, weights=weights) #importance as AIC: the lower the better!
    sort.imp<-names(sort(imp)) 
  } else
  { sort.imp <- sequence }
  
  pairs <- which(abs(cm)>= threshold, arr.ind=T) # identifies correlated variable pairs
  index <- which(pairs[,1]==pairs[,2])           # removes entry on diagonal
  pairs <- pairs[-index,]                        # -"-
  
  exclude <- NULL
  for (i in 1:length(sort.imp))
  {
    if ((sort.imp[i] %in% row.names(pairs))&
        ((sort.imp[i] %in% exclude)==F)) {
      cv<-cm[setdiff(row.names(cm),exclude),sort.imp[i]]
      cv<-cv[setdiff(names(cv),sort.imp[1:i])]
      exclude<-c(exclude,names(which((abs(cv)>=threshold)))) }
  }
  
  pred_sel <- sort.imp[!(sort.imp %in% unique(exclude)),drop=F]
  return(list(AIC=sort(imp), cor_mat=cm, pred_sel=pred_sel))
}

#-------------------------------------------------------------------------------

# 3. Function for cross-validation ---------------------------------------------

crossvalSDM <- function(model, kfold=5, traindat, colname_species, colname_pred,
                        env_r=NULL, colname_coord=NULL, weights=NULL) {
  
  weights.full <- weights
  
  if (length(kfold)==1) {
    # Make k-fold data partitions
    ks <- dismo::kfold(traindat, k = kfold, by = traindat[,colname_species])
  } else {
    ks <- kfold
    kfold <- length(unique(kfold))
  }
  
  cross_val_preds = numeric(length = nrow(traindat))
  
  for(i in seq_len(kfold)){
    cv_train <- traindat[ks!=i,]
    cv_test <- traindat[ks==i,]
    
    # Because we used the gbm.step() for BRTs, we need a small work-around:
    if (class(model)[1]=='gbm') {
      cv_train_gbm <- cv_train;
      if (!is.null(model$gbm.call)) {
        names(cv_train_gbm)[names(cv_train_gbm)==colname_species] <- 
          model$response.name}
    }
    
    if (!is.null(weights)) {
      weights <- weights.full[ks!=i]
    }
    
    # We update the model for the new training data
    modtmp <- switch(class(model)[1],
                     Bioclim = dismo::bioclim(env_r[[colname_pred]], cv_train[cv_train[, colname_species]==1, colname_coord]),
                     Domain = dismo::domain(env_r[[colname_pred]], cv_train[cv_train[, colname_species]==1, colname_coord]),
                     glm = update(model, data=cv_train),
                     Gam = update(model, data=cv_train, weights=weights),
                     gam = update(model, data=cv_train, weights=weights),
                     negbin = update(model, data=cv_train),
                     rpart = update(model, data=cv_train),
                     # rpart = rpart::rpart(as.formula(paste(colname_species,'~',paste(colname_pred, collapse='+'))), data=cv_train),
                     randomForest = update(model, data=cv_train),
                     randomForest.formula = update(model, data=cv_train),  
                     gbm = switch(ifelse(is.null(model$gbm.call),"GBM","GBM.STEP"), 
                                  GBM.STEP = gbm::gbm(model$call, 'bernoulli', data=cv_train_gbm[,c(colname_pred,model$response.name)], n.trees=model$gbm.call$best.trees, shrinkage=model$gbm.call$learning.rate, bag.fraction=model$gbm.call$bag.fraction, interaction.depth=model$gbm.call$tree.complexity),
                                  GBM = gbm::gbm(model$call, 'bernoulli', data=cv_train_gbm[,c(colname_pred,model$response.name)], 
                                                 n.trees=model$n.trees, shrinkage=model$shrinkage, bag.fraction=model$bag.fraction, interaction.depth=model$interaction.depth)),
                     maxnet = maxnet::maxnet(p= cv_train[,colname_species], data= cv_train[,colname_pred, drop=F]))
    
    # We make predictions for k-fold:
    cross_val_preds[ks==i] <- predictSDM(modtmp, cv_test[, colname_pred, drop=F])
  }
  cross_val_preds
}

#-------------------------------------------------------------------------------

# 4. Function for cross-validation ---------------------------------------------

#' evalSDM
#'
#' Evaluate SDM perfomance
#' @param observation vector containing the observed response 
#' @param predictions vector containing the predictions
#' @param thresh.method a string indicating which method to use for optimising the binarising threshold (see ?PresenceAbsence::optimal.thresholds. Defaults to "MaxSens+Spec" (the maximum of sensitivity+specificity).
#' @param req.sens additional argument to PresenceAbsence::optimal.thresholds()
#' @param req.spec additional argument to PresenceAbsence::optimal.thresholds()
#' @param FPC additional argument to PresenceAbsence::optimal.thresholds()
#' @param FNC additional argument to PresenceAbsence::optimal.thresholds()
#' @return A dataframe with performance statistics.
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' preds_cv <- crossvalSDM(m1, kfold=5, traindat=Anguilla_train, colname_species = 'Angaus', colname_pred = 'SegSumT')
#' evalSDM(Anguilla_train$Angaus, preds_cv)
#' @export
evalSDM <- function(observation, predictions, thresh.method='MaxSens+Spec', req.sens=0.85, req.spec = 0.85, FPC=1, FNC=1){
  thresh.dat <- data.frame(ID=seq_len(length(observation)), 
                           obs = observation,
                           pred = predictions)
  
  thresh <- PresenceAbsence::optimal.thresholds(DATA= thresh.dat, req.sens=req.sens, req.spec = req.spec, FPC=FPC, FNC=FNC)
  cmx.opt <- PresenceAbsence::cmx(DATA= thresh.dat, threshold=thresh[thresh$Method==thresh.method,2])
  
  data.frame(AUC = PresenceAbsence::auc(thresh.dat, st.dev=F),
             TSS = TSS(cmx.opt), 
             Kappa = PresenceAbsence::Kappa(cmx.opt, st.dev=F),
             Sens = PresenceAbsence::sensitivity(cmx.opt, st.dev=F),
             Spec = PresenceAbsence::specificity(cmx.opt, st.dev=F),
             PCC = PresenceAbsence::pcc(cmx.opt, st.dev=F),
             D2 = expl_deviance(observation, predictions),
             thresh = thresh[thresh$Method==thresh.method,2])
}
