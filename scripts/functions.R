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

#' select07_cv
#'
#' Select weakly correlated variables based on cross-validated univariate variable importance \insertCite{Zurell2020}{mecofun}. Extension of select07() function described in \insertCite{Dormann2013}{mecofun}. In contrast to select07(), the univariate variable importance is estimated as explained deviance obtained on cross-validated predictions. Variable importance can also be pre-defined by hand.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param X Matrix or data.frame containing the predictor variables
#' @param y vector of response variable
#' @param kfold number of folds for random cross-validation or vector with group assignments (indexing which data point belongs to which fold)
#' @param family a description of the error distribution and link function to be used in the model.
#' @param univar a character string indicating the regression method to be used for estimating univariate importance. Must be one of the strings "glm1", "glm2" (default), or "gam". "glm1" will estimate a generalised linear model (GLM) with a linear predictor, "glm2" a GLM with a second order polynomial, and "gam" a generalised additive model (GAM) with smooting splines 
#' @param threshold a numeric value indicating the absolute value of the correlation coefficient above which the paired correlation are judged as problematic.
#' @param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of "spearman" (default), "kendall", or "pearson".
#' @param sequence an optional character vector providing the order of importance of the predictors. This overrides the univar method.
#' @param weights an optional vector of prior weights to be used in univariate GLMs or GAMs
#' 
#' @return A list with three objects: "D2" a numeric vector containing the explained deviance for the univariate models, "cor_mat" containing the correlation matrix, and "pred_sel" a character vector with the names of the remaining, weakly correlated variables. The variables are ordered according to their univariate variable importance (starting with most important variable).
#' 
#' @examples 
#' data(Anguilla_train)
#' select07_cv(X=Anguilla_train[,3:10], y=Anguilla_train[,2])
#' 
#' @seealso [select07()]
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
select07_cv <- function(X, y, kfold=5, family="binomial",univar="glm2", threshold=0.7, method="spearman", sequence=NULL, weights=NULL)
{
  # selects variables based on removing correlations > 0.7, retaining those
  # variables more important with respect to y
  # Order of importance can be provided by the character vector 'sequence'
  
  # 1. step: cor-matrix
  # 2. step: importance vector
  # 3. step: identify correlated pairs
  # 4. step: in order of importance: remove collinear less important variable,
  #           recalculate correlation matrix a.s.f.
  
  # Make k-fold data partitions
  if (length(kfold)==1) {
    ks <- dismo::kfold(y, k = kfold)
  } else {
    ks <- kfold
  }
  
  compute.univar.cv <- function(variable, response, family,univar,ks,weights){
    preds <- numeric(length(response))
    
    for (n in unique(ks)) {
      df <- data.frame(occ=response,env=variable)
      train_df <- df[!ks ==n,]
      test_df <-  df[ks==n, ]
      
      m1 <- switch(univar,
                   glm1 = glm(occ ~ env, data=train_df, family=family, weights=weights[!ks ==n]),
                   glm2 = glm(occ ~ poly(env,2), data=train_df, family=family, weights=weights[!ks ==n]),
                   gam = mgcv::gam(occ ~ s(env,k=4), data=train_df, family=family, weights=weights[!ks ==n]))
      
      preds[ks==n] <- predict(m1,newdata=test_df,type='response')
    }
    d2 <- expl_deviance(response,preds)
    ifelse(d2<0,0,d2)
  }
  
  imp <- apply(X, 2, compute.univar.cv, response=y, family=family, univar=univar, ks=ks,weights=weights)
  
  cm <- cor(X, method=method)
  
  if (is.null(sequence)) {
    sort.imp <- colnames(X)[order(imp,decreasing=T)]
  } else { 
    sort.imp <- sequence 
  }
  
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
  return(list(D2=sort(imp, decreasing = T), cor_mat=cm, pred_sel=pred_sel))
}



#' select07_cv_boyce
#'
#' Select weakly correlated variables based on cross-validated univariate variable importance \insertCite{Zurell2020}{mecofun}. Extension of select07() function described in \insertCite{Dormann2013}{mecofun}. In contrast to select07(), the univariate variable importance is estimated as explained deviance obtained on cross-validated predictions. Variable importance can also be pre-defined by hand.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param X Matrix or data.frame containing the predictor variables
#' @param y vector of response variable
#' @param kfold number of folds for random cross-validation or vector with group assignments (indexing which data point belongs to which fold)
#' @param family a description of the error distribution and link function to be used in the model.
#' @param univar a character string indicating the regression method to be used for estimating univariate importance. Must be one of the strings "glm1", "glm2" (default), or "gam". "glm1" will estimate a generalised linear model (GLM) with a linear predictor, "glm2" a GLM with a second order polynomial, and "gam" a generalised additive model (GAM) with smooting splines 
#' @param threshold a numeric value indicating the absolute value of the correlation coefficient above which the paired correlation are judged as problematic.
#' @param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of "spearman" (default), "kendall", or "pearson".
#' @param sequence an optional character vector providing the order of importance of the predictors. This overrides the univar method.
#' @param weights an optional vector of prior weights to be used in univariate GLMs or GAMs
#' 
#' @return A list with three objects: "D2" a numeric vector containing the explained deviance for the univariate models, "cor_mat" containing the correlation matrix, and "pred_sel" a character vector with the names of the remaining, weakly correlated variables. The variables are ordered according to their univariate variable importance (starting with most important variable).
#' 
#' @examples 
#' data(Anguilla_train)
#' select07_cv(X=Anguilla_train[,3:10], y=Anguilla_train[,2])
#' 
#' @seealso [select07()]
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
select07_cv_boyce <- function(X, y, kfold=5, family="binomial",univar="glm2", threshold=0.7, method="spearman", sequence=NULL, weights=NULL)
{
  # selects variables based on removing correlations > 0.7, retaining those
  # variables more important with respect to y
  # Order of importance can be provided by the character vector 'sequence'
  
  # 1. step: cor-matrix
  # 2. step: importance vector
  # 3. step: identify correlated pairs
  # 4. step: in order of importance: remove collinear less important variable,
  #           recalculate correlation matrix a.s.f.
  
  # Make k-fold data partitions
  if (length(kfold)==1) {
    ks <- dismo::kfold(y, k = kfold)
  } else {
    ks <- kfold
  }
  
  compute.univar.cv <- function(variable, response, family,univar,ks,weights){
    preds <- numeric(length(response))
    
    for (n in unique(ks)) {
      df <- data.frame(occ=response,env=variable)
      train_df <- df[!ks ==n,]
      test_df <-  df[ks==n, ]
      
      m1 <- switch(univar,
                   glm1 = glm(occ ~ env, data=train_df, family=family, weights=weights[!ks ==n]),
                   glm2 = glm(occ ~ poly(env,2), data=train_df, family=family, weights=weights[!ks ==n]),
                   gam = mgcv::gam(occ ~ s(env,k=4), data=train_df, family=family, weights=weights[!ks ==n]))
      
      preds[ks==n] <- predict(m1,newdata=test_df,type='response')
    }
    
    # Retrieve the indices of the presences
    presences_indices <- which(y == 1)
    
    # Just retain cross-validated predictions of presences
    preds_presences <- preds[presences_indices]
    
    # Calculate Boyce index
    boyce_index <- ecospat.boyce(fit = preds, obs = preds_presences, nclass=0, 
                                 window.w="default", res=100, PEplot = FALSE, 
                                 rm.duplicate = TRUE, method = 'kendall')
    
    # Extract the Boyce index value
    boyce_index$cor
  }
  
  imp <- apply(X, 2, compute.univar.cv, response=y, family=family, univar=univar, ks=ks,weights=weights)
  
  cm <- cor(X, method=method)
  
  if (is.null(sequence)) {
    sort.imp <- colnames(X)[order(imp,decreasing=T)]
  } else { 
    sort.imp <- sequence 
  }
  
  pairs <- which(abs(cm)>= threshold, arr.ind=T) # identifies correlated variable pairs
  index <- which(pairs[,1]==pairs[,2])           # removes entry on diagonal
  pairs <- pairs[-index,]                        # -"-
  
  exclude <- NULL
  for (i in 1:length(sort.imp)) {
    if ((sort.imp[i] %in% row.names(pairs))&
        ((sort.imp[i] %in% exclude)==F)) {
      cv<-cm[setdiff(row.names(cm),exclude),sort.imp[i]]
      cv<-cv[setdiff(names(cv),sort.imp[1:i])]
      exclude<-c(exclude,names(which((abs(cv)>=threshold)))) }
  }
  
  pred_sel <- sort.imp[!(sort.imp %in% unique(exclude)),drop=F]
  return(list(D2=sort(imp, decreasing = T), cor_mat=cm, pred_sel=pred_sel))
}




#-------------------------------------------------------------------------------

# 3. Function for cross-validation ---------------------------------------------

#' predictSDM
#'
#' Make SDM predictions 
#' @param model model object
#' @param newdata a data frame in which to look for variables for which predictions should be made. 
#' @return A numeric vector with predictions.
#' @examples 
#' data(Anguilla_train)
#' data(Anguilla_test)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' predictSDM(m1, Anguilla_test)
#' @export
predictSDM <- function(model, newdata) {
  switch(class(model)[1],
         Bioclim = predict(model, newdata),
         Domain = predict(model, newdata),
         glm = predict(model, newdata, type='response'),
         Gam = predict(model, newdata, type='response'),
         gam = predict(model, newdata, type='response'),
         negbin = predict(model, newdata, type='response'),
         rpart = predict(model, newdata),
         randomForest.formula = switch(model$type,
                                       regression = predict(model, newdata, type='response'),
                                       classification = predict(model, newdata, type='prob')[,2]),
         randomForest = switch(model$type,
                               regression = predict(model, newdata, type='response'),
                               classification = predict(model, newdata, type='prob')[,2]),
         gbm = switch(ifelse(is.null(model$gbm.call),"GBM","GBM.STEP"), 
                      GBM.STEP =  predict.gbm(model, newdata, 
                                              n.trees=model$gbm.call$best.trees, type="response"),
                      GBM = predict.gbm(model, newdata, 
                                        n.trees=model$n.trees, type="response")),
         maxnet = predict(model, newdata, type="logistic"))
}



#' crossvalSDM
#'
#' A function for deriving cross-validated predictions. The function partitions the data into k folds, determines the model algorithm, updates the model for the new training data and makes predictions to the hold-out data using this algorithm.
#' @param model model object
#' @param traindat a data frame holding the training data. 
#' @param colname_species a character string indicating the name of the column holding the response data
#' @param colname_pred a character vector indicating the names of the columns holding the predictor variables
#' @param env_r a raster stack of the environmental predictors
#' @param colname_coord a character vector indicating the names of the columns holding the species coordinates
#' @param kfold a single numeric value indicating the number of folds that the data will be split in or a numeric vector holding the indices for the data partition
#' @return A numeric vector with cross-validated predictions.
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' preds_cv <- crossvalSDM(m1, kfold=5, traindat=Anguilla_train, colname_species = 'Angaus', colname_pred = 'SegSumT')
#' evalSDM(Anguilla_train$Angaus, preds_cv)
#' @export
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

# 4. Cross-validation function for GLMs with weights ---------------------------


crossval_glm <- function(model, kfold=5, traindat, colname_species, colname_pred,
                         env_r=NULL, colname_coord=NULL, weights=NULL) {
  
  # cross-validate GLM with weights - somehow the mecofun function doesn't work for weights in GLM although it works for GAM
  
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
    
    data_final <- cv_train
    data_final$weights <- weights[ks!=i]
    
    # We update the model for the new training data
    modtmp <- update(model, data=cv_train, weights=data_final$weights)
    
    # We make predictions for k-fold:
    cross_val_preds[ks==i] <- predictSDM(modtmp, cv_test[, colname_pred, drop=F])
  }
  cross_val_preds
}

#-------------------------------------------------------------------------------

# 4. Function for evaluation of performance metrics ----------------------------

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

#' TSS
#'
#' Calculates the true skill statistic (sensitivity+specificity-1) \insertCite{allouche2006}{mecofun}.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param cmx a confusion matrix
#' 
#' @return A numeric value.
#' 
#' @examples  TSS()
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
TSS = function(cmx){
  PresenceAbsence::sensitivity(cmx, st.dev=F) + 
    PresenceAbsence::specificity(cmx, st.dev=F) - 1
}


#' expl_deviance
#'
#' Calculates the explained deviance based on the dismo package.
#' 
#' @param obs a numeric vector of observations
#' @param pred a numeric vector of predictions
#' @param family a description of the error distribution and link function to be used in the model.
#' 
#' @return A numeric value.
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' expl_deviance(Anguilla_train$Angaus, m1$fitted)
#' 
#' @seealso [calc.deviance()]
#' 
#' @export
expl_deviance <- function(obs, pred, family='binomial'){
  if (family=='binomial') {pred <- ifelse(pred<.00001,.00001,ifelse(pred>.9999,.9999,pred))}
  
  null_pred <- rep(mean(obs), length(obs))
  
  1 - (dismo::calc.deviance(obs, pred, family=family) / 
         dismo::calc.deviance(obs, null_pred, family=family))
}


#-------------------------------------------------------------------------------

# 5. Function for response plot ------------------------------------------------

#' inflated_response
#'
#' plot inflated response curves - inflated partial dependence plots \insertCite{Zurell2012}{mecofun}. Plot effect of one variable on response variable over the range (min,mean,median,max and quartiles) of other predictors. As the number of combinations increases exponentially, the maximum number of combinations can be set with lhsample. Whenever lhsample is exceeded, candidate combinations are drawn by latin hypercube sampling.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param object model object.
#' @param predictors a data frame with predictor variables. 
#' @param select.columns optional character vector indicating subset of predictors to plot
#' @param label optional character vector indicating alternative names of predictors for labelling plots
#' @param len a numeric value indicating the number of intervals for drawing the environmental gradients
#' @param lhsample a numeric value indicating the number of latin hypercube samples to draw
#' @param lwd line width
#' @param method character indicating at which values the other predictors are held constant. Needs to take a value of "mean", "stat3" (default), or "stat6". "stat3" considers minimum, mean and maximum values of predictors. "stat6" considers min,mean,median,max and quartiles.
#' @param disp can take options "all" (default) or "eo.mask" - in the latter case, eo.mask() is used to distinguish between areas of the estimated environmental niche / plotting areas that are supported by data and those that require extrapolation.
#' @param overlay.mean logical value. If true, then the mean response curve is overlaid on the inflated plot
#' @param ylab y axis label
#' @param col.curves colour of response curves
#' @param col.novel colour of novel environments
#' @param col.mean colour of mean response
#' @param lwd.known line width for known environments
#' @param lwd.mean line width of mean response
#' @param ... further plotting parameters
#' 
#' @return A numeric vector with predictions.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2) + poly(SegTSeas,2) + poly(DSDist,2), data=Anguilla_train, family='binomial')
#' par(mfrow=c(1,3))
#' inflated_response(m1,Anguilla_train[,c('SegSumT','SegTSeas','DSDist')])
#' 
#' @export
inflated_response=function(object,predictors,select.columns=NULL,label=NULL,
                           len=50,lhsample=100, lwd=1, ylab=NULL,
                           method="stat3",disp="all",overlay.mean=T,
                           col.curves='grey',col.novel='grey',col.mean='black',lwd.known=2,lwd.mean=2,ylim=c(0,1),...){
  
  if (is.null(select.columns)) select.columns=seq_len(ncol(predictors))
  
  for (i in select.columns)
  {
    summaries=data.frame(matrix(0,6,ncol(predictors)))
    for (iz in 1:ncol(predictors)) {
      summaries[,iz]=summary(predictors[,iz])
    }
    if (method=="stat3") {
      summaries.j=as.matrix(summaries[c(1,4,6),-i],ncol=(ncol(predictors)-1));comb=min(lhsample,3^(ncol(predictors)-1));nc=3
    } else
      if (method=="stat6") {
        summaries.j=as.matrix(summaries[,-i],ncol=(ncol(predictors)-1));comb=min(lhsample,6^(ncol(predictors)-1));nc=6
      } else
        if (method=="mean") {
          summaries.j=as.matrix(summaries[4,-i],ncol=(ncol(predictors)-1));comb=1;nc=1;overlay.mean=F
        }
    
    dummy.j=as.matrix(predictors[1:len,-i],ncol=(ncol(predictors)-1))
    
    if (comb<lhsample) {
      mat=vector("list",ncol(dummy.j))
      for (m in 1:ncol(dummy.j)) mat[[m]]=1:nc
      mat=expand.grid(mat)
    } else {
      mat=round(qunif(lhs::randomLHS(lhsample,ncol(dummy.j)),1,nrow(summaries.j)),0)
    }
    
    if (is.null(label)) {
      label=names(predictors)
    }
    
    for (r in 1:nrow(mat))
    {
      for (j in 1:ncol(dummy.j))
      {
        dummy.j[,j]=as.vector(rep(summaries.j[mat[r,j],j],len))
      }
      
      dummy=data.frame(seq(min(predictors[,i]),max(predictors[,i]),length=len),dummy.j)
      names(dummy)[-1]=names(predictors)[-i]
      names(dummy)[1]=names(predictors)[i]
      
      curves <- predictSDM(object, dummy)
      
      # display all lines in same type
      if (disp=='all')
      {
        if (r==1)
        {
          if (i==1) plot(dummy[,names(predictors)[i]],
                         curves,type="l",ylim=ylim,xlab=label[i],
                         lwd=lwd,col=col.curves,ylab=ylab, ...)
          else plot(dummy[,names(predictors)[i]],
                    curves,type="l",ylim=ylim,xlab=label[i],lwd=lwd,col=col.curves,
                    ylab='', ...)
        }
        else lines(dummy[,names(predictors)[i]],
                   curves,lwd=lwd,col=col.curves,...)
      }
      
      # highlight extrapolation to novel environmental conditions
      if (disp=='eo.mask')
      {
        novel=eo.mask(predictors,dummy)
        curves.known=curves
        curves.known[novel==1]=NA
        curves.novel=curves
        curves.novel[novel==0]=NA
        
        if (r==1)
        {
          if (i==1) {plot(dummy[,names(predictors)[i]],
                          curves.known,type="l",ylim=ylim,xlab=label[i],
                          lwd=lwd.known,col=col.curves,ylab=ylab,...)
            lines(dummy[,names(predictors)[i]],
                  curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
          else {plot(dummy[,names(predictors)[i]],
                     curves.known,type="l",ylim=ylim,xlab=label[i],lwd=lwd.known,
                     col=col.curves,ylab='',...)
            lines(dummy[,names(predictors)[i]],
                  curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
        }
        else {lines(dummy[,names(predictors)[i]],
                    curves.known,lwd=lwd.known,col=col.curves,...)
          lines(dummy[,names(predictors)[i]],
                curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
      }
    }
    
    #-------------------------------------------------
    # now, this is for overlaying mean response curve
    if (overlay.mean==T)
    {
      dummy=predictors[1:len,]
      dummy[,i]=seq(min(predictors[,i]),max(predictors[,i]),length=len)
      for (j in 1:ncol(predictors))
      {
        if (j!=i) 
        {
          dummy[,j]=rep(mean(predictors[,j]),len)
        }
      }
      
      curves <- predictSDM(object, dummy)
      
      lines(dummy[,names(predictors)[i]],
            curves,lwd=lwd.mean,col=col.mean,...)
    }    
  }}

#' partial_response
#'
#' plot partial response curves. Plot effect of one variable on response variable while keeping the other predictors constant at their mean.
#' 
#' @param object model object
#' @param predictors a data frame with predictor variables. 
#' @param select.columns optional character vector indicating subset of predictors to plot
#' @param label optional character vector indicating alternative names of predictors for labelling plots
#' @param ylab y axis label
#' @param len a numeric value indicating the number of intervals for drawing the environmental gradients
#' @param lwd line width
#' @param col colour of response curves
#' @param lwd line width
#' @param ... further plotting parameters
#' 
#' @return A numeric vector with predictions.
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2) + poly(SegTSeas,2) + poly(DSDist,2), data=Anguilla_train, family='binomial')
#' par(mfrow=c(1,3))
#' partial_response(m1,Anguilla_train[,c('SegSumT','SegTSeas','DSDist')])
#' 
#' @export
partial_response=function(object,predictors,select.columns=NULL, label=NULL, len=50,
                          col='black',ylab=NULL, ...){
  
  inflated_response(object,predictors,select.columns,label,len,method='mean',
                    col.curves=col, ylab=ylab,...)
}
