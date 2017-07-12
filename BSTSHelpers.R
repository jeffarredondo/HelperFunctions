library(bsts)
library(ggplot2)
library(doParallel)
library(foreach)

select.best.bsts <- function(Y = 1, X = 1, univariate = T, data_frequency = c("monthly","daily"), ncores = 2, ...){
  
  #this function will evaluate and build the best BSTS state space 
  #specification model for your data based on the 1 step ahead prediction errors
  require(doParallel)
  require(foreach)
  require(bsts)
  
  ifelse(length(X) == 1, X <- 1, X <- as.matrix(X))
  
  build.state.spec <- function(Y, modelType,...){
    require(bsts)
    ss <- list()
    if(modelType == "LocalLevel"){
      ss <- AddLocalLevel(ss, Y)
    } else if(modelType == "LocalLinear"){
      ss <- AddLocalLinearTrend(ss, Y)
    } else if(modelType == "SemiLocalLinear"){
      ss <- AddSemilocalLinearTrend(ss, Y)
    } else if(modelType == "ARLag1"){
      ss <- AddAr(ss, Y, 1)
    } else if(modelType == "ARLag1_2"){
      ss <- AddAr(ss, Y, lags = 1)
      ss <- AddAr(ss, Y, lags = 2)
    } else if(modelType == "StudentLocalLinear"){
      ss <- AddStudentLocalLinearTrend(ss, Y)
    } else if(modelType == "LocalLevelwAR1"){
      ss <- AddLocalLevel(ss, Y)
      ss <- AddAr(ss, Y, lags = 1)
    }
    return(ss)
  }
  
  build.seasonal.spec <- function(Y, ss, seasonalDurations, numberSeasons, ...){
    require(bsts)
    ss <- AddSeasonal(ss, Y, nseasons = numberSeasons[1], season.duration = seasonalDurations[1])
    if(length(Y) > 2*seasonalDurations[2]){
      ss <- AddSeasonal(ss, Y, nseasons = numberSeasons[2], season.duration = seasonalDurations[2])
    }
    #if(length(Y) > 2*seasonalDurations[3]){
    #  ss <- AddSeasonal(ss, Y, nseasons = numberSeasons[3], season.duration = seasonalDurations[3])
    #}
    return(ss)
  }
  
  select.best.predictors <- function(Y, X, ss){
    toy.fit <- bsts(Y ~ X, ss, niter = 1000, seed = 2017, ping = 0) 
    coefs <- data.frame(summary(toy.fit)$coefficients[-(which(row.names(summary(toy.fit)$coefficients)=="(Intercept)")),])
    vars <- row.names(coefs[coefs$inc.prob>=max(summary(toy.fit)$coefficients[,5])*.5,])
    for(k in 1:length(vars)){
      if(substr(vars[k],1,1)=="X"){
        vars[k] <- substr(vars[k],2,nchar(vars[k]))
      }
    }
    return(vars)
  }
  
  modelTypes <- c("LocalLevel",
                  "LocalLinear",
                  "SemiLocalLinear",
                  "ARLag1",
                  "ARLag1_2",
                  "StudentLocalLinear",
                  "LocalLevelwAR1")

  if(data_frequency=="monthly"){
    seasonalDurations <- c(1,3)
    numberSeasons <- c(12,4)
  } else if(data_frequency == "daily"){
    seasonalDurations <- c(30,90,365)
    numberSeasons <- c(12,4,1)
  } else{
    print("Not a valid frequency type")
    break
  }

  #start cluster
  doParallel::registerDoParallel(cores = ncores)
  #build possible model types across ncores
  r <- foreach::foreach(i=1:length(modelTypes),.combine = cbind) %dopar% {
    
    ss <- build.state.spec(Y, modelType = modelTypes[i])
    ss <- build.seasonal.spec(Y, ss, seasonalDurations, numberSeasons)
    #univariate or multivariate time series model
    if(univariate == T){
      toy.fit <- bsts(Y, ss, niter = 10000, seed = 2017, ping = 0) 
    }else{
      toy.fit <- bsts(Y ~ X, ss, niter = 10000, seed = 2017, ping = 0) 
    }
    #last line is returned to r
    oneStepSums <- abs(sum(colMeans(toy.fit$one.step.prediction.errors)))
  }
  #stop cluster
  doParallel::stopImplicitCluster()
  registerDoSEQ()
  
  #select best model 
  best.bsts <- modelTypes[which.min(r)]
  #build out full model
  ss <- build.state.spec(Y, modelType = best.bsts)
  ss <- build.seasonal.spec(Y, ss, seasonalDurations, numberSeasons)
  
  #select regressors from elastic net
  if(univariate == T){
    
  }else{
    best.vars <- select.best.predictors(Y, X, ss)
    best_X <- X[,which(colnames(X) %in% best.vars)]
    best_X <- as.matrix(best_X)
    colnames(best_X) <- best.vars
    assign("best_vars_list",best.vars,envir = globalenv())
  }
  
  #univariate or multivariate time series model
  if(univariate == T){
    final.fit <- bsts(Y, ss, niter = 100000, seed = 2017, ping = 0) 
  }else{
    final.fit <- bsts(Y ~ best_X, ss, niter = 100000, seed = 2017, ping = 0) 
  }
  return(final.fit)
}

# 
# 
# ##test functions
# data("AirPassengers")
# # Predictor <- seq(1:144)*seq(1:144)
# # test <- select.best.bsts(Y = AirPassengers, X = cbind(Predictor1 = Predictor, Predictor2 = Predictor), univariate = F, data_frequency = "monthly")
# # summary(test)
# 
# 
# test <- select.best.bsts(Y = AirPassengers[1:140], univariate = T, data_frequency = "monthly")
# summary(test)
# plot.bsts(test)
# plot.bsts(test, "components")
# 
# p <- predict.bsts(test, horizon = 4, burn = SuggestBurn(.1, test))
# mean(abs(round(p$mean,0)/AirPassengers[141:144]-1))



