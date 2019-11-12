rm(list = ls())

args=(commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text=args[[i]]))
}

### need to input:
# method = c("glmnetcr", "mboost", "marginal", "rdvs", "knockoff")
# statistic = c("AIC", "BIC", "path", "mboost", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio",
# "cv", "randomForest")
# ncores: number of cores for parallel computing
# q_t1e: target Type I Error

### method + statistic combination:
# "glmnetcr" + "AIC" / "BIC" / "cv"
# "mboost"  + "mboost"
# "marginal" + "Jonckheere" / "uni_cumulative" / "uni_sratio"
# "rdvs" + "AIC" / "BIC" / "path" / "cv" / "mboost" / "randomForest"
# "knockoff" + "AIC" / "BIC" / "path" / "cv" / "mboost" / "Bayesian" / "randomForest"

if(statistic == "Bayesian") options("expressions"=100000)

library(glmnetcr)
library(doMC)
library(rjags)
library(knockoff)
library(dclone)
library(mvnfast)
library(mboost)
library(coda)
library(ordinalForest)
library(clinfun)
library(VGAM)

set.seed(1234)

load("LiverSamples.RData")   # n = 56, p = 1505 (1413 after removing NA and low sd)
X = t(exprs(data))
# feature_keep_ind = apply(X, 2, function(x) !any(is.na(x))) & (apply(X, 2, sd)>0.01)
feature_keep_ind = apply(X, 2, function(x) !any(is.na(x))) & (colMeans(X)<=0.95) & (colMeans(X)>=0.05)
X = X[,feature_keep_ind]
gene_name[[3]] = featureData(data)@data$ID[feature_keep_ind]
y <- factor(pData(data)$TissueType, c("Normal", "Cirrhosis non-HCC", "Tumor"), ordered=TRUE)
# data = list(X = X, y = y)


# load("LiverSamples.RData")
# X = data$X
# y = data$y
n = nrow(X)
p = ncol(X)
# ncores = 28
# q_t1e = 0.01       # target Type I Error
q_fdr = 0.2       # target FDR
m = 100 # number of repeated experiments to get reference distribution
nClass = 3

# JAGS Set-up
adaptSteps <- 10000              #number of steps to "tune" the samplers
burnInSteps <- 10000             #number of steps to "burn-in" the samplers
nChains <- 3                    #number of chains to run
numSavedSteps <-15000          #total number of steps in chains to save
thinSteps <- 3                 	#number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain


####
glmnet_stat <- function(X, y, alpha = 1, criterion = criterion){
  p = ncol(X)
  fit = glmnetcr(X, y, maxit = 500, alpha = alpha)
  step = select.glmnetcr(fit, which = criterion)
  Z = abs(coef(fit, s = step)$beta[1:p])
}

####
path_stat <- function(X, y){
  p = ncol(X)
  fit = glmnetcr::glmnetcr(X, y, maxit = 500, nlambda = 100, standardize = T)
  first_nonzero <- function(x) match(T, abs(x) > 0)
  indices <- apply(fit$beta[1:p,], 1, first_nonzero)
  names(indices) <- NULL
  Z = ifelse(is.na(indices), 0, fit$lambda[indices] * n)
}

####
cv_stat <- function(X, y){
  n_folds = 5
  nlambda=100
  restructure <- glmnetcr::cr.backward(x = X, y = y, weights = rep(1, n))
  lambda_max = max(abs(t(restructure[, -c(1, 2)]) %*% restructure[,"y"])) / n
  lambda_min = lambda_max * 0.01
  k1 = (0:(nlambda-1)) / nlambda
  lambda = lambda_max * (lambda_min/lambda_max)^k1
  folds_i = sample(rep(1:n_folds, length.out = n))
  error = matrix(NA, n_folds, nlambda)
  for (k in 1:n_folds) {
    test_i = which(folds_i == k)
    Xtr = X[-test_i, ]
    Xt = X[test_i, ]
    ytr = y[-test_i]
    yt = y[test_i]
    fit = glmnetcr::glmnetcr(Xtr, ytr, maxit = 500, lambda = lambda, standardize = T)
    pred = predict(fit, Xt)$class
    error[k,] = apply(pred, 2, function(pr) mean(pr != yt))
  }
  mean_error = colMeans(error)
  select = which.min(mean_error)
  fit = glmnetcr::glmnetcr(X, y, maxit = 500, lambda = lambda[select], standardize = T)
  Z = as.vector(abs(fit$beta[1:ncol(X),]))
}

####
bayes_stat <- function(X, y, parallel = T){
  y = as.numeric(y)
  ## initial values 
  init_beta = lm(y~X-1)$coefficients
  inits1 <- list("alpha0" = sort(c(rnorm(1,log(.5),.5), rnorm(1,log(2),.5))), "beta" = init_beta)
  inits2 <- list("alpha0" = c(log(.5), log(2)), "beta" = init_beta)
  inits3 <- list("alpha0" = sort(c(rnorm(1,log(.5),.5), rnorm(1,log(2),.5))), "beta" = init_beta)
  data = list(X = X, y = matrix(y,ncol=1), n = n, p = ncol(X), k = nClass)
  
  if (parallel) {
    cl <- parallel::makePSOCKcluster(nChains)
    dclone::parJagsModel(cl = cl, name = "res", file = "bayes.txt", data = data, n.chains = nChains, 
                         quiet=FALSE, inits = list(inits1, inits2, inits3), n.adapt=adaptSteps)
    cat( "Burning in the MCMC chain...\n")
    dclone::parUpdate(cl = cl, object = "res", n.iter = burnInSteps)
    cat("Sampling final MCMC chain...\n" )
    codaSamples <- dclone::parCodaSamples(cl = cl, model = "res", variable.names=c("beta"), n.iter=nIter, 
                                          thin=thinSteps)
  } else{
    model = rjags::jags.model(file = "bayes.txt", data = data, n.chains = nChains, quiet=FALSE,
                              inits = list(inits1, inits2, inits3), n.adapt=adaptSteps)
    cat( "Burning in the MCMC chain...\n")
    update(model, n.iter=burnInSteps)  
    cat("Sampling final MCMC chain...\n" )
    codaSamples <- rjags::coda.samples(model, variable.names=c("beta"), n.iter=nIter, 
                                       thin=thinSteps)
  }
  
  mcmcChain <- as.matrix(codaSamples)
  Z = abs(colMeans(mcmcChain))
  diag = gelman.diag(codaSamples, multivariate = F)
  cat(paste("PSRF =", max(diag$psrf[,2])))
  return(Z)
}

####
mboost_stat <- function(X, y){  
  data = data.frame(X = X, y = y)
  model = mboost(y ~ ., data = data, control = boost_control(mstop = 100), baselearner = "bbs",
                 family = PropOdds())
  cv10f = cv(model.weights(model), type="kfold", B = 5)
  cvm = cvrisk(model, folds = cv10f, papply = mclapply)
  best.model = model[mstop(cvm)]
  Z = as.numeric(varimp(best.model))
}

####
RF_stat <- function(X, y){
  data = data.frame(X = X, Y = y)
  model = ordfor("Y", data)
  Z = model$varimp
}

#### MAIN ####

main <- function(method, statistic){

  X = scale(X)
  
  ####
  if (method == "glmnetcr"){
    if (statistic == "cv"){
      Z = cv_stat(X, y)
    } else{
      Z = glmnet_stat(X, y, alpha = 1, statistic)
    }
    selected = (1:p)[Z > 0] # selected variables
  }
  
  ####
  if (method == "mboost"){
    Z = mboost_stat(X, y)
    selected = (1:p)[Z > 0] # selected variables
  }
  
  ####
  if (method == "marginal"){
    pval = switch(statistic,
                  "Jonckheere" = apply(X, 2, function(x) jonckheere.test(x, y, nperm = 1000)$p.value),
                  "uni_cumulative" = apply(X, 2, 
                                           function(x) summary(vglm(y~x, family = cumulative(par=TRUE)))@coef3[3,4]),
                  "uni_sratio" = apply(X, 2, 
                                       function(x) summary(vglm(y~x, family = sratio(par=TRUE, rev=T)))@coef3[3,4]))
    pval_adj = p.adjust(pval, method = "BH")
    selected = (1:p)[pval_adj < q_fdr]
  } 
  
  ####
  if (method == "rdvs"){
    registerDoMC(ncores)
    Z = foreach(i = 1:m, .combine=rbind) %dopar% {
      inert = rnorm(n, mean = 0, sd = 1)
      Xaug = cbind(X, inert)
      return(switch(statistic,
                    "AIC" = glmnet_stat(Xaug, y, alpha = 1, criterion = "AIC"),
                    "BIC" = glmnet_stat(Xaug, y, alpha = 1, criterion = "BIC"),
                    "path" = path_stat(Xaug, y),
                    "cv" = cv_stat(Xaug, y),
                    "mboost" = mboost_stat(Xaug, y),
                    # "Bayesian" = bayes_stat(Xaug, y, parallel = F), # not calculated due to efficiency
                    "randomForest" = RF_stat(Xaug, y)))  # m * (p+1)
    }
    W = colMeans(Z[,1:p])
    T1 = quantile(Z[,p+1], probs=1-q_t1e)
    selected = (1:p)[W > T1]
  }
  
  ####
  if (method == "knockoff"){
    X_k = create.second_order(X, method = "asdp", shrink = T)  #knockoffs
    Xaug = cbind(X, X_k)  # augmented X
    Z = switch(statistic,
               "AIC" = glmnet_stat(Xaug, y, alpha = 1, criterion = "AIC"),
               "BIC" = glmnet_stat(Xaug, y, alpha = 1, criterion = "BIC"),
               "path" = path_stat(Xaug, y),
               "cv" = cv_stat(Xaug, y),
               "mboost" = mboost_stat(Xaug, y),
               "Bayesian" = bayes_stat(Xaug, y, parallel = T),
               "randomForest" = RF_stat(Xaug, y))
    orig = 1:p
    W = Z[orig] - Z[orig+p]
    T1 = knockoff.threshold(W, fdr = q_fdr, offset = 1)  # data-dependent threshold
    selected = (1:p)[W > T1]
  }
  
  return(colnames(X)[selected])
}

start.time = Sys.time()
selection = main(method, statistic)
Sys.time() - start.time

print(selection)

save(selection, file = paste("selection417_real", method, statistic, ".RData", sep = "_"))
