rm(list = ls())

args=(commandArgs(TRUE))
for(i in 1:length(args)){
  eval(parse(text=args[[i]]))
}

### need to input:
# method = c("glmnetcr", "mboost", "marginal", "rdvs", "knockoff")
# statistic = c("AIC", "BIC", "path", "mboost", "Bayesian", "Jonckheere", "uni_cumulative", "uni_sratio",
# "cv", "randomForest")
# seed: random seed, e.g., 1234
# A_list: vector of amplitude values, e.g., seq(0.2,1,0.2)
# ncores: number of cores for parallel computing
# nIteration: number of repetitive iterations
# q_t1e: target Type I error rate level
# rho: AR(1) correlation of design matrix

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
library(ordinalForest)
library(clinfun)
library(VGAM)

set.seed(seed)

n = 300
p = 1000
nTrue = 50        # number of true predictors
nClass = 3        # number of classes
# A_list = seq(0.2,1,0.2)  # amplitude
# ncores = 28
# nIteration = 30  # number of iterations
# rho = 0.5 # autocorrelation
H <- abs(outer(1:p, 1:p, "-"))
Sigma = rho^H
# q_t1e = 0.01       # target Type I Error
q_fdr = 0.2       # target FDR
m = 100 # number of repeated experiments to get reference distribution

# JAGS Set-up
adaptSteps <- 12000              #number of steps to "tune" the samplers
burnInSteps <- 12000             #number of steps to "burn-in" the samplers
nChains <- 3                    #number of chains to run
numSavedSteps <- 12000          #total number of steps in chains to save
thinSteps <- 4                 	#number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain

data.sample <- function(A)  {
  X = rmvn(n, mu=rep(0,p), sigma = Sigma, ncores = ncores)
  beta = rep(A, p) * sample(c(1,-1), p, replace = T)
  nonzero = sample(p, nTrue)
  delta = (1:p) %in% nonzero + 0 
  beta_delta = matrix(beta*delta, ncol = 1)
  Z = X %*% beta_delta + rlogis(n, location = 0, scale = 1)
  y = cut(Z, breaks = quantile(Z, probs = seq(0, 1, by = 1/nClass)), ordered_result = T, 
          labels = 1:nClass, include.lowest = T)
  return(list(X = X, beta_delta = beta_delta, y = y))
}

####
glmnet_stat <- function(X, y, alpha = 1, criterion = criterion){
  p = ncol(X)
  fit = glmnetcr::glmnetcr(X, y, maxit = 500, alpha = alpha)
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
  Z = c(Z, max(diag$psrf[,2]))
  names(Z)[length(Z)] = "PSRF"
  return(Z)
}

mboost_stat <- function(X, y){  
  data = data.frame(X = X, y = y)
  model = mboost(y ~ ., data = data, control = boost_control(mstop = 100), baselearner = "bbs",
                 family = PropOdds())
  cv10f = cv(model.weights(model), type="kfold", B = 5)
  cvm = cvrisk(model, folds = cv10f, papply = mclapply)
  best.model = model[mstop(cvm)]
  Z = as.numeric(varimp(best.model))
}

RF_stat <- function(X, y){
  data = data.frame(X = X, Y = y)
  model = ordfor("Y", data, num.threads = ncores)
  Z = model$varimp
}

fdp <- function(selected, nonzero) {
  if (length(selected)==0) return(0)
  else return(sum(!selected %in% nonzero) / max(1, length(selected)))
}

power <- function(selected, nonzero) {
  if (length(selected)==0) return(0)
  else return(sum(selected %in% nonzero) / nTrue)
}

type1Error <- function(selected, nonzero){
  if (length(selected)==0) return(0)
  else return(sum(!selected %in% nonzero) / (p-nTrue))
}

#### MAIN ####

iter <- function(A, method, statistic){
  
  data = data.sample(A)
  X = data$X
  y = data$y
  X = scale(X)  
  nonzero = which(data$beta_delta!=0)
  
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
    # if(statistic == "Bayesian"){
    #   PSRF = Z[,ncol(Z)] # length m
    #   Z = Z[,-ncol(Z)]
    # }
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
    if(statistic == "Bayesian"){
      PSRF = Z[length(Z)]
      Z = Z[-length(Z)]
    }
    orig = 1:p
    W = Z[orig] - Z[orig+p]
    T1 = knockoff.threshold(W, fdr = q_fdr, offset = 1)  # data-dependent threshold
    selected = (1:p)[W > T1]
  }
  
  output = matrix(c(fdp(selected, nonzero), power(selected, nonzero), type1Error(selected, nonzero)), 
                  ncol = 1)
  rownames(output) = c("FDP","Power","Type1Error")
  if (statistic == "Bayesian") colnames(output) = PSRF
  return(output)
}


start.time = Sys.time()
results = lapply(A_list, 
                 function(A){
                   print(paste("A =", A))
                   if (method == "rdvs"){
                     res = replicate(nIteration, iter(A, method = method, statistic = statistic), 
                                     simplify = "matrix")
                   } else{
                     registerDoMC(ncores)
                     res = foreach(i = 1:nIteration, .combine=cbind, 
                                   .packages = c("glmnetcr","rjags","knockoff","dclone", "mvnfast",
                                                 "mboost","ordinalForest","clinfun","VGAM")) %dopar% 
                                                 { iter(A, method = method, statistic = statistic) }
                   }
                   return(res)
                })
end.time = Sys.time()
end.time - start.time

if(length(A_list)==5) A = "" else A = 10*A_list
save(results, file = paste("result1028", A, seed, method, statistic, rho, q_t1e, ".RData", sep = "_"))
