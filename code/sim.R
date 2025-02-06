# load packages and functions --------------------------------------------------------
library(doParallel)
library(tpc)
library(tidyverse)
source("fxn.R")

# simulation -------------------------------------------------------------------------
d <- 10 # number of nodes
avg_nb <- 7 # expected number of neighbors per node in true graph
tier <- c(1,1,1,2,2,2,2,2,3,3) 
i <- 6; j <- 10 # estimand is the causal effect of i on j

#### DON'T RUN ######################################
set.seed(123)
trueDAG <- randDAG(d, avg_nb, "er", wFUN = list(wFUN, 0.5, 1.0))
trueDAG <- topsort(trueDAG)
trueBeta <- causalEffect_modified(trueDAG, y = j, x = i) 
#####################################################

nsim <- 500
nb_max <- 7 # maximum number of neighbors per node 
nu <- 0.025
n <- 500 # sample size
M <- 1000 # number of resamples
c = 0.01 # c star in the threshold adjustment factor tau 
L = (nb_max+1) * d*(d-1)/2 # maximum number of independencies to be evaluated
tau = c*(log(n)/M)^(1/L) # threshold adjustment factor
thres = tau * qnorm(nu/(2*L)) # threshold to compare with z(pcorr) for the new independence test (negative number, retain null (remove edge) if -|z(pcorr)| > threshold)
z = -qnorm((0.05-nu)/2) # z score for constructing CI

cl <- makeCluster(detectCores() - 1)  
registerDoParallel(cl)
pb <- txtProgressBar(min = 1, max = nsim, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
system.time({
  sim <- foreach(ii = 1:nsim, .options.snow = list(progress = progress), .packages = c("tpc", "tidyverse", "igraph", "mvtnorm", "pcalg", "truncnorm")) %dopar% {
    set.seed(ii)
    
    # generate true DAG
    trueDAG <- randDAG(d, avg_nb, "er", wFUN = list(wFUN, 0.5, 1.0))
    trueDAG <- topsort(trueDAG)
    # true causal effect
    trueBeta <- causalEffect_modified(trueDAG, y = j, x = i) 
    
    # generate data according to true DAG
    data <- rmvDAG_modified(n, trueDAG, errDist = 'normal')
    suffStat <- list(C = cor(data), n = n)
    
    # SUPPOSE TRUE DAG IS KNOWN
    pa_x <- which(as(trueDAG, "matrix")[,i] != 0) 
    dat <- as.data.frame(data[, c(j, i, pa_x)]) 
    colnames(dat)[1:2] <- c("y", "x")
    beta <- coef(lm(y ~., data = dat))["x"] 
    var <- diag(vcov(lm(y ~., data = dat)))["x"]
    tru.CI <- beta + c(-1, 1)* qnorm(0.975) * sqrt(var)
    tru.CI_len <- tru.CI[2] - tru.CI[1] # CI length
    if (trueBeta > tru.CI[1] & trueBeta < tru.CI[2]) {
      tru.cover <- 1
    } else
      tru.cover <- 0
    
    # NAIVE APPROACH 
    naive.est <- tpc(suffStat, indepTest = gaussCItest, alpha = 0.01, p = d, tiers = tier)
    naive.amat <- as(naive.est, "amat")
    
    # check if we have a valid graph
    naive.valid <- ifelse(isValidGraph(naive.amat, type = "pdag"), TRUE, FALSE)
    
    # construct CI if graph is valid  
    if (naive.valid == TRUE) {
      naive.res <- simple_ida(data, naive.amat, x = i, y = j)$res
      naive.CI_l <- naive.res[,"beta"] - qnorm(0.975) * sqrt(naive.res[,"var"])
      naive.CI_u <- naive.res[,"beta"] + qnorm(0.975) * sqrt(naive.res[,"var"])
      naive.CI <- c(min(naive.CI_l), max(naive.CI_u ))
      naive.CI_len <- naive.CI[2] - naive.CI[1] # CI length
      if (trueBeta > naive.CI[1] & trueBeta < naive.CI[2]) {
        naive.cover <- 1
      } else
        naive.cover <- 0
    } else if (naive.valid == FALSE) {
      naive.CI <- NA 
      naive.CI_len <- NA
      naive.cover <- NA      
    }
    
    # RESAMPLING APPROACH
    tpc.est <- vector("list", M) 
    amat <- vector("list", M) 
    valid_m <- keep_m <- c()
    pa <- c()
    for (m in 1:M) {
      # tPC algorithm with the new independence test
      tpc.est[[m]] <- tpc(suffStat, indepTest = resamplingTestGauss, alpha = thres, p = d, tiers = tier) 
      amat[[m]] <- as(tpc.est[[m]], "amat")
      
      # check if we have a valid graph
      if (isValidGraph(amat[[m]], type = "pdag")) {
        valid_m <- c(valid_m, m)
      }
      
      # keep graph if it is valid  
      if (m %in% valid_m) {
        keep_m <- c(keep_m, m)
      }
      
      # record parents of i
      pa <- c(pa, which(amat[[m]][i,] == 1))
    }
    
    # construct CI if keep_m is not empty 
    if (!is.null(keep_m)) {
      res <- NULL
      # for each estimated graph in keep_m, estimate the effect of i on j 
      for (m in keep_m) {
        res <- rbind(res, simple_ida(data, amat[[m]], x = i, y = j)$res)
      }
      # construct CI
      lwr <- upr <- rep(NA, nrow(res))
      res <- cbind(res, lwr, upr)
      res[,"lwr"] <- res[,"beta"] - z*sqrt(res[,"var"])
      res[,"upr"] <- res[,"beta"] + z*sqrt(res[,"var"])
      CI <- c(min(res[,"lwr"]), max(res[,"upr"])) # take the union of the CIs
      CI_len <- CI[2] - CI[1] 
      if (trueBeta > CI[1] & trueBeta < CI[2]) {
        cover <- 1
      } else
        cover <- 0
    } else {
      CI <- NA
      CI_len <- NA
      cover <- NA
    }
    
    return(list(trueBeta = trueBeta,
                tru.CI = tru.CI,
                tru.CI_len = tru.CI_len,
                tru.cover = tru.cover,
                naive.CI = naive.CI,
                naive.CI_len = naive.CI_len,
                naive.cover = naive.cover,
                naive.valid = naive.valid,
                CI = CI, 
                CI_len = CI_len,
                cover = cover,
                n_valid_m = length(valid_m),
                n_keep_m = length(keep_m),
                pa = unique(pa)))
  }
})



# coverage
tru.cover <- sapply(sim, f <- function(l) {getElement(l, "tru.cover")})
naive.cover <- sapply(sim, f <- function(l) {getElement(l, "naive.cover")})
cover <- sapply(sim, f <- function(l) {getElement(l, "cover")})
mean(tru.cover, na.rm = TRUE) 
mean(naive.cover, na.rm = TRUE)  
mean(cover, na.rm = TRUE) 

# CI length
tru.CI_len <- sapply(sim, f <- function(l) {getElement(l, "tru.CI_len")})
naive.CI_len <- sapply(sim, f <- function(l) {getElement(l, "naive.CI_len")})
CI_len <- sapply(sim, f <- function(l) {getElement(l, "CI_len")})
mean(tru.CI_len, na.rm = TRUE)
mean(naive.CI_len, na.rm = TRUE)  
mean(CI_len, na.rm = TRUE) 

# % kept graphs
naive.valid <- sapply(sim[1:nsim], f <- function(l) {getElement(l, "naive.valid")})
mean(naive.valid)
valid <- sapply(sim[1:nsim], f <- function(l) {getElement(l, "n_valid_m")})
keep <- sapply(sim[1:nsim], f <- function(l) {getElement(l, "n_keep_m")})
mean(valid/M)
mean(keep/M)



