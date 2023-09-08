# load packages and functions --------------------------------------------------------
library(foreach)
library(doParallel)
library(tpc)
source("for-ting-hsuan.R")
source("fxn.R")

# true DAG ---------------------------------------------------------------------------
set.seed(123)
trueDAG <- randDAG(10, 7, "er", wFUN = list(wFUN, 0.5, 1.0))
trueDAG <- topsort(trueDAG)
# we are interested in 9 -> 10
i <- 9; j <- 10 
# estimated effect using true DAG
trueBeta <- causalEffect_modified(trueDAG, y = j, x = i) # -0.322

# simulation -------------------------------------------------------------------------
registerDoParallel(detectCores()) 
nsim <- 500
n <- 500 # sample size
M <- 100 # number of estimated graphs
# specify threshold to compare with z(pcorr) for the new independence test
# NOTE: should be a negative number, retain null (remove edge) if -|z(pcorr)| > threshold
thres <- -1.96
z <- -thres

# NOTE: for now tPC tiers = 1:10; need to modify code a bit for weakened order info
system.time({
  sim <- foreach(ii = 1:nsim) %dopar% {
    set.seed(ii)
    # generate data according to true DAG
    data <- rmvDAG_modified(n, trueDAG, errDist = 'normal')
    suffStat <- list(C = cor(data), n = n)
    
    # NAIVE APPROACH 
    naive.est <- tpc(suffStat, indepTest = gaussCItest, alpha = 0.05, p = 10, tiers = c(1:10)) 
    naive.amat <- as(naive.est, "amat")
    
    # check if we have a valid graph
    naive.valid <- ifelse(isValidGraph(naive.amat, type = "pdag"), TRUE, FALSE)
    
    # construct CI if graph is valid  
    if (naive.valid == TRUE) {
      naive.res <- simple_ida(data, naive.amat, x = i, y = j)$res
      naive.CI <- naive.res[,"beta"] + c(-1, 1)* z * sqrt(naive.res[,"var"])
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
    for (m in 1:M) {
      # tPC algorithm with the new independence test
      tpc.est[[m]] <- tpc(suffStat, indepTest = resamplingTestGauss, alpha = thres, p = 10, tiers = c(1:10)) 
      amat[[m]] <- as(tpc.est[[m]], "amat")
      
      # check if we have a valid graph
      if (isValidGraph(amat[[m]], type = "pdag")) {
        valid_m <- c(valid_m, m)
      }
      
      # keep graph if it is valid and has directed edge i -> j 
      if (amat[[m]][i,j] == 0 & amat[[m]][j,i] == 1 & m %in% valid_m) {
        keep_m <- c(keep_m, m)
      }
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
    
    return(list(naive.CI = naive.CI,
                naive.CI_len = naive.CI_len,
                naive.cover = naive.cover,
                naive.valid = naive.valid,
                CI = CI, 
                CI_len = CI_len,
                cover = cover,
                n_valid_m = length(valid_m),
                n_keep_m = length(keep_m)))
  }
})


#########################

#load(file = "sim_tpc.RData")

# number of valid/kept graphs
library(tidyverse)
library(reshape2)
library(ggplot2)
valid <- sapply(sim[1:500], f <- function(l) {getElement(l, "n_valid_m")})
keep <- sapply(sim[1:500], f <- function(l) {getElement(l, "n_keep_m")})
df <- tibble(ii = 1:length(valid), valid = valid, keep = keep)
ggplot(melt(df, id = "ii"), aes(x = ii, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "identity") 

# coverage
naive.cover <- cover <- c()
for (i in 1:nsim) {
  if (is.na(sim[[i]]$naive.cover))
    naive.cover <- c(naive.cover, NA)
  else
    naive.cover <- c(naive.cover, getElement(sim[[i]], "naive.cover"))
}
for (i in 1:nsim) {
  if (is.na(sim[[i]]$cover))
    cover <- c(cover,  NA)
  else
    cover <- c(cover,  getElement(sim[[i]], "cover"))
}
mean(naive.cover, na.rm = TRUE)
mean(cover, na.rm = TRUE) 
sum(!is.na(naive.cover))
sum(!is.na(cover)) 
which(is.na(cover))
# naive: 65.4% (500)
# resampling: 83.6% (500)

# CI length
naive.CI_len <- sapply(sim, f <- function(l) {getElement(l, "naive.CI_len")})
CI_len <- sapply(sim, f <- function(l) {getElement(l, "CI_len")})
mean(naive.CI_len, na.rm = TRUE) #naive: 0.188
mean(CI_len, na.rm = TRUE) #resampling: 0.265

# plot CI
CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[1]})
CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "CI")[2]})
naive.CI_l <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[1]})
naive.CI_u <- sapply(sim[1:250], f <- function(l) {getElement(l, "naive.CI")[2]})
df <- tibble(ii = 1:length(CI_l), CI_l = CI_l, CI_u = CI_u, naive.CI_l = naive.CI_l, naive.CI_u = naive.CI_u)
df %>%
  ggplot() +
  geom_errorbar(aes(x = ii, ymin = CI_l, ymax = CI_u), colour = "orange") +
  geom_errorbar(aes(x = ii, ymin = naive.CI_l, ymax = naive.CI_u), colour = "blue", alpha = 0.4) +
  geom_hline(yintercept = trueBeta, linetype = "dashed", color = "red")
