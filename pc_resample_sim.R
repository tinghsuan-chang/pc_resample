# load packages -------------------------------------------------------------------------
required.packages <- c("pcalg", "parallel", "doSNOW")
packages.to.install <- setdiff(required.packages, installed.packages())
if(length(packages.to.install) > 0) 
  install.packages(packages.to.install, dependencies = TRUE, INSTALL_opts = '--no-lock')
library(pcalg)
library(parallel)
library(doSNOW)

# create and register cluster ------------------------------------------------------------
cl <- parallel::makeCluster(20) 
doSNOW::registerDoSNOW(cl)

# true DAG -------------------------------------------------------------------------------
set.seed(112233)
p <- 10 # number of nodes
trueDAG <- pcalg::randomDAG(n = p, prob = 0.4)
# we are interested in 4 -> 8
i <- 4; j <- 8 
# estimated effect using true DAG
trueBeta <- causalEffect(trueDAG, y = j, x = i) # 1.336992

# start simulation ----------------------------------------------------------------------
nsim <- 500
pc_resample_sim <- foreach(ii = 1:nsim, .packages = c("pcalg", "parallel", "doSNOW")) %dopar% {
  
  set.seed(ii)
  
  # call the functions I created
  source("pc_resample_fxn.R")
  
  # use the new independence test
  indepTest <- resamplingTestGauss
  
  # specify threshold to compare with z(pcorr)
  # should be a negative number, retain null (remove edge) if -|z(pcorr)| > threshold
  alpha <- -1.96 
  
  n <- 5000 # sample size
  M <-  500 # number of estimated graphs
  # generate data according to true DAG
  data <- rmvDAG(n, trueDAG, errDist = "normal")
  suffStat <- list(C = cor(data), n = n) 
  
  pc.est <- vector("list", M) 
  amat <- vector("list", M) # adjacency matrices
  keep_m <- c() # keep estimated CPDAGs with directed edge i -> j
  
  for (m in 1:M) {
    pc.est[[m]] <- pc(suffStat, indepTest, alpha, p = p)
    amat[[m]] <- as(pc.est[[m]], "amat")
    
    # keep CPDAG if it has directed edge i -> j 
    if (amat[[m]][i,j] == 0 & amat[[m]][j,i] == 1) {
      keep_m <- c(keep_m, m)
    }
  }
  
  # go to next iteration if keep_m is empty
  if (is.null(keep_m)) {
    next
  }
  
  res <- NULL
  # for each estimated CPDAG in keep_m, estimate the lower bound of the effect of i on j 
  for (m in keep_m) {
    res <- rbind(res, simple_ida(data, amat[[m]], x = i, y = j)$min)
  }
  
  # construct CI
  z <- 1.96
  lwr <- upr <- rep(NA, nrow(res))
  res <- cbind(res, lwr, upr)
  res[,"lwr"] <- res[,"beta"] - z*sqrt(res[,"var"])
  res[,"upr"] <- res[,"beta"] + z*sqrt(res[,"var"])
  CI <- c(min(res[,"lwr"]), max(res[,"upr"])) # take the union of the CIs
  
  if (trueBeta > CI[1] & trueBeta < CI[2]) {
    cover <- 1
  } else
    cover <- 0
  
  return(list(CI = CI, 
              cover = cover,
              n_keep_m = length(keep_m)))
}

  
save(pc_resample_sim, file = "pc_resample_sim.RData")




