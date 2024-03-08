library(pcalg)
library(tpc)
source("fxn.R")

data <- read.delim("data/data.txt", header = TRUE, sep = ",")
data <- data[,1:11] # exclude intervention variables
lab <- colnames(data)
d <- ncol(data) # 11 variables
n <- nrow(data) # 7466 obs
suffStat <- list(C = cor(data), n = n)
tier <- c(2,2,1,1,1,2,2,1,1,2,2) # tier 1: plc, pkc, pka, pip2, pip3

# PARAMETER SETTINGS 
nb_max <- 7 # maximum number of neighbors per node 
nu <- 0.025
M <- 100 # number of resamples
c = 0.01 # c star in the shrinkage parameter tau 
L = (nb_max+1) * d*(d-1)/2 # max number of independencies to be evaluated
tau = c*(log(n)/M)^(1/L) # shrinkage parameter
thres = tau * qnorm(nu/(2*L)) # threshold to compare with z(pcorr) for the new independence test (negative number, retain null (remove edge) if -|z(pcorr)| > threshold)
z = -qnorm((0.05-nu)/2) # z score for constructing CI

# STEP 1: PC W/ RESAMPLING & SCREENING 
tpc.est <- vector("list", M) 
amat_all <- amat_t2 <- vector("list", M) 
valid_all <- valid_t2 <- NULL
keep <- NULL
set.seed(123)
for (m in 1:M) {
  tpc.est[[m]] <- tpc(suffStat, indepTest = resamplingTestGauss, alpha = thres, labels = lab, tiers = tier) 
  amat_all[[m]] <- as(tpc.est[[m]], "amat")
  amat_t2[[m]] <- amat_all[[m]][tier == 2, tier == 2]
  # check if entire graph is valid 
  if (isValidGraph(amat_all[[m]], type = "pdag")) {
    valid_all <- c(valid_all, m)
  }
  # check if graph is valid among tier 2 nodes
  if (isValidGraph(amat_t2[[m]], type = "pdag")) {
    valid_t2 <- c(valid_t2, m)
  }
  # keep graph if it's valid among tier 2 nodes and has raf -> mek edge
  if ((amat_t2[[m]]["mek","raf"] == 1) & (m %in% valid_t2)) {
    keep <- c(keep, m)
  }
}
# c*=0.006: keep 6/100, valid_all 3/100, valid_t2 9/100
# c*=0.01: keep 4/100, valid_all 4/100, valid_t2 5/100
# c*=0.02: keep 6/100, valid_all 4/100, valid_t2 8/100

# STEP 2: AGGREGATION
res <- NULL
for (m in keep) {
  # tier 1 parent(s) of erk 
  pa1 <- labels(which(amat_all[[m]]["erk",] == 1 & tier == 1))
  # all DAGs in the equiv. class represented by amat_t2
  allDags <- pdag2allDags(amat_t2[[m]])
  beta <- NULL
  var <- NULL
  # for each DAG in the equiv. class (represented by amat_t2)...
  for (k in 1:nrow(allDags$dags)) {
    # get its adjacency matrix
    mat <- matrix(allDags$dags[k,], 6, 6, byrow = TRUE) 
    rownames(mat) <- c("raf", "mek", "erk", "akt", "p38", "jnk")
    # find tier 2 parent(s) of erk
    pa2 <- which(mat["erk",] == 1) 
    # do lm(mek ~ erk + pa1 + pa2)
    dat <- as.data.frame(data[, c("mek", "erk", pa1, rownames(mat)[pa2])]) 
    beta[k] <- coef(lm(mek ~., data = dat))["erk"] 
    var[k] <- diag(vcov(lm(mek ~., data = dat)))["erk"]
  }
  res <- rbind(res, cbind(beta, var))
}

# construct CI
lwr <- upr <- rep(NA, nrow(res))
res <- cbind(res, lwr, upr)
res[,"lwr"] <- res[,"beta"] - z*sqrt(res[,"var"])
res[,"upr"] <- res[,"beta"] + z*sqrt(res[,"var"])
CI <- c(min(res[,"lwr"]), max(res[,"upr"])) # take the union of the CIs
# c*=0.006: (-0.68, 7.46e-16) w/ warning msgs "essentially perfect fit: summary may be unreliable"
# c*=0.01: (-0.26, -0.11)
# c*=0.02: (-1.38, 2.54e-15) w/ warning msgs "essentially perfect fit: summary may be unreliable"
