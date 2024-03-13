library(pcalg)
library(tpc)
source("fxn.R")

data <- read.delim("data/data.txt", header = TRUE, sep = ",")
data <- data[,1:11] # exclude intervention variables
data <- data[,c("plc", "pkc", "pka", "pip2", "pip3", "raf", "mek", "erk", "akt", "p38", "jnk")]
tier <- c(rep(1,5), rep(2,6)) # tier 1: plc, pkc, pka, pip2, pip3
lab <- colnames(data)
d <- ncol(data) # 11 variables
n <- nrow(data) # 7466 obs
suffStat <- list(C = cor(data), n = n)


# PARAMETER SETTINGS 
nb_max <- 7 # maximum number of neighbors per node 
nu <- 0.025
M <- 100 # number of resamples
c = 0.1 # c star in the shrinkage parameter tau 
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
  # keep graph if (1) it's valid among tier 2 nodes and (2) raf & mek are adjacent
  if ((m %in% valid_t2) & (amat_t2[[m]]["mek","raf"] %in% c(1,2) | amat_t2[[m]]["raf","mek"] == 1)) {
    keep <- c(keep, m)
  }
}
# c*=0.01: keep 8/100, valid_all 5/100, valid_t2 8/100
# c*=0.1: keep 4/100, valid_all 2/100, valid_t2 4/100
# c*=0.2: keep 7/100, valid_all 4/100, valid_t2 7/100

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
    # skip if akt is a parent of erk 
    if (4 %in% pa2) next
    # do lm(akt ~ erk + pa1 + pa2)
    dat <- as.data.frame(data[, c("akt", "erk", pa1, rownames(mat)[pa2])]) 
    beta[k] <- coef(lm(akt ~., data = dat))["erk"] 
    var[k] <- diag(vcov(lm(akt ~., data = dat)))["erk"]
  }
  res <- rbind(res, cbind(beta, var))
}

# construct CI
lwr <- upr <- rep(NA, nrow(res))
res <- cbind(res, lwr, upr)
res[,"lwr"] <- res[,"beta"] - z*sqrt(res[,"var"])
res[,"upr"] <- res[,"beta"] + z*sqrt(res[,"var"])
CI <- c(min(res[,"lwr"], na.rm = T), max(res[,"upr"], na.rm = T)) # take the union of the CIs
# c*=0.01: (0.83, 0.96)
# c*=0.1: (0.79, 0.93)
# c*=0.2: (0.79, 0.86)


# ----------------------------------------
# STEP 2: AGGREGATION (adjust all t1 vars)
res <- NULL
for (m in keep) {
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
    # skip if akt is a parent of erk 
    if (4 %in% pa2) next
    # do lm(ark ~ erk + all t1 vars + pa2)
    dat <- as.data.frame(data[, c("akt", "erk", lab[tier == 1], rownames(mat)[pa2])]) 
    beta[k] <- coef(lm(akt ~., data = dat))["erk"] 
    var[k] <- diag(vcov(lm(akt ~., data = dat)))["erk"]
  }
  res <- rbind(res, cbind(beta, var))
}

# construct CI
lwr <- upr <- rep(NA, nrow(res))
res <- cbind(res, lwr, upr)
res[,"lwr"] <- res[,"beta"] - z*sqrt(res[,"var"])
res[,"upr"] <- res[,"beta"] + z*sqrt(res[,"var"])
CI <- c(min(res[,"lwr"], na.rm = T), max(res[,"upr"], na.rm = T)) # take the union of the CIs
# c*=0.01: (0.83, 0.93)
# c*=0.1: (0.84, 0.93)
# c*=0.2: (0.84, 0.88)