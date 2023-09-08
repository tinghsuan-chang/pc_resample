# a new independence test 
resamplingTestGauss <- function(x, y, S, suffStat) {
  # - x, y, S: integer position of X, Y and set of variables S, respectively
  # - suffStat$C: correlation matrix
  # - suffStat$n: sample size
  # pcalg::zStat returns Fisher's z-transform of pcorr (x,y|S)
  # z approximately follows N(z(true_pcorr), 1)
  z <- zStat(x, y, S, C = suffStat$C, n = suffStat$n)
  
  # resample z-transform pcorr
  resample_z <- rnorm(1, z, 1)
  
  # returns -|z(pcorr)|  
  return(-abs(resample_z))
} 

# function to estimate possible causal effect(s) of x on y in a equiv. class
simple_ida <- function (data, amat, x, y) {
  p <- ncol(data) # number of nodes
  allDags <- pdag2allDags(amat) # all DAGs in the equiv. class represented by amat
  if (is.null(allDags$dags))
    return(NULL)
  else 
    nDags <- nrow(allDags$dags) # number of DAGs in the equiv. class
  
  beta <- NULL
  var <- NULL
  # for each DAG in the equiv. class...
  for (k in 1:nDags) {
    # get its adjacency matrix
    mat <- matrix(allDags$dags[k,], p, p, byrow = TRUE) 
    # find parent(s) of x
    pa_x <- which(mat[x,] == 1) 
    # do lm(y ~ x + pa(x, G))
    dat <- as.data.frame(data[, c(y, x, pa_x)]) 
    colnames(dat)[1:2] <- c("y", "x")
    beta[k] <- coef(lm(y ~., data = dat))["x"] 
    var[k] <- diag(vcov(lm(y ~., data = dat)))["x"] 
  }
  res <- cbind(beta, var)
  
  return(list(res = res, 
              min = res[which.min(abs(res[,"beta"])),], 
              max = res[which.max(abs(res[,"beta"])),])
         )
}