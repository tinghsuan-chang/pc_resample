library(igraph)
library(mvtnorm)
library(pcalg)

# A new independence test 
resamplingTestGauss <- function(x, y, S, suffStat) {
  # x, y, S: integer position of X, Y and set of variables S, respectively
  # suffStat$C: correlation matrix
  # suffStat$n: sample size
  # pcalg::zStat returns Fisher's z-transform of pcorr (x,y|S)
  # z approximately follows N(z(true_pcorr), 1)
  z <- zStat(x, y, S, C = suffStat$C, n = suffStat$n)
  
  # resample z-transform pcorr
  resample_z <- rnorm(1, z, 1)
  
  # returns -|z(pcorr)|  
  return(-abs(resample_z))
} 

# Function to estimate possible causal effect(s) of x on y in a equiv. class
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

#' Topologically sorts a DAG generated from pcalg::randDAG
#'
#' @param g a pcalg DAG object
#' @returns a topologically sorted DAG
topsort <- function(g) {
  amat <- wgtMatrix(g)
  toporder <- topo_sort(igraph.from.graphNEL(g), mode = 'in')
  amat_ord <- amat[toporder, toporder]
  rownames(amat_ord) <- as.character(1:length(g@nodes)) ## added
  colnames(amat_ord) <- as.character(1:length(g@nodes)) ## added
  igraph.to.graphNEL(
    graph_from_adjacency_matrix(amat_ord, mode = 'directed', weighted = TRUE)
  )
}

#' Samples from a DAG
#' Originally from https://github.com/fdabl/Centrality-Causality/blob/master/R/helpers.R
#' Added by DMalinsky: re-scaling the edge weights by column, to prevent large weights for nodes w/ many parents
rmvDAG_modified <- function (n, dag, errDist = c("normal", "cauchy", "t4", "mix", 
                                                 "mixt3", "mixN100"), mix = 0.1, errMat = NULL, back.compatible = FALSE, 
                             use.node.names = !back.compatible) 
{
  stopifnot(is(dag, "graph"), (p <- length(dag@nodes)) >= 
              2)
  weightMatrix <- if (back.compatible) 
    wgtMatrix.0(dag)
  else wgtMatrix(dag)
  nonZeros <- which(weightMatrix != 0, arr.ind = TRUE)
  if (nrow(nonZeros) > 0) {
    if (any(nonZeros[, 1] - nonZeros[, 2] < 0) || any(diag(weightMatrix) != 
                                                      0)) 
      stop("Input DAG must be topologically ordered!")
  }
  
  ###### MODIFY WEIGHT MATRIX HERE
  ## RESCALE AS IN MOOIJ ET AL
  scales<-matrix(0,p,1)
  for( i in 1:p ) {
    scales[i] <- (sum(weightMatrix[i,]^2) + 1)
    weightMatrix[i,] <- weightMatrix[i,] / scales[i]^0.5
  }
  ###### MODIFY WEIGHT MATRIX HERE
  
  errDist <- match.arg(errDist)
  if (grepl("^mix", errDist)) 
    eMat <- function(outs) {
      X <- c(rnorm(n * p - length(outs)), outs)
      matrix(sample(X), nrow = n)
    }
  if (is.null(errMat)) {
    errMat <- switch(errDist,
                     normal = matrix(rnorm(n * p), nrow = n),
                     cauchy = matrix(rcauchy(n * p), nrow = n), 
                     t4 = matrix(rt(n * p, df = 4), nrow = n),
                     mix = eMat(rcauchy(round(mix * n * p))),
                     mixt3 = eMat(rt(round(mix * n * p), df = 3)),
                     mixN100 = eMat(rnorm(round(mix * n * p), sd = 10)))
  }
  else {
    stopifnot(!is.null(dim.eM <- dim(errMat)), dim.eM == c(n, p), is.numeric(errMat))
  }
  if (use.node.names) 
    colnames(errMat) <- dag@nodes
  if (sum(abs(weightMatrix)) > 0) { # bug fixed! (see https://github.com/cran/pcalg/pull/2)
    X <- errMat
    for (j in 2:p) {
      ij <- 1:(j - 1)
      X[, j] <- X[, j] + X[, ij, drop = FALSE] %*% weightMatrix[j, ij]
    }
    X
  }
  else errMat
}

wFUN <- function(m,lB,uB) { 
  res <- rep(0,m)
  for(i in 1:m){
    if(rbinom(1,1,.5)==1){
      res[i] <- runif(1,lB,uB) } 
    else res[i] <- -runif(1,lB,uB) 
  }
  res
}

causalEffect_modified <- function(g, y, x){
  wmat <- wgtMatrix(g)
  p <- ncol(wmat)
  
  ## MODIFY WEIGHT MATRIX TO ACCOUNT FOR RESCALING
  scales<-matrix(0,p,1)
  for( i in 1:p ) {
    scales[i] <- (sum(wmat[i,]^2) + 1)
    wmat[i,] <- wmat[i,] / scales[i]^0.5
  }
  ## 
  
  vec <- matrix(0, p, 1)
  vec[x] <- 1
  if (y - x > 1) {
    for (i in (x + 1):y) vec[i] <- wmat[i, ] %*% vec
    vec[y]
  }
  else {
    wmat[y, x]
  }
}

## to simulate data run
# myDAG <- randDAG(10, 7, "er",wFUN=list(wFUN,0.5,1.0))
# myDAG <- topsort(myDAG)
# d.mat <- rmvDAG_modified(n, myDAG, errDist = 'normal')