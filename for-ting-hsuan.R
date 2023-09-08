library('igraph')
library('mvtnorm')
library('pcalg')

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