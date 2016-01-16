
# COMMUNITY - CLUSTERING COEFFICIENT

#' Binary Directed Clustering Coefficient
#' 
#' The clustering coefficient is the fraction of triangle around
#' a node (equiv. the fraction of a node's neighbors that are 
#' neighbors of each other)
#' 
#' Methodological note: In directed graphs, 3 nodes generate up to 8
#' triangles (2*2*2 edges). The number of existing triangles is the main
#' diagonal of S^3/2. The number of all (in or out) neighbour pairs is
#' K(K-1)/2. Each neighbour pair may generate two triangles. "False pairs"
#' are i<->j edge pairs (these do not generate triangles). The number of
#' false pairs is the main diagonal of A^2.
#' Thus the maximum possible number of triangles =
#'   = (2 edges)*([ALL PAIRS] - [FALSE PAIRS])
#' = 2 * (K(K-1)/2 - diag(A^2))
#' = K(K-1) - 2(diag(A^2))
#' 
#' @param A : a matrix - binary directed connection matrix
#' 
#' @return C : a vector - clustering coefficient vector
#' 
#' NOT TESTED
UNC.clustering.coef.bd <- function(A){
  S <- A + t(A) # symmetrized input graph
  K <- colSums(W>0)
  cyc3 <- diag(S %*% (S %*% S)) / 2
  K[which(cyc3 == 0)] <- Inf
  CYC3 <- K * (K-1) - 2 * diag(A %*% A)
  C <- cyc3 / CYC3
  return(C)
}
clustering.coef.bd <- compiler::cmpfun(UNC.clustering.coef.bd)



#' Binary Undirected Clustering Coefficient
#' 
#' The clustering coefficient is the fraction of triangle around
#' a node (equiv. the fraction of a node's neighbors that are 
#' neighbors of each other)
#' 
#' @param A : a matrix - binary undirected connection matrix
#' 
#' @return C : a vector - clustering coefficient vector
#' 
#' NOT TESTED
UNC.clustering.coef.bu <- function(G){
  n <- nrow(G)
  C <- rep(0,n)
  
  for (u in 1:n){
    V <- which(G[u,]>0)
    k <- length(V)
    if (k >= 2){
      S <- G[V,V]
      C[u] <- sum(S) / (k * k - k)
    }
  }
}
clustering.coef.bu <- compiler::cmpfun(UNC.clustering.coef.bu)



#' Weighted Directed Clustering Coefficient
#' 
#' The weighted clustering coefficient is the average "intensity" of
#' triangles around a node.
#' 
#' Methodological note (also see clustering_coef_bd)
#' The weighted modification is as follows:
#'   - The numerator: adjacency matrix is replaced with weights matrix ^ 1/3
#' - The denominator: no changes from the binary version
#' 
#' The above reduces to symmetric and/or binary versions of the clustering
#' coefficient for respective graphs.
#' 
#' @param W : a matrix - weighted directed connection matrix
#' 
#' @return C : a vector - clustering coefficient vector
#' 
#' NOT TESTED
UNC.clustering.coef.wd <- function(W){
  A <- binarize(W)
  S <- W**(1/3) + t(W)**(1/3)
  K <- colSums(W+t(W))
  cyc3 <- diag(ws %*% (ws %*% ws))
  K[which(cyc3==0)] <- Inf
  CYC3 <- K * (K - 1) - 2 * diag(A %*% A)
  C <- cyc3 / CYC3
  return(C)
}
clustering.coef.wd <- compiler::cmpfun(UNC.clustering.coef.wu)



#' Weighted Undirected Clustering Coefficient
#' 
#' The weighted Und. clustering coefficient is the
#' average "intensity" of triangles around a node
#' 
#' @param W : a matrix - weighted undirected connection matrix
#' 
#' @return C : a vector - clustering coefficient vector
#' 
#' NOT TESTED
UNC.clustering.coef.wu <- function(W){
  K <- colSums(W)
  ws <- W**(1/3)
  cyc3 <- diag(ws %*% (ws %*% ws))
  K[which(cyc3 == 0)] <- Inf
  C <- cyc3 / (K * (K-1))
  return(C)
}
clustering.coef.wu <- compiler::cmpfun(UNC.clustering.coef.wu)

