
# CLUSTERING - TRANSITIVITY

#' Binary Directed Transitivity
#' 
#' Transitivity is the ratio of 'triangles to triplets' in the network.
#' (A classical version of the clustering coefficient).
#'
#' Methodological note: In directed graphs, 3 nodes generate up to 8
#' triangles (2*2*2 edges). The number of existing triangles is the main
#' 
#' diagonal of S^3/2. The number of all (in or out) neighbour pairs is
#' K(K-1)/2. Each neighbour pair may generate two triangles. "False pairs"
#' are i<->j edge pairs (these do not generate triangles). The number of
#' false pairs is the main diagonal of A^2. Thus the maximum possible
#' number of triangles = (2 edges)*([ALL PAIRS] - [FALSE PAIRS])
#' = 2 * (K(K-1)/2 - diag(A^2))
#' = K(K-1) - 2(diag(A^2))
#' 
#' @param A : a matrix - binary directed connection matrix
#' 
#' @return T : a float - transitivity scalar
#' 
#' NOT TESTED
UNC.transitivity.bd <- function(A){
  S <- A + t(A) # symmetrized input graph
  K <- colSums(S)
  cyc3 <- diag(ws %*% (ws %*% ws)) / 2
  K[which(cyc3 == 0)] <- Inf
  CYC3 <- K * (K - 1) - 2 * diag(A %*% A)
  C <- sum(cyc3) / sum(CYC3)
  return(C)
}
transitivity.bd <- compiler::cmpfun(UNC.transitivity.bd)



#' Binary Undirected Transitivity
#' 
#' Transitivity is the ratio of 'triangles to triplets' in the network.
#' (A classical version of the clustering coefficient).
#' 
#' @param A : a matrix - binary undirected connection matrix
#' 
#' @return T : a float - transitivity scalar
#' 
#' NOT TESTED
UNC.transitivity.bu <- function(A){
  tri3 <- trace(A %*% (A %*% A))
  tri2 <- sum( (A %*% A) - trace( A %*% A ) )
  C <- tri3 / tri2
  return(C)
}
transitivity.bu <- compiler::cmpfun(UNC.transitivity.bu)



#' Weighted Dircted Transitivity
#' 
#' #' Transitivity is the ratio of 'triangles to triplets' in the network.
#' (A classical version of the clustering coefficient).
#' 
#' Methodological note (also see note for clustering_coef_bd)
#' The weighted modification is as follows:
#'  - The numerator: adjacency matrix is replaced with weights matrix ^ 1/3
#' - The denominator: no changes from the binary version
#' 
#' The above reduces to symmetric and/or binary versions of the clustering
#' coefficient for respective graphs.
#' 
#' @param A : a matrix - weighted directed connection matrix
#' 
#' @return T : a float - transitivity scalar
#' 
#' NOT TESTED
UNC.transitivity.wd <- function(W){
  A <- binarize(W)
  S <- W**(1/3) + t(W)**(1/3)
  K <- colSums(A + t(A))
  cyc3 <- diag(S %*% (S %*% S)) / 2
  K[which(cyc3) == 0] <- Inf
  CYC3 <- K * (K - 1) - 2 * diag(A %*% A)
  C <- sum(cyc3) / sum(CYC3)
  return(C)
}
transitivty.wd <- compiler::cmpfun(UNC.transitivity.wd)



#' Weighted Undirected Transitivity
#' 
#' Transitivity is the ratio of 'triangles to triplets' in the network.
#' (A classical version of the clustering coefficient).
#' 
#' @param W : a matrix - weighted undirected connection matrix
#' 
#' @return t : an integer - transitivity scalar
#' 
#' NOT TESTED
UNC.transitivity.wu <- function(W){
  #W <- Matrix::Matrix(W)
  K <- colSums(W)
  ws <- W**(1/3)
  cyc3 <- diag(ws %*% (ws %*% ws))
  t <- sum(cyc3) / sum(K * (K-1))
  return(t)
}
transitivity.wu <- compiler::cmpfun(UNC.transitivity.wu)


