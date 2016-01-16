
# Code for Distance functions

#' Binary Reachibility Matrix
#' 
#' The binary reachability matrix 'R' describes reachibility
#' between all pairs of nodes. An entry (u,v)=1 means that
#' there exists a path from node u to node v, and (u,v)=0
#' means there does not.
#' 
#' The distance matrix 'D' contains lengths of shortest paths
#' between all pairs of nodes. An entry (u,v) represents
#' the length of shortest path from node u to node v. The
#' average shortest path length is the characteristic
#' path length of the network.
#' 
#' @param CIJ : a matrix - binary (un)directed connection matrix
#' 
#' @return RD : a list - RD has two elements: 'R' is the binary
#' reachability matrix, and 'D' is the distance matrix.
#' 
breadthdist <- function(CIJ){
  n <- nrow(CIJ)
  D <- Matrix::Matrix(0,nrow=n,ncol=n)
  for (i in 1:n){
    D[i,] <- breadth(CIJ,i)
  }
  D[D==0] <- Inf
  R <- D[D!=Inf]
  return(list(R=R,D=D))
}

#' Breadth-First Search
#' 
#' Implementation of Breadth-First Search in R (yikes!)
#' 
#' @param CIJ : a matrix - binary (un)directed connection matrix
#' @param src : an integer - the source vertex index
#' 
#' @return DB : a list - DB has two elements: 'distance' is a vector of
#' distances between the source (src) and the ith vertex (0 for src),
#' and 'branch' is a vector of vertices that precede 'i' in the BFS (-1 for src)
#' 
#' #' @useDynLib bctR
#' @importFrom Rcpp sourceCpp
#' 
breadth <- function(CIJ, src){
  return()
}


