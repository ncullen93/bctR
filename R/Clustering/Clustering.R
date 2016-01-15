# Clustering Code
# Uses "Matrix" package for fast matrix operations - similar to using numpy/scipy.

#' Weighted Undirected Clustering Coefficient
#' 
#' The weighted Und. clustering coefficient is the
#' average "intensity" of triangles around a node
#' 
#' @param W : a matrix - weighted undirected connection matrix
#' 
#' @return C : a vector - clustering coefficient vector
#' 
clustering.coef.wu <- function(W){
  W <- Matrix::Matrix(W) # convert to fast matrix
  K <- apply(W,2,function(row) sum(row>0))
  ws <- W**(1/3)
  cyc3 <- diag(ws %*% (ws %*% ws)) # quite slow
  K[which(cyc3 == 0)] <- Inf
  C <- cyc3 / (K * (K-1))
  return(C)
}


#' Consensus of a Graph
#' 
#' This algorithm seeks a consensus partition of the agreement
#' matrix D. The algorithm used here is almost identical to the 
#' one introduced in Lancichinetti & Fortunato (2012): The agreement
#' matrix D is thresholded at a level TAU to remove any weak elements.
#' The resulting matrix is then partitioned REPS number of times using
#' the Louvain algorithm (in principle, any clustering algorithm that
#' can handle weighted matrices is a sutiable alternative to the Louvain
#' algorithm and can be subsituted in its place). This clustering
#' produces a set of partitions from which a new agreement is built. If
#' the partitions have not converged to a single representative partition,
#' the above process repeats itself, starting with the newly built
#' agreement matrix.
#' 
#' Note: In this implementation, the elements of the agreemenet matrix
#' must be converted into probabilities.
#' 
#' Note: This implementation is slightly different from the original
#' algorithm proposed by Lancichinetti & Fortunato. In its original
#' version, if the thresholding produces singleton communities, those
#' nodes are reconnected to the network. Here, we leave any singleton
#' communities disconnected.
#' 
#' @param D : a matrix - agreement matrix with entries between [0,1]
#' denoting the probability of finding node i in the same cluster as node j.
#' @param tau : a float - threshold which controls the resolution of the
#' reclustering
#' @param reps : an integer - number of times the clustering algorithm is
#' reapplied. Default value is 1000
#' 
#' @return ciu : a vector - the consensus partition
#' 
consensus.und <- function(D,
                          tau,
                          reps=1000){
  
}



#' Connected Components of an Undirected Graph
#' 
#' Returns the components of an undirected graph specified by the 
#' binary and undirected adjacency matrix 'A'. Components and their
#' constituent nodes are assigned the same index and stored in the
#' vector 'comp'. The vector 'comp.sizes' contains the number of
#' nodes belonging to each component. Furthermore, 'members' and
#' 'size' are combined into one list ' comp
#' 
#' Disconnected nodes will appear as components with a component
#' size of 1.
#' 
#' The identity of each component (i.e. its numerical value in the 
#' result) is not guaranteed to be identical to the value return in
#' other versions of BCT, although the component topology is.
#' 
#' In the Python-BCT, networkx is used to do the computation efficiently.
#' In the Matlab-BCT, the Dulmage-Mendelsohn decomposition is computed.
#' 
#' Note: Python version takes 1.8s w/out networkx and 54ms w/ networkx w/ sample.data
#' 
#' @param A : a matrix - binary undirected adjacency matrix
#' 
#' @return comp : a list - each list element is a graph component, and
#' each component has two elements: 'members' - the vector of nodes belonging
#' to that component, and 'size' - the number of nodes in that component
#' 
#' 
get.components <- function(A){
  A <- binarize(A)
  n <- nrow(A)
  diag(A) <- 0
  
  cptvec <- rep(0,n)
  R <- breadthdist(A)[['R']]
  # for
}




#' Weighted Undirected Transitivity
#' 
#' Transitivity is the ratio of 'triangles to triplets' in the network.
#' (A classical version of the clustering coefficient).
#' 
#' @param W : a matrix - weighted undirected connection matrix
#' 
#' @return t : an integer - transitivity scalar
#' 
transitivity.wu <- function(W){
  W <- Matrix::Matrix(W)
  K <- apply(W,2,function(row) sum(row>0))
  ws <- W**(1/3)
  cyc3 <- diag(ws %*% (ws %*% ws))
  t <- sum(cyc3) / sum(K * (K-1))
  return(t)
}








