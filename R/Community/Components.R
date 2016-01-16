
# COMMUNITY - COMPONENTS

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
#' Here, we use the union-find algorithm
#' 
#' Note: Python version takes 1.8s w/out networkx and 54ms w/ networkx w/ sample.data
#' This R version clocks in at 20ms! 
#' 
#' @param A : a matrix - binary undirected adjacency matrix
#' 
#' @return comp : a list - each list element is a graph component, and
#' each component has two elements: 'members' - the vector of nodes belonging
#' to that component, and 'size' - the number of nodes in that component
#' 
#' NOT TESTED
get.components <- function(A){
  A <- binarize(A)
  n <- nrow(A)
  diag(A) <- 0
  
  comps <- lapply(1:n,function(x) c(x))  # each node starts as its own component
  
  for (u in 1:n){
    nbrs <- which(as.vector(A[u,])>0)
    for (v in nbrs){
      idx1 <- Position(function(x) u %in% x, comps, nomatch=0) # findset(u)
      idx2 <- Position(function(x) v %in% x, comps, nomatch=0) # findset(v)
      if (idx1 != idx2){
        # union(u,v)
        comps[[min(idx1,idx2)]] <- union(comps[[idx1]],comps[[idx2]])
        comps[[max(idx1,idx2)]] <- NULL
      }
    }
  } 
  
  return(list(comps,sapply(comps,length)))
}
get.components <- compiler::cmpfun(get.components)


