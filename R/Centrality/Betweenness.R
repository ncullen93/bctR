
# Centrality - Betweenness

#' Binary Graph Node Betweenness
#' 
#' Node betweenness centrality is the fraction
#' of all shortest paths in the network that
#' contain a given node - i.e nodes with high values of
#' betweenness centrality participate in a large
#' number of shortest paths.
#' 
#' Betweenness centrality may be normalizeed to the range [0,1]
#' as BC/[(N-1)(N-2)], where N is number of nodes in the graph
#' 
#' @param A : a matrix - binary (un)directed connection matrix
#' 
#' @return BC : a vector - node betweenness centrality vector
#' 
#' NOT TESTED
betweenness.bin <- function(G){
  n <- nrow(G) # number of nodes
  I <- diag(n) # identity matrix
  d <- 1 # path length
  NPd <- G # number of paths of length |d|
  NSPd <- G # number of shortest paths of length |d|
  NSP <- G # number of shortest paths of any length
  L <- G # length of shortest paths
  
  diag(NSP) <- 1
  diag(L) <- 1
  
  # calculate NSP and L
  while ( any(NSPd > 0) ){
    d <- d + 1
    NPd <- NPd %*% G
    NSPd <- NPd * (L == 0) # matrix product?
    NSP <- NSP + NSPd
    L <- L + d * (NSPd != 0)
  }
  L[L == 0] <- Inf # L for disconnected vertices is inf
  L[I > 0] <- 0
  NSP[NSP == 0] <- 1 # NSP for disconnected vertices is 1
  
  DP <- matrix(0,n,n) # vertex on vertex dependency - prob. dont need to instantiate this ?
  diam <- d - 1
  
  # calculate DP
  DP <- sum(vapply(diam:1,
                   function(d){
                     DPd1 <- ((L == d) * (1 + DP) / NSP) %*% t(G) * ((L == (d - 1)) * NSP)
                     return(DPd1)
                   },
                   numeric(1))
            )
  
  BC <- colSums(DP)
  return(BC)
}
betweenness.bin <- compiler::cmpfile(betweenness.bin)



#' Weighted Graph Node Betweenness
#' 
#' Node betweenness is the fraction of all shortest
#' paths in the network that contain a given node. Nodes
#' with high values of betweenness centrality participate
#' in a large number of shortest paths.
#' 
#' The input matrix "G" must be a connection-length matrix,
#' typically obtained via a mapping from weight to length.
#' For instance, in a weighted correlation network, higher
#' correlations are more naturally interpreted as shorter
#' distances and the input matrix should consequently be
#' some inverse of the connectivity matrix.
#' 
#' Betweenness centrality may be normalized to the range [0,1]
#' as BC/[(N-1)(N-2)], where N is the number of nodes in the graph
#' 
#' @param L : a matrix - (un)directed weighted connection matrix
#' 
#' @return BC : a vector - node betweenness centrality vector
#' 
#' NOT TESTED
betweenness.wei <- function(G){
  n <- nrow(G)
  BC <- rep(0,n)
  
  for (u in 1:n){
    D <- rep(Inf,n)
    D[u] <- 0 # distance from u
    NP <- rep(0,n)
    NP[u] <- 1 # number of paths from u
    S <- rep(TRUE,n) # distance permanance
    P <- matrix(0,n,n) # predecessors
    Q <- rep(0,n)
    q <- n - 1 # order of non-increasing distance
    
    G1 <- G
    V <- c(u)
    
    while( TRUE ){
      S[V] <- 0 # distance u->V is now permanent
      G1[,V] <- 0 # no in-edges as already shortest
      for (v in V){
        Q[q] <- V
        q <- q - 1
        W <- which(G1[v,] > 0)
        for (w in W){
          Duw <- D[v] + G1[v,w] # path length to be tested
          # if new u->w is shorter than old
          if Duw < D[w]{
            D[w] <- Duw
            NP[w] <- NP[v] # NP(u->w) = NP of new path
            P[w,] <- 0
            P[w,v] <- 1
          }
          # else if new u->w is equal to the old
          else if (Duw == D[w]){
            NP[w] <- NP[w] + NP[v] # NP(u->w) sum of old and new
            P[w,v] <- 1 # v is also predecessor
          }
        }
      }
      if (length(D[S]) == 0) break # all nodes were reached
      if (min(D[S]) == Inf){ # some nodes cannot be reached
        Q[1:(q+1)] <- which(D == min(D[S]))
        break
      }
      V <- which(D == min(D[S]))
    }
    
    DP <- rep(0, n)
    for (w in Q[1:(n-1)]){
      BC[w] <- BC[w] + DP[w]
      for (v in which(P[w,]>0)[1]){
        DP[v] <- DP[v] + (1 + DP[w]) * NP[v] / NP[w]
      }
    }
  }
  return(BC)
}
betweenness.wei <- compiler::cmpfile(betweenness.wei)



#' Binary Matrix Edge Betweenness
#' 
#' Edge betweenness centrality is the fraction of all shortest
#' paths in the network that contain a given edge - i.e. edges with high
#' values of edge betweenness centrality participate in a large
#' number of shortest paths.
#' 
#' Edge betweenness centrality can be normalized to the range [0,1] as
#' EBC/[(N-1)(N-2)], where N is the number of nodes in the network
#' 
#' @param A : a matrix - binary (un)directed connection matrix
#' 
#' @return EBC - a list - element one "EBC" is a matrix (edge betweenness
#' centrality matrix), and element two "BC" is a vector (node betweennesss
#' centrality vector)
#' 
#' NOT TESTED
edge.betweenness.bin <- function(G){
  n <- nrow(G)
  BC <- rep(0,n)
  EBC <- matrix(0,n,n)
  
  
  for (u in 1:n){
    D <- rep(0,n)
    D[u] <- 1
    NP <- rep(0,n)
    NP[u] <- 1
    P <- matrix(0,n,n)
    Q <- rep(0,n)
    q <- n - 1
    
    Gu <- G
    V <- c(u)
    while (any(V)){
      Gu[,V] <- 0
      for (v in V){
        Q[q] <- v
        q <- q - 1
        W <- which(Gu[v] > 0)
        for (w in W){
          if (D[w]){
            NP[w] <- NP[w] + NP[v]
            P[w,v] <- 1
          }
          else{
            D[w] <- 1
            NP[w] <- NP[v]
            P[w,v] <- 1
          }
        }
      }
      V <- which(Gu[V,] > 0) # might be wrong here
    }
    if (any(D)){ # some vertices unreachable
      Q[,q] <- which(D > 0) # these are first in line ??
    }
    DP <- rep(0,n) # dependency
    for (w in q[,(n-1)]){
      BC[w] <- BC[w] + DP[w]
      for (v in which(P[w,])[1]){
        DPvw <- (1 + DP[w]) * NP[v] / NP[w]
        DP[v] <- DP[v] + DPvw
        EBC[v,w] <- EBC[v,w] + DPvw
      }
    }
  }
  EBC <- list(EBC=EBC,
              BC=BC)
  return(EBC)
}
edge.betweenness.bin <- compiler::cmpfile(edge.betweenness.bin)



#' Weighted Matrix Edge Betweenness
#' 
#' Edge betweenness centrality is the fraction of all shortest
#' paths in the network that contain a given edge - i.e. edges with high
#' values of edge betweenness centrality participate in a large
#' number of shortest paths.
#' 
#' The input matrix "G" must be a connection-length matrix,
#' typically obtained via a mapping from weight to length.
#' For instance, in a weighted correlation network, higher
#' correlations are more naturally interpreted as shorter
#' distances and the input matrix should consequently be
#' some inverse of the connectivity matrix.
#' 
#' @param L : a matrix - (un)directed weighted connection matrix
#' 
#' @return EPC : a list - element one "EBC" is a matrix (edge betweenness
#' centrality matrix), and element two "BC" is a vector (node betweenness
#' centrality vector)
#' 
#' NOT TESTED
edge.betweenness.wei <- function(G){
  n <- nrow(G)
  BC <- rep(0,n)
  EBC <- matrix(0,n,n)
  
  for (u in 1:n){
    D <- rep(Inf,n)
    D[u] <- 0 # distance from u
    NP <- rep(0,n)
    NP[u] <- 1 # number of paths from u
    S <- rep(TRUE,n) # distance permanance
    P <- matrix(0,n,n) # predecessors
    Q <- rep(0,n)
    q <- n - 1 # order of non-increasing distance
    
    G1 <- G
    V <- c(u)
    
    while( TRUE ){
      S[V] <- 0 # distance u->V is now permanent
      G1[,V] <- 0 # no in-edges as already shortest
      for (v in V){
        Q[q] <- V
        q <- q - 1
        W <- which(G1[v,] > 0)
        for (w in W){
          Duw <- D[v] + G1[v,w] # path length to be tested
          # if new u->w is shorter than old
          if Duw < D[w]{
            D[w] <- Duw
            NP[w] <- NP[v] # NP(u->w) = NP of new path
            P[w,] <- 0
            P[w,v] <- 1
          }
          # else if new u->w is equal to the old
          else if (Duw == D[w]){
            NP[w] <- NP[w] + NP[v] # NP(u->w) sum of old and new
            P[w,v] <- 1 # v is also predecessor
          }
        }
      }
      if (length(D[S]) == 0) break # all nodes were reached
      if (min(D[S]) == Inf){ # some nodes cannot be reached
        Q[1:(q+1)] <- which(D == min(D[S]))
        break
      }
      V <- which(D == min(D[S]))
    }
    
    DP <- rep(0, n)
    for (w in Q[1:(n-1)]){
      BC[w] <- BC[w] + DP[w]
      for (v in which(P[w,]>0)[1]){
        DPvw <- (1 + DP[w]) * NP[v] / NP[w]
        DP[v] <- DP[v] + DPvw
        EBC[v,w] <- EBC[v,w] + DPvw
      }
    }
  }
  EBC <- list(EBC=EBC, 
              BC=BC)
  return(EBC)
}
edge.betweenness.wei <- compiler::cmpfile(edge.betweenness.wei)




