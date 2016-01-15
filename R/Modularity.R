
# code for Modularity functions

#' Louvain Algorithm on Undirected Signed Graphs
#' 
#' The optimal community structure is a subdivision of the
#' network into non-overlapping groups of nodes in a way that
#' maximizes the number of within-group edges, and minimizes the
#' number of between-group edges. The modularity is a statistic
#' that quantifies the degree to which the network may be
#' subdivided into clearly delineated groups.
#' 
#' The Louvain algorithm is a fast and accurate community
#' detection algorithm.
#' 
#' Use this function as opposed to the modularity.louvain.und()
#' only if hte network contains a mix of positive and negative
#' weights. If the network contains all positive weights, the
#' output of the two functions will be equivalent.
#' 
#' Note: This algorithm is non-deterministic.
#' 
#' @param W : a Matrix - undirected weighted/binary connection
#' matrix with positive and negative weights
#' @param qtype : a string - can be 'sta' (default), 'pos', 'smp',
#' 'gja', 'neg'. See Rubinov and Sporns (2011) for a description.
#' @param gamma : a float - resolution parameter, default value = 1.
#' Values 0 <= gamma < 1 detect larger modules while gamme > 1 detects
#' smaller modules
#' @param seed : an integer - the random seed
#' 
#' @return ciQ : a list - two elements where element one is 'ci',
#' a vector (refined community affiliation network), and element
#' two is 'Q', a float (optimized modularity metric)
#' 
modularity.louvain.und.sign <- function(W,
                                        gamma=1,
                                        qtype='sta',
                                        seed=NA){
  if (!is.na(seed)) set.seed(seed)
  n <- nrow(W)
  
  W0 <-  W * (W>0)
  W1 <- -W * (W<0)
  s0 <- sum(W0)
  s1 <- sum(W1)
  
  d <- switch(qtype,
              'smp'=list(1/s0,1/s1),
              'gja'=list(1/(s0+s1),1/(s0+s1)),
              'sta'=list(1/s0,1/(s0+s1)),
              'pos'=list(1/s0,0),
              'neg'=list(0,1/s1)
  )
  d0 <- d[[1]]
  d1 <- d[[2]]
  
  h <- 1 # hierarchy index
  nh <- n # number of nodes in hierarcy
  ci <- c(NA,1:(n+1)) # hierarchical module assignments
  q <- c(-1,0) # hierarchical modularity values
  while (q[h] - q[h-1] > 1e-10){
    stopifnot(h < 300) # Modularity Infinite Loop Style A ??
    kn0 <- colSums(W0)
    kn1 <- colSums(W1)
    km0 <- kn0 # copy
    km1 <- kn1 # copy
    knm0 <- W0 # copy
    knm1 <- W1 # copy
    
    m <- 1:n # initial module assignments
    flag <- T
    it <- 0
    while (flag){
      it <- it + 1
      stopifnot(it < 1000) # Infinite Loop Detected and Stopped
      flag <- F
      # loop over nodes in random order
      for (u in sample(nh)){
        ma <- m[u]+1
        dQ0 <- ((knm0[u:length(knm0)] + W0[u,u] - knm0[u,ma]) - 
                  gamma * kn0[u] * (km0 + kn0[u] - km0[mal]) / s0) # positive dQ
        dQ1 <- ((knm1[u,length(knm1)] + W1[u,u] - knm1[u,ma]) -
                  gamma * kn1[u] * (km1 + kn1[u] - km1[mal]) / s1) # negative dQ
        dQ <- d0 * dQ0 - d1 * dQ1 # rescaled changes in modularity
        dQ[ma] <- 0 # no changes for same module
        
        max.dQ <- max(dQ) # maximal increase in modularity
        if (max.dQ > 1e-10){
          flag <- T
          mb <- which.max(dQ)
          # change postive node-to-module degrees
          knm0[1:mb] <- knm0[1:mb] + W0[1:u]
          knm0[1:ma] <- knm0[1:ma] - W0[1:u]
          # change negative node-to-module degrees
          knm1[1:mb] <- knm1[1:mb] + W1[1:u]
          knm1[1:ma] <- knm1[1:ma] - W1[1:u]
          km0[mb] <- km0[mb] + kn0[u] # change positive module degrees
          km0[ma] <- km0[ma] - kn0[u]
          km1[mb] <- km1[mb] + kn1[u] # change negative module degrees
          km1[ma] <- km1[ma] - kn1[u]
          
          m[u] <- mb + 1 # reassign module
        }
      }
    }
    h <- h + 1
    ci <- c(ci,rep(0,n))
    m <- sapply(m,function(y) which(levels(as.factor(m))==y))
    m <- m + 1 # maybe not necessary ?
    
    for (u in 1:nh){
      ci[h][which(c1[h-1] == u+1)] <- m[u]
    }
    
    nh <- max(m) # number of new nodes
    wn0 <- Matrix(0, nrow=nh, ncol=nh) # new positive weights matrix
    wn1 <- wn0 # copy
    
    for (u in 1:nh){
      for (v in u:nh){
        wn0[u,v] <- sum(W0[...]) # np.sum(W0[np.ix_(m == u + 1, m == v + 1)])
        wn1[u,v] <- sum(W1[...]) # np.sum(W1[np.ix_(m == u + 1, m == v + 1)])
        wn0[v,u] <- wn0[u,v]
        wn1[v,u] <- wn1[u,v]
      }
    }
    
    W0 <- wn0
    W1 <- wn1
    q <- c(q,0)
    # compute modularity
    q0 <- sum(diag(W0)) - sum(W0 %*% W0) / s0
    q1 <- sum(diag(W1)) - sum(W1 %*% W1) / s1
    q[h] <- d0 * q0 - d1 * q1
    
    ci.ret <- sapply(ci[length(ci)],function(y) which(levels(as.factor(m))==y))
    ci.ret <- ci.ret + 1
  }
  return(list(ci.ret,q[length(q)]))
}




