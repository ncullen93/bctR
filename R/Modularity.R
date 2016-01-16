
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
#' Note: Function is not validated/running yet.
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
UNC.modularity.louvain.und.sign <- function(W,
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
  
  h <- 2 # hierarchy index
  nh <- n # number of nodes in hierarcy
  ci <- list(1:n) # hierarchical module assignments
  #q <- c(-1,0) # hierarchical modularity values
  q <- numeric(100)
  q[1:2] <- c(-1,0)
  
  while ( (q[h] - q[h-1]) > 1e-10 ){
    
    stopifnot(h < 300) # Modularity Infinite Loop Style A ??
    kn0 <- colSums(W0)
    kn1 <- colSums(W1)
    # copying is expensive...
    km0 <- kn0
    km1 <- kn1
    knm0 <- W0
    knm1 <- W1
    
    m <- 1:n # initial module assignments
    flag <- T
    it <- 0
    while (flag){
      it <- it + 1
      stopifnot(it < 1000) # Infinite Loop Detected and Stopped
      flag <- F
      # loop over nodes in random order
      for (u in sample(nh)){
        ma <- m[u]
        dQ0 <- ((knm0[u:length(knm0)] + W0[u,u] - knm0[u,ma]) - 
                  gamma * kn0[u] * (km0 + kn0[u] - km0[ma]) / s0) # positive dQ
        dQ1 <- ((knm1[u,length(knm1)] + W1[u,u] - knm1[u,ma]) -
                  gamma * kn1[u] * (km1 + kn1[u] - km1[ma]) / s1) # negative dQ
        dQ <- d0 * dQ0 - d1 * dQ1 # rescaled changes in modularity
        dQ[ma] <- 0 # no changes for same module
        
        max.dQ <- max(dQ) # maximal increase in modularity
        if ( max.dQ > 1e-10 ){
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
          
          m[u] <- mb # reassign module
        }
      }
    }
    m <- as.factor(m)
    m <- vapply(m,function(y) which(levels(m)==y),numeric(1)) # new module assignments
    h <- h + 1
    ci[[h]] <- m[ci[[h-1]]][1:n]
    
    nh <- max(m) # number of new nodes
    wn0 <- matrix(nrow=nh, ncol=nh) # new positive weights matrix
    wn1 <- wn0 # copy

    # this is bad R code
    for (u in 1:nh){
      for (v in u:nh){
        wn0[u,v] <- sum(W0[m==u,m==v])
        wn1[u,v] <- sum(W1[m==u,m==v])
        wn0[v,u] <- wn0[u,v]
        wn1[v,u] <- wn1[u,v]
      }
    }
    
    W0 <- wn0
    W1 <- wn1
    #q <- c(q,0) # this is slow
    q[h] <- 0
    # compute modularity
    q0 <- sum(diag(W0)) - sum(W0 %*% W0) / s0
    q1 <- sum(diag(W1)) - sum(W1 %*% W1) / s1
    q[h] <- d0 * q0 - d1 * q1
    
    ci.ret <- vapply(ci[[length(ci)]],function(y) which(levels(as.factor(m))==y),numeric(1))
  }
  return(list(ci.ret=ci.ret,q[length(q)]))
}
modularity.louvain.und.sign <- compiler::cmpfun(UNC.modularity.louvain.und.sign)



#' Louvain Modularity Algorithm on Undirected Graph
#' 
#' The optimal community structure is a subdivision of the network into
#' nonoverlapping groups of nodes in a way that maximizes the number of
#' within-group edges, and minimizes the number of between-group edges.
#' The modularity is a statistic that quantifies the degree to which the
#' network may be subdivided into such clearly delineated groups.
#'
#' The Louvain algorithm is a fast and accurate community detection
#' algorithm (as of writing). The algorithm may also be used to detect
#' hierarchical community structure.
#' 
#' R Microbenchmark - Fast enough..
#' Unit: milliseconds
#' expr      min       lq     mean   median       uq      max neval
#' fun    8.890078 11.65477 12.90705 12.62741 13.85725 19.57911   100
#' 
#' WITH compile::cmpfun() - Fast!
#' Unit: milliseconds
#' expr      min       lq     mean   median       uq      max neval
#' fun    6.015344 7.543102 9.385713 9.529057 10.69335 13.49019   100
#' 
#' Note: Function is not validated yet.
#' 
#' @param W : a Matrix - undirected weighted/binary connection matrix
#' @param gamma : a float - resolution parameter. default value=1. 
#' Values 0 <= gamma < 1 detect larger modules while gamma > 1 
#' detects smaller modules.
#' @param hierarchy : a boolean - enables hier. output
#' @param seed : an integer - random seed
#' 
#' @return ciQ : a list - two elements where element one is 'ci',
#' a vector (refined community affiliation network), and element
#' two is 'Q', a float (optimized modularity metric).If hierarchical 
#' output enabled, becomes an Hx1 array of floats instead.
#' 

UNC.modularity.louvain.und <- function(W,
                                  gamma=1,
                                  hierarchy=FALSE,
                                  seed=NA){
  if (!is.na(seed)) set.seed(seed)
  
  n <- nrow(W) # number of nodes
  s <- sum(W) # total edge weight
  h <- 1 # hierarchy index
  ci <- list(1:n) # hierarchical module assignments
  q <- numeric(100)
  q[1] <- -1
  #q <- c(-1) # hierarhcical modularity values
  n0 <- n
  
  while (TRUE){
    stopifnot(h < 300) # infinite loop
    k <- rowSums(W) # node degree
    Km <- k # module degree
    Knm <- W # node-to-module degree

    m <- 1:n # initial module assignments
    flag <- TRUE
    it <- 0
    # GOOD UP TO HERE
    while (flag){
      #cat('Iteration: ' , it, '\n')
      it <- it + 1
      stopifnot(it < 1000) # infinite loop
      
      flag <- FALSE
      
      for (i in sample(n)){
        ma <- m[i] # module assignment
        # algorithm condition
        dQ <- ((Knm[i,] - Knm[i,ma] + W[i,i]) -
                 gamma * k[i] * (Km - Km[ma] + k[i]) / s)
        dQ[ma] <- 0 # change to itself is 0
        max.dQ <- max(dQ) # find maximum modularity increase
                              
        if (max.dQ > 1e-10){
          j <- which.max(dQ)
          
          Knm[,j] <- Knm[,j] + W[,i] # change node-to-module degrees
          Knm[,ma] <- Knm[,ma] - W[,i]
          
          Km[j] <- Km[j] + k[i] # change module degrees
          Km[ma] <- Km[ma] - k[i]
          
          m[i] <- j # reassign module
          flag <- TRUE
        }
      }
    }
    
    m <- as.numeric(as.factor(m))
    h <- h + 1
    ci[[h]] <- m[ci[[h-1]]][1:n]
    
    n <- max(m) # new number of modules
    W1 <- matrix(nrow=n,ncol=n) # new weighted matrix
    
    for (i in 1:n){
      for (j in i:n){
        # pool weights of nodes in same module
        wp <- sum(W[m==i,m==j])
        W1[i,j] <- wp
        W1[j,i] <- wp
      }
    }
    
    W <- W1
    
    q[h] <- sum(diag(W)) / s - gamma * sum((W/s) %*% (W/s))
    if ( (q[h] - q[h-1]) < 1e-10 ) break
  }
  
  if (hierarchy){
    ci <- ci[[2:(length(ci)-1)]]
    q <- q[2:(length(q)-1)]
    ciq <- list(ci=ci,q=q)
  }
  else{
    ciq <- list(ci=ci[[h-1]],q=q[h-1])
  }
  return(ciq)
}
modularity.louvain.und <- compiler::cmpfun(UNC.modularity.louvain.und)






