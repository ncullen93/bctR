
# COMMUNITY - CONSENSUS

#' Agreement Unweighted
#' 
#' Takes as input a set of vertex partitions CI of
#' dimensions [vertex x partition]. Each column in CI
#' contains the assignments of each vertex to a
#' class/community/module. This function aggregates the
#' partitions in CI into a square [vertex x vertex] agreement
#' matrix D, whose elements indicate the number of times any
#' two vertices were assigned to the same class.
#' 
#' In the case that the number of nodes and partitions in CI
#' is large (greater than ~1000 nodes or greater than ~1000
#' partitions), the script can be made faster by computing D
#' in pieces. The optional input "buffsz" determines the 
#' size of each piece. Trial and error has found that 
#' "buffsz" ~ 150 works well.
#' 
#' @param ci : a matrix - set of M (possibly degenerate)
#' partitions of N nodes
#' @param buffsz : an integer/NA - sets buffer size
#' 
#' @return D : a matrix - the agreement matrix
#' 
#' NOT TESTED
agreement <- function(ci,
                          buffsz=NA){
  m <- dim(ci)[1]
  n <- dim(ci)[2]
  
  if (is.na(buffsz)) buffsz <- 1000
  
  if (m <= buffsz){
    ind <- matrix(1,nrow=m,ncol=n)
    D <- ind %*% t(ind)
  }
  else{
    a <- seq(1, m, by=buffsz)
    b <- seq(buffsz, m, by=buffsz)
    if (length(a)!=length(b)) b <- append(b, m)
    D <- Reduce(function(x,y) x+y,
                Map(a,b,
                    function(i,j){
                      y <- ci[,i:j]
                      ind <- matrix(1,nrow(y),ncol(y))
                      return(ind %*% t(ind))
                    })
    )
  }
  diag(D) <- 0
  return(D)
}
agreement <- compiler::cmpfun(agreement)



#' Agreement Weighted
#' 
#' D = AGREEMENT_WEIGHTED(CI,WTS) is identical to AGREEMENT, with the
#' exception that each partitions contribution is weighted according to
#' the corresponding scalar value stored in the vector WTS. As an example,
#' suppose CI contained partitions obtained using some heuristic for
#' maximizing modularity. A possible choice for WTS might be the Q metric
#' (Newman's modularity score). Such a choice would add more weight to
#' higher modularity partitions.
#' 
#' NOTE: Unlike AGREEMENT, this script does not have the input argument
#' BUFFSZ.
#' 
#' @param ci : a matrix - set of M (possibly degenerate) partitions
#' of N nodes
#' @param wts : a vector - relative weight of each partition
#' 
#' @return D : a matrix - weighted agreement matrix
#' 
#' NOT TESTED
agreement.weighted <- function(ci,
                                   wts){
  dim <- dim(ci)
  wts <- wts / sum(wts) # normalize
  D <- Reduce(function(x,y) x+y,
              Map(a,b,
                  function(i,j){
                    y <- ci[,i:j]
                    ind <- matrix(1,nrow(y),ncol(y))
                    return(ind %*% t(ind))
                  })
  )
  return(D)
}
agreement.weighted <- compiler::cmpfun(agreement.weighted)



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
#' NOT TESTED
consensus.und <- function(D,
                              tau,
                              reps=1000){
  
  n <- nrow(D)
  flag <- T
  while (flag){
    flag <- F
    dt <- D * (D >= tau) # not sure if matrix multiplication or filtering?
    diag(dt) <- 0
    
    if (sum(dt==0)==0){
      ciu <- 1:n
    }
    else{
      cis <- matrix(0, nrow=n, ncol=reps)
      for (i in 1:reps){
        cis[,i] <- modularity.louvain.und.sign(dt)
      }
      ciu <- unique.partitions(cis)
      nu <- ncol(ciu)
      if (nu > 1){
        flag <- T
        D <- agreement(cis) / reps
      }
    }
  }
  return() # np.squeeze(ciu+1) ??
}
consensus.und <- compiler::cmpfun(consensus.und)


