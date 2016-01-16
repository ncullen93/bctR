
# Code for Utility functions

#' Threshold Proportional
#' 
#' This function "thresholds" the connectivity matrix by preserving a
#' proportion p (0<p<1) of the strongest weights. All other weights, and
#' all weights on the main diagonal (self-self connections) are set to 0.
#' 
#' Note: For data w/ negative numbers, we consider Absolute Value of weights.
#' 
#' @param W : an R Matrix - Weighted Connectivity Matrix
#' @param p : a float - Proportional Weight Threshold
#' @param copy : a boolean - Whether to modify in place or not
#' 
#' @return W : Threshold Connectivity Matrix
#' 
UNC.threshold.proportional <- function(W, 
                                   p, 
                                   copy=F){
  stopifnot(p > 0, p <= 1)
  #if (copy) W <- W
  n <- nrow(W) # number of nodes
  diag(W) <- 0 # clear the diagonal, modifies in place
  
  # if symmetric matrix, ensure symmetry is preserved
  s.flag <- F
  ud <- 1
  if (isSymmetric(W)) {
    s.flag <- T
    ud <- 2
    W[lower.tri(W)] <- 0
  }
  
  I <- order(abs(W),decreasing=T) # sort indices by value magnitude
  en <- as.integer(round((n*n-n) * p/ud))+1 # num. links to preserve
  W[I[en:length(I)]] <- 0 # remove the smallest links
  
  if (s.flag) W <- W + t(W) # add back the lower triangle
  
  return(W)
}
threshold.proportional <- compiler::cmpfun(UNC.threshold.proportional)

#' Threshold Absolute
#' 
#' This function thresholds the connectivity matrix by absolute weight
#' magnitude. All weights below the given threshold, and all weights
#' on the main diagonal (self-self connections) are set to 0.
#' 
#' @param W : an R Matrix - Weighted Connectivity Matrix
#' @param thr : a float - Absolute Weight Threshold
#' @param copy : a boolean - Whether to modify in place or not
#' 
#' @return W : Threshold Connectivity Matrix
#' 
UNC.threshold.absolute <- function(W,
                               thr,
                               copy=F){
  diag(W) <- 0 # clear the diagonal, modifies in place
  W[W < thr] <- 0
  return(W)
}
threshold.absolute <- compiler::cmpfun(UNC.threshold.absolute)

#' Weight Conversion
#' 
#' This function may either binarize an input weighted connection matrix,
#' normalize an input weighted connection matrix or convert an input
#' weighted connection matrix to a weighted connection-length matrix.
#' 
#' Binarization converts all present connection weights to 1.
#' 
#' Normalization scales all weight magnitudes to the range [0,1] and
#' should be done prior to computing some weighted measures, such as the
#' weighted clustering coefficient.
#' 
#' Conversion of connection weights to connection lengths is needed
#' prior to computation of weighted distance-based measures, such as
#' distance and betweenness centrality. In a weighted connection network,
#' higher weights are naturally interpreted as shorter lengths. The
#' connection-lengths matrix here is defined as the inverse of the
#' connection-weights matrix.
#' 
#' Options for 'wcm' param:
#'        'binarize' : binarize weights
#'        'normalize' : normalize weights
#'        'lengths' : convert weights to lengths (invert matrix)
#' 
#' @param W : an R Matrix - Weighted Connectivity Matrix
#' @param wcm : a string - Weight Conversion Command
#' 
#' @return W : an R Matrix - Connectivity Matrix with 
#' specified changes
#' 
weight.conversion <- function(W,
                              wcm,
                              copy=F){
  stopifnot(wcm %in% c('binarize','normalize','lengths'))
  if (wcm == 'binarize'){
    W <- binarize(W)
  }
  else if (wcm == 'normalize'){
    W <- normalize(W)
  }
  else if (wcm == 'lengths'){
    W <- invert(W)
  }
}

#' Binarize
#' 
#' Binarizes an input weighted connection matrix,
#' i.e. any value > 0 gets set to 1
#' 
#' @param W : an R matrix - weighted connectivity matrix
#' @param copy : boolean
#' 
#' @return W : an R matrix - binary weighted connectivity matrix
#' 
binarize <- function(W,
                     copy=F){
  W[W!=0] <- 1
  return(W)
}

#' Normalize
#' 
#' Normalizes an input weighted connection matrix,
#' i.e. divides each value by the abs. max. value in matrix
#' 
#' @param W : an R matrix - weighted connectivity matrix
#' @param copy : boolean
#' 
#' @return W : an R matrix - normalized weighted connectivity matrix
#' 
normalize <- function(W,
                      copy=F){
  W <- W / max(abs(W))
  return(W)
}

#' Invert
#' 
#' Inverts elementwise the weights in an input connection matrix,
#' i.e. changes W from a matrix of internode strengths to 
#' a matrix of internode distances
#' 
#' @param W : an R matrix - weighted connectivity matrix
#' @param copy : boolean
#' 
#' @return W : an R matrix - inverted weighted connectivity matrix
#' 
invert <- function(W,
                   copy=F){
  W[which(W>0)] <- 1 / W[which(W>0)]
  return(W)
}

#' Autofix
#' 
#' Fix a bunch of common problems. 
#' More specifically, remove Inf and NaN, ensure 
#' exact binariness and symmetry (i.e. remove floating point
#' instability), and zero diagonal.
#' 
#' @param W : an R matrix - weighted connectivity matrix
#' @param copy : boolean
#' 
#' @return W : an R matrix - autofixed weighted connectivity matrix
#' 
autofix <- function(W,
                    copy=F){
  
}

