// [[Rcpp::depends(RcppArmadillo)]] 
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
IntegerMatrix clustering_coef_bd(IntegerMatrix A) {
  int nrow = A.nrow(), ncol = A.ncol();
  
  IntegerMatrix S(nrow,ncol);
  S = A + transpose(A)
  
  return(S)
}