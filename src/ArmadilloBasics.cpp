// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;

/*
 Types:
      arma::vec
      arma::mat
 Construction:
      arma::mat x(n,r)
 Conversion:
      Rcpp::NumericMatrix x -> arma::mat y
          arma::mat y = as<arma::mat>(x)
      arma::mat x -> Rcpp::NumericMatrix y
          Rcpp::NumericMatrix y = wrap(x)
 Dimensions:
      x.n_rows
      x.n_cols
 Printing:
      x.print()
 Fill:
      x.fill(value)
 Reshape:
      x.reshape(n,r)
 Indexing:
      x(i,j) -> i,j element
      x.row(i) -> entire ith row
      x.rows(0,1) -> rows 0 and 1
      x.col(j) -> entire jth column
      x.cols(0,1) -> columns 0 and 1
 Element-Wise Operators:
      x+x
      x-x
      x % x -> multiplication
      function(x) -> any function
 Transpose:
      x.t()
 Matrix Operations:
      x.t() * x
 Inversion:
      inv(x*x.t())
      x.i()
 Decompositions:
      chol(x*x.t())
 
 
 */