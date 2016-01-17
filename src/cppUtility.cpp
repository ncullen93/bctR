// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

#include <algorithm>
#include <iostream>
#include <vector>

//#include <Rcpp.h>
#include <RcppArmadillo.h>
#include <boost/math/common_factor.hpp> 
#include <boost/numeric/ublas/matrix.hpp>

using namespace Rcpp;

/*
 RcppArmadillo Basics:
 
 Matrix - arma::mat
 
 */




// [[Rcpp::export]]
arma::mat a1(arma::mat x) {
  x.t();
  
  
  
  return(x);
}


// [[Rcpp::export]]
int computeGCD(int a, int b) {
  return boost::math::gcd(a, b);
}

// [[Rcpp::export]]
int computeLCM(int a, int b) {
  return boost::math::lcm(a, b);
}

// [[Rcpp::export]]
IntegerMatrix ijkalgorithm(IntegerMatrix A, 
                           IntegerMatrix B) {
  int n = A.ncol();
  
  IntegerMatrix C(n);
  
  for (int i = 0; i < n; i++) {
    for (int k = 0; k < n; k++) {
      for (int j = 0; j < n; j++) {
        C(i,j) += A(i,k) * B(k,j);
      }
    }
  }
  return C;
}

/*
 Vector: (IntegerVector, NumericVector, LogicalVector, CharacterVector) 
 Scalar: (int, double, bool, String)
 Matrix: (IntegerMatrix, NumericMatrix, LogicalMatrix, CharacterMatrix) 
 RObject useAnRfunction(Function f) {}
 */

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

