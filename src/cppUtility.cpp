#include <algorithm>
#include <iostream>
#include <vector>

#include <Rcpp.h>
using namespace Rcpp;


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

// [[Rcpp::export]]
IntegerVector BFS_uw(NumericMatrix){

}

// [[Rcpp::export]]
IntegerVector BFS_uu(IntegerMatrix){

}