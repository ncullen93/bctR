# bctR
Brain Connectivity Toolbox in R

This is an effort to import the Brain Connectivity Toolbox to the R programming language. The package is currently under development and is maintained by N. Cullen of the Di2Ag Laboratory at Dartmouth College <ncullen.th@dartmouth.edu>. I warmly support any contributions - send me an email if you would like to collaborate. 

The code is mostly in pure R, but I am planning to implement a few functions in C++ (w/ Rcpp) which would never need to be modified - e.g. breadth-first search.

Additionally, I am planning to use parallel processing wherever possible so that performance will be quite fast - especially when run on a high-performance computing cluster.

My code is heavily based right now off the python version: https://github.com/aestrivex/bctpy

Expected Date of Completion: March 1, 2016.
