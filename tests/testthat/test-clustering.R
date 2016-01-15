context("Clustering")

# Expectations - https://tgmstat.wordpress.com/2013/06/26/devtools-and-testthat-r-packages/
#
# expect_true(x)	checks that an expression is true.
# expect_false(x)	checks that an expression is false.
# expect_is(x, y)	checks that an object inherit()s from a specified class
# expect_equal(x, y)	check for equality with numerical tolerance
# expect_equivalent(x, y)	a more relaxed version of equals() that ignores attributes
# expect_identical(x, y)	check for exact equality
# expect_matches(x, y)	matches a character vector against a regular expression.
# expect_output(x, y)	matches the printed output from an expression against a regular expression
# expect_message(x, y)	checks that an expression shows a message
# expect_warning(x, y)	expects that you get a warning
# expect_error(x, y)	verifies that the expression throws an error.


#' Test 'clustering.coef.wu'
#' 
#' TESTS:
#' 1/14/16 - PASSED
test.cluscoef.wu <- function(){
  x <- load.sample(thres=0.23)
  cc <- clustering.coef.wu(x)
  stopifnot(round(sum(cc),3)==187.959)
  cat("Successful")
}

#' Test 'clustering.coef.wu' on signed.sample data
#' 
#' TESTS:
#' 1/14/16 - PASSED
test.cluscoef.signed <- function(){
  x <- load.signed.sample(thres=0.85)
  cc <- clustering.coef.wu(x)
  stopifnot(Im(sum(cc))==0)
  cat('Successful')
}

#' Test 'transitivity.wu'
#' 
#' TESTS:
#' 1/14/16 - PASSED
test.transitivity.wu <- function(){
  x <- load.sample(thres=0.23)
  t <- transitivity.wu(x)
  stopifnot(round(t,3)==1.329)
  cat('Successful')
}

#' Test 'transitivty.wu' on signed.sample data
#' 
#' Tests:
#' 1/14/16 - PASSED
test.transitivity.signed <- function(){
  x <- load.signed.sample(thres=0.85)
  t <- transitivity.wu(x)
  stopifnot(Im(t)==0)
  cat('Successful')
}

#' Test 'get.components'
#' 
#' Function is not implemented yet
test_component <- function(){}
  
#' Test 'consensus.und' and 'modularity.und'
#' 
test_consensus <- function(){
  x <- load.sample(thres=0.38)
  ci <- consensus.und(x, 0.1, reps=50)
  stopifnot(max(ci)==4)
  #_,q <- modularity.und(x, kci=ci)
  stopifnot(round(q,2)==0.27)
  cat('Successful')
}
  
test_cluscoef_wd <- function(){}
  
test_transitivity_wd <- function(){}
  
test_cluscoef_bu <- function(){}
  
test_transitivity_bu <- function(){}
  
test_cluscoef_bd <- function(){}
  
test_transitivity_bd <- function(){}
  