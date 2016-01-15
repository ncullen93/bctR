context("Clustering")

# https://tgmstat.wordpress.com/2013/06/26/devtools-and-testthat-r-packages/
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


test_that("clustering.coef.wu gives correct answer",{
  x <- load.sample(thres=0.23)
  cc <- clustering.coef.wu(x)
  
  expect_equal(round(sum(cc),3),187.959)
})

test_that("clustering.coef.wu works for signed data",{
  x <- laod.signed.sample(thres=0.85)
  cc <- clustering.coef.wu(x)
  
  expect_equal(Im(sum(cc)),0)
})

test_that("transitivity.wu gives correct answer",{
  x <- load.sample(thres=0.23)
  t <- transitivity.wu(x)
  
  expect_equal(round(t,3),1.329)
})

test_that("transitivity.wu works on signed data",{
  x <- load.signed.sample(thres=0.85)
  t <- transitivity.wu(x)
  
  expect_equal(Im(t),0)
})

test_that("get.components gives correct answer",{
  x <- load.sparse.sample()
  c.list <- get.components(x)
  
  expect_equal(max(c.list[[1]]),19)
  expect_equal(max(c.list[[2]]),72)
})

test_consensus <- function(){}
  
test_cluscoef_wd <- function(){}
  
test_transitivity_wd <- function(){}
  
test_cluscoef_bu <- function(){}
  
test_transitivity_bu <- function(){}
  
test_cluscoef_bd <- function(){}
  
test_transitivity_bd <- function(){}
  