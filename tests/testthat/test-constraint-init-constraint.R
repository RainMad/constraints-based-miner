library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'init'")

test_that("Constratint 'init' works", {
  
  activity1 = "A"
  
  result <- init_constraint(eventlog, activity1)
  
  expect_true(filter_result(result, Trace1))
  expect_false(filter_result(result, Trace8))
  expect_false(filter_result(result, Trace6))
})