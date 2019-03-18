library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'end'")

test_that("Constratint 'end' works", {
  
  activity1 = "A"
  
  result <- end_constraint(eventlog, activity1)
  
  expect_false(filter_result(result, Trace1))
  expect_true(filter_result(result, Trace8))
  expect_false(filter_result(result, Trace6))
})