library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'participation'")

test_that("Constratint 'participation' works", {

  activity = "A"
  
  result <- participation(eventlog, activity)
  
  expect_true(filter_result(result, Trace1))
  expect_true(filter_result(result, Trace2))
  expect_false(filter_result(result, Trace3))
})

