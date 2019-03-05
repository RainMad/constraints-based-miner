library(testthat)
library(shinytest)

context("test-name")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("../../R", compareImages = FALSE))
})

