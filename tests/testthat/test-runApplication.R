library(testthat)
library(shinytest)

context("Test if the Application runs.")

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  skip('Skip because of directory issues')
  expect_pass(testApp("../../R", compareImages = FALSE))
})