library(testthat)
library(shinytest)

context("test constraints")


test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("../../R", compareImages = FALSE))
})

test_that("Constratint works", {
  eventlog <- tibble(CASE_concept_name = c("Trace 1", "Trace 1", 
                                           "Trace 2", "Trace 2", "Trace 2",
                                           "Trace 3", "Trace 3"),
                     activity_id = c("A", "B", 
                                     "A", "B", "A",
                                     "B", "C"))
  activity = "A"
  
  result <- at_most_once(eventlog, activity)
  
  expect_true(result %>% filter(CASE_concept_name == "Trace 1") %>% pull(resp))
  expect_false(result %>% filter(CASE_concept_name == "Trace 2") %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == "Trace 3") %>% pull(resp))
})

