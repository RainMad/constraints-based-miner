library(testthat)
library(shinytest)
library(dplyr)

Sys.setenv("R_TESTS" = "")
context("test constraint 'at most once'")
source("inst/constraintsBasedApp/constraints.R")

test_that("Constratint 'at most once' works", {
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

