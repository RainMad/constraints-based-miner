library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'end'")


filter_result <- function(result, case){
  return (result %>% filter(CASE_concept_name == case) %>% pull(resp))
}

test_that("Constratint 'end' works", {
  eventlog <- tibble(CASE_concept_name = c("Trace 1", "Trace 1", "Trace 1", 
                                           "Trace 2", "Trace 2",
                                           "Trace 3"),
                     activity_id = c("A", "B", "C", 
                                     "B", "A",
                                     "C"))
  
  activity1 = "A"
  
  result <- end_constraint(eventlog, activity1)
  
  expect_false(filter_result(result, "Trace 1"))
  expect_true(filter_result(result, "Trace 2"))
  expect_false(filter_result(result, "Trace 3"))
})