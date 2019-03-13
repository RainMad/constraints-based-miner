library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'responded existence'")


filter_result <- function(result, case){
  return (result %>% filter(CASE_concept_name == case) %>% pull(resp))
}

test_that("Constratint 'responded existence' works", {
  eventlog <- tibble(CASE_concept_name = c("Trace 1", "Trace 1", 
                                           "Trace 2", "Trace 2", "Trace 2",
                                           "Trace 3", "Trace 3",
                                           "Trace 4",
                                           "Trace 5", "Trace 5", "Trace 5",
                                           "Trace 6", "Trace 6"),
                     activity_id = c("A", "B", 
                                     "A", "B", "A",
                                     "B", "C",
                                     "A",
                                     "A", "A", "C",
                                     "B", "A"))
  activity1 = "A"
  activity2 = "B"
  
  result <- responded_existence(eventlog, activity1, activity2)
  
  expect_true(filter_result(result, "Trace 1"))
  expect_true(filter_result(result, "Trace 2"))
  expect_true(filter_result(result, "Trace 3"))
  expect_false(filter_result(result, "Trace 4"))
  expect_false(filter_result(result, "Trace 5"))
  expect_true(filter_result(result, "Trace 6"))
})