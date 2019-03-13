library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'chain response'")


filter_result <- function(result, case){
  return (result %>% filter(CASE_concept_name == case) %>% pull(resp))
}

test_that("Constratint 'chain response' works", {
  eventlog <- tibble(CASE_concept_name = c("Trace 1", "Trace 1", "Trace 1", 
                                           "Trace 2", "Trace 2",
                                           "Trace 3",
                                           "Trace 4",
                                           "Trace 5", "Trace 5", "Trace 5",
                                           "Trace 6", "Trace 6", "Trace 6",
                                           "Trace 7", "Trace 7", "Trace 7", "Trace 7", "Trace 7"),
                     activity_id = c("A", "B", "C", 
                                     "B", "A",
                                     "C",
                                     "B",
                                     "A", "B", "A",
                                     "B", "A", "B",
                                     "A", "A", "C", "D", "B"))
  activity1 = "A"
  activity2 = "B"
  
  result <- chain_response(eventlog, activity1, activity2)
  
  expect_true(filter_result(result, "Trace 1"))
  expect_false(filter_result(result, "Trace 2"))
  expect_true(filter_result(result, "Trace 3"))
  expect_true(filter_result(result, "Trace 4"))
  expect_false(filter_result(result, "Trace 5"))
  expect_true(filter_result(result, "Trace 6"))
  expect_false(filter_result(result, "Trace 7"))
})