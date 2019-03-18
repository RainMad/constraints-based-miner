library(testthat)
library(shinytest)
library(dplyr)

context("test constraint 'at most once'")

test_that("Constratint 'at most once' works", {

  activity = "A"
  
  result <- at_most_once(eventlog, activity)
  
  expect_true(result %>% filter(CASE_concept_name == Trace1) %>% pull(resp))
  expect_false(result %>% filter(CASE_concept_name == Trace2) %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == Trace3) %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == Trace4) %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == Trace5) %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == Trace6) %>% pull(resp))
  expect_false(result %>% filter(CASE_concept_name == Trace7) %>% pull(resp))
  expect_true(result %>% filter(CASE_concept_name == Trace8) %>% pull(resp))
})

