library(testthat)
library(constraintsbasedminer)

devtools::load_all("../constraintsbasedminer", export_all = TRUE)

test_check("constraintsbasedminer")

