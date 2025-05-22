library(dplyr)
library(tidyr)
library(testthat)
test_that("Outliers check", {

  ### test cols to add

  outliers1 <- identify_outliers(raw_data,columns_to_add = "X_uuid",columns_to_remove = "enumerator_num")
  outliers2 <- identify_outliers(raw_data,columns_to_add = "X_uuid")

  expect_false("enumerator_num" %in% outliers1$question)
  expect_true("X_uuid" %in% names(outliers1))
  expect_true("enumerator_num" %in% outliers2$question)

  ## test with null in unique number
  outliers3 <- identify_outliers(raw_data,columns_to_add = "X_uuid",minumum_unique_value_of_variable = 10)
  outliers4 <- identify_outliers(raw_data,columns_to_add = "X_uuid",minumum_unique_value_of_variable = NULL)

  expect_false("air_coolers_nb" %in% outliers3$question)
  expect_true("air_coolers_nb" %in% outliers4$question)


})
