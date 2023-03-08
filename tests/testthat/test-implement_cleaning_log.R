library(dplyr)
library(assertthat)

testthat::test_that("implement cleaning log",{

  clean_data_fi <- implement_cleaning_log(df = raw_data,df_uuid = "X_uuid",
                         cl = cleaning_log,
                         cl_change_type_col = "change_type",
                         cl_change_col =  "questions",cl_new_val = "new_value",
                         cl_uuid = "X_uuid")
  expect_equal(nrow(clean_data_fi),nrow(clean_data))


  expect_error(implement_cleaning_log(df = raw_data,df_uuid = "X_uuid",
                                        cl = cleaning_log,
                                        cl_change_type_col = "change_type",
                                        change_type_for_blank_response = "c",
                                        cl_change_col =  "questions",cl_new_val = "new_value",
                                        cl_uuid = "X_uuid"))

  expect_error(implement_cleaning_log(df = raw_data,df_uuid = "X_uuid",
                         cl = cleaning_log,
                         cl_change_type_col = "change_ype",
                         cl_change_col =  "questions",cl_new_val = "new_value",
                         cl_uuid = "X_uuid"))


})


testthat::test_that("check cleaning log",{

  expect_no_error(check_cleaning_log(df = raw_data,df_uuid = "X_uuid",cl = cleaning_log,
                     cl_change_type_col = "change_type",
                     change_type_for_change_response = "change_response",cl_change_col = "questions",
                     cl_uuid = "X_uuid",cl_new_val = "new_value"))

  expect_error(check_cleaning_log(df = raw_data,df_uuid = "X_uuid",cl = cleaning_log,
                                     cl_change_type_col = "change_type",
                                     change_type_for_change_response = "change_esponse",cl_change_col = "questions",
                                     cl_uuid = "X_uuid",cl_new_val = "new_value"))

  expect_error(check_cleaning_log(df = raw_data,df_uuid = "X_uuid",cl = cleaning_log,
                                  cl_change_type_col = "chage_type",
                                  change_type_for_change_response = "change_response",cl_change_col = "questions",
                                  cl_uuid = "X_uuid",cl_new_val = "new_value"))


  })



