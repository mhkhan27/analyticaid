
## test for find_qualitative_cols_from_kobo
testthat::test_that("Check_ find_qualitative_cols_from_kobo",{
expect_length(
find_qualitative_cols_from_kobo(df = clean_data,kobo_survey_sheet = survey),21)
})


## test for number only


testthat::test_that("Check find_integer_cols",{
expect_false(all(integer_only(c("1","2","x","X1","1.0"))))
  expect_true(all(integer_only(c(1,2,3))))
  expect_true(all(integer_only(c(1,2,3))))

})


## test find_integer_cols

testthat::test_that("Check find_integer_cols",{
  expect_length(
    find_integer_cols(df = clean_data),38)
})


## test for fix_data_type (with character)

testthat::test_that("Check fix_data_type",{
  DF <- raw_data %>% fix_data_type()
  DF_character <- raw_data %>% fix_data_type(character_cols = "X_index")
  expect_identical(class(DF$X_index),"numeric")
  expect_identical(class(DF_character$X_index),"character")
})



## test for fix_data_type (with remove_all_NA_col ==F)
testthat::test_that("Check remove_all_NA_col in fix_data_type",{
  DFx <- raw_data
  DFx$empty <- NA
  expect_equal(ncol(fix_data_type(df = DFx ,remove_all_NA_col = T)) ,ncol(raw_data))
  expect_false(ncol(fix_data_type(df = DFx,remove_all_NA_col = F)) == ncol(raw_data))
})



