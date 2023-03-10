library(dplyr)
library(openxlsx)
# test for read and write -------------------------------------------------

## test for read excel with sheet all and data_type_fix is ture and remove all NA cols T
testthat::test_that("check_read_sheets",{

  data <- read_sheets(test_path("testdata","sample_data_for_read_sheet.xlsx"),sheets = c("survey","choices"))
  expect_null(data)

  data <- read_sheets(test_path("testdata","sample_data_for_read_sheet.xlsx"),sheets = c("survey","choices"),output_as_list = T)
  expect_length(data,2)

  data <- read_sheets(test_path("testdata","sample_data_for_read_sheet.xlsx"),sheets ="survey",output_as_list = T)
  expect_length(data,1)

  data <- read_sheets(test_path("testdata","sample_data_for_read_sheet.xlsx"),output_as_list = T)
  expect_length(data,3)


  expect_error(read_sheets(test_path("testdata","sample_data_for_read_sheet.xlsx"),sheets = c("surv","choices"))
)

})
## test for read excel with sheet  NOT all and data_type_fix is false and remove all NA F



# write excel -------------------------------------------------------------


