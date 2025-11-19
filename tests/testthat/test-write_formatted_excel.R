library(testthat)
library(openxlsx)
library(randomcoloR)

test_that("write_formatted_excel creates a valid Excel file", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    sheet1 = data.frame(
      category = c("a", "b", "a"),
      value = c(10, 20, 30)
    )
  )

  write_formatted_excel(test_list, tmpfile)

  expect_true(file.exists(tmpfile))

  wb <- loadWorkbook(tmpfile)
  expect_equal(length(wb$sheet_names), 1)
  expect_equal(wb$sheet_names, "sheet1")

  df_read <- read.xlsx(tmpfile, sheet = "sheet1")
  expect_equal(df_read$category, c("a", "b", "a"))
  expect_equal(df_read$value, c(10, 20, 30))
})



test_that("Multiple sheets are created correctly", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    hh = data.frame(id = 1:3),
    indv = data.frame(name = c("x", "y"))
  )

  write_formatted_excel(test_list, tmpfile)

  wb <- loadWorkbook(tmpfile)

  expect_equal(wb$sheet_names, c("hh", "indv"))

  df_hh <- read.xlsx(tmpfile, sheet = "hh")
  df_indv <- read.xlsx(tmpfile, sheet = "indv")

  expect_equal(df_hh$id, 1:3)
  expect_equal(df_indv$name, c("x", "y"))
})




test_that("No error when cols_for_color is NULL", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    sheet1 = data.frame(x = 1:5, y = letters[1:5])
  )

  expect_error(
    write_formatted_excel(test_list, tmpfile, cols_for_color = NULL),
    NA
  )
})




test_that("write_formatted_excel returns workbook + style log", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    sheet1 = data.frame(
      group = c("red", "blue", "red"),
      value = c(10, 20, 30)
    )
  )

  out <- write_formatted_excel(
    write_list = test_list,
    output_path = tmpfile,
    cols_for_color = "group"
  )

  # Output structure
  expect_true(is.list(out))
  expect_true("wb" %in% names(out))
  expect_true("style_log" %in% names(out))

  # Workbook saved
  expect_true(file.exists(tmpfile))

  # Style log is a tibble
  expect_s3_class(out$style_log, "tbl_df")

})

test_that("Header style is logged correctly", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    sheet1 = data.frame(a = 1:3, b = 4:6)
  )

  out <- write_formatted_excel(
    write_list = test_list,
    output_path = tmpfile
  )

  log <- out$style_log

  header_row <- log |> dplyr::filter(style_type == "header")
  expect_equal(nrow(header_row), 1)

  # Header must always be row 1
  expect_equal(header_row$rows[[1]], 1)

  # Columns should span all variables
  expect_equal(header_row$cols[[1]], 1:2)
})


test_that("Body style is logged correctly", {

  tmpfile <- tempfile(fileext = ".xlsx")

  df <- data.frame(a = 1:3, b = 4:6)

  out <- write_formatted_excel(
    write_list = list(sheet1 = df),
    output_path = tmpfile
  )

  log <- out$style_log

  body <- log |> dplyr::filter(style_type == "body")
  expect_equal(nrow(body), 1)

  # Body rows must be 2:(n+1)
  expect_equal(body$rows[[1]], 2:4)

  # Columns correct
  expect_equal(body$cols[[1]], 1:2)
})

test_that("Group styling is logged correctly", {

  tmpfile <- tempfile(fileext = ".xlsx")

  df <- data.frame(
    group = c("red", "blue", "red"),
    value = c(1, 2, 3)
  )

  out <- write_formatted_excel(
    write_list = list(sheet1 = df),
    output_path = tmpfile,
    cols_for_color = "group"
  )

  log <- out$style_log

  group_styles <- log |> dplyr::filter(style_type == "group")

  # There should be 2 groups: red, blue
  expect_equal(length(unique(df$group)), nrow(group_styles))

  # Check each group’s rows
  red_row <- group_styles |> dplyr::filter(group_value == "red")
  expect_equal(red_row$rows[[1]], c(2, 4))  # data rows +1 for header

  blue_row <- group_styles |> dplyr::filter(group_value == "blue")
  expect_equal(blue_row$rows[[1]], 3)
})


test_that("Multiple sheets are handled and logged correctly", {

  tmpfile <- tempfile(fileext = ".xlsx")

  test_list <- list(
    hh   = data.frame(id = 1:2),
    indv = data.frame(name = c("x", "y"))
  )

  out <- write_formatted_excel(
    write_list = test_list,
    output_path = tmpfile
  )

  log <- out$style_log

  expect_true(all(c("hh", "indv") %in% log$sheet))

  # Each sheet must have a header and body style logged
  expect_equal(
    log |> dplyr::filter(style_type == "header") |> nrow(),
    2
  )
  expect_equal(
    log |>  dplyr::filter(style_type == "body") |> nrow(),
    2
  )
})


test_that("style_log has correct structure", {

  tmpfile <- tempfile(fileext = ".xlsx")

  df <- data.frame(group = c("a","b"), value = 1:2)

  out <- write_formatted_excel(
    write_list = list(s1 = df),
    output_path = tmpfile,
    cols_for_color = "group"
  )

  log <- out$style_log

  expect_true(all(c("sheet","style_type","group_value","rows","cols") %in% colnames(log)))

  expect_true(is.list(log$rows))
  expect_true(is.list(log$cols))

})





