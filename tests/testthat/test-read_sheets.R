library(testthat)
library(openxlsx)
library(dplyr)

testthat::test_that("read_sheets reads a sheet by name", {

  # Create temp Excel file
  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  addWorksheet(wb, "indv")

  writeData(wb, "hh", tibble(a = 1:3, b = c("x","y","z")))
  writeData(wb, "indv", tibble(id = c(10,20)))
  saveWorkbook(wb, tmp, overwrite = TRUE)

  df <- read_sheets(tmp, sheets = "hh")

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("a", "b"))
  expect_equal(nrow(df), 3)
})

test_that("read_sheets reads a sheet by index", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  addWorksheet(wb, "indv")

  writeData(wb, "hh", tibble(a = 1:3))
  writeData(wb, "indv", tibble(id = c(5,6)))
  saveWorkbook(wb, tmp, overwrite = TRUE)

  df <- read_sheets(tmp, sheets = 1)   # should load "hh"

  expect_equal(nrow(df), 3)
  expect_equal(names(df), "a")
})

test_that("read_sheets handles multiple sheets and returns list", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  addWorksheet(wb, "indv")

  writeData(wb, "hh", tibble(a = 4))
  writeData(wb, "indv", tibble(b = 2))
  saveWorkbook(wb, tmp, overwrite = TRUE)

  out <- read_sheets(tmp, sheets = c("hh", "indv"))

  expect_type(out, "list")
  expect_equal(names(out), c("hh", "indv"))
  expect_equal(out$hh$a, 4)
  expect_equal(out$indv$b, 2)
})

testthat::test_that("Invalid sheet name throws error", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_error(
    read_sheets(tmp, sheets = "missing"),
    "Sheet\\(s\\) not found"
  )
})

test_that("Invalid sheet index throws error", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_error(
    read_sheets(tmp, sheets = 5),
    "index out of range"
  )
})

test_that("Columns starting with '_' get renamed", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  writeData(wb, "hh", tibble(`_id` = 1:3, name = c("a","b","c")))
  saveWorkbook(wb, tmp, overwrite = TRUE)

  df <- read_sheets(tmp, sheets = "hh", data_type_fix = FALSE)

  expect_true("X_id" %in% names(df))
  expect_false("_id" %in% names(df))
})

test_that("NA-only columns are removed", {

  tmp <- tempfile(fileext = ".xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "hh")
  writeData(wb, "hh", tibble(a = 1:3, b = NA, c = c(NA, NA, NA)))
  saveWorkbook(wb, tmp, overwrite = TRUE)

  df <- read_sheets(tmp, sheets = "hh", remove_all_NA_col = TRUE)
  df_2 <- read_sheets(tmp, sheets = "hh", remove_all_NA_col = F)

  expect_false("b" %in% names(df))   # because all NA after fix
  expect_true("b" %in% names(df_2))   # because all NA after fix

  expect_false("c" %in% names(df))
  expect_true("a" %in% names(df))
})
