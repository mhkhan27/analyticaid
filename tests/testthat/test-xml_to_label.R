library(dplyr)
library(testthat)

test_that("create_lookup_table builds expected xml->label map and uniqueness", {
  # survey has a column literally named 'label' to match your function's select(label = label)
  survey <- data.frame(
    type  = c("text", "integer", "select_one colors", "select_multiple fruits"),
    name  = c("name_txt", "age_int", "fav_color", "eat_fruits"),
    label = c("Your name", "Your age", "Favorite color", "Fruits you eat"),
    stringsAsFactors = FALSE
  )

  choices <- data.frame(
    list_name = c("colors","colors","fruits","fruits","fruits"),
    name      = c("red","blue","apple","banana","orange"),
    label     = c("Red","Blue","Apple","Banana","Orange"),
    stringsAsFactors = FALSE
  )

  lut <- create_lookup_table(survey, choices, label = "label")

  # Should contain mappings for text/integer/select_one parents
  expect_true(all(c("name_txt","age_int","fav_color","eat_fruits") %in% lut$xml_label))

  # Should contain expanded select_multiple child columns
  expect_true(all(
    c("eat_fruits.apple", "eat_fruits.banana", "eat_fruits.orange") %in% lut$xml_label
  ))

  # Uniqueness of target labels
  expect_equal(anyDuplicated(lut$col_name_lable), 0)
})


test_that("rename_xml_to_label_col renames matching columns and skips missing", {
  survey <- data.frame(
    type  = c("text", "select_one colors"),
    name  = c("name_txt", "fav_color"),
    label = c("Your name", "Favorite color"),
    stringsAsFactors = FALSE
  )

  choices <- data.frame(
    list_name = c("colors","colors"),
    name      = c("red","blue"),
    label     = c("Red","Blue"),
    stringsAsFactors = FALSE
  )

  df <- data.frame(
    name_txt = c("Alice","Bob"),
    # this column doesn't exist in df: fav_color -> ensure skip is silent
    age_int  = c(30, 28),
    stringsAsFactors = FALSE
  )

  out <- rename_xml_to_label_col(df, survey, choices, label = "label")

  # name_txt should be renamed to 'Your name'
  expect_true("Your name" %in% names(out))
  expect_false("name_txt" %in% names(out))

  # age_int has no mapping in survey; remains unchanged
  expect_true("age_int" %in% names(out))
})


test_that("xml_to_label_only_concat converts select-multiple ids into labels", {
  survey <- data.frame(
    type  = c("select_multiple fruits"),
    name  = c("eat_fruits"),
    label = c("Fruits you eat"),
    stringsAsFactors = FALSE
  )

  choice <- data.frame(
    list_name = c("fruits","fruits","fruits"),
    name      = c("apple","banana","orange"),
    label     = c("Apple","Banana","Orange"),
    stringsAsFactors = FALSE
  )

  # encoded SM values as space-separated ids
  df <- data.frame(
    eat_fruits = c("apple banana", "orange", NA_character_, ""),
    stringsAsFactors = FALSE
  )

  out <- xml_to_label_only_concat(df, survey, choice, label = "label")

  expect_equal(out$eat_fruits[1], "Apple, Banana")
  expect_equal(out$eat_fruits[2], "Orange")
  expect_true(is.na(out$eat_fruits[3]))
  expect_true(is.na(out$eat_fruits[4]))
})


test_that("xml_to_lable maps select-one ids to labels (no rename)", {
  survey <- data.frame(
    type  = c("select_one colors", "select_multiple fruits"),
    name  = c("fav_color", "eat_fruits"),
    label = c("Favorite color", "Fruits you eat"),
    stringsAsFactors = FALSE
  )

  choice <- data.frame(
    list_name = c("colors","colors","fruits","fruits"),
    name      = c("red","blue","apple","banana"),
    label     = c("Red","Blue","Apple","Banana"),
    stringsAsFactors = FALSE
  )

  df <- data.frame(
    fav_color = c("red","blue"),
    eat_fruits = c("apple banana", "banana"),
    stringsAsFactors = FALSE
  )

  out <- xml_to_lable(df, survey, choice, label = "label", change_col_name = FALSE)

  # select-one values should be labels now
  expect_equal(out$fav_color, c("Red","Blue"))

  # select-multiple should be concatenated to labels
  expect_equal(out$eat_fruits, c("Apple, Banana", "Banana"))

  # column names unchanged when change_col_name = FALSE
  expect_true(all(c("fav_color","eat_fruits") %in% names(out)))
})

test_that("xml_to_lable optionally renames columns to labels", {
  survey <- data.frame(
    type  = c("select_one colors"),
    name  = c("fav_color"),
    label = c("Favorite color"),
    stringsAsFactors = FALSE
  )

  choice <- data.frame(
    list_name = c("colors","colors"),
    name      = c("red","blue"),
    label     = c("Red","Blue"),
    stringsAsFactors = FALSE
  )

  df <- data.frame(fav_color = c("red","blue"), stringsAsFactors = FALSE)

  out <- xml_to_lable(df, survey, choice, label = "label", change_col_name = TRUE)

  # column should be renamed
  expect_true("Favorite color" %in% names(out))
  expect_false("fav_color" %in% names(out))
  # values are labels
  expect_equal(out[["Favorite color"]], c("Red","Blue"))
})


test_that("missing columns in data are ignored without error", {
  survey <- data.frame(
    type  = c("text", "select_one colors"),
    name  = c("exists_col", "missing_in_df"),
    label = c("Exists", "Missing col"),
    stringsAsFactors = FALSE
  )

  choices <- data.frame(
    list_name = "colors",
    name      = "red",
    label     = "Red",
    stringsAsFactors = FALSE
  )

  df <- data.frame(exists_col = "x", stringsAsFactors = FALSE)

  # Should not error even though 'missing_in_df' isn't present
  out <- rename_xml_to_label_col(df, survey, choices, label = "label")
  expect_true("Exists" %in% names(out))
  expect_false("missing_in_df" %in% names(out))
})



