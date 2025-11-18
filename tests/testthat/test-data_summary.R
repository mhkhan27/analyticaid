

testthat::test_that("Function returns expected structure", {

  df <- data.frame(
    a = c("yes", "no", "yes"),
    b = c(1, 2, 3),
    c_other = c("x", "y", "z"),   # should be excluded
    X_flag = c("x", "x", "y")     # should be excluded
  )

  out <- df_summary(df)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("column", "n_unique", "unique_values", "top_stats") %in% names(out)))
  expect_false("c_other" %in% out$column)
  expect_false("X_flag" %in% out$column)
})

test_that("Numeric columns compute mean/median/sd correctly", {

  df <- data.frame(num = c(10, 20, 30, NA))

  out <- df_summary(df)

  row <- out |> filter(column == "num")

  expect_equal(row$n_unique, 3)
  expect_true(grepl("mean=20.00", row$top_stats))
  expect_true(grepl("median=20.00", row$top_stats))
  expect_true(grepl("sd=10.00", row$top_stats))
  expect_true(grepl("Total N- 3", row$top_stats))
})

test_that("Categorical top-x summary works", {

  df <- data.frame(cat = c("a","a","b","c"))

  out <- df_summary(df, top_reported_col = 2)
  row <- out |> filter(column == "cat")

  # Should show only a and b (top 2)
  expect_true(grepl("a \\(", row$top_stats))
  expect_true(grepl("b \\(", row$top_stats))
  expect_false(grepl("c \\(", row$top_stats))  # excluded

  expect_true(grepl("Total N- 4", row$top_stats))
})

test_that("Logical columns summarize correctly", {

  df <- data.frame(flag = c(TRUE, FALSE, TRUE, TRUE))

  out <- df_summary(df)
  row <- out |> filter(column == "flag")

  expect_true(grepl("TRUE", row$top_stats))
  expect_true(grepl("FALSE", row$top_stats))
  expect_true(grepl("Total N- 4", row$top_stats))
})
