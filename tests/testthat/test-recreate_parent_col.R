test_that("check", {

  test_data <- dplyr::tibble(
    uuid = paste0("uuid_", 1:6),
    gender = rep(c("male", "female"), 3),
    reason = c(
      "xx yy", "xx zy",
      "zy", "xx xz zy",
      NA_character_, "xz"
    ),
    reason.xx = c(0, 1, 0, 1, 0, 0),
    reason.yy = c(1, 0, 0, 0, 1, 0),
    reason.xz = c(0, 0, 0, 1, 0, 1),
    reason.zy = c(0, 1, 1, 1, 0, 0),
    reason_zy = c(NA_character_, "A", "B", "C", NA_character_, NA_character_)
  )


  ## actual
 actual_data <- dplyr::tibble(
    uuid = paste0("uuid_", 1:6),
    gender = rep(c("male", "female"), 3),
    reason = c("yy", "xx zy","zy", "xx xz zy", "yy", "xz"
    ),
    reason.xx = c(0, 1, 0, 1, 0, 0),
    reason.yy = c(1, 0, 0, 0, 1, 0),
    reason.xz = c(0, 0, 0, 1, 0, 1),
    reason.zy = c(0, 1, 1, 1, 0, 0),
    reason_zy = c(NA_character_, "A", "B", "C", NA_character_, NA_character_)
  )

 recreate_parent_column(dataset = test_data)

  testthat::expect_equal (recreate_parent_column(dataset = test_data), actual_data)
})
