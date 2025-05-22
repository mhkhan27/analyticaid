test_that("check auto parents", {
  expect_equal(length(auto_detect_sm_parents(clean_data)),19)
})

test_that("check auto child", {
  expect_equal(nrow(auto_sm_parent_child(clean_data)) ,188)
})
