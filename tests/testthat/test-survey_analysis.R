library(testthat)

################################ START::OLD TESTS ###############################################




# tests -------------------------------------------------------------------


testthat::test_that("check survey collapse functions", {
  set.seed(123)
  weight_df <- data.frame(
    neighbourhood = unique(analyticaid::clean_data$neighbourhood),
    weight = runif(length(unique(analyticaid::clean_data$neighbourhood)), min = 0.4, max = 1.2)
  )

  df_i <- analyticaid::clean_data |>
    dplyr::filter(neighbourhood != "al_senaa_al_shamaliya") |>
    dplyr::left_join(weight_df)

  dfsvy_w <- srvyr::as_survey(df_i, strata = "neighbourhood", weights = "weight")
  dfsvy <- srvyr::as_survey(df_i)

  # Binary column
  engaged_weighted <- survey_collapse_binary_long(dfsvy_w, "how_engaged_by_office.complaints_box") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0548, engaged_weighted)

  engaged_unweighted <- survey_collapse_binary_long(dfsvy, "how_engaged_by_office.complaints_box") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0555, engaged_unweighted)



  ### add test for weighted n

  ### add test for unweighted_n


  ## add test for group

  engaged_weighted_group <- survey_collapse_binary_long(dfsvy_w, "how_engaged_by_office.complaints_box",disag = "neighbourhood") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0909, engaged_weighted_group[1])




  # Numeric column
  age_weighted <- survey_collapse_binary_long(dfsvy_w, "age_hoh") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(55.4431, age_weighted)

  age_unweighted <- survey_collapse_binary_long(dfsvy, "age_hoh") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(53.8571, age_unweighted)



  ### add test for weighted n

  ### add test for unweighted_n


  ## add test for group

  age_weighted_group <- survey_collapse_binary_long(dfsvy_w, "age_hoh",disag = "neighbourhood")

  engaged_weighted_group <- survey_collapse_binary_long(dfsvy_w,
                                                        "how_engaged_by_office.complaints_box",
                                                        disag = "neighbourhood") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0548, engaged_weighted)




  # Categorical column
  gender_weighted <- survey_collapse_categorical_long(dfsvy_w, "gender_hoh") |>
    dplyr::filter(variable_val == "male") |> dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.9451, gender_weighted)

  gender_unweighted <- survey_collapse_categorical_long(dfsvy, "gender_hoh") |>
    dplyr::filter(variable_val == "male") |> dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.9428, gender_unweighted)


  #### Survey analysis

  # Binary column
  engaged_weighted <- survey_analysis(df_i,weights = T,
                                      strata = "neighbourhood", weight_column = "weight",
                                      vars_to_analyze  =  "how_engaged_by_office.complaints_box") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0548, engaged_weighted)

  engaged_unweighted <- survey_analysis(df_i,weights = F,
                                        vars_to_analyze  =  "how_engaged_by_office.complaints_box") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.0555, engaged_unweighted)

  # Numeric column
  age_weighted <- survey_analysis(df_i,weights = T,
                                  strata = "neighbourhood",
                                  weight_column = "weight",vars_to_analyze = "age_hoh" )|>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(55.4431, age_weighted)

  age_unweighted <-survey_analysis(df_i,weights = F,
                                   vars_to_analyze = "age_hoh" )|>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(53.8571, age_unweighted)

  # Categorical column
  gender_weighted <- survey_analysis(df_i,weights = T,
                                     strata = "neighbourhood",
                                     weight_column = "weight",
                                     vars_to_analyze  =  "gender_hoh") |>
    dplyr::filter(choice == "male") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.9451, gender_weighted)

  gender_unweighted <-survey_analysis(df_i,weights = F,
                                      vars_to_analyze  =  "gender_hoh") |>
    dplyr::filter(choice == "male") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(0.9428, gender_unweighted)

})

################################ END::OLD TESTS ###############################################

################################ START::NEW TESTS ###############################################

# creating moc file -------------------------------------------------------


# ---- minimal mock survey data ----
set.seed(42)

n <- 200
toy_raw <- tibble::tibble(
  w      = runif(n, 0.5, 2),                # weights
  strata = sample(c("A","B"), n, TRUE),
  sex    = sample(c("F","M"), n, TRUE),
  # categorical variable
  channel = factor(sample(c("tv","radio","web"), n, TRUE)),
  # logical variable
  smoke = sample(c(TRUE, FALSE), n, TRUE, prob = c(0.3, 0.7)),
  # numeric variable
  bmi   = rnorm(n, 23, 3),
  # select-multiple style children (booleans)
  how_engaged_by_office = NA_character_,
  `how_engaged_by_office.face_to_face_home`   = sample(c(TRUE,FALSE), n, TRUE, prob = c(0.4, 0.6)),
  `how_engaged_by_office.email`               = sample(c(TRUE,FALSE), n, TRUE, prob = c(0.2, 0.8)),
  `how_engaged_by_office.twitter`             = sample(c(TRUE,FALSE), n, TRUE, prob = c(0.1, 0.9))
)

# srvyr design
toy_svy <- srvyr::as_survey(toy_raw, strata = strata, weights = w)

# mapping for select-multiple parent/child
sm_map <- tibble::tibble(
  sm_parent = "how_engaged_by_office",
  sm_child = c(
    "how_engaged_by_office.face_to_face_home",
    "how_engaged_by_office.email",
    "how_engaged_by_office.twitter"
  )
)

# Provide auto_sm_parent_child used by survey_analysis()
# (Parent is the substring before the first dot)
auto_sm_parent_child <- function(df_variables) {
  nm <- names(df_variables)
  has_dot <- grepl("\\.", nm)
  tibble::tibble(
    sm_parent = sub("\\..*$", "", nm[has_dot]),
    sm_child  = nm[has_dot]
  ) |>
    dplyr::distinct()
}





test_that("mutate_key_pair adds columns with provided names/values", {
  df <- tibble::tibble(a = 1:3)
  out <- mutate_key_pair(df,
                         names  = c("subset_1_name","subset_1_val"),
                         values = c("sex","F"))
  expect_true(all(c("subset_1_name","subset_1_val") %in% names(out)))
  expect_equal(unique(out$subset_1_name), "sex")
  expect_equal(unique(out$subset_1_val),  "F")
})


test_that("get_na_response_rates returns counts and percents", {
  d <- data.frame(x = c(1, NA, 2), y = c(NA, NA, 3))
  res <- get_na_response_rates(d)
  expect_true(all(c("question","num_non_response","perc_non_response") %in% names(res)))
  expect_equal(res$num_non_response[res$question == "x"], 1)
  expect_equal(res$num_non_response[res$question == "y"], 2)
})

test_that("survey_collapse_binary_long works for logical (weighted & unweighted)", {
  out <- survey_collapse_binary_long(
    df   = toy_svy,
    x    = "smoke",
    disag = "sex",
    na_val = NA_real_,
    sm_sep = "/"
  )

  expect_true(all(c("stat","weighted_n","unweighted_n","variable","variable_val") %in% names(out)))
  # should have one row per sex*level of grouping (stat is proportion of TRUE)
  expect_true(all(out$unweighted_n >= 0))
  # weighted_n is numeric and positive
  expect_true(all(is.finite(out$weighted_n)))
})

test_that("survey_collapse_binary_long works for numeric (adds medians)", {
  out <- survey_collapse_binary_long(
    df   = toy_svy,
    x    = "bmi",
    disag = NULL,
    na_val = NA_real_,
    sm_sep = "/"
  )
  expect_true(all(c("stat","weighted_n","unweighted_n","variable","variable_val") %in% names(out)))
  # median_unweighted should exist because you added it
  expect_true("median_unweighted" %in% names(out))
  expect_true(is.numeric(out$median_unweighted))
})

test_that("survey_collapse_categorical_long returns stat + weighted/unweighted counts", {
  out <- survey_collapse_categorical_long(
    df   = toy_svy,
    x    = "channel",
    disag = "sex",
    na_val = NA_character_
  )

  expect_true(all(c("stat","weighted_n","unweighted_n","variable","variable_val") %in% names(out)))
  # proportions bounded [0,1]
  expect_true(all(out$stat >= 0 & out$stat <= 1))
  # within each sex group, sum of proportions across channel ~ 1
  sums <- out |>
    dplyr::group_by(subset_1_val) |>
    dplyr::summarise(s = sum(stat), .groups = "drop")
  expect_true(all(abs(sums$s - 1) < 1e-6))
})

test_that("survey_analysis runs end-to-end and maps SM child to parent", {
  # analyze SM children + others
  vars <- c("smoke", "bmi",
            "how_engaged_by_office.face_to_face_home",
            "how_engaged_by_office.email")

  out_no_disag <- survey_analysis(
    df = toy_raw,                # note: survey_analysis creates the design internally
    weights = F,
    weight_column = "w",
    strata = "strata",
    vars_to_analyze = vars,
    disag = NULL,
    na_val = NA
  )

  expect_true(all(c("main_variable","choice","analysis_type","key_index") %in% names(out_no_disag)))
  # SM children should be mapped to parent
  expect_true("how_engaged_by_office" %in% out_no_disag$main_variable)

  # with disaggregation
  out_disag <- survey_analysis(
    df = toy_raw ,
    weights = TRUE,
    weight_column = "w",
    strata = "strata",
    vars_to_analyze = vars,
    disag = c("sex"),
    na_val = NA
  )

  expect_true(all(c("response_count","key_index") %in% names(out_disag)))
  # check that subset columns show up
  expect_true(all(c("subset_1_name","subset_1_val") %in% names(out_disag)))
})



################################ END::NEW TESTS ###############################################


