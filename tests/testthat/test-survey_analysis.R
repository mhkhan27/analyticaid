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

  # Numeric column
  age_weighted <- survey_collapse_binary_long(dfsvy_w, "age_hoh") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(55.4431, age_weighted)

  age_unweighted <- survey_collapse_binary_long(dfsvy, "age_hoh") |>
    dplyr::pull(stat) |> round(4)
  testthat::expect_equal(53.8571, age_unweighted)

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
