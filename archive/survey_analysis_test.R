rm(list=ls())

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(srvyr)
library(analyticaid)
# source("R/survey_analysis.R")


### get na respose rate

na_rate <- data.frame(
  a = c("a","2","Aa",NA_character_),
  b= c("c e","r f", "e r", "y e"),
  b.c = c(1,0,0,0),
  b.e=c(1,0,0,1),
  b.r =c(0,1,1,0),
  b.y =c(NA_real_,0,NA_real_,1),
  age = c(13,13,NA_integer_,12)
)

function_get_na <- get_na_response_rates(na_rate)
actual <- tibble(question = c("a", "b", "b.c", "b.e", "b.r", "b.y", "age"),
       num_non_response = c(1L, 0L, 0L, 0L, 0L, 2L, 1L),
       perc_non_response = c(25, 0, 0, 0, 0, 50, 25))

testthat::expect_equal(
  function_get_na,actual
)


# survey design --------------------------------------------------


df <- analyticaid::clean_data |> mutate(
  strata = neighbourhood
)

sample_frame <- df$neighbourhood |> tibble::as_tibble() |> reframe(
  count =n(),.by = "value"
) |> mutate(weight = count/sum(count)) |>
  select(strata = value,
         weight)

df_f <-df |> left_join(sample_frame,by = "strata")

dfsvy <- as_survey(df_f)
dfsvy_w <- as_survey(df_f |> filter(strata !="al_senaa_al_shamaliya"),weight = "weight",strata ="strata")




# binary ------------------------------------------------------------------

## check  binary variable + unweighted + disagg
actual <- survey_collapse_numeric_long(df = dfsvy,reporting_col = "unweighted",
                                      x = "how_engaged_by_office.face_to_face_home",
                                      disag = "gender_hoh")
expected <- structure(list(variable = c("how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home"),
                           variable_val = c("how_engaged_by_office.face_to_face_home",
                                            "how_engaged_by_office.face_to_face_home"),
                           subset_1_name = c("gender_hoh","gender_hoh"),
                           subset_1_val = c("female", "male"),
                           stat = c(0.515151515151515,0.730275229357791),
                           count_weighted = c(17, 398),
                           count_by_subset_weighted = c(33,545),
                           count_unweighted = c(17L, 398L),
                           count_by_subset_unweighted = c(33L,545L),
                           count_by_question_weighted = c(578, 578),
                           count_by_question_unweighted = c(578L,578L)),
                      row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))


testthat::expect_equal(
  expected,actual
)

## check  binary variable + weighted +disagg
actual <- survey_collapse_numeric_long(df = dfsvy_w,
                                      x = "how_engaged_by_office.face_to_face_home",
                                      disag = "gender_hoh")

expected <- structure(list(variable = c("how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home"),
                           variable_val = c("how_engaged_by_office.face_to_face_home",
                                            "how_engaged_by_office.face_to_face_home"),
                           subset_1_name = c("gender_hoh","gender_hoh"),
                           subset_1_val = c("female", "male"),
                           stat = c(0.475128644939966, 0.730641478098401),
                           count_weighted = c(0.47923875432526, 12.1782006920415 ),
                           count_by_subset_weighted = c(1.00865051903114, 16.6678200692041 ),
                           count_unweighted = c(17L, 397L),
                           count_by_subset_unweighted = c(33L, 544L),
                           count_by_question_weighted = c(17.6764705882353, 17.6764705882353 ),
                           count_by_question_unweighted = c(577L, 577L)),
                      row.names = c(NA,-2L),
                      class = c("tbl_df", "tbl", "data.frame"))


testthat::expect_equal(
  expected,actual
)
## check  binary variable + unweighted + without disagg
actual <- survey_collapse_numeric_long(df = dfsvy,
                                      x = "how_engaged_by_office.face_to_face_home")
expected<- structure(list(variable = "how_engaged_by_office.face_to_face_home",
                          variable_val = "how_engaged_by_office.face_to_face_home",
                          stat = 0.717993079584782, count_weighted = 415, count_by_subset_weighted = 578,
                          count_unweighted = 415L, count_by_subset_unweighted = 578L,
                          count_by_question_weighted = 578, count_by_question_unweighted = 578L), row.names = c(NA,
                                                                                                                -1L), class = c("tbl_df", "tbl", "data.frame"))

testthat::expect_equal(
  expected,actual
)
## check  binary variable + weighted + without disagg
actual <- survey_collapse_numeric_long(df = dfsvy_w,
                                      x = "how_engaged_by_office.face_to_face_home")

expected <- structure(list(variable = "how_engaged_by_office.face_to_face_home",
                           variable_val = "how_engaged_by_office.face_to_face_home",
                           stat = 0.716061466183812, count_weighted = 12.6574394463668,
                           count_by_subset_weighted = 17.6764705882353, count_unweighted = 414L,
                           count_by_subset_unweighted = 577L, count_by_question_weighted = 17.6764705882353,
                           count_by_question_unweighted = 577L), row.names = c(NA, -1L
                           ), class = c("tbl_df", "tbl", "data.frame"))
testthat::expect_equal(
  expected,actual
)
## check  binary variable + unweighted + multi-disagg
actual <- survey_collapse_numeric_long(df = dfsvy,
                                      x = "how_engaged_by_office.face_to_face_home",
                                      disag =  c("gender_hoh","strata")) [c(1,10,12),]

expected <- structure(list(variable = c("how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home"
), variable_val = c("how_engaged_by_office.face_to_face_home",
                    "how_engaged_by_office.face_to_face_home",
                    "how_engaged_by_office.face_to_face_home"
), subset_1_name = c("gender_hoh", "gender_hoh", "gender_hoh"
), subset_2_name = c("strata", "strata", "strata"),
subset_1_val = c("female","female", "female"),
subset_2_val = c("al_askary1", "al_qadissiya_2", "al_rabea"),
stat = c(0.333333333333333, 0.5, 0.666666666666667 ),
count_weighted = c(1, 1, 2),
count_by_subset_weighted = c(3,2, 3),
count_unweighted = c(1L, 1L, 2L),
count_by_subset_unweighted = c(3L, 2L, 3L),
count_by_question_weighted = c(578, 578, 578),
count_by_question_unweighted = c(578L, 578L, 578L)),
row.names = c(NA, -3L), class = c("tbl_df", "tbl",  "data.frame"))

testthat::expect_equal(
  expected,actual
)
## check  binary variable + weighted + multi-disagg
actual <- survey_collapse_numeric_long(df = dfsvy_w,
                                      x = "how_engaged_by_office.face_to_face_home",
                                      disag = c("gender_hoh","strata") )[c(10,20,24),]

expected <- structure(list(variable = c("how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home",
                                        "how_engaged_by_office.face_to_face_home"
), variable_val = c("how_engaged_by_office.face_to_face_home",
                    "how_engaged_by_office.face_to_face_home",
                    "how_engaged_by_office.face_to_face_home"
), subset_1_name = c("gender_hoh", "gender_hoh", "gender_hoh"
), subset_2_name = c("strata", "strata", "strata"),
subset_1_val = c("female","male", "male"),
subset_2_val = c("al_qadissiya_2", "al_askary2", "al_jazeera2"),
stat = c(0.5, 0.578947368421053, 0.933333333333333 ),
count_weighted = c(0.041522491349481, 0.36159169550173, 0.363321799307959),
count_by_subset_weighted = c(0.0830449826989619, 0.624567474048443,   0.389273356401384),
count_unweighted = c(1L, 11L, 14L), count_by_subset_unweighted = c(2L, 19L, 15L),
count_by_question_weighted = c(17.6764705882353, 17.6764705882353,17.6764705882353),
count_by_question_unweighted = c(577L, 577L, 577L)), row.names = c(NA, -3L),
class = c("tbl_df", "tbl", "data.frame"))


testthat::expect_equal(
  expected,actual
)


######### need to add test for numeric ##########################################

## check  numeric variable + unweighted + without_disagg

actual <- survey_collapse_numeric_long(df = dfsvy,
                                      x = "age_respondent_r")

expected <- structure(list(variable = "age_respondent_r", variable_val = "age_respondent_r",
                           stat = 45.3702422145329, count_weighted = 578, count_by_subset_weighted = 578,
                           count_unweighted = 578L, count_by_subset_unweighted = 578L,
                           median_weighted = 45, median_unweighted = 45, count_by_question_weighted = 578,
                           count_by_question_unweighted = 578L), row.names = c(NA, -1L
                           ), class = c("tbl_df", "tbl", "data.frame"))

testthat::expect_equal(
  expected,actual
)

## check  numeric variable + weighted + without_disagg
actual <- survey_collapse_numeric_long(df = dfsvy_w,
                                      x = "age_respondent_r")

expected <- structure(list(variable = "age_respondent_r", variable_val = "age_respondent_r",
                           stat = 45.4046197513948, count_weighted = 17.6764705882353,
                           count_by_subset_weighted = 17.6764705882353, count_unweighted = 577L,
                           count_by_subset_unweighted = 577L, median_weighted = 45,
                           median_unweighted = 45L, count_by_question_weighted = 17.6764705882353,
                           count_by_question_unweighted = 577L), row.names = c(NA, -1L
                           ), class = c("tbl_df", "tbl", "data.frame"))

testthat::expect_equal(
  expected,actual
)

## check  numeric variable + unweighted + disagg

actual <- survey_collapse_numeric_long(df = dfsvy, x = "age_respondent_r",disag = "gender_hoh")

expected <- structure(list(variable = c("age_respondent_r", "age_respondent_r"
), variable_val = c("age_respondent_r", "age_respondent_r"),
subset_1_name = c("gender_hoh", "gender_hoh"),
subset_1_val = c("female", "male"),
stat = c(52.1515151515152, 44.9596330275229),
count_weighted = c(33, 545),
count_by_subset_weighted = c(33, 545),
count_unweighted = c(33L,545L),
count_by_subset_unweighted = c(33L, 545L),
median_weighted = c(55,  45),
median_unweighted = c(55L, 45L),
count_by_question_weighted = c(578,578),
count_by_question_unweighted = c(578L, 578L)),
row.names = c(NA,-2L), class = c("tbl_df", "tbl", "data.frame"))
testthat::expect_equal(
  expected,actual
)
testthat::expect_equal(
  expected,actual
)
## check  numeric variable + weighted + disagg

actual <- survey_collapse_numeric_long(df = dfsvy_w, x = "age_respondent_r",disag = "gender_hoh")

expected <- structure(list(variable = c("age_respondent_r", "age_respondent_r"
), variable_val = c("age_respondent_r", "age_respondent_r"),
subset_1_name = c("gender_hoh", "gender_hoh"),
subset_1_val = c("female",  "male"),
stat = c(52.9142367066895, 44.9501764583766),
count_weighted = c(1.00865051903114,  16.6678200692041),
count_by_subset_weighted = c(1.00865051903114, 16.6678200692041),
count_unweighted = c(33L, 544L),
count_by_subset_unweighted = c(33L, 544L),
median_weighted = c(56, 45),
median_unweighted = c(55, 45),
count_by_question_weighted = c(17.6764705882353, 17.6764705882353 ),
count_by_question_unweighted = c(577L, 577L)),
row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))


testthat::expect_equal(
  expected,actual
)

## check  numeric variable + weighted + multi-disagg
actual <- survey_collapse_numeric_long(df = dfsvy_w, x = "age_respondent_r",disag = c("gender_hoh","strata"))[c(12,8),]

exopected <- structure(list(variable = c("age_respondent_r", "age_respondent_r"
), variable_val = c("age_respondent_r", "age_respondent_r"),
subset_1_name = c("gender_hoh", "gender_hoh"),
subset_2_name = c("strata",  "strata"),
subset_1_val = c("female", "female"),
subset_2_val = c("al_rabea", "al_noor"),
stat = c(37.6666666666667, 40),
count_weighted = c(0.0674740484429066,   0.0397923875432526),
count_by_subset_weighted = c(0.0674740484429066,   0.0397923875432526),
count_unweighted = c(3L, 1L),
count_by_subset_unweighted = c(3L, 1L),
median_weighted = c(37, 40),
median_unweighted = c(37,  40),
count_by_question_weighted = c(17.6764705882353, 17.6764705882353 ),
count_by_question_unweighted = c(577L, 577L)),
row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))

testthat::expect_equal(
  expected,actual
)
#### catagorical #########

##catagorical+unweighted+no_disa
actual <- survey_collapse_categorical_long(df = dfsvy,
                                           x = "shelter_occupation" )[c(1,3),]
expected <- structure(list(variable = c("shelter_occupation", "shelter_occupation"
), variable_val = c("granted_family", "granted_others"),
stat = c(0.0322003577818102, 0.0143112701252256),
count_weighted = c(18, 8), count_unweighted = c(18L, 8L),
count_by_subset_weighted = c(559, 559),
count_by_subset_unweighted = c(559L,  559L),
count_by_question_weighted = c(559, 559),
count_by_question_unweighted = c(559L, 559L)),
row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))

testthat::expect_equal(
  expected,actual
)

##catagorical+weighted+no_disa
actual <- survey_collapse_categorical_long(df = dfsvy_w,
                                           x = "shelter_occupation" )[c(5,10),]

expected <-structure(list(variable = c("shelter_occupation", NA),
                          variable_val = c("owned",  NA), stat = c(0.76834605597816, NA),
                          count_weighted = c(13.060553633218, NA), count_unweighted = c(425L, NA),
                          count_by_subset_weighted = c(16.9982698961938,  NA),
                          count_by_subset_unweighted = c(558L, NA),
                          count_by_question_weighted = c(16.9982698961938,  NA),
                          count_by_question_unweighted = c(558L, NA)), row.names = c(NA,-2L),
                     class = c("tbl_df", "tbl", "data.frame"))


testthat::expect_equal(
  expected,actual
)

##catagorical+unweighted+disag
actual <- survey_collapse_categorical_long(df = dfsvy,reporting_col = "we",
                                      x = "shelter_occupation",
                                      disag =  c("gender_hoh"))[c(12,8),]
expected <- structure(list(variable = c("shelter_occupation", "shelter_occupation"
), variable_val = c("squatted", "granted_others"),
subset_1_name = c("gender_hoh", "gender_hoh"),
subset_1_val = c("male", "male"),
stat = c(0.0246212121284648, 0.0151515151515165),
count_weighted = c(13, 8), count_unweighted = c(13L, 8L),
count_by_subset_weighted = c(528, 528),
count_by_subset_unweighted = c(528L, 528L),
count_by_question_weighted = c(559, 559),
count_by_question_unweighted = c(559L, 559L)),
row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame" ))

testthat::expect_equal(
  expected,actual
)

##catagorical+weighted+disag
actual <- survey_collapse_categorical_long(df = dfsvy_w,
                                           x = "shelter_occupation",
                                           disag =  c("gender_hoh"))[c(20,12),]
expected <- structure(list(variable = c(NA, "shelter_occupation"), variable_val = c(NA,
"squatted"), subset_1_name = c(NA, "gender_hoh"), subset_1_val = c(NA,
"male"), stat = c(NA, 0.0247844827619748), count_weighted = c(NA,
0.397923875432526), count_unweighted = c(NA, 12L), count_by_subset_weighted = c(NA,
16.0553633217993), count_by_subset_unweighted = c(NA, 527L),
    count_by_question_weighted = c(NA, 16.9982698961938), count_by_question_unweighted = c(NA,
    558L)), row.names = c(NA, -2L), class = c("tbl_df", "tbl",
"data.frame"))

testthat::expect_equal(
  expected,actual
)

##catagorical+weighted+ multi-disag
actual <- survey_collapse_categorical_long(df = dfsvy_w,
                                           x = "shelter_occupation",
                                           disag =  c("gender_hoh","strata"))[c(20,12),]

expected <- structure(list(variable = c("shelter_occupation", "shelter_occupation"
), variable_val = c("owned", "owned"),
subset_1_name = c("gender_hoh", "gender_hoh"),
subset_2_name = c("strata", "strata"),
subset_1_val = c("female", "female"),
subset_2_val = c("al_wahda1", "al_qala_1"),
stat = c(0.999999999944667,  0.5),
count_weighted = c(0.103806228373702, 0.0242214532871972 ),
count_unweighted = c(3L, 1L),
count_by_subset_weighted = c(0.103806228373702,  0.0484429065743945),
count_by_subset_unweighted = 3:2,
count_by_question_weighted = c(16.9982698961938, 16.9982698961938),
count_by_question_unweighted = c(558L, 558L )),
row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame" ))

testthat::expect_equal(
  expected,actual
)


# survey analysis ---------------------------------------------------------

cols_to_ana <-(dfsvy$variables |> names())[130:200]

d <-(df_f |>  filter(strata !="al_senaa_al_shamaliya"))[130:200]


analysis <- survey_analysis(df = df_f |> filter(strata !="al_senaa_al_shamaliya"),
                            weights = T,
                            reporting_col = "oth",
                            weight_column = "weight",
                            strata = "strata",
                            vars_to_analyze =cols_to_ana )

analysis_by_hoh_gender <- survey_analysis(df = df_f |> filter(strata !="al_senaa_al_shamaliya"),
                            weights = F,disag = "gender_hoh",
                            reporting_col = "weigted",
                            weight_column = "weight",
                            strata = "strata",
                            vars_to_analyze =cols_to_ana )


