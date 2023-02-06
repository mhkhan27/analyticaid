
# illuminate <img src='man/figures/illuminate.png' align="right" height="80.5" />

<!-- <!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr) -->
<!-- [![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr?branch=main) -->
<!-- <!-- badges: end -->

## Overview

illuminate is designed for making the data analysis easy and less time
consuming. The package is based on tidyr, dplyr,srvyr packages. Most
common functions are-

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mhkhan27/illuminate")
```

## ***1. Read and write file***

- `read_sheets()` read all sheet in an excel file and makes sure that
  data stores in appropriate data type.
- `write_formatted_excel()` write formatted excel.

## ***2. Data cleaning***

- `outlier_checks()`
- `others_checks()`
- `survey_duration_from_audit()`
- `logical_checks()`

## ***3. Creating cleaning log***

## ***4. Implementing cleaning log***

- `implementin_cleaning_log()` Once you have raw data and cleaning
  log,you can use `implementin_cleaning_log()` function to get the clean
  data. Note that you need at least three column in your cleaning log to
  run the function which are - 1. uuid, 2. question name which must be
  matched with the data set and 3- a column specifying the what kind of
  change you are expecting. It is recommend to run the
  check_cleaning_log() beforehand which checks the cleaning log
  accuracy. See the documentation with `?check_cleaning_log()` and
  `?implementing_cleaning_log()` for more details.

#### EXAMPLE:: Implementing cleaning log

##### Step 1:: Call libraries and read data

``` r
library(illuminate)
library(tidyverse)

cleaning_log <- read.csv("data/01_implementing_cleaning_log/cleaning_log.csv")
data <- read.csv("data/01_implementing_cleaning_log/data.csv")
```

##### Step 2:: Check the cleaning log

`check_cleaning_log()` will flag any potential issue(s) within the
cleaning log. If there is no issue then there will be a messege saying
**no issues in cleaning log found**

``` r
check_cleaning_log(df = data,
                   df_uuid = "X_uuid",
                   cl = cleaning_log,
                   cl_change_type_col = "changed",
                   change_type_for_change_response = "Changed",
                   cl_change_col = "question.name",cl_uuid = "X_uuid",cl_new_val = "new.value"
)
```

##### Step 3:: Getting the clean data from cleaning log

`implement_cleaning_log()` will apply the cleaning log on raw data and
will provide the clean data.

``` r
clean_data <- implement_cleaning_log(df = data,
                       df_uuid = "X_uuid",
                       cl = cleaning_log,
                       cl_change_type_col = "changed",
                       change_type_for_change_response = "Changed",
                       change_type_for_blank_response = NA,
                       change_type_for_no_change = c("No_Changes","Confirmed"),
                       change_type_for_deletion = c("NOT_ANS_PELASE_REMOVE", "From must be deleted"),
                       cl_change_col = "question.name",cl_uuid = "X_uuid",cl_new_val = "new.value"
                       )
```

## ***5. Data Analysis***

- `survey_analysis()` calculate the weighted mean/proporation/median and
  unweighted count for all existing variable is the dataset.

### EXAMPLE :: Survey analysis

##### Step 0:: Call libraries

``` r
library(illuminate)
library(tidyverse)
library(purrr)
library(readxl)
library(openxlsx)
library(srvyr)
```

##### Step 1:: Read data

Read data with `read_sheets()`. This will make sure your data is stored
as appropriate data type. It is important to make sure that all the
select multiple are stored as logical, otherwise un weighted count will
be wrong. It is recommended to use the `read_sheets()` to read the data.
use \``?read_sheets()` for more details.

``` r
read_sheets("data/data.xlsx",data_type_fix = T,remove_all_NA_col = T,na_strings = c("NA",""," "))
```

The avobe code will give a dataframe called `data_up`

##### Step OPTIONAL:: Preaparing the data

<span style="color: red;">ONLY APPLICABLE WHEN YOU ARE NOT READING YOUR
DATA WITH `read_sheets()`</span>

``` r
# data_up <- read_excel("data/data.xlsx")
data_up <- fix_data_type(df = data_up)
```

##### Step 2:: Weight calculation

To do the weighted analysis, you will need to calculate the weights. If
your dataset already have the weights column then you can ignore this
part

###### Read sampleframe

``` r
read_sheets("data/sampling_frame.xlsx")
```

This will give a dataframe called `sampling frame`

``` r
weights <- data_up %>% group_by(governorate1) %>% summarise(
  survey_count = n()
) %>% left_join(sampling_frame,by =c("governorate1"="strata.names")) %>% 
  mutate(sample_global=sum(survey_count),
         pop_global=sum(population),
         survey_weight= (population/pop_global)/(survey_count/sample_global)) %>% select(governorate1,survey_weight)
```

###### Add weights to the dataframe

``` r
data_up <- data_up %>% left_join(weights)
```

##### Step 3.1:: Weighted analysis

``` r
overall_analysis <- survey_analysis(df = data_up,weights = T,weight_column = "survey_weight",strata = "governorate1")
```

##### Step 3.2:: Unweighted analysis

``` r
columns_to_analyze <- data_up[20:50] %>% names() 
overall_analysis <- survey_analysis(df = data_up,weights = F,vars_to_analyze = columns_to_analyze )
```

##### Step 3.3:: Weighted and disaggregated by gender analysis

Use `?survey_analysis()` to know about the perameters.

``` r
# dummy code. Please define the name of the columns which you would like to analyze. Default will analyze all the variables exist in the dataset. 

analysis_by_gender <-  survey_analysis(df = data_up,weights = T,weight_column = "survey_weight",vars_to_analyze = columns_to_analyze,
                                       strata = "governorate1",disag = c("va_child_income","gender_speaker"))
```

##### Step 4:: Export with `write_formatted_excel()`

You can use any function to export the analysis however
`write_formatted_excel()` can export formatted file. See the
documentation for more details

``` r
write_list <- list(overall_analysis ="overall_analysis",
                   analysis_by_gender ="analysis_by_gender"
)

write_formatted_excel(write_list,"analysis_output.xlsx",
                      header_front_size = 12,
                      body_front = "Times")
```
