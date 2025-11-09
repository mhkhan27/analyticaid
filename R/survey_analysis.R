#' @name mutate_key_pair
#' @rdname mutate_key_pair
#' @title Mutate columns on based on a list of names and values
#'
#' @description conditionally mutate on columns based
#' on a list of column names and values. This is mostly useful for conditional
#' mutate commands and can currently only mutate uniform columns.
#' It is used inside the survey collapse functions
#'
#' @param df dataframe
#' @param names names of columns to mutate
#' @param values uniform values to mutate

mutate_key_pair<- function(df, names, values){
  df |>
    tibble::add_column(!!!rlang::set_names(as.list(values),nm=names))
}


#' Get NA percentages and frequency for each column in the data set
#' @param data The dataset (data.frame).
#' @return frequency and proportion table of NA rates.
#' @export

get_na_response_rates<-function(data){

  na_count_per_question<-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_percent_per_question <-sapply(data, function(y) ((sum(length(which(is.na(y)))))/nrow(data))*100)
  non_response_df<-data.frame(num_non_response=na_count_per_question,perc_non_response= na_percent_per_question)
  non_response_df1<-non_response_df |>
    tibble::rownames_to_column("question")
  return( non_response_df1 |> as_tibble())
}




# ======================================================
# survey_collapse_numeric_long
# ======================================================
#' Collapse a logical/numeric survey variable to tidy long stats (weighted & unweighted counts)
#'
#' Aggregates a single variable from a \code{srvyr} design into tidy long format,
#' returning the survey-weighted mean (with variance stats), and both
#' \strong{weighted} and \strong{unweighted} counts. If \code{disag} variables
#' are supplied, results are computed by group and annotated with
#' \code{subset_*_name} / \code{subset_*_val} columns (via \code{mutate_key_pair()}).
#' For select-multiple style names, the parent variable is derived by trimming
#' the suffix after \code{sm_sep}.
#'
#' @param df A \code{tbl_svy} object created with \code{srvyr::as_survey()} (or compatible).
#' @param x Character scalar. The name of the variable to collapse.
#'   Can be logical (treated as TRUE/FALSE) or numeric (treated as continuous).
#' @param disag Optional character vector of column names to disaggregate by
#'   (analogous to \code{dplyr::group_by()} before collapsing).
#' @param na_val Replacement value for \code{NA}s before aggregation.
#'   If \code{NA_real_} (default), rows with missing \code{x} are removed.
#'   If a number is provided, missing \code{x} are replaced with that value.
#' @param sm_sep Single-character separator used by select-multiple columns
#'   (default \code{"/"}). The function derives a \code{variable} column by
#'   removing the child suffix after the last occurrence of this separator.
#' @param reporting_col One of "weighted", "both", or "unweighted".
#'
#' @details
#' \itemize{
#'   \item For \emph{logical} \code{x}: \code{stat} is the weighted proportion of TRUE;
#'         \code{weighted_n} is the survey-weighted count of TRUE; \code{unweighted_n}
#'         is the raw count of TRUE (ignoring weights).
#'   \item For \emph{numeric} \code{x}: \code{stat} is the weighted mean;
#'         \code{weighted_n} / \code{unweighted_n} count non-missing values;
#'         \code{median} is the survey-weighted median (with variance stats).
#' }
#'
#' @return A tibble with at least:
#' \describe{
#'   \item{stat}{Survey-weighted mean (with variance columns from \code{vartype="ci"}).}
#'   \item{weighted_n}{Survey-weighted count (TRUE for logical; non-missing for numeric).}
#'   \item{unweighted_n}{Unweighted count (TRUE for logical; non-missing for numeric).}
#'   \item{median}{(Numeric only) Survey-weighted median (with variance columns).}
#'   \item{variable_val}{The original \code{x} name.}
#'   \item{variable}{Parent variable name (child suffix after \code{sm_sep} removed).}
#'   \item{subset_*_name / subset_*_val}{Present when \code{disag} is provided.}
#' }
#'
#' @section Notes:
#' This function expects \code{df} to be a survey design (\code{tbl_svy}) built with
#' the correct weights/strata/PSU.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(srvyr)
#'
#' # toy data
#' set.seed(1)
#' dat <- tibble(
#'   w     = runif(100, 0.5, 2),
#'   sex   = sample(c("F","M"), 100, TRUE),
#'   urban = sample(c("U","R"), 100, TRUE),
#'   smoke = sample(c(TRUE, FALSE), 100, TRUE),
#'   bmi   = rnorm(100, 23, 3)
#' )
#'
#' dsgn <- as_survey(dat, weights = w)
#'
#' # Logical example: weighted proportion/count of smokers, by sex
#' survey_collapse_numeric_long(dsgn, x = "smoke", disag = "sex")
#'
#' # Numeric example: weighted mean/median of BMI, overall
#' survey_collapse_numeric_long(dsgn, x = "bmi")
#' }
#'
#' @seealso \code{\link[srvyr]{survey_mean}}, \code{\link[srvyr]{survey_total}},
#'   \code{\link[srvyr]{survey_median}}
#'
#' @export
#' @importFrom dplyr reframe group_by mutate filter select rename_with any_of across relocate last_col everything
#' @importFrom rlang sym syms .data
#' @importFrom srvyr survey_mean survey_total survey_median unweighted as_survey
#' @importFrom stringr str_detect
#' @importFrom glue glue

survey_collapse_numeric_long <- function(df, x, disag = NULL,
                                         reporting_col= c("both","weighted","unweighted"),
                                         na_val = NA_real_,
                                         sm_sep = "/") {

  reporting_col <- rlang::arg_match(reporting_col)

  old_opt <- getOption("survey.lonely.psu")   # save old setting
  options(survey.lonely.psu = "adjust")  # set new one
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)


  # Handle NA filtering or replacement
  if (is.na(na_val) & !all(!is.na(df$variables[[x]]))) {
    df <- df |>
      dplyr::filter(!is.na(!!rlang::sym(x)))
  }
  if (!is.na(na_val)) {
    df <- df |>
      dplyr::mutate(!!x := ifelse(is.na(!!rlang::sym(x)), na_val, !!rlang::sym(x)))
  }

  # Group by disaggregations (reframe returns one row per group)
  if (!is.null(disag)) {
    df <- df |> dplyr::group_by(!!!rlang::syms(disag))
  }

  # Build result with reframe()
  if (is.logical(df$variables[[x]])) {
    # LOGICAL: counts of TRUE
    res <- df |>
      dplyr::reframe(
        stat         = suppressWarnings(srvyr::survey_mean(!!rlang::sym(x), na.rm = TRUE,vartype = "ci")),
        count_weighted   = suppressWarnings(srvyr::survey_total(!!rlang::sym(x), na.rm = TRUE,vartype = "ci" )),
        count_by_subset_weighted   = suppressWarnings(srvyr::survey_total(as.numeric(!is.na(!!rlang::sym(x))),vartype = "ci")),
        count_unweighted = sum(!!rlang::sym(x) == TRUE, na.rm = TRUE),
        count_by_subset_unweighted = sum(!is.na(!!rlang::sym(x)))
      )
  } else {
    # NUMERIC: counts of non-missing
    res <- df |>
      dplyr::reframe(
        stat         = suppressWarnings(srvyr::survey_mean(!!rlang::sym(x), na.rm = TRUE, vartype = "ci")),

        count_weighted   = suppressWarnings(srvyr::survey_total(!is.na(!!rlang::sym(x)), na.rm = TRUE,vartype = "ci")),
        count_by_subset_weighted  = suppressWarnings(srvyr::survey_total(as.numeric(!is.na(!!rlang::sym(x))), vartype = "ci")),

        count_unweighted = sum(!is.na(!!rlang::sym(x))),
        count_by_subset_unweighted = sum(!is.na(!!rlang::sym(x))),

        median_weighted  = suppressWarnings(srvyr::survey_median(!!rlang::sym(x), na.rm = TRUE, vartype = "ci")),
        median_unweighted = median(!!rlang::sym(x),na.rm = T),
      )
  }

  # Mark which variable was collapsed
  res <- res |> dplyr::mutate(variable_val = x,
                              count_by_question_weighted = sum(res$count_by_subset_weighted,na.rm = T),
                              count_by_question_unweighted = sum(res$count_by_subset_unweighted,na.rm = T),
                              )




   # Disaggregation labels (if provided)
  if (!is.null(disag)) {
    subset_names <- glue::glue("subset_{1:length(disag)}_name")
    subset_vals  <- glue::glue("subset_{1:length(disag)}_val")

    res <- res |>
      dplyr::rename_with(~ glue::glue("subset_{seq_along(.x)}_val"), .cols = dplyr::all_of(disag)) |>
      mutate_key_pair(names = subset_names, values = disag) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(subset_vals), as.character))
  } else {
    subset_names <- character(0)
    subset_vals  <- character(0)
  }

  # Strip select_multiple child suffix → parent variable
  res <- res |>
    dplyr::mutate(
      variable = sub(paste0("\\", sm_sep, "[^", sm_sep, "]*$"), "", variable_val)
    ) |>
    dplyr::select(
      dplyr::any_of(c("variable", "variable_val",
                      as.character(subset_names), as.character(subset_vals))),
      dplyr::everything()) |>
        dplyr::select(-ends_with("_upp"),-ends_with("_low"),-ends_with("_se"))



  if(reporting_col== "weighted"){
    res <- res |> select(-contains("_unweighted"))
    names(res) <- names(res) |> stringr::str_replace_all("_weighted","")
  }
  if(reporting_col== "unweighted"){
    res <- res |> select(-contains("_weighted"))
    names(res) <- names(res) |> stringr::str_replace_all("_unweighted","")
  }

  return(res)

}




# ======================================================
# survey_collapse_categorical_long
# ======================================================



#' @title Collapse categorical data into tidy long format
#' @description Aggregates categorical survey data using [srvyr::survey_mean()].
#' @inheritParams survey_collapse_numeric_long
#' @return A long-format data frame with proportions and counts
#' @export
#'

survey_collapse_categorical_long <- function(df, x,
                                             reporting_col= c("both","weighted","unweighted"),
                                             disag = NULL,
                                             na_val = NA_character_) {



  reporting_col <- rlang::arg_match(reporting_col)

  old_opt <- getOption("survey.lonely.psu")   # save old setting
  options(survey.lonely.psu = "adjust")  # set new one
  on.exit(options(survey.lonely.psu = old_opt), add = TRUE)


  # NA handling (same pattern as before)
  if (is.na(na_val)) {
    df <- df |> dplyr::filter(!is.na(!!rlang::sym(x)))
  }
  if (!is.na(na_val)) {
    df <- df |>
      dplyr::mutate(!!x := ifelse(is.na(!!rlang::sym(x)), na_val, !!rlang::sym(x)))
  }

  # grouping: by disag (if any) and the categorical var x
  group_by_vars <- if (!is.null(disag)) rlang::syms(c(disag, x)) else rlang::syms(x)
  df <- df |> dplyr::group_by(!!!group_by_vars, .drop = FALSE)

  # one row per group with proportions + counts
  # - stat: weighted proportion of each level of x within its (disag) group
  # - weighted_n: weighted count of rows in that level (non-missing x)
  # - unweighted_n: raw count of rows in that level


  res <- df |>
    dplyr::reframe(
      stat         = suppressWarnings(srvyr::survey_prop(na.rm = TRUE, vartype = "ci")),

      count_weighted   = suppressWarnings(srvyr::survey_total(!is.na(!!rlang::sym(x)), na.rm = TRUE)),
      # count_by_subset_weighted  = srvyr::survey_total(as.numeric(!is.na(!!rlang::sym(x)))),

      count_unweighted = sum(!is.na(!!rlang::sym(x))),
      # count_by_subset_unweighted = sum(!is.na(!!rlang::sym(x))),

    ) |>
    dplyr::mutate(variable = x) |>
    dplyr::rename(variable_val = !!rlang::sym(x))

  if (is.null(disag)) {
    res <- res |>
      mutate( count_by_subset_weighted = sum(count_weighted,na.rm = T),
              count_by_subset_unweighted = sum(count_unweighted,na.rm = T),
              count_by_question_weighted = sum(count_weighted,na.rm = T),
              count_by_question_unweighted = sum(count_unweighted,na.rm = T)
              )
  }



  # add subset name/value columns when disag provided
  if (!is.null(disag)) {
    subset_names <- glue::glue("subset_{1:length(disag)}_name")
    subset_vals  <- glue::glue("subset_{1:length(disag)}_val")


    res <- res |>
      dplyr::rename_with(~ glue::glue("subset_{seq_along(.x)}_val"), .cols = dplyr::all_of(disag)) |>
      mutate_key_pair(names = subset_names, values = disag) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(subset_vals), as.character))

        # Mark which variable was collapsed
    res <- res |> dplyr::ungroup() |>
      dplyr::group_by(!!!srvyr::syms(subset_vals) )|>
      dplyr::mutate(
        count_by_subset_weighted = sum(count_weighted,na.rm = T),
        count_by_subset_unweighted = sum(count_unweighted,na.rm = T),
      )  |> dplyr::ungroup() |>
      dplyr::mutate(count_by_question_weighted = sum(count_weighted,na.rm = T),
                    count_by_question_unweighted = sum(count_unweighted,na.rm = T))
  } else {
    subset_names <- character(0)
    subset_vals  <- character(0)
  }




  res<- res |>
    dplyr::select(
      dplyr::any_of(c("variable", "variable_val",
                      as.character(subset_names), as.character(subset_vals))),
      dplyr::everything())|>
    dplyr::select(-ends_with("_upp"),-ends_with("_low"),-ends_with("_se"))

  if(reporting_col== "weighted"){
    res <- res |> select(-contains("_unweighted"))
    names(res) <- names(res) |> stringr::str_replace_all("_weighted","")
    }
  if(reporting_col== "unweighted"){
    res <- res |> select(-contains("_weighted"))
    names(res) <- names(res) |> stringr::str_replace_all("_unweighted","")
    }
  return(res)


}


# ======================================================
# survey_analysis
# ======================================================

#' @title Batch Collapse Survey Data into tidy long format
#' @description Uses [srvyr::survey_mean()] to collapse or aggregate survey data
#'   by calling [survey_collapse_categorical_long()] and [survey_collapse_numeric_long()].
#'   Extracted and adapted from the butteR package.
#' @inheritParams survey_collapse_numeric_long
#' @param weights Logical; whether to apply survey weights
#' @param weight_column Column name for weights
#' @param strata Strata variable
#' @param vars_to_analyze Variables to collapse
#' @param question_lable Logical; if TRUE, append Kobo labels
#' @param kobo_path Path to Kobo XLSForm
#' @return A tidy long data frame of survey results
#' @export

survey_analysis <- function(df,
                            weights = FALSE,
                            weight_column = NULL,
                            reporting_col = "both",
                            strata,
                            vars_to_analyze = NULL,
                            disag = NULL,
                            na_val = NA,
                            sm_sep = "/",
                            question_lable = FALSE,
                            kobo_path = NULL) {




  if (weights && reporting_col != "weighted" && reporting_col != "both") {
    stop("reporting_col must be either 'weighted' or 'both' when weights = TRUE")
  }

  if (!weights && !(reporting_col %in% c("unweighted"))) {
    message("reporting_col must be 'unweighted' when weights = FALSE. Forcing reporting_col to 'unweighted'.")
    reporting_col <- "unweighted"
  }


  # --- Pre-clean vars ---------------------------------------------------------
  if (!is.null(vars_to_analyze)) vars_to_analyze <- vars_to_analyze[vars_to_analyze %in% names(df)]
  if (is.null(vars_to_analyze)) vars_to_analyze <- names(df)
  if (!is.null(weight_column)) vars_to_analyze <- vars_to_analyze[!vars_to_analyze %in% weight_column]

  # --- Convert to srvyr object ------------------------------------------------
  df <- if (weights) srvyr::as_survey(df, strata = dplyr::all_of(strata), weight = dplyr::all_of(weight_column)) else srvyr::as_survey(df)

  # --- Placeholder functions if missing --------------------------------------

  sm_parent_child_all <- auto_sm_parent_child(df$variables)
  sm_parent_child_vars <- sm_parent_child_all |> dplyr::filter(sm_parent %in% vars_to_analyze)
  not_sm <- vars_to_analyze[!vars_to_analyze %in% sm_parent_child_vars$sm_parent]
  vars_to_analyze <- c(not_sm, sm_parent_child_vars$sm_child)
  if (!is.null(disag)) vars_to_analyze <- setdiff(vars_to_analyze, disag)


  # --- Determine analysis type -----------------------------------------------
  calculation_type <- lapply(df$variables, class) |> as.data.frame() |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "main_variable", values_to = "type") |>
    dplyr::mutate(
      type = dplyr::case_when(main_variable %in% sm_parent_child_all$sm_child ~ "logical", TRUE ~ type),
      analysis_type = dplyr::case_when(
        type %in% c("numeric", "integer") ~ "mean",
        type == "logical" ~ "prop_select_multiple",
        TRUE ~ "prop_select_one"
      )
    ) |> dplyr::select(-type)


  # --- Collapse variables -----------------------------------------------------
  res_list <- list()
  n  <- length(vars_to_analyze)
  pb <- txtProgressBar(min = 0, max = n, style = 3)

  # for (i in vars_to_analyze) {
  #   print(i)
  for (k in seq_along(vars_to_analyze)) {
    i <- vars_to_analyze[[k]]   # current variable name (or index)
     # message(i)  # optional: print current var

    if (is.character(df$variables[[i]]) | is.factor(df$variables[[i]])) {
      res_list[[i]] <- survey_collapse_categorical_long(df, i, disag, NA_character_,reporting_col=reporting_col) |>
        dplyr::mutate(type ="proportion")
    } else if (is.logical(df$variables[[i]]) | is.numeric(df$variables[[i]])) {
      res_list[[i]] <- survey_collapse_numeric_long(df, i, disag, NA_real_, sm_sep,reporting_col=reporting_col) |>
        dplyr::mutate(type = case_when(variable %in% c(sm_parent_child_all$sm_parent,
                                                            sm_parent_child_all$sm_child)  ~ "mean|binary",
                                       T~"mean|numeric"))
    }
    setTxtProgressBar(pb, k)
  }

  close(pb)

  ###
  output_result <- dplyr::bind_rows(res_list) |>
    tidyr::separate(variable_val, c("question", "options"), sep = "\\.", extra = "merge") |>
    dplyr::mutate(
      main_variable = dplyr::case_when(is.na(variable) | variable == "" ~ question, TRUE ~ variable),
      choice = dplyr::case_when(!is.na(options) & options != "" ~ options, TRUE ~ question),
      choice = dplyr::case_when(main_variable == choice ~ NA_character_, TRUE ~ choice)
    ) |>
    dplyr::select(main_variable, choice, dplyr::everything()) |>
    dplyr::select(-variable, -question, -options) |>
    suppressWarnings()

  # --- Merge label info (optional) --------------------------------------------
  if (question_lable && !is.null(kobo_path)) {
    read_sheets(kobo_path)
    survey <- survey |> dplyr::select(name, dplyr::starts_with("label::"))
    choices <- choices |> dplyr::select(name, dplyr::starts_with("label::")) |>
      dplyr::distinct(name, .keep_all = TRUE)
    names(choices) <- paste0("choice_", names(choices))
    output_result <- output_result |>
      dplyr::left_join(survey, by = c("main_variable" = "name")) |>
      dplyr::left_join(choices, by = c("choice" = "choice_name")) |>
      dplyr::select(main_variable, dplyr::starts_with("label::"), choice,
                    dplyr::starts_with("Choice_label"), dplyr::everything())
  } else {
    output_result <- output_result

  }

  output_result <- output_result |>
    dplyr::left_join(sm_parent_child_all, by = c("main_variable" = "sm_child")) |>
    dplyr::mutate(
      main_variable = dplyr::coalesce(sm_parent, main_variable)
    ) |>
    dplyr::select(-sm_parent) |>
    dplyr::relocate("type",.after = last_col())


  # --- Return -----------------------------------------------------------------
  output_result
}
