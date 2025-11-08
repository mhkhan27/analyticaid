#' Create a lookup table mapping XML names to human labels
#'
#' Builds a mapping from the XLSForm `survey`/`choices` sheets to support
#' renaming data columns from XML `name` to readable labels. Expands
#' `select_multiple` questions into `name.choice` style entries and includes
#' parent `select_multiple` columns.
#'
#' @param survey A data frame of the XLSForm **survey** sheet; must include
#'   columns `type`, `name`, and the label column specified in `label`.
#' @param choices A data frame of the XLSForm **choices** sheet; must include
#'   columns `list_name`, `name`, and the label column specified in `label`.
#' @param label Character scalar giving the exact label column name to use
#'   (e.g., `"label::English (en)"`).
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{col_name_lable}{Target (unique) label to rename to.}
#'   \item{xml_label}{Source XML column name to rename from.}
#' }
#'
#' @examples
#' \dontrun{
#' lut <- create_lookup_table(survey, choices, label = "label::English (en)")
#' head(lut)
#' }
#' @export
#' @importFrom dplyr select filter mutate left_join bind_rows distinct relocate any_of rename_with
#' @importFrom stringr str_replace_all
create_lookup_table<- function(survey,choices,label = "label::English (en)"){
  # Base lookup from survey: keep type, name, and dynamic label column
  lookup_table <- survey |>
    dplyr::select(type,name, label = label ) |>
    dplyr::filter(
      !is.na(label)
    )

  # Parent columns of select_multiple (keep original parent question mapping)
  parent_col_xml_lab <- lookup_table |> dplyr::filter(
    grepl("select_mu",type )) |>
    dplyr::select(col_name_lable = label,
                  xml_label = name)

  # Map select_one/text/integer directly: label -> name
  lookup_table_select_one <- lookup_table |> dplyr::filter(
    grepl("select_one|text|integer",type )
  ) |> dplyr::select(col_name_lable = label,
                     xml_label = name)



  # select_multiple ---------------------------------------------------------

  # Build choice list with label column
  choice_list <- choices |> dplyr::select(list_name,name_choice=name,
                                          choice_label=label)

  # For select_multiple: create name.choice entries with "Label/ChoiceLabel"
  lookup_table_select_multiple <- lookup_table |> dplyr::filter(
    grepl("select_mu",type )
  ) |> dplyr::mutate(
    list_name = stringr::str_replace_all(type, "select_multiple ","")
  ) |> dplyr::left_join(choice_list,multiple = "all",relationship = "many-to-many",,by = "list_name" ) |> dplyr::mutate(
    col_name_lable = paste0(label,"/",choice_label),
    xml_label = paste0(name,".",name_choice)
  ) |> dplyr::select(col_name_lable,xml_label)



  # Combine: select_one, select_multiple-expanded, and parent select_multiple
  lookup_table <-lookup_table_select_one |>
    dplyr::bind_rows(lookup_table_select_multiple) |>
    dplyr::bind_rows(parent_col_xml_lab)

  # Ensure target labels are unique (append _1, _2, … if duplicated)
  lookup_table$col_name_lable <- make.unique(lookup_table$col_name_lable,sep = "_")

  return(lookup_table)

}

#' Rename data frame columns from XML names to labels
#'
#' Uses `create_lookup_table()` to build a mapping, then renames columns of
#' `data` where XML `name` matches, replacing with human-readable labels.
#' Only existing columns are affected.
#'
#' @param data A data frame whose columns (XML names) you want to rename.
#' @param survey XLSForm `survey` data frame (see `create_lookup_table()`).
#' @param choices XLSForm `choices` data frame (see `create_lookup_table()`).
#' @param label Character scalar naming the label column to use in both sheets.
#'
#' @return A data frame with columns renamed where matches exist.
#'
#' @examples
#' \dontrun{
#' df2 <- rename_xml_to_label_col(df, survey, choices, "label::English (en)")
#' }
#' @export
rename_xml_to_label_col <- function(data,survey,choices,label){

  # Build mapping table and a named vector: names=xml, values=labels
  lookup_table <- create_lookup_table(survey = survey,choices = choices,label = label)
  rename_vector <- setNames(lookup_table$col_name_lable, lookup_table$xml_label)

  # Rename only columns that exist in `data`
  data <- data |>
    dplyr::rename_with(
      ~ rename_vector[.x],
      .cols = dplyr::any_of(names(rename_vector))   # only columns that exist
    )

  return(data)
}




#' Replace partial matches with labels and concatenate (helper)
#'
#' Given a provider string (often space-separated ids), finds partial matches
#' in `lookup_table$choice_id` and concatenates corresponding labels.
#'
#' @param provider A character scalar representing encoded choices.
#' @param lookup_table A data frame with columns `choice_id` and `choice_lab`.
#'
#' @return A single character string of concatenated labels, or the original
#'   `provider` if no matches are found.
#' @keywords internal
replace_value <- function(provider, lookup_table) {
  # Collect labels whose ids are detected within `provider`
  matches <- lookup_table$choice_lab[sapply(lookup_table$choice_id, function(x) string::str_detect(provider, x))]

  if (length(matches) > 0) {
    return(paste(matches, collapse = ", "))
  } else {
    return(provider)  # Keep the original if no match
  }
}

#' Convert select-multiple values to concatenated labels (non-destructive)
#'
#' For each select-multiple question present in `data`, converts encoded
#' response values into a comma-separated list of human-readable choice labels.
#' Keeps original column names and replaces values in place.
#'
#' @param data A data frame containing XML-named columns.
#' @param survey XLSForm `survey` sheet.
#' @param choice XLSForm `choices` sheet.
#' @param label Label column to use (default `"label::English"`).
#'
#' @return The input `data` with select-multiple columns converted to
#'   comma-separated label strings (or `NA` when no matches).
#'
#' @examples
#' \dontrun{
#' df_sm <- xml_to_label_only_concat(df, survey, choice, "label::English")
#' }
#' @export
xml_to_label_only_concat  <- function(data,survey,choice,
                                      label= "label::English"
) {

  # function
  replace_value <- function(provider, lookup_table) {

    # Find labels corresponding to any detected choice ids in provider
    matches <- lookup_table$choice_lab[sapply(lookup_table$choice_id, function(x) stringr::str_detect(provider, x))]

    # Remove NA values from matches
    matches <- matches[!is.na(matches)]

    if (length(matches) > 0) {
      result <- paste(matches, collapse = ", ")
    } else {
      result <- NA  # Assign actual NA instead of returning "NA"
    }

    return(result)
  }

  # Prepare survey mapping for select_* questions
  survey_m <-  survey |>
    dplyr::filter(grepl("select_",type)) |>
    dplyr::mutate(list_name = type |>
                    stringr::str_replace_all("select_multiple","") |>
                    trimws()) |>
    dplyr::relocate("list_name",.after = "type") |>
    dplyr::select(name =list_name,question_id =name)

  # Prepare choices linked to survey questions present in data
  choice_sm <- choice |>
    dplyr::select(name = list_name,
                  choice_id = name,
                  choice_lab = label)|>
    dplyr::left_join(survey_m,relationship = "many-to-many",by = "name") |>
    dplyr::select(question_id,choice_id,choice_lab) |>
    dplyr::distinct() |>
    dplyr::filter(question_id %in% names(data))

  # For each question id, convert encoded values to concatenated labels
  for (i in unique(choice_sm$question_id)) {
    choice_fis <- choice_sm |>
      dplyr::filter(question_id == i)

    # Check if column exists in data before applying function
    if (i %in% colnames(data)) {
      data[[i]] <- sapply(data[[i]], replace_value, lookup_table = choice_fis)
    }

  }

  return(data)

}

#' Convert XML-coded values to labels and optionally rename columns
#'
#' Replaces select-one values with their labels, converts select-multiple
#' values to concatenated labels via `xml_to_label_only_concat()`, and
#' optionally renames columns from XML names to label names.
#'
#' @param data A data frame containing XML-named columns.
#' @param survey XLSForm `survey` sheet.
#' @param choice XLSForm `choices` sheet.
#' @param label Label column to use (default `"label::English"`).
#' @param change_col_name Logical; if `TRUE`, apply `rename_xml_to_label_col()`
#'   after value conversions to rename columns to label text.
#'
#' @return A transformed data frame with values converted to labels and, if
#'   requested, column names renamed to labels.
#'
#' @examples
#' \dontrun{
#' df_lbl <- xml_to_lable(df, survey, choice, "label::English", change_col_name = TRUE)
#' }
#' @export
xml_to_lable <- function(data, survey,
                         choice,
                         label= "label::English",
                         change_col_name =F){

  ### Preparing lookup table

  # Survey mapping for select_* questions
  survey_select <- survey |>
    dplyr::filter(grepl("select_",type)) |>
    dplyr::mutate(list_name = type |>
                    stringr::str_replace_all("select_one","") |>
                    trimws()) |>
    dplyr::select(name =list_name,question_id =name)

  # Choices joined to survey questions that exist in data
  choice_s <- choice |>
    dplyr::select(name = list_name,
                  choice_id = name,
                  choice_lab = label )|>
    dplyr::left_join(survey_select,relationship = "many-to-many",by = "name") |>
    dplyr::select(question_id,choice_id,choice_lab) |>
    dplyr::distinct() |>
    dplyr::filter(question_id %in% names(data))

  # Replace select_one codes with labels per question column
  for(i in unique(choice_s$question_id)) {

    choice_fi <- choice_s |>
      dplyr::filter(question_id == i)
    data <- data |>
      dplyr::mutate(!!rlang::sym(i):= dplyr::case_when(
        data[[i]] %in% choice_fi$choice_id ~
          choice_fi$choice_lab[match( data[[i]], choice_fi$choice_id)],
        TRUE ~ data[[i]]
      ))


  }

  # Convert select_multiple values to concatenated labels
  df <-  xml_to_label_only_concat(data = data,survey = survey,choice = choice,label = label)

  # Optionally rename columns to label names
  if(change_col_name) {
    df<- rename_xml_to_label_col(data = data,survey = survey,choices = choice,label = label)
  }

  return(df)
}
