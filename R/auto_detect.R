#' @name auto_detect_sm_parents
#' @rdname auto_detect_sm_parents
#' @title Detect select multiple parent columns
#' @description `auto_detect_sm_parents` is mean to detect select multiple parent columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a list of select multiple parent columns in data set.
#' @export
auto_detect_sm_parents <- function(dataset, sm_separator = ".") {
  sm_parents <-
    sub(glue::glue(".[^\\{sm_separator}]*$"),
        "",
        colnames(dataset))
  sm_parents <- data.frame(col_names = sm_parents[sm_parents != ""])
  select_multiple_detected <- sm_parents |>
    dplyr::group_by(col_names) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n > 1) |>
    dplyr::select(col_names)
  return(as.character(select_multiple_detected$col_names))
}
#' @name auto_sm_parent_child
#' @rdname auto_sm_parent_child
#' @title detect and group together select multiple parent and children columns
#' @description `auto_sm_parent_children` is mean to detect select multiple parent columns & children columns in a way that does
#' not rely on the XLSForm as the input
#' @param dataset dataset to correct
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a data frame containing the the child select multiple columns alongside there parents and
#' a log with all changes recorded.
#' @export


auto_sm_parent_child <- function(dataset, sm_separator = ".") {
  sm_parents <- auto_detect_sm_parents(dataset, sm_separator)
  sm_child <- dataset |>
    dplyr::select(dplyr::starts_with(glue::glue("{sm_parents}{sm_separator}"))) |>
    colnames()
  dplyr::tibble(sm_parent = sub(glue::glue(".[^\\{sm_separator}]*$"), "", sm_child),
                sm_child)
}


