#' This function recreates the columns for select multiple questions
#'
#' @param dataset data frame
#' @param uuid_column uuid column in the dataset. Default is "uuid".
#' @param kobo_survey Kobo survey sheet.
#' @param kobo_choices Kobo choices sheet.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#' @export
#' @examples
#' test_data <- dplyr::tibble(
#'   uuid = paste0("uuid_", 1:6),
#'   gender = rep(c("male", "female"), 3),
#'   reason = c(
#'     "xx,yy", "xx,zy",
#'     "zy", "xx,xz,zy",
#'     NA_character_, "xz"
#'   ),
#'   reason.xx = c(0, 1, 0, 1, 0, 0),
#'   reason.yy = c(1, 0, 0, 0, 1, 0),
#'   reason.xz = c(0, 0, 0, 1, 0, 1),
#'   reason.zy = c(0, 1, 1, 1, 0, 0),
#'   reason_zy = c(NA_character_, "A", "B", "C", NA_character_, NA_character_)
#' )
#' recreate_parent_column(dataset = test_data, uuid_column = "uuid", sm_separator = ".")
#'
recreate_parent_column <- function(dataset,
                                   uuid_column = "uuid",
                                   kobo_survey = NULL,
                                   kobo_choices = NULL,
                                   sm_separator = ".") {
  checked_data <- dataset

  initial_order <- names(dataset)

  if (is.null(kobo_survey)) {
    old_name <- names(dataset)
    number_of_separator <- names(dataset) |>
      stringr::str_count(pattern = paste0("\\", sm_separator)) |>
      max(na.rm = T)
    for (i in 1:number_of_separator) {
      names(dataset) <-
        sub(paste0("(\\", sm_separator, ".*?)\\", sm_separator),
            "\\1_",
            names(dataset))
    }

    cols_order <- dataset |> names()


    difference_df <- dplyr::tibble(old_name = old_name,
                                   new_name = cols_order) |> dplyr::filter(old_name != new_name)

    if (nrow(difference_df) > 0) {
      warning(
        "Column(s) names are renamed as multiple separators are found in dataset column names. Please see the above table with the new name."
      )

      print(difference_df)
    }


    select_multiple <-
      auto_sm_parent_child(dataset, sm_separator = sm_separator)
  }

  if (!is.null(kobo_survey)) {
    choice_to_join <- kobo_choices |> dplyr::select(list_name, name)

    select_multiple <- kobo_survey |>
      dplyr::filter(grepl("select_multiple", type)) |>
      dplyr::select(type, name) |>
      dplyr::mutate(type = stringr::str_replace_all(type, "select_multiple ", "")) |>
      dplyr::rename(list_name = type,
                    sm_parent = name) |>
      dplyr::left_join(choice_to_join, multiple = "all", by = "list_name") |>
      dplyr::mutate(sm_child = paste0(sm_parent, sm_separator, name)) |>
      dplyr::select(sm_parent, sm_child)

    missing_column <-
      select_multiple$sm_child[!select_multiple$sm_child %in% names(dataset)]

    if (length(missing_column) > 0) {
      print(missing_column)
      warning(paste0(
        "Ignoring the above column(s) as they do not exist in the dataset."
      ))
    }
    select_multiple <-
      select_multiple |> dplyr::filter(sm_child %in% names(dataset))
  }

  if (nrow(select_multiple) > 0) {
    select_multiple_list <- list()

    for (i in select_multiple$sm_parent) {
      select_multi_single <-
        select_multiple |> dplyr::filter(sm_parent == i)
      concat_col <- select_multi_single$sm_parent |> unique()
      choice_cols <- select_multi_single$sm_child |> unique()

      df_only_cols <-
        dataset |> dplyr::select(dplyr::all_of(choice_cols),
                                  dplyr::all_of(uuid_column))

      pivot_long <-
        df_only_cols |> dplyr::mutate_at(names(df_only_cols), as.character)

      final_df <- pivot_long |>
        tidyr::pivot_longer(
          cols = !dplyr::all_of(uuid_column),
          names_to = "cols",
          values_to = "value"
        ) |>
        dplyr::filter(value == 1 |
                        value == TRUE |
                        value == "1" | value == "TRUE") |>
        dplyr::group_by(!!rlang::sym(uuid_column)) |>
        dplyr::summarise(!!rlang::sym(concat_col) := paste0(cols, collapse = " "))

      final_df[[concat_col]] <-
        final_df[[concat_col]] |> stringr::str_replace_all(paste0(concat_col, "."), "")

      select_multiple_list[[concat_col]] <- final_df
    }

    final_df_for_export <-
      purrr::reduce(select_multiple_list, dplyr::full_join, by = uuid_column)
    concat_col_names_from_fina_export <- final_df_for_export |>
      dplyr::select(!dplyr::all_of(uuid_column)) |>
      names()

    data_with_fix_concat <- dataset |>
      dplyr::select(-dplyr::all_of(concat_col_names_from_fina_export)) |>
      dplyr::left_join(final_df_for_export, by = uuid_column)

    if (is.null(kobo_survey)) {
      data_with_fix_concat <-
        data_with_fix_concat |> dplyr::select(dplyr::all_of(cols_order))
    }
    if (!is.null(kobo_survey)) {
      data_with_fix_concat <-
        data_with_fix_concat |> dplyr::select(dplyr::all_of(initial_order))
    }}

  if (nrow(select_multiple) == 0) { data_with_fix_concat = dataset}

    return(data_with_fix_concat)

}
