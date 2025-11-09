#' Read audit CSVs from a ZIP into a named list
#'
#' Scans a ZIP archive for files ending with `audit.csv` (assumed path
#' structure `<uuid>/audit.csv`), reads each audit into a data frame,
#' and returns a list named by the corresponding UUID. If a `dataset`
#' is provided, only audits whose UUIDs appear in `dataset[[uuid_column]]`
#' are read; requested UUIDs missing from the ZIP are added as empty
#' data frames (and a warning is issued).
#'
#' @param audit_zip_path Character. Path to a `.zip` file containing one
#'   or more audit CSVs located under folders like `<uuid>/audit.csv`.
#' @param dataset Optional data frame used to filter which audits to read.
#'   When supplied, only UUIDs present in `dataset[[uuid_column]]` are
#'   returned, preserving the order of that vector.
#' @param uuid_column Character scalar. Name of the UUID column in `dataset`
#'   (default `"uuid"`). Ignored when `dataset` is `NULL`.
#'
#' @details
#' The function expects a consistent directory layout inside the ZIP where
#' each audit resides at the same depth and the UUID is the path segment
#' immediately preceding `audit.csv`. If this structure is not consistent,
#' the function stops with an error. When `dataset` is provided:
#' \itemize{
#'   \item Audits present in the ZIP but not in `dataset` are ignored (warning).
#'   \item UUIDs present in `dataset` but missing from the ZIP are included
#'         as empty data frames (warning).
#' }
#'
#' @return A named list of data frames (one per UUID). Names are UUIDs.
#'   When `dataset` is provided, the list is ordered to match
#'   `dataset[[uuid_column]]`.
#'
#' @examples
#' \dontrun{
#' # Read all audits found in the zip
#' audits <- create_audit_list("audits.zip")
#'
#' # Read only audits matching UUIDs in a dataset
#' df <- data.frame(uuid = c("abc-123", "xyz-789"))
#' audits <- create_audit_list("audits.zip", dataset = df, uuid_column = "uuid")
#' }
#'
#' @export
#' @importFrom utils unzip
#' @importFrom utils read.table
#' @importFrom dplyr rename filter pull select rowwise mutate ungroup
#' @importFrom stringr str_detect str_split
#' @importFrom purrr map set_names map_dbl
#' @importFrom glue glue
#'
create_audit_list <- function(audit_zip_path,
                              dataset = NULL,
                              uuid_column = "uuid") {
  list_of_files <- utils::unzip(audit_zip_path, list = TRUE) |>
    dplyr::rename(path = Name) |>
    dplyr::filter(stringr::str_detect(path, pattern = "audit.csv"))

  audit_pos <- list_of_files |>
    dplyr::pull(path) |>
    stringr::str_split("/") |>
    purrr::set_names(list_of_files$path) |>
    purrr::map_dbl(~ which(stringr::str_detect(.x, "audit.csv"))) |>
    unique()

  if (length(audit_pos) != 1) {
    stop("Please check the audit zip, some folders are not following the same structure.")
  }

  all_uuid_df <- list_of_files |>
    dplyr::select(path) |>
    dplyr::rowwise() |>
    dplyr::mutate(uuid = unlist(stringr::str_split(path, "/"))[[audit_pos - 1]]) |>
    dplyr::ungroup()

  if (!is.null(dataset)) {
    if (uuid_column %in% names(dataset) != 1) {
      msg <- glue::glue("The variable ", uuid_column, " cannot be identified in the dataset provided.")
      stop(msg)
    }
    look_up_vector <- dataset[[uuid_column]]

    if (any(!all_uuid_df$uuid %in% look_up_vector)) {
      msg <- glue::glue(nrow(all_uuid_df) - length(look_up_vector), " audit files are found but not in the dataset. They won't be read.")
      warning(msg)
    }

    all_uuid_df <- all_uuid_df |>
      dplyr::filter(uuid %in% look_up_vector)
  }

  list_of_audits <- all_uuid_df$path |>
    purrr::map(~ read.table(unz(audit_zip_path, filename = .x), header = TRUE, quote = "\"", sep = ",")) |>
    purrr::set_names(all_uuid_df$uuid)

  if (!is.null(dataset)) {
    if (length(list_of_audits) < length(look_up_vector)) {
      to_add_vector <- look_up_vector[!look_up_vector %in% names(list_of_audits)]
      msg <- glue::glue(length(to_add_vector), " audit files were not found. They were added as empty dataframes.")
      warning(msg)
      to_add <- purrr::map(to_add_vector, function(xx) {
        list_of_audits[[1]] |> dplyr::filter(event == "IWANTANEMPTYAUDIT")
      }) |>
        purrr::set_names(to_add_vector)

      list_of_audits <- append(list_of_audits, to_add)
    }
  }

  return(list_of_audits)
}


#' Calculate duration between two questions from an audit log
#'
#' Computes the time elapsed between the **start** timestamp of a
#' `start_question` and the **end** timestamp of an `end_question` in a
#' single audit file. If a question node appears multiple times (e.g., edits
#' or select-multiple behavior), the function uses the **minimum** `start`
#' for the starting question and the **maximum** `end` for the ending question.
#'
#' If either question is missing (e.g., due to skip logic or not present in
#' the audit), the calculation yields `-Inf` for the duration (via
#' `max(numeric(0)) - min(numeric(0))` semantics).
#'
#' @param audit_file A data frame containing one audit log with at least the
#'   columns `event`, `node`, `start`, and `end`. Timestamps are expected in
#'   milliseconds since the epoch.
#' @param start_question Character scalar giving the trailing node label of the
#'   starting question (matched at the end of `node`, e.g., `"question1"` matches
#'   `"/form/group/question1"`).
#' @param end_question Character scalar giving the trailing node label of the
#'   ending question (matched at the end of `node`).
#'
#' @details
#' Nodes are matched with a regex that anchors to the end of the `node` path,
#' so only exact trailing matches are used. When a question occurs multiple
#' times, the earliest `start` and latest `end` are taken to span the full interaction.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{duration_ms}{Elapsed time in milliseconds.}
#'   \item{duration_minutes}{Elapsed time in minutes, rounded to one decimal.}
#' }
#'
#' @examples
#' \dontrun{
#' some_audit <- data.frame(
#'   event = c("form start", rep("question", 5)),
#'   node  = c("", paste0("/xx/question", 1:5)),
#'   start = c(1661415887295, 1661415887301, 1661415890819,
#'             1661415892297, 1661415893529, 1661415894720),
#'   end   = c(NA, 1661415890790, 1661415892273,
#'             1661415893506, 1661415894703, 1661415896452)
#' )
#' create_duration_from_audit_with_start_end(
#'   some_audit, "question1", "question3"
#' )
#' }
#'
#' @export
#' @importFrom dplyr select
#'

create_duration_from_audit_with_start_end <- function(audit_file, start_question, end_question) {
  df_start_subset <- audit_file[grepl(paste0("\\/(?:", start_question, ")$"), audit_file$node), ]
  start_question_df <- df_start_subset |> dplyr::select(start)
  start_question_df <- min(start_question_df$start)

  df_end_subset <- audit_file[grepl(paste0("\\/(?:", end_question, ")$"), audit_file$node), ]
  end_question_df <- df_end_subset |> dplyr::select(end)
  end_question_df <- max(end_question_df$end)

  duration_ms <- end_question_df - start_question_df
  duration_minutes <- round(duration_ms / 1000 / 60, 1)

  data.frame(
    duration_ms = duration_ms,
    duration_minutes = duration_minutes
  )
}

#' Calculate total duration from an audit log by summing all question times
#'
#' Computes the total time spent across all question nodes in a single
#' audit file. The duration is defined as the difference between the
#' summed `end` and `start` timestamps of all question events, ignoring
#' empty `node` values (such as *form start*, *end screen*, or *form save*).
#'
#' @param audit_file A data frame containing one audit log with at least the
#'   columns `node`, `start`, and `end`. Timestamps are expected in
#'   milliseconds since the epoch.
#'
#' @details
#' Rows where `node` is empty (`""`) are excluded prior to summation.
#' The resulting duration reflects the total time actively spent on
#' all questions combined, not the overall elapsed form time.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{duration_ms}{Total duration across all questions in milliseconds.}
#'   \item{duration_minutes}{Total duration in minutes, rounded to one decimal.}
#' }
#'
#' @examples
#' \dontrun{
#' some_audit <- data.frame(
#'   event = c("form start", rep("question", 5)),
#'   node  = c("", paste0("/xx/question", 1:5)),
#'   start = c(1661415887295, 1661415887301, 1661415890819,
#'             1661415892297, 1661415893529, 1661415894720),
#'   end   = c(NA, 1661415890790, 1661415892273,
#'             1661415893506, 1661415894703, 1661415896452)
#' )
#' create_duration_from_audit_sum_all(some_audit)
#' }
#'
#' @export
#' @importFrom dplyr filter

create_duration_from_audit_sum_all <- function(audit_file) {
  audit_file <- audit_file |> dplyr::filter(node != "")
  duration_ms <- sum(audit_file$end - audit_file$start)
  duration_minutes <- round(duration_ms / 1000 / 60, 1)
  data.frame(
    duration_ms = duration_ms,
    duration_minutes = duration_minutes
  )
}

#' Add audit-derived durations to a dataset
#'
#' Wrapper around \code{\link{create_duration_from_audit_with_start_end}} and
#' \code{\link{create_duration_from_audit_sum_all}} that computes durations from
#' per-interview audit logs and left-joins them to a dataset by UUID.
#'
#' @param dataset A data frame to which the duration columns will be added.
#' @param col_name_prefix Character scalar used as the prefix for new columns.
#'   Defaults to \code{"duration_audit"}. The function creates columns with the
#'   following suffixes when requested:
#'   \itemize{
#'     \item \code{"_start_end_ms"}, \code{"_start_end_minutes"} — duration between a specific
#'           start and end question (milliseconds and minutes).
#'     \item \code{"_sum_all_ms"}, \code{"_sum_all_minutes"} — total duration obtained by
#'           summing all question times (milliseconds and minutes).
#'   }
#'   If any of these columns already exist, the function stops with an error.
#' @param uuid_column Character scalar; the UUID column name in \code{dataset}.
#'   Default is \code{"uuid"}.
#' @param audit_list A named list of audit data frames, one per interview,
#'   where each element name is the interview UUID. Each audit data frame must
#'   contain at least the columns \code{event}, \code{node}, \code{start}, and \code{end}.
#' @param start_question Optional character scalar specifying the trailing node label
#'   of the starting question used for the start–end duration.
#' @param end_question Optional character scalar specifying the trailing node label
#'   of the ending question used for the start–end duration.
#' @param sum_all Logical; if \code{TRUE} (default), also compute and add the total
#'   duration obtained by summing all question times.
#'
#' @details
#' When both \code{start_question} and \code{end_question} are provided, the function
#' computes the duration between the earliest \code{start} timestamp of the start
#' question and the latest \code{end} timestamp of the end question using
#' \code{\link{create_duration_from_audit_with_start_end}}. Independently, when
#' \code{sum_all = TRUE}, it computes the total question time using
#' \code{\link{create_duration_from_audit_sum_all}}. Results are then joined to
#' \code{dataset} by matching \code{uuid_column} to the names of \code{audit_list}.
#'
#' The function validates inputs and will stop when:
#' \itemize{
#'   \item Only one of \code{start_question}/\code{end_question} is provided.
#'   \item \code{uuid_column} is missing from \code{dataset}.
#'   \item None of the \code{audit_list} names are found in \code{dataset[[uuid_column]]}.
#'   \item Required audit columns are missing.
#' }
#'
#' @return The input \code{dataset} with additional duration columns as requested.
#'
#' @examples
#' \dontrun{
#' list_audit <- list(
#'   uuid1 = data.frame(
#'     event = c("form start", rep("question", 5)),
#'     node  = c("", paste0("/xx/question", 1:5)),
#'     start = c(1661415887295, 1661415887301, 1661415890819,
#'               1661415892297, 1661415893529, 1661415894720),
#'     end   = c(NA, 1661415890790, 1661415892273,
#'               1661415893506, 1661415894703, 1661415896452)
#'   ),
#'   uuid2 = data.frame(
#'     event = c("form start", rep("question", 5)),
#'     node  = c("", paste0("/xx/question", 1:5)),
#'     start = c(1661415887295, 1661415887301, 1661415890819,
#'               1661415892297, 1661415893529, 1661415894720),
#'     end   = c(NA, 1661415890790, 1661415892273,
#'               1661415893506, 1661415894703, 1661415896452)
#'   )
#' )
#' some_dataset <- data.frame(
#'   X_uuid = c("uuid1", "uuid2"),
#'   question1 = c("a", "b"),
#'   question2 = c("a", "b"),
#'   question3 = c("a", "b"),
#'   question4 = c("a", "b"),
#'   question5 = c("a", "b")
#' )
#' # Add sum-all durations
#' add_duration_from_audit(some_dataset, uuid_column = "X_uuid", audit_list = list_audit)
#'
#' # Add start–end duration only
#' add_duration_from_audit(
#'   some_dataset, uuid_column = "X_uuid", audit_list = list_audit,
#'   start_question = "question1", end_question = "question3",
#'   sum_all = FALSE
#' )
#'
#' # Add both start–end and sum-all durations
#' add_duration_from_audit(
#'   some_dataset, uuid_column = "X_uuid", audit_list = list_audit,
#'   start_question = "question1", end_question = "question3",
#'   sum_all = TRUE
#' )
#' }
#'
#' @seealso \code{\link{create_duration_from_audit_with_start_end}},
#'   \code{\link{create_duration_from_audit_sum_all}}
#'
#' @export
#' @importFrom dplyr left_join mutate
#' @importFrom purrr map set_names
#' @importFrom glue glue


add_duration_from_audit <- function(dataset,
                                    col_name_prefix = "duration_audit",
                                    uuid_column = "uuid",
                                    audit_list,
                                    start_question = NULL,
                                    end_question = NULL,
                                    sum_all = TRUE) {
  # checks input
  if (is.null(start_question) & !is.null(end_question)) {
    stop("start_question is missing")
  }
  if (!is.null(start_question) & is.null(end_question)) {
    stop("end_question is missing")
  }
  if (!is.null(start_question) & !is.null(end_question)) {
    new_names_start_end <- paste0(col_name_prefix, c("_start_end_ms", "_start_end_minutes"))
    if (any(new_names_start_end %in% names(dataset))) {
      msg <- glue::glue(col_name_prefix, " seems to be already used as name in your dataset.")
      stop(msg)
    }
  }

  if (sum_all) {
    new_names_sum_all <- paste0(col_name_prefix, c("_sum_all_ms", "_sum_all_minutes"))
    if (any(new_names_sum_all %in% names(dataset))) {
      msg <- glue::glue(col_name_prefix, " seems to be already used as name in your dataset.")
      stop(msg)
    }
  }

  # calculate duration for each audit file
  if (!is.null(start_question) & !is.null(end_question)) {
    duration_with_start_end <- audit_list |>
      purrr::map(~ create_duration_from_audit_with_start_end(.x,
                                                             start_question = start_question,
                                                             end_question = end_question
      )) |>
      purrr::set_names(names(audit_list)) |>
      do.call(rbind, .) |>
      `names<-`(new_names_start_end) |>
      dplyr::mutate(uuid = row.names(.))
  }

  if (!uuid_column %in% names(dataset)) {
    msg <- glue::glue(uuid_column, " variable cannot be found in the dataset.")
    stop(msg)
  }

  if (all(!names(audit_list) %in% dataset[[uuid_column]])) {
    stop("It seems no uuid are found as name of any data frame of audit list, make sure the data frame are saved with the uuid as name.")
  }

  audit_check <- audit_list |>
    purrr::map_lgl(function(xx) {
      all(c("node", "start", "end", "event") %in% names(xx))
    }) |>
    all()
  if (!audit_check) {
    stop("Some columns are missing in the audits, please make sure to have at least event, node, start, end")
  }

  if (sum_all) {
    duration_with_sum_all <- audit_list |>
      purrr::map(create_duration_from_audit_sum_all) |>
      purrr::set_names(names(audit_list)) |>
      do.call(rbind, .) |>
      `names<-`(new_names_sum_all) |>
      dplyr::mutate(uuid = row.names(.))
  }

  if (exists("duration_with_sum_all")) {
    dataset <- dataset |>
      dplyr::left_join(duration_with_sum_all, by = stats::setNames("uuid", uuid_column))
  }

  if (exists("duration_with_start_end")) {
    dataset <- dataset |>
      dplyr::left_join(duration_with_start_end, by = stats::setNames("uuid", uuid_column))
  }

  return(dataset)
}

#' Check whether durations fall outside a specified range
#'
#' Flags records whose duration is strictly less than `lower_bound` or strictly
#' greater than `higher_bound`. Accepts either a data frame or a list that
#' contains a data frame under the name `"checked_dataset"`. The function
#' returns the input (as a list) and adds a log data frame under `log_name`
#' containing UUIDs and offending values.
#'
#' @param dataset A data frame to check, or a list with the data frame stored
#'   under the element `"checked_dataset"`. If a data frame is provided, it is
#'   wrapped into a list automatically.
#' @param column_to_check Character scalar giving the name of the duration column
#'   to evaluate.
#' @param uuid_column Character scalar with the UUID column name in `dataset`.
#'   Defaults to `"uuid"`.
#' @param log_name Character scalar naming the element to store the log in the
#'   returned list. Defaults to `"duration_log"`.
#' @param lower_bound Numeric lower threshold; values **strictly below** this
#'   are flagged. Default is `25`.
#' @param higher_bound Numeric upper threshold; values **strictly above** this
#'   are flagged. Default is `90`.
#'
#' @return A list with:
#' \describe{
#'   \item{checked_dataset}{The input data frame (possibly wrapped from `dataset`).}
#'   \item{`log_name`}{A data frame of flagged rows with columns:
#'     \code{uuid}, \code{old_value} (the offending duration), \code{question}
#'     (the checked column name), and \code{issue} (a descriptive message).}
#' }
#'
#' @examples
#' \dontrun{
#' testdata <- data.frame(
#'   uuid = c(letters[1:7]),
#'   duration_audit_start_end_ms = c(2475353, 375491, 2654267, 311585, 817270, 2789505, 8642007),
#'   duration_audit_start_end_minutes = c(41, 6, 44, 5, 14, 46, 144)
#' )
#'
#' # Simple check using default bounds (wrapped automatically)
#' check_duration(testdata, column_to_check = "duration_audit_start_end_minutes")
#'
#' # Provide custom bounds
#' check_duration(
#'   testdata,
#'   column_to_check = "duration_audit_start_end_ms",
#'   lower_bound = 375490,
#'   higher_bound = 8642000
#' )
#'
#' # Chain multiple checks, storing separate logs
#' testdata |>
#'   check_duration(column_to_check = "duration_audit_start_end_minutes") |>
#'   check_duration(
#'     column_to_check = "duration_audit_start_end_ms",
#'     log_name = "duration_in_ms",
#'     lower_bound = 375490,
#'     higher_bound = 8642000
#'   )
#' }
#'
#' @export
#' @importFrom dplyr mutate filter select all_of rename
#' @importFrom rlang sym
#' @importFrom glue glue
check_duration <- function(dataset,
                           column_to_check,
                           uuid_column = "uuid",
                           log_name = "duration_log",
                           lower_bound = 25,
                           higher_bound = 90) {
  if (is.data.frame(dataset)) {
    dataset <- list(checked_dataset = dataset)
  }
  if (!("checked_dataset" %in% names(dataset))) {
    stop("Cannot identify the dataset in the list")
  }

  if (!(column_to_check %in% names(dataset[["checked_dataset"]]))) {
    msg <- glue::glue("Cannot find ", column_to_check, " in the names of the dataset")
    stop(msg)
  }

  log <- dataset[["checked_dataset"]] |>
    dplyr::mutate(duration_check = !!rlang::sym(column_to_check) < lower_bound |
                    !!rlang::sym(column_to_check) > higher_bound) |>
    dplyr::filter(duration_check) |>
    dplyr::select(dplyr::all_of(c(uuid_column, column_to_check))) |>
    dplyr::mutate(
      question = column_to_check,
      issue = "Duration is lower or higher than the thresholds"
    ) |>
    dplyr::rename(
      old_value = !!rlang::sym(column_to_check),
      uuid = !!rlang::sym(uuid_column)
    )

  dataset[[log_name]] <- log
  return(dataset)
}
