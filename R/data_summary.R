#' Summarize Unique Values and Basic Statistics for Kobo-Style Datasets
#'
#' `df_summary()` inspects each column of a dataframe and produces a
#' summary containing the number of unique values, the list of unique values,
#' and a formatted statistics string.
#'
#' The function is especially designed for Kobo-based survey datasets where:
#' - numeric values may be encoded as character strings,
#' - logical, categorical, and numeric fields all require different summarization,
#' - some columns (e.g., `X_*`, `_other`, or survey matrix parents) must be excluded.
#'
#' The output returns one row per column that passes the filtering rules.
#'
#' @param df A dataframe or tibble containing survey data.
#' @param max_unique Integer. Maximum number of unique values allowed in a
#'   column to be included in the output. Columns with more than this number
#'   of unique values are skipped. Default is `20`.
#' @param top_reported_col Integer. Number of most frequently occurring
#'   categories to display when summarizing categorical variables.
#'   Default is `3`.
#'
#' @details
#'
#' The function performs the following steps:
#'
#' \enumerate{
#'   \item Removes unwanted columns:
#'         \itemize{
#'           \item Columns starting with `"X_"`.
#'           \item Columns ending with `"_other"`.
#'           \item Survey parent columns auto-detected using
#'                 \code{auto_detect_sm_parents()}.
#'         }
#'   \item Detects numeric-like columns: even if values are stored as
#'         characters (`"1"`, `"2"`, `"3"`), they are treated as numeric.
#'   \item Computes column-specific statistics:
#'         \itemize{
#'           \item \strong{Numeric}: mean, median, sd, and total non-missing N.
#'           \item \strong{Logical}: percentage breakdown of TRUE/FALSE.
#'           \item \strong{Categorical}: top X most frequent categories with
#'                 percentage and total N.
#'         }
#'   \item Returns a tibble with four columns:
#'         \describe{
#'           \item{\code{column}}{Column name.}
#'           \item{\code{n_unique}}{Number of unique (non-NA) values.}
#'           \item{\code{unique_values}}{Comma-separated list of unique values.}
#'           \item{\code{top_stats}}{Formatted statistics string.}
#'         }
#' }
#'
#' @return A tibble summarizing each eligible column in the input dataframe.
#'
#' @examples
#' \dontrun{
#' df <- tibble(
#'   gender = c("male", "female", "female"),
#'   age = c("20", "22", "25"),   # numeric-like characters
#'   consent = c(TRUE, FALSE, TRUE)
#' )
#'
#' df_summary(df)
#'
#' df_summary(df, max_unique = 10, top_reported_col = 2)
#' }
#'
#' @export



df_summary <- function(df, max_unique = 20,top_reported_col = 3) {

  cols <- df |>
    dplyr::select(
      -dplyr::starts_with("X_"),
      -dplyr::ends_with("_other")
    )


  parent <- auto_detect_sm_parents(cols)

  cols <- cols |> dplyr::select(-all_of(parent))

  purrr::map_df(names(cols), function(colname) {

    x_raw <- cols[[colname]]
    vals <- unique(x_raw)
    vals <- vals[!is.na(vals)]

    if (length(vals) > max_unique) return(NULL)

    # --- Detect numeric-like columns (common in Kobo) ---
    suppressWarnings({
      x_num <- as.numeric(x_raw)
    })

    is_num <- !all(is.na(x_num)) & !is.logical(x_raw)

    # ---- STAT CALCULATION ----
    stat_value <- if (is_num) {

      x2 <- x_num[!is.na(x_num)]

      sprintf(
        "mean=%.2f; median=%.2f; sd=%.2f || Total N- %d",
        mean(x2,na.rm=T), median(x2,na.rm = T), sd(x2,na.rm = T), length(x2)
      )

    } else if (is.logical(x_raw)) {

      tab <- table(x_raw, useNA = "no")
      total <- sum(tab)
      percent <- round(100 * tab / total, 1)

      paste0(
        paste(paste0(names(tab), " (", percent, "%)"), collapse = "; "),
        " || Total N- ", total
      )

    } else {
      # categorical
      tab <- sort(table(x_raw, useNA = "no"), decreasing = TRUE)
      total <- sum(tab)
      topx <- head(tab, top_reported_col)
      percent <- round(100 * topx / total, 0)

      paste0(
        paste(paste0(names(topx), " (", percent, "%)"), collapse = "; "),
        " || Total N- ", total
      )
    }


    data.frame(
      column = colname,
      n_unique = length(vals),
      unique_values = paste(vals, collapse = ", "),
      top_stats = stat_value
    )
  })
}
