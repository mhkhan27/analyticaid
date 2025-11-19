#' Read and Clean Multiple Excel Sheets with Automated Type Fixing
#'
#' `read_sheets()` reads one or more sheets from an Excel file and applies a set
#' of standard cleaning operations such as:
#' - renaming problematic column names,
#' - converting underscores-prefixed columns (e.g., Kobo) to safe names,
#' - fixing data types using `fix_data_type()`,
#' - removing columns that contain only `NA`.
#'
#' The function supports selecting sheets either by **name** (e.g., `"hh"`)
#' or by **index** (e.g., `1`), making it versatile for different workflows.
#'
#' @param dataset_path Character string. Path to the Excel file.
#' @param sheets Either `"all"` (default), a **character vector** of sheet names,
#'   or a **numeric vector** of sheet indices.
#'   - Example: `sheets = "hh"`
#'   - Example: `sheets = 1`
#'   The function ensures proper validation and resolves numeric indices to
#'   sheet names internally.
#' @param remove_all_NA_col Logical. If `TRUE`, columns with all `NA` values are
#'   removed after reading. Default is `TRUE`.
#' @param data_type_fix Logical. If `TRUE`, the function applies
#'   \code{fix_data_type()} to standardize variable types. Default is `TRUE`.
#' @param na_strings Character vector. Strings to be treated as missing values
#'   during sheet import. Default is `c("", "NA", "N/A", " ")`.
#' @param character_cols Optional character vector. Column names that should
#'   always be treated as character even during type fixing.
#'
#' @details
#' The function performs several cleaning steps for each sheet:
#'
#' \enumerate{
#'   \item Reads the sheet using `read.xlsx()` from `openxlsx` with custom NA handling.
#'   \item Replaces slashes (`/`) in column names with dots (`.`).
#'   \item Detects Kobo-style underscore-prefixed columns (e.g., `"_id"`)
#'         and renames them to safe names (`"X_id"`).
#'   \item Optionally applies `fix_data_type()` for automatic type coercion.
#'   \item Optionally removes columns with all missing values.
#' }
#'
#' Sheets can be selected in three different ways:
#' \itemize{
#'   \item `"all"` — read all sheets in the file.
#'   \item By name — e.g., `sheets = c("hh", "indv")`.
#'   \item By index — e.g., `sheets = 1:2`.
#' }
#'
#' @return
#' - If **one** sheet is selected, the function returns a **data.frame**.
#' - If **multiple** sheets are selected, the function returns a **named list**
#'   of cleaned data.frames (list element names match the sheet names).
#'
#' @examples
#' \dontrun{
#' # Read all sheets
#' read_sheets("survey.xlsx")
#'
#' # Read one sheet by name
#' hh <- read_sheets("survey.xlsx", sheets = "hh")
#'
#' # Read one sheet by index
#' hh <- read_sheets("survey.xlsx", sheets = 1)
#'
#' # Read specific sheets by name
#' data_list <- read_sheets("survey.xlsx", sheets = c("hh", "indv"))
#'
#' # Disable type fixing
#' read_sheets("survey.xlsx", data_type_fix = FALSE)
#' }
#'
#' @seealso
#' \code{\link{fix_data_type}} for automated data type cleaning.
#'
#' @export


read_sheets <- function(dataset_path,
                        sheets = "all",
                        remove_all_NA_col = TRUE,
                        data_type_fix = TRUE,
                        na_strings = c("", "NA", "N/A", " "),
                        character_cols = NULL) {
  # Get sheet names
  sheet_name <- openxlsx::getSheetNames(dataset_path)


  # If user did NOT request all sheets
  if (!identical(sheets, "all")) {

    # --- Case 1: User provided numeric index ---
    if (is.numeric(sheets)) {

      # Validate index
      assertthat::assert_that(
        all(sheets >= 1 & sheets <= length(sheet_name)),
        msg = "Sheet index out of range."
      )

      sheets <- sheet_name[sheets]   # convert index → name
    }

    # --- Case 2: User provided name ---
    if (is.character(sheets)) {

      missing <- dplyr::setdiff(sheets, sheet_name)
      assertthat::assert_that(
        length(missing) == 0,
        msg = paste0("Sheet(s) not found: ", paste(missing, collapse = ", "))
      )
    }

    # Final sheet list to read
    sheet_name <- sheets
  }

  df_all <- list()

  for (sheet in sheet_name) {

    # Read sheet
    df <- openxlsx::read.xlsx(
      dataset_path,
      sheet = sheet,
      na.strings = na_strings,
      detectDates = FALSE
    )

    # Clean column names
    colnames(df) <- colnames(df) |> stringr::str_replace_all("/", ".")

    # Detect & rename columns beginning with "_"
    cols_starting <- names(df)[startsWith(names(df), "_")]
    if (length(cols_starting) > 0) {
      df <- df |> dplyr::rename_with(~ paste0("X", .x), dplyr::all_of(cols_starting))
    }

    # Fix data types if required
    if (isTRUE(data_type_fix)) {
      df <- fix_data_type(
        df,
        remove_all_NA_col = remove_all_NA_col,
        na.string = na_strings,
        character_cols = character_cols
      )
    }

    # Remove columns that are all NA
    if (isTRUE(remove_all_NA_col)) {
      df <- df |> dplyr::select(dplyr::where(~ !all(is.na(.x))))
    }

    df_all[[sheet]] <- df
  }

  # Return single sheet as data.frame, multiple sheets as named list
  if (length(df_all) == 1) {
    return(df_all[[1]])
  } else {
    return(df_all)
  }
}

