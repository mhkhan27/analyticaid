#' Write Formatted Excel Workbook With Testable Style Logging
#'
#' `write_formatted_excel()` writes one or more data frames to an Excel file
#' with customizable header styling, body styling, and optional row coloring
#' based on a grouping variable.
#'
#' In addition to producing a formatted Excel workbook, the function also
#' returns a *testable style log* that records every styling action applied:
#' - which sheet was styled
#' - the type of style (`"header"`, `"body"`, or `"group"`)
#' - the grouping value (for group coloring)
#' - the exact rows and columns affected
#'
#' This makes the function fully unit-testable using `testthat`, without
#' depending on the internal XML/styling mechanics of the Excel file or the
#' version of `{openxlsx}`.
#'
#'
#' @param write_list A named list of data frames.
#'   Each list element becomes one sheet in the Excel file, and its name is used
#'   as the sheet name.
#'
#' @param output_path File path where the Excel workbook will be saved.
#'
#' @param cols_for_color Optional. A single column name in the data frame whose
#'   unique values will be used to apply row-level color styling.
#'   If `NULL` (default), no group coloring is applied.
#'
#' @param header_front_size Numeric. Font size for the header row. Default: `12`.
#'
#' @param header_front_color Character. Text color for the header row. Default: `"#FFFFFF"`.
#'
#' @param header_fill_color Character. Background fill color for the header row.
#'   Default: `"darkblue"`.
#'
#' @param header_front Character. Font family for the header row. Default: `"Aptos"`.
#'
#' @param body_front Character. Font family for all body rows. Default: `"Aptos"`.
#'
#' @param body_front_size Numeric. Font size for body text. Default: `11`.
#'
#' @details
#'
#' The function applies three categories of styling:
#'
#' **1. Header Styling**
#' A bold, centered header row with customizable background and font options.
#'
#' **2. Body Styling**
#' Standard body row styling applied to all non-header rows.
#'
#' **3. Group Row Coloring (Optional)**
#' If `cols_for_color` is provided, rows sharing the same value in that column
#' receive a unique background color (generated using `randomcoloR::randomColor()`).
#'
#' All style operations are logged in a tibble called `style_log`, containing:
#'
#' \itemize{
#'   \item `sheet` — Sheet name
#'   \item `style_type` — `"header"`, `"body"`, or `"group"`
#'   \item `group_value` — Group identifier for `"group"` styles
#'   \item `rows` — List column containing styled row indices
#'   \item `cols` — List column containing styled column indices
#' }
#'
#' This log allows unit tests to verify styling logic without reading Excel style XML.
#'
#'
#' @return A list of two elements:
#' \describe{
#'   \item{\code{wb}}{The generated `openxlsx` workbook object.}
#'   \item{\code{style_log}}{A tibble logging every style applied, suitable for `testthat` verification.}
#' }
#'
#'
#' @examples
#' \dontrun{
#'
#' # Example data
#' df <- data.frame(
#'   group = c("A", "B", "A"),
#'   value = c(5, 10, 15)
#' )
#'
#' # Write styled workbook
#' out <- write_formatted_excel(
#'   write_list = list(sheet1 = df),
#'   output_path = "styled.xlsx",
#'   cols_for_color = "group"
#' )
#'
#' # Examine style log
#' out$style_log
#'
#' # Example testthat usage:
#' # expect_equal(out$style_log$style_type[1], "header")
#' }
#'
#' @seealso
#' \code{\link[openxlsx]{addStyle}}, \code{\link[randomcoloR]{randomColor}}
#'
#' @export



write_formatted_excel <- function(write_list, output_path,
                                  cols_for_color = NULL,
                                  header_front_size = 12,
                                  header_front_color = "#FFFFFF",
                                  header_fill_color = "darkblue",
                                  header_front = "Aptos",
                                  body_front = "Aptos",
                                  body_front_size = 11) {

  stopifnot(is.list(write_list))

  # This will store info about every style applied
  style_log <- tibble::tibble(
    sheet = character(),
    style_type = character(),   # header / body / group
    group_value = character(),
    rows = list(),
    cols = list()
  )

  # Workbook
  wb <- openxlsx::createWorkbook()

  for (i in seq_along(write_list)) {

    sheet_name <- names(write_list)[i]
    dataset <- write_list[[i]]

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = i, dataset, rowNames = FALSE)
    openxlsx::addFilter(wb, sheet = i, rows = 1, cols = 1:ncol(dataset))
    openxlsx::freezePane(wb, sheet = i, firstCol = TRUE, firstRow = TRUE)

    # ----- HEADER STYLE -----
    headerStyle <- openxlsx::createStyle(
      fontSize = header_front_size,
      fontColour = header_front_color,
      halign = "center",
      valign = "center",
      fontName = header_front,
      textDecoration = "bold",
      fgFill = header_fill_color,
      border = "TopBottomLeftRight",
      borderColour = "#fafafa",
      wrapText = TRUE
    )

    openxlsx::addStyle(wb, i, headerStyle,
                       rows = 1, cols = 1:ncol(dataset), gridExpand = TRUE)

    style_log <- dplyr::add_row(
      style_log,
      sheet = sheet_name,
      style_type = "header",
      group_value = NA_character_,
      rows = list(1),
      cols = list(1:ncol(dataset))
    )

    # ----- BODY STYLE -----
    bodyStyle <- openxlsx::createStyle(
      fontSize = body_front_size,
      fontName = body_front,
      border = "TopBottomLeftRight",
      borderColour = "#4F81BD",
      valign = "center",
      halign = "left"
    )

    openxlsx::addStyle(wb, i, bodyStyle,
                       rows = 2:(nrow(dataset)+1),
                       cols = 1:ncol(dataset),
                       gridExpand = TRUE)

    style_log <- dplyr::add_row(
      style_log,
      sheet = sheet_name,
      style_type = "body",
      group_value = NA_character_,
      rows = list(2:(nrow(dataset)+1)),
      cols = list(1:ncol(dataset))
    )

    # ----- GROUP COLORING (TESTABLE) -----
    if (!is.null(cols_for_color)) {

      uniq_vals <- unique(dataset[[cols_for_color]])

      for (g in uniq_vals) {

        rows_to_style <- which(dataset[[cols_for_color]] == g) + 1
        fill <- randomcoloR::randomColor(1, luminosity = "light")

        style <- openxlsx::createStyle(
          fgFill = fill,
          fontSize = body_front_size,
          fontName = body_front,
          border = "TopBottomLeftRight",
          borderColour = "#4F81BD",
          valign = "center",
          halign = "left"
        )

        openxlsx::addStyle(
          wb, sheet = i, style,
          rows = rows_to_style,
          cols = 1:ncol(dataset),
          gridExpand = TRUE
        )

        # LOG FOR TESTING
        style_log <- dplyr::add_row(
          style_log,
          sheet = sheet_name,
          style_type = "group",
          group_value = as.character(g),
          rows = list(rows_to_style),
          cols = list(1:ncol(dataset))
        )
      }
    }
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

  # Return BOTH workbook and style log (this is what makes it testable)
  return(list(
    wb = wb,
    style_log = style_log
  ))
}
