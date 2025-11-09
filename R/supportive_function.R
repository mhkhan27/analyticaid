#' Left join all data frames in a list
#'
#' Performs a sequential left join on all elements of a list of data frames,
#' using a common key column present in all elements.
#'
#' @param list A list of data frames to be joined.
#' @param by Character vector of column name(s) to join by. Must exist in all elements.
#'
#' @return A data frame resulting from left-joining all list elements by the specified key.
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(id = 1:3, x = c("A", "B", "C"))
#' df2 <- data.frame(id = 1:3, y = c(10, 20, 30))
#' df3 <- data.frame(id = 2:3, z = c(TRUE, FALSE))
#' list_of_dfs <- list(df1, df2, df3)
#' left_join_for_list(list_of_dfs, by = "id")
#' }
#'
#' @export
#' @importFrom stats setNames
#'
left_join_for_list <- function(list,by){
  Reduce(function(...) merge(..., by=by, all.x=TRUE), list)
}


#' Find nearest spatial feature
#'
#' For each feature in `x`, finds the nearest feature in `y`
#' and computes the distance to it.
#'
#' @param x An `sf` or `sfc` object representing origin features.
#' @param y An `sf` or `sfc` object representing target features.
#'
#' @return An `sf` object identical to `x`, with two additional columns:
#' \describe{
#'   \item{y_index}{Index of the nearest feature in `y` for each element of `x`.}
#'   \item{distance}{Distance from each feature in `x` to its nearest `y`.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' x <- st_as_sf(data.frame(lon = 1:3, lat = 1:3), coords = c("lon", "lat"), crs = 4326)
#' y <- st_as_sf(data.frame(lon = c(2, 4), lat = c(2, 4)), coords = c("lon", "lat"), crs = 4326)
#' nearest_feature(x, y)
#' }
#'
#' @export
#' @importFrom sf st_nearest_feature st_distance

nearest_feature<- function(x, y){
  y_index <- st_nearest_feature(x= x, y= y)
  distance <- st_distance(x= x, y= y[y_index,], by_element=T)
  x |>
    cbind(y_index= y_index, distance= distance)
}


#' Remove columns containing only NA values
#'
#' Removes all columns from a data frame where every value is `NA`.
#'
#' @param df A data frame or tibble.
#'
#' @return A data frame with columns containing all `NA` values removed.
#'
#' @examples
#' df <- data.frame(a = c(1, NA), b = c(NA, NA))
#' remove_all_na_cols(df)
#'
#' @export

remove_all_na_cols <- function(df){
 return(df[, colSums(is.na(df)) < nrow(df)])
}



#' Remove rows containing only NA values
#'
#' Removes all rows from a data frame where every value is `NA`.
#'
#' @param df A data frame or tibble.
#'
#' @return A data frame with rows containing all `NA` values removed.
#'
#' @examples
#' df <- data.frame(a = c(1, NA), b = c(NA, NA))
#' remove_all_na_rows(df)
#'
#' @export
# Remove rows with all NA values
remove_all_na_rows <- function(df){
return(df[rowSums(is.na(df)) < ncol(df), ])
}


