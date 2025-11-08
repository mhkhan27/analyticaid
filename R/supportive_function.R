#'
#' Left join in list
#'
#'
#' @param list list of dataframes
#' @param by primary key that exists in all element
#' @return outcome of left join based on the primary key
#' @export
#'



left_join_for_list <- function(list,by){
  Reduce(function(...) merge(..., by=by, all.x=TRUE), list)
}


#' @param x st object
#' @param y st object
#' @return distance from x to nearest y
#' @export
#'


nearest_feature<- function(x, y){
  y_index <- st_nearest_feature(x= x, y= y)
  distance <- st_distance(x= x, y= y[y_index,], by_element=T)
  x %>%
    cbind(y_index= y_index, distance= distance)
}



# Remove columns with all NA values

remove_all_na_cols <- function(df){
 return(df[, colSums(is.na(df)) < nrow(df)])
}


# Remove rows with all NA values
remove_all_na_rows <- function(df){
return(df_cleaned[rowSums(is.na(df_cleaned)) < ncol(df_cleaned), ])
}


