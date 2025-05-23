#'
#' read all the sheets from a Excel file
#'
#'
#' @param dataset_path location of the dataset
#' @param remove_all_NA_col A logical parameter. TRUE (default) with remove columns with all NAs
#' @param data_type_fix will fix the data type.
#' @param na_strings only valid when data_type_fix is true
#' @param sheets Sheet to read. Must be the name of the sheet.Default is all, which will read all the available sheets in the excel file.
#' @param character_cols name of the columns which must read as characters even if its a non character column.
#' @param output_as_list TRUE to have a list with all the excel tabs. FALSE will be making a separate data frame for each tab.
#' @return All of the sheet as individual dataframe
#' @export
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom stringr str_replace_all
#' @importFrom dplyr select rename_at select_if


read_sheets<- function(dataset_path,
                           remove_all_NA_col = T,
                           data_type_fix =T,
                           na_strings = c("","NA","N/A"," "),
                           sheets = "all",
                           character_cols=NULL,
                           output_as_list =F){

  sheet_name <- getSheetNames(dataset_path)

  if(all(sheets!="all")) {

  sheets_notice <- sheets[!sheets %in% sheet_name]

  assertthat::assert_that(all(sheets %in% sheet_name),
                          msg= paste0("Error:", paste0(sheets_notice,collapse = ","), " was not found in the dataset"))
  }

  if(all(sheets=="all")){sheet_name <- sheet_name}

  if(all(sheets!="all")){sheet_name <- sheet_name[sheet_name %in% sheets]}


  df_all <- list()
  for (i in sheet_name) {
    assign(i,read.xlsx(dataset_path,sheet = i,na.strings = na_strings))
    df <- get(i)




    colnames(df) <- colnames(df) |> str_replace_all("/",".")
    df_st_with <- df |> select(starts_with("_")) |> names()
    df <- df |> rename_at(df_st_with,~paste0("X",.))



    if(data_type_fix == T){

      df <-  fix_data_type(df,
                           remove_all_NA_col = remove_all_NA_col,
                           na.string = na_strings,
                           character_cols)

        }


    ## remove all NA
    if(remove_all_NA_col == T){
      df <- df |> select_if(function(x) !all(is.na(x))) ### remove all NA columns
    }


    df_all[[i]] <-df
    assign(i,df)
  }
  if(output_as_list ==F){list2env(df_all,.GlobalEnv)}
  if(output_as_list ==T){df_all}

}




#'
#' This function provide formatted excel file
#'
#' @param write_list a list file (can be compile with single or multiple dataframe)
#' @param output_path Path for output file
#' @param header_front_color hexcode for header front color (default is white)
#' @param header_front_size Header front size (default is 12)
#' @param header_fill_color hexcode for header fill color (default is Red)
#' @param header_front Define the name of the front for header (default is Arial Narrow)
#' @param body_front Define the name of the front for body (default is Arial Narrow)
#' @param body_front_size Body front size (default is 11)
#' @param cols_for_color Column name in the dataframe which should be use for colorizing the cell. The default is null.
#' @return Nicely formatted excel file
#' @export
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData addFilter freezePane addStyle setRowHeights setColWidths saveWorkbook
#' @importFrom randomcoloR randomColor




write_formatted_excel <- function(write_list,output_path,
                        cols_for_color = NULL,
                        header_front_size = 12,
                        header_front_color= "#FFFFFF",
                        header_fill_color = "darkblue",
                        header_front = "Aptos",
                        body_front =  "Aptos",
                        body_front_size = 11){


  headerStyle <- createStyle(fontSize = header_front_size,
                             fontColour = header_front_color,
                             halign = "center",
                             valign = "center",
                             fontName =header_front,
                             textDecoration = "bold",
                             fgFill = header_fill_color,
                             border = "TopBottomLeftRight ",
                             borderColour = "#fafafa",
                             wrapText = T)

  bodyStyle <- createStyle(fontSize = body_front_size,
                           fontName = body_front,
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")




  wb <- createWorkbook()


  number_of_sheet <- length(write_list)

  for(i in 1:number_of_sheet ){

    dataset_name <- names(write_list[i])
    dataset <-  get(dataset_name)


    addWorksheet(wb,dataset_name)
    writeData(wb, sheet = i,dataset, rowNames = F)
    addFilter(wb,sheet =  i, row = 1, cols = 1:ncol(dataset))
    freezePane(wb, sheet = i, firstCol = TRUE, firstRow = T)
    addStyle(wb, sheet = i, headerStyle, rows = 1, cols = 1:ncol(dataset), gridExpand = TRUE)
    addStyle(wb, sheet = i, bodyStyle, rows = 1:nrow(dataset)+1, cols = 1:ncol(dataset), gridExpand = TRUE)
    setColWidths(wb, i, cols = 1:ncol(dataset), widths = 25)
    setRowHeights(wb, i, 1, 20)

    if(!is.null(cols_for_color)){
      u = unique(dataset[[cols_for_color]])

      for(x in u){
        y = which(dataset[[cols_for_color]] == x)

        random.color <- randomColor(1, luminosity = "light")

        style <- createStyle(fgFill=random.color,
                             fontSize = body_front_size,
                             fontName = body_front,
                             border = "TopBottomLeftRight ",
                             borderColour = "#4F81BD",
                             valign = "center",
                             halign = "left")


        addStyle(wb, sheet = i, style, rows = y+1, cols = 1:ncol(dataset), gridExpand = TRUE)



      }


    }

  }


  saveWorkbook(wb, file = output_path, overwrite = TRUE)

}

