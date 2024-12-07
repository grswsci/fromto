#' @title subset_row1
#' @description subset row1
#' @param df data frame
#' @param col_name col name
#' @param row_var row varible
#' @return df data frame
subset_row1 = function(df,
                       col_name,
                       row_var) {
  df = clinical[which(df[,col_name] %in% c(row_var)),,drop = FALSE]
  return(df)
}
#' @title ifelse_row_1
#' @description ifelse_row
#' @param df data frame
#' @param col_add col add name
#' @param row_var row varible
#' @param row_var_if row var if
#' @param row_var_else row var else
#' @return df data frame
ifelse_row_1 = function(df,
                        col_add,
                        col_name,
                        row_var,
                        row_var_if,
                        row_var_else) {
  df[,col_add] = ifelse(df[,col_name] == row_var,row_var_if,row_var_else)
  return(df)
}
#' @title ifelse_row_2
#' @description ifelse_row_2
#' @param df data frame
#' @param col_add col add name
#' @param row_var row varible
#' @param row_var_if row var if
#' @param row_var2 row varible2
#' @param row_var2_if row var2 if
#' @param row_var2_else row var2 else
#' @return df data frame
ifelse_row_2 = function(df,
                        col_add,
                        col_name,
                        row_var,
                        row_var_if,
                        row_var2,
                        row_var2_if,
                        row_var2_else) {
  df[,col_add] = ifelse(df[,col_name] == row_var,row_var_if,
                        ifelse(df[,col_name] == row_var2,row_var2_if,row_var2_else)
                        )
  return(df)
}
#' @title ifelse_row_3
#' @description ifelse_row_3
#' @param df data frame
#' @param col_add col add name
#' @param row_var row varible
#' @param row_var_if row var if
#' @param row_var2 row varible2
#' @param row_var2_if row var2 if
#' @param row_var3 row varible3
#' @param row_var3_if row var3 if
#' @param row_var3_else row var2 else
#' @return df data frame
ifelse_row_3 = function(df,
                        col_add,
                        col_name,
                        row_var,
                        row_var_if,
                        row_var2,
                        row_var2_if,
                        row_var3,
                        row_var3_if,
                        row_var3_else) {
  df[,col_add] = ifelse(df[,col_name] == row_var,row_var_if,
                        ifelse(df[,col_name] == row_var2,row_var2_if,
                               ifelse(df[,col_name] == row_var3,row_var3_if,row_var3_else))
  )
  return(df)
}
