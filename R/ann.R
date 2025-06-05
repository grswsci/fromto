ann_smart = function(df1,
                     df2,
                     df1_var = "Ensembl_ID",
                     df2_var = "id",
                     add_var = "gene"){
  #合并
  df_merge = merge_col_add_v1(data1 = df1,
                              data2 = df2,
                              data1_var = df1_var,
                              data2_var_same_data1 = df2_var,
                              data2_var_add_data1 = add_var)
  #调整位置
  df_merge = df_merge[,c(1,ncol(df_merge),2:(ncol(df_merge)-1))]
  colnames(df_merge)[2] = "symbol"
  qs::qsave(df_merge,"df_merge_check.qs")
  df_merge = df_merge[,c(2:ncol(df_merge))]

  ensure_numeric_columns <- function(df, start_col = 2) {
    if (ncol(df) >= start_col) {
      cols_to_check <- start_col:ncol(df)
      if (!all(sapply(df[, cols_to_check], is.numeric))) {
        df <- numericize_by_column(df, column = cols_to_check)
      }
    }
    return(df)
  }

  df_merge <- ensure_numeric_columns(df_merge)

  return(df_merge)
}
