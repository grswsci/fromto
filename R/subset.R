subset_smart_v1 = function(df, method_use = "max"){
  library(dplyr)

  # 检查第一列是否为基因名列
  if(!"symbol" %in% colnames(df)){
    stop("数据框必须包含名为'symbol'的基因名列")
  }

  if(method_use == "max"){
    # 纯dplyr优化版本
    numeric_cols <- names(select(df, where(is.numeric)))

    df_filtered <- df %>%
      # 使用向量化操作计算行均值
      mutate(mean_expression = rowMeans(select(., all_of(numeric_cols)), na.rm = TRUE)) %>%
      # 一次性完成分组和筛选
      group_by(symbol) %>%
      filter(mean_expression == max(mean_expression, na.rm = TRUE)) %>%
      slice(1) %>%  # 处理并列情况，只保留第一个
      ungroup() %>%
      select(-mean_expression)

  } else if(method_use == "mean"){
    df_filtered <- df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = 'drop')
    df_filtered = df_filtered[rowSums(is.na(df_filtered)) != (ncol(df_filtered) - 1), ]

  } else if(method_use == "median"){
    df_filtered <- df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)),
                .groups = 'drop')
    df_filtered = df_filtered[rowSums(is.na(df_filtered)) != (ncol(df_filtered) - 1), ]

  } else if(method_use == "sum"){
    df_filtered <- df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
                .groups = 'drop')
    df_filtered = df_filtered[rowSums(is.na(df_filtered)) != (ncol(df_filtered) - 1), ]

  } else {
    stop("method_use 必须是以下之一: 'max', 'mean', 'median', 'sum'")
  }

  return(df_filtered)

}
subset_smart = function(df, method_use = "max"){
  library(dplyr)

  # 检查第一列是否为基因名列
  if(!"symbol" %in% colnames(df)){
    stop("数据框必须包含名为'symbol'的基因名列")
  }

  if(method_use == "max"){
    # 原有的最大值方法
    df_filtered = df %>%
      # 计算每行的平均表达量（除了第一列基因名）
      rowwise() %>%
      mutate(mean_expression = mean(c_across(-symbol), na.rm = TRUE)) %>%
      ungroup() %>%
      # 按基因名分组，保留平均表达量最高的转录本
      group_by(symbol) %>%
      slice_max(order_by = mean_expression, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      # 移除辅助列
      select(-mean_expression)

  } else if(method_use == "mean"){
    # 平均值方法：对重复基因的表达值取平均
    df_filtered = df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = 'drop')

  } else if(method_use == "median"){
    # 中位数方法：对重复基因的表达值取中位数
    df_filtered = df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)),
                .groups = 'drop')

  } else if(method_use == "sum"){
    # 求和方法：对重复基因的表达值求和（可选）
    df_filtered = df %>%
      group_by(symbol) %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
                .groups = 'drop')

  } else {
    stop("method_use 必须是以下之一: 'max', 'mean', 'median', 'sum'")
  }

  return(df_filtered)
}

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
