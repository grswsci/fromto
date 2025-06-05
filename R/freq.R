freq_count = function(df, var_use) {
  # 检查 var_use 是否存在
  if (!(var_use %in% names(df))) {
    stop(paste0("变量 '", var_use, "' 不在数据框的列中。"))
  }

  # 获取除 var_use 以外的所有列名
  other_vars = names(df)[names(df) != var_use]

  # 结果存储为命名向量
  result = numeric(length(other_vars))
  names(result) = other_vars

  # 遍历每个其他变量
  for (i in seq_along(other_vars)) {
    var = other_vars[i]

    # 去除 NA 和空字符串 ""，并提取非空的 var_use 值
    valid_values = df[[var_use]][!is.na(df[[var]]) & df[[var]] != ""]

    # 统计唯一值个数
    unique_count = length(unique(valid_values))

    # 存入结果
    result[i] = unique_count
  }
  result = data.frame(col_other = names(result),freq_count = result)
  rownames(result) = NULL
  return(result)
}
