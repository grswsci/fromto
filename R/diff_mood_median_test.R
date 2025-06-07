diff_mood_median_test <- function(fmla_use, 
                                  fmla_data, 
                                  group_use, 
                                  var_use) {
  
  # 获取响应变量和分组变量
  # 直接使用变量名获取数据，与其他函数保持一致
  response <- fmla_data[[var_use]]
  group_var <- fmla_data[[group_use]]
  
  # 检查数据类型
  if (!is.numeric(response)) {
    stop("Response variable must be numeric.")
  }
  
  # 获取唯一分组数量
  unique_groups <- unique(group_var)
  n_groups <- length(unique_groups)
  
  # 结果容器
  result <- list()
  
  # 检查分组数量
  if (n_groups < 2) {
    stop("Number of groups must be at least 2.")
  }
  
  # 计算总体中位数
  overall_median <- median(response, na.rm = TRUE)
  
  # 创建列联表
  # 行：分组，列：高于/低于等于中位数
  contingency_table <- matrix(0, nrow = n_groups, ncol = 2)
  rownames(contingency_table) <- unique_groups
  colnames(contingency_table) <- c("Above_Median", "At_Or_Below_Median")
  
  # 填充列联表
  for (i in seq_along(unique_groups)) {
    group_data <- response[group_var == unique_groups[i]]
    group_data <- group_data[!is.na(group_data)]  # 移除NA值
    
    above_median <- sum(group_data > overall_median)
    at_or_below_median <- sum(group_data <= overall_median)
    
    contingency_table[i, 1] <- above_median
    contingency_table[i, 2] <- at_or_below_median
  }
  
  # 执行卡方检验
  test_result <- tryCatch({
    chisq.test(contingency_table)
  }, error = function(e) {
    message("Chi-square test failed: ", e$message)
    NULL
  })
  
  # 计算额外统计信息
  result$test_used <- "Mood's median test"
  result$overall_median <- overall_median
  result$contingency_table <- contingency_table
  result$n_groups <- n_groups
  
  # 计算各组中位数
  group_medians <- sapply(unique_groups, function(g) {
    group_data <- response[group_var == g]
    median(group_data, na.rm = TRUE)
  })
  names(group_medians) <- unique_groups
  result$group_medians <- group_medians
  
  # 提取卡方检验结果
  if (!is.null(test_result)) {
    result$statistic <- test_result$statistic
    result$p.value <- test_result$p.value
    result$method <- paste("Mood's median test (Chi-square test on", n_groups, "groups)")
    result$data.name <- deparse(fmla_use)
    result$degrees_of_freedom <- test_result$parameter
    result$expected_frequencies <- test_result$expected
    result$residuals <- test_result$residuals
  } else {
    result$statistic <- NA_real_
    result$p.value <- NA_real_
    result$method <- "Mood's median test failed"
    result$data.name <- deparse(fmla_use)
    result$degrees_of_freedom <- NA_integer_
    result$expected_frequencies <- NA
    result$residuals <- NA
  }
  
  # 添加效应量计算（Cramér's V）
  if (!is.null(test_result) && !is.na(result$statistic)) {
    n_total <- sum(contingency_table)
    cramers_v <- sqrt(result$statistic / (n_total * (min(dim(contingency_table)) - 1)))
    result$effect_size <- cramers_v
    result$effect_size_interpretation <- ifelse(cramers_v < 0.1, "negligible",
                                                ifelse(cramers_v < 0.3, "small",
                                                       ifelse(cramers_v < 0.5, "medium", "large")))
  } else {
    result$effect_size <- NA_real_
    result$effect_size_interpretation <- NA_character_
  }
  
  return(result)
}

# 使用示例和结果解释函数
print_mood_median_result <- function(mood_result) {
  cat("=== Mood's Median Test Results ===\n")
  cat("Method:", mood_result$method, "\n")
  cat("Overall median:", round(mood_result$overall_median, 4), "\n")
  cat("Chi-square statistic:", round(mood_result$statistic, 4), "\n")
  cat("Degrees of freedom:", mood_result$degrees_of_freedom, "\n")
  cat("P-value:", format.pval(mood_result$p.value), "\n")
  cat("Effect size (Cramér's V):", round(mood_result$effect_size, 4), 
      "(", mood_result$effect_size_interpretation, ")\n")
  
  cat("\nGroup medians:\n")
  print(round(mood_result$group_medians, 4))
  
  cat("\nContingency table:\n")
  print(mood_result$contingency_table)
  
  cat("\nInterpretation:")
  if (mood_result$p.value < 0.05) {
    cat(" Significant difference in medians between groups.\n")
  } else {
    cat(" No significant difference in medians between groups.\n")
  }
}