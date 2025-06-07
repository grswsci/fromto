diff_rank_sum_test <- function(fmla_use, 
                               fmla_data, 
                               group_use, 
                               var_use) {
  
  # 获取响应变量和分组变量
  response <- eval(fmla_use, envir = fmla_data)
  group_var <- fmla_data[[group_use]]
  
  # 获取唯一分组数量
  n_groups <- length(unique(group_var))
  
  # 结果容器
  result <- list()
  
  # 判断分组数量并选择合适的检验方法
  if (n_groups == 2) {
    # 两样本 Wilcoxon 秩和检验（即 Mann-Whitney U Test）
    test_result <- tryCatch({
      wilcox.test(fmla_use, data = fmla_data)
    }, error = function(e) {
      message("Wilcoxon test failed: ", e$message)
      NULL
    })
    
    result$test_used <- "Wilcoxon rank sum test"
    
  } else if (n_groups > 2) {
    # Kruskal-Wallis 检验
    test_result <- tryCatch({
      kruskal.test(fmla_use, data = fmla_data)
    }, error = function(e) {
      message("Kruskal-Wallis test failed: ", e$message)
      NULL
    })
    
    result$test_used <- "Kruskal-Wallis test"
    
  } else {
    stop("Number of groups must be at least 2.")
  }
  
  # 提取统计量和 p 值
  if (!is.null(test_result)) {
    result$statistic <- test_result$statistic
    result$p.value <- test_result$p.value
    result$method <- test_result$method
    result$data.name <- test_result$data.name
  } else {
    result$statistic <- NA_real_
    result$p.value <- NA_real_
    result$method <- "Test failed or not applicable"
    result$data.name <- deparse(fmla_use)
  }
  
  return(result)
}