diff_permutation_test <- function(fmla_use, 
                                  fmla_data, 
                                  group_use, 
                                  var_use,
                                  n_permutations = 10000) {
  
  # 获取响应变量和分组变量
  response <- eval(fmla_use[[2]], envir = fmla_data)  # 提取公式左侧的响应变量
  group_var <- fmla_data[[group_use]]
  
  # 检查数据完整性
  if (length(response) != length(group_var)) {
    stop("Response variable and grouping variable must have the same length")
  }
  
  # 移除缺失值
  complete_cases <- !is.na(response) & !is.na(group_var)
  response <- response[complete_cases]
  group_var <- group_var[complete_cases]
  
  if (length(response) == 0) {
    stop("No complete cases found")
  }
  
  # 获取唯一分组数量
  unique_groups <- unique(group_var)
  n_groups <- length(unique_groups)
  
  # 结果容器
  result <- list()
  
  # 判断分组数量并选择合适的检验方法
  if (n_groups == 2) {
    # 两样本置换检验
    result <- two_sample_permutation_test(response, group_var, unique_groups, n_permutations)
    result$test_used <- "Two-sample permutation test"
    
  } else if (n_groups > 2) {
    # 多样本置换检验（类似于Kruskal-Wallis的置换版本）
    result <- multi_sample_permutation_test(response, group_var, unique_groups, n_permutations)
    result$test_used <- "Multi-sample permutation test"
    
  } else {
    stop("Number of groups must be at least 2.")
  }
  
  # 添加公共信息
  result$method <- paste0(result$test_used, " (", n_permutations, " permutations)")
  result$data.name <- paste(deparse(fmla_use[[2]]), "by", group_use)
  result$n_permutations <- n_permutations
  result$n_groups <- n_groups
  result$group_sizes <- table(group_var)
  
  return(result)
}

# 两样本置换检验辅助函数
two_sample_permutation_test <- function(response, group_var, unique_groups, n_permutations) {
  
  group1 <- response[group_var == unique_groups[1]]
  group2 <- response[group_var == unique_groups[2]]
  
  # 观察到的检验统计量（均值差）
  observed_diff <- mean(group1) - mean(group2)
  
  # 置换检验
  combined_data <- c(group1, group2)
  n1 <- length(group1)
  n_total <- length(combined_data)
  
  # 生成置换分布
  permuted_diffs <- replicate(n_permutations, {
    # 随机重新分配组别
    permuted_indices <- sample(n_total, n1)
    perm_group1 <- combined_data[permuted_indices]
    perm_group2 <- combined_data[-permuted_indices]
    
    mean(perm_group1) - mean(perm_group2)
  })
  
  # 计算p值（双尾检验）
  p_value <- mean(abs(permuted_diffs) >= abs(observed_diff))
  
  # 也可以使用秩和统计量作为替代
  observed_rank_sum <- sum(rank(combined_data)[1:n1])
  
  return(list(
    statistic = observed_diff,
    statistic_name = "Mean difference",
    rank_sum_statistic = observed_rank_sum,
    p.value = p_value,
    permutation_distribution = permuted_diffs,
    observed_statistic = observed_diff
  ))
}

# 多样本置换检验辅助函数
multi_sample_permutation_test <- function(response, group_var, unique_groups, n_permutations) {
  
  # 观察到的检验统计量（组间方差与组内方差的比值，类似F统计量）
  observed_statistic <- calculate_f_like_statistic(response, group_var)
  
  # 置换检验
  permuted_statistics <- replicate(n_permutations, {
    # 随机打乱分组标签
    permuted_groups <- sample(group_var)
    calculate_f_like_statistic(response, permuted_groups)
  })
  
  # 计算p值
  p_value <- mean(permuted_statistics >= observed_statistic)
  
  return(list(
    statistic = observed_statistic,
    statistic_name = "F-like statistic",
    p.value = p_value,
    permutation_distribution = permuted_statistics,
    observed_statistic = observed_statistic
  ))
}

# 计算类似F统计量的函数
calculate_f_like_statistic <- function(response, group_var) {
  
  # 总体均值
  grand_mean <- mean(response)
  
  # 组均值
  group_means <- tapply(response, group_var, mean)
  group_sizes <- table(group_var)
  
  # 组间平方和
  between_ss <- sum(group_sizes * (group_means - grand_mean)^2)
  
  # 组内平方和
  within_ss <- sum(tapply(response, group_var, function(x) sum((x - mean(x))^2)))
  
  # 自由度
  df_between <- length(unique(group_var)) - 1
  df_within <- length(response) - length(unique(group_var))
  
  # F-like统计量
  if (df_within > 0 && within_ss > 0) {
    f_statistic <- (between_ss / df_between) / (within_ss / df_within)
  } else {
    f_statistic <- Inf
  }
  
  return(f_statistic)
}

