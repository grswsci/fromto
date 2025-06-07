diff_ks_test <- function(fmla_use,
                         fmla_data,
                         group_use,
                         var_use) {
  if (!requireNamespace("moments", quietly = TRUE)) {
    install.packages("moments")
    library(moments)
  } else {
    library(moments)
  }
  options(warn = -1)
  # 获取响应变量和分组变量
  response <- fmla_data[[var_use]]
  group_var <- fmla_data[[group_use]]

  # 检查数据类型
  if (!is.numeric(response)) {
    stop("Response variable must be numeric.")
  }

  # 获取唯一分组
  unique_groups <- unique(group_var)
  n_groups <- length(unique_groups)

  # 结果容器
  result <- list()

  # 检查分组数量
  if (n_groups < 2) {
    stop("Number of groups must be at least 2.")
  } else if (n_groups == 2) {
    # 两样本KS检验
    group1_data <- response[group_var == unique_groups[1]]
    group2_data <- response[group_var == unique_groups[2]]

    # 移除NA值
    group1_data <- group1_data[!is.na(group1_data)]
    group2_data <- group2_data[!is.na(group2_data)]

    # 执行KS检验
    test_result <- tryCatch({
      ks.test(group1_data, group2_data)
    }, error = function(e) {
      message("KS test failed: ", e$message)
      NULL
    })

    result$test_used <- "Two-sample Kolmogorov-Smirnov test"
    result$comparison <- paste(unique_groups[1], "vs", unique_groups[2])
    result$group1_n <- length(group1_data)
    result$group2_n <- length(group2_data)

    # 计算分布统计量
    result$group1_stats <- list(
      mean = mean(group1_data, na.rm = TRUE),
      median = median(group1_data, na.rm = TRUE),
      sd = sd(group1_data, na.rm = TRUE),
      skewness = ifelse(require(moments, quietly = TRUE),
                        moments::skewness(group1_data), NA),
      kurtosis = ifelse(require(moments, quietly = TRUE),
                        moments::kurtosis(group1_data), NA)
    )

    result$group2_stats <- list(
      mean = mean(group2_data, na.rm = TRUE),
      median = median(group2_data, na.rm = TRUE),
      sd = sd(group2_data, na.rm = TRUE),
      skewness = ifelse(require(moments, quietly = TRUE),
                        moments::skewness(group2_data), NA),
      kurtosis = ifelse(require(moments, quietly = TRUE),
                        moments::kurtosis(group2_data), NA)
    )

  } else {
    # 多组比较：进行两两KS检验
    result$test_used <- "Multiple pairwise Kolmogorov-Smirnov tests"
    result$n_comparisons <- choose(n_groups, 2)

    # 存储两两比较结果
    pairwise_results <- list()
    p_values <- numeric()
    d_statistics <- numeric()
    comparisons <- character()

    comparison_idx <- 1
    for (i in 1:(n_groups-1)) {
      for (j in (i+1):n_groups) {
        group_i <- unique_groups[i]
        group_j <- unique_groups[j]

        group_i_data <- response[group_var == group_i]
        group_j_data <- response[group_var == group_j]

        # 移除NA值
        group_i_data <- group_i_data[!is.na(group_i_data)]
        group_j_data <- group_j_data[!is.na(group_j_data)]

        # 执行KS检验
        ks_result <- tryCatch({
          ks.test(group_i_data, group_j_data)
        }, error = function(e) {
          list(statistic = NA, p.value = NA, method = "KS test failed")
        })

        # 存储结果
        comparison_name <- paste(group_i, "vs", group_j)
        pairwise_results[[comparison_name]] <- ks_result
        p_values[comparison_idx] <- ks_result$p.value
        d_statistics[comparison_idx] <- ks_result$statistic
        comparisons[comparison_idx] <- comparison_name

        comparison_idx <- comparison_idx + 1
      }
    }

    result$pairwise_results <- pairwise_results
    result$p_values <- p_values
    result$d_statistics <- d_statistics
    result$comparisons <- comparisons

    # 多重比较校正
    if (length(p_values) > 1) {
      result$p_adjusted_bonferroni <- p.adjust(p_values, method = "bonferroni")
      result$p_adjusted_fdr <- p.adjust(p_values, method = "fdr")
    }

    # 汇总统计
    test_result <- list(
      statistic = max(d_statistics, na.rm = TRUE),
      p.value = min(p_values, na.rm = TRUE),
      method = "Multiple pairwise KS tests"
    )
  }

  # 提取统计量和p值
  if (!is.null(test_result)) {
    result$statistic <- test_result$statistic
    result$p.value <- test_result$p.value
    result$method <- test_result$method
    result$data.name <- deparse(fmla_use)

    # KS检验的效应量解释
    # D统计量本身就是效应量（最大分布差异）
    result$effect_size <- test_result$statistic
    result$effect_size_name <- "D statistic (max distribution difference)"

    # D统计量解释
    if (!is.na(result$effect_size)) {
      result$effect_size_interpretation <- ifelse(result$effect_size < 0.1, "negligible",
                                                  ifelse(result$effect_size < 0.3, "small",
                                                         ifelse(result$effect_size < 0.5, "medium", "large")))
    } else {
      result$effect_size_interpretation <- NA_character_
    }

  } else {
    result$statistic <- NA_real_
    result$p.value <- NA_real_
    result$method <- "KS test failed"
    result$data.name <- deparse(fmla_use)
    result$effect_size <- NA_real_
    result$effect_size_interpretation <- NA_character_
  }

  return(result)
}

# 结果解释函数
print_ks_result <- function(ks_result) {
  cat("=== Kolmogorov-Smirnov Test Results ===\n")
  cat("Method:", ks_result$method, "\n")

  if (ks_result$test_used == "Two-sample Kolmogorov-Smirnov test") {
    cat("Comparison:", ks_result$comparison, "\n")
    cat("Sample sizes:", ks_result$group1_n, "vs", ks_result$group2_n, "\n")
    cat("D statistic:", round(ks_result$statistic, 4), "\n")
    cat("P-value:", format.pval(ks_result$p.value), "\n")
    cat("Effect size:", round(ks_result$effect_size, 4),
        "(", ks_result$effect_size_interpretation, ")\n")

    cat("\nGroup 1 statistics:\n")
    print(lapply(ks_result$group1_stats, function(x) round(x, 4)))
    cat("\nGroup 2 statistics:\n")
    print(lapply(ks_result$group2_stats, function(x) round(x, 4)))

  } else {
    cat("Number of pairwise comparisons:", ks_result$n_comparisons, "\n")
    cat("Overall D statistic:", round(ks_result$statistic, 4), "\n")
    cat("Minimum p-value:", format.pval(ks_result$p.value), "\n")

    cat("\nPairwise results:\n")
    for (i in seq_along(ks_result$comparisons)) {
      cat(sprintf("%s: D=%.4f, p=%s\n",
                  ks_result$comparisons[i],
                  ks_result$d_statistics[i],
                  format.pval(ks_result$p_values[i])))
    }

    if (!is.null(ks_result$p_adjusted_fdr)) {
      cat("\nFDR-adjusted p-values:\n")
      for (i in seq_along(ks_result$comparisons)) {
        cat(sprintf("%s: p_adj=%s\n",
                    ks_result$comparisons[i],
                    format.pval(ks_result$p_adjusted_fdr[i])))
      }
    }
  }

  cat("\nInterpretation:")
  if (ks_result$p.value < 0.05) {
    cat(" Significant difference in distributions between groups.\n")
    cat(" The distributions differ in shape, location, or spread.\n")
  } else {
    cat(" No significant difference in distributions between groups.\n")
    cat(" The distributions are statistically similar.\n")
  }
}
