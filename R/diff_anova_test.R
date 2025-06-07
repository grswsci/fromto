diff_anova_test <- function(fmla_use, 
                            fmla_data, 
                            group_use, 
                            var_use) {
  
  # 提取响应变量和分组变量
  response <- eval(fmla_use, envir = fmla_data)
  group_var <- fmla_data[[group_use]]
  
  # 结果容器
  result <- list()
  
  # 正态性检验（按组）
  normality_check <- fmla_data %>%
    group_by(.[[group_use]]) %>%
    summarise(
      n = n(),
      p_value = if (n() >= 3) {
        tryCatch({
          shapiro.test(.[[var_use]])$p.value
        }, error = function(e) NA_real_)
      } else {
        NA_real_
      }
    )
  
  # 获取所有组的 p 值，并判断是否都显著（p > 0.05 表示服从正态）
  normal_p_values <- normality_check$p_value
  all_normal <- all(normal_p_values > 0.05, na.rm = TRUE)
  
  # 方差齐性检验（Levene 检验）
  var_homogeneity <- tryCatch({
    leveneTest(fmla_use, data = fmla_data)$`Pr(>F)`[1]
  }, error = function(e) {
    NA_real_
  })
  
  # 判断是否满足 ANOVA 的前提条件（正态 + 方差齐）
  meet_assumptions <- !is.na(var_homogeneity) && var_homogeneity > 0.05 && all_normal
  
  # 根据前提条件决定是否执行 ANOVA
  if (meet_assumptions) {
    anova_result <- aov(fmla_use, data = fmla_data)
    anova_summary <- summary(anova_result)
    
    result$anova_table <- anova_summary
    result$means <- model.tables(anova_result, type = "means", se = TRUE)
    result$assumptions <- list(
      normality = normality_check,
      homogeneity_p_value = var_homogeneity,
      meets_assumptions = meet_assumptions
    )
    
  } else {
    result$anova_table <- "Not applicable"
    result$means <- NA
    result$assumptions <- list(
      normality = normality_check,
      homogeneity_p_value = var_homogeneity,
      meets_assumptions = meet_assumptions
    )
  }
  
  return(result)
}