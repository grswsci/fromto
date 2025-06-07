diff_t_test = function(fmla_use,
                       fmla_data,
                       group_use,
                       var_use){
  # 方差齐性检验
  var_test_check = tryCatch({
    var.test(fmla_use, data = fmla_data)
  }, error = function(e) NULL)
  
  # 正态性检验（分组）
  normality_check = fmla_data %>%
    group_by(.[[group_use]]) %>%
    summarise(
      p_value = if (n() >= 3) {
        tryCatch({
          shapiro.test(.[[var_use]])$p.value
        }, error = function(e) NA_real_)
      } else {
        NA_real_
      }
    )
  
  # 获取所有组的 p 值，并判断是否都显著（例如 p > 0.05 表示服从正态）
  normal_p_values = normality_check$p_value
  all_normal = all(normal_p_values > 0.05, na.rm = TRUE)
  
  # 方差齐性检验的 p 值（如果存在）
  var_homogeneity <- tryCatch({
    leveneTest(fmla_use, data = fmla_data)$`Pr(>F)`[1]
  }, error = function(e) {
    NA_real_
  })
  
  # 判断是否满足 t 检验的前提条件（正态 + 方差齐）
  meet_assumptions <- !is.na(var_homogeneity) && var_homogeneity > 0.05 && all_normal
  
  
  # 根据前提条件决定是否进行 t 检验
  if (meet_assumptions) {
    t_test_result = t.test(fmla_use, data = fmla_data)
  } else {
    t_test_result = list()
    t_test_result$t_test = "Not applicable"
    t_test_result$statistic = NA
    t_test_result$p.value = NA
    t_test_result$assumptions = list(
      normality = normality_check,
      homogeneity_p_value = var_homogeneity,
      meets_assumptions = meet_assumptions
    )
  }
  return(t_test_result)
}
