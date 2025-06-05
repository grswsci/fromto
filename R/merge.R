# 方案1: 使用match()函数进行向量化匹配
merge_col_add_v1 <- function(data1, data2, data1_var, data2_var_same_data1, data2_var_add_data1) {
  duplicated_vars = length(as.vector(data2[,data2_var_same_data1])[duplicated(data2[,data2_var_same_data1])])
  ensure_dataframe <- function(x) {
    if (!is.data.frame(x)) {
      return(as.data.frame(x))
    }
    return(x)
  }

  data1 <- ensure_dataframe(data1)
  data2 <- ensure_dataframe(data2)

  if (duplicated_vars == 0) {
    cat("一对一关系 - 可以安全使用match方法\n")

    # 使用match进行向量化匹配
    match_indices <- match(data1[, data1_var], data2[, data2_var_same_data1])

    # 根据匹配结果提取对应值，未匹配的自动为NA
    add_col <- data2[match_indices, data2_var_add_data1]

    # 添加新列
    data1$add_col <- add_col
    return(data1)

  } else {
    cat("一对多关系 - 建议使用merge方法\n")

    # 创建用于合并的临时数据框
    merge_data <- data2[, c(data2_var_same_data1, data2_var_add_data1), drop = FALSE]
    names(merge_data) <- c(data1_var, "add_col")

    # 使用左连接保持data1的所有行
    result <- merge(data1, merge_data, by = data1_var, all.x = TRUE)

    return(result)
  }

}

# 方案2: 使用dplyr（语法清晰，性能好）

merge_col_add_v2 <- function(data1, data2, data1_var, data2_var_same_data1, data2_var_add_data1) {
  library(dplyr)
  ensure_dataframe <- function(x) {
    if (!is.data.frame(x)) {
      return(as.data.frame(x))
    }
    return(x)
  }

  data1 <- ensure_dataframe(data1)
  data2 <- ensure_dataframe(data2)


  # 创建连接映射
  join_by_vars <- setNames(data2_var_same_data1, data1_var)

  # 执行左连接
  result <- data1 %>%
    left_join(
      data2 %>% select(all_of(c(data2_var_same_data1, data2_var_add_data1))),
      by = join_by_vars
    ) %>%
    rename(add_col = !!data2_var_add_data1)

  return(result)
}

# 性能测试示例
benchmark_merge_functions <- function() {
  # 创建测试数据
  set.seed(123)
  n1 <- 10000
  n2 <- 5000

  data1 <- data.frame(
    id = sample(1:n2, n1, replace = TRUE),
    value1 = rnorm(n1)
  )

  data2 <- data.frame(
    id = 1:n2,
    value2 = rnorm(n2)
  )

  # 使用microbenchmark进行性能测试
  if (require(microbenchmark)) {
    results <- microbenchmark(
      original = merge_col_add(data1, data2, "id", "id", "value2"),
      match_method = merge_col_add_v1(data1, data2, "id", "id", "value2"),
      base_merge = merge_col_add_v2(data1, data2, "id", "id", "value2"),
      data_table = merge_col_add_v3(data1, data2, "id", "id", "value2"),
      dplyr_method = merge_col_add_v4(data1, data2, "id", "id", "value2"),
      times = 10
    )
    print(results)
  }
}

# 使用示例
example_usage <- function() {
  # 示例数据
  data1 <- data.frame(
    key = c("A", "B", "C", "D"),
    val1 = 1:4
  )

  data2 <- data.frame(
    key = c("A", "C", "E"),
    val2 = c(10, 30, 50)
  )

  # 测试各种方法
  cat("原始方法结果:\n")
  print(merge_col_add(data1, data2, "key", "key", "val2"))

  cat("\n向量化方法1结果:\n")
  print(merge_col_add_v1(data1, data2, "key", "key", "val2"))

  cat("\nmerge方法结果:\n")
  print(merge_col_add_v2(data1, data2, "key", "key", "val2"))
}
rbind_smart = function(df1, df2) {if (!is.data.frame(df1)) df1 = as.data.frame(df1, stringsAsFactors = FALSE);if (!is.data.frame(df2)) df2 = as.data.frame(df2, stringsAsFactors = FALSE);all_columns = union(names(df1), names(df2));missing_in_df1 = setdiff(all_columns, names(df1));missing_in_df2 = setdiff(all_columns, names(df2));if (length(missing_in_df1) > 0) {df1[missing_in_df1] = NA};if (length(missing_in_df2) > 0) {df2[missing_in_df2] = NA};sorted_cols = sort(all_columns);rbind(df1[sorted_cols], df2[sorted_cols])}
#' @title merge_col_add
#' @description Merge add line from another data frame
#' @param data1 data frame
#' @param data2 data frame
#' @param data1_var data1_var
#' @param data2_var_same_data1 data2_var_same_data1
#' @param data2_var_add_data1 data2_var_add_data1
merge_col_add = function(data1,
                         data2,
                         data1_var,
                         data2_var_same_data1,
                         data2_var_add_data1
){
  data1 = as.data.frame(data1)
  data2 = as.data.frame(data2)
  add_col = c()
  for (variable in data1[,data1_var]) {
    if(variable %in% data2[,data2_var_same_data1]){
      data2_subset = data2[which(data2[,data2_var_same_data1] == variable),,drop = FALSE]
      add_col = c(add_col,data2_subset[,data2_var_add_data1])
    }else{
      add_col = c(add_col,NA)
    }
  }
  data1 = cbind(data1,add_col)
  return(data1)
}
#' @title merge_row
#' @description Merge by rownames
#' @param data1 data frame
#' @param data2 data frame
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 10
#' n_cols1 = 3
#' data1 = data.frame(matrix(rnorm(n_rows * n_cols1), nrow = n_rows))
#' rownames(data1) = paste0("sample", 1:n_rows)
#' colnames(data1) = paste0("var", 1:n_cols1)
#' set.seed(456)
#' n_cols2 = 2
#' data2 = data.frame(matrix(runif(n_rows * n_cols2), nrow = n_rows))
#' rownames(data2) = paste0("sample", 1:n_rows)
#' colnames(data2) = paste0("var", (n_cols1 + 1):(n_cols1 + n_cols2))
#' result = merge_row(data1, data2)
#' print(result)

merge_row <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  data2 = data2[samesample,,drop=FALSE]
  data3 = cbind(data1,data2)
  return(data3)
}

#' @title merge_col
#' @description Merge by colnames
#' @param data1 data frame
#' @param data2 data frame
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 10
#' n_cols1 = 3
#' data1 = data.frame(matrix(rnorm(n_rows * n_cols1), nrow = n_rows))
#' rownames(data1) = paste0("sample", 1:n_rows)
#' colnames(data1) = paste0("var", 1:n_cols1)
#' data1 = t(data1)
#'
#' set.seed(456)
#' n_cols2 = 2
#' data2 = data.frame(matrix(runif(n_rows * n_cols2), nrow = n_rows))
#' rownames(data2) = paste0("sample", 1:n_rows)
#' colnames(data2) = paste0("var", (n_cols1 + 1):(n_cols1 + n_cols2))
#' data2 = t(data2)
#'
#' result = merge_col(data1, data2)
#' print(result)

merge_col <- function(data1,data2){
  samesample = intersect(colnames(data1),colnames(data2))
  data1 = data1[,samesample,drop=FALSE]
  data2 = data2[,samesample,drop=FALSE]
  data3 = rbind(data1,data2)
  return(data3)
}

#' @title merge_plot
#' @description Merge by plot
#' @param type plot type, you can choose "pdf","jpg","png" and "tiff"
#' @param ncol How many pictures are there in each column
#' @return ggplot object
#' @examples
#' # examples
#' set.seed(123)
#' n = 150
#' data = data.frame(Type = sample(c("A", "B", "C"), n, replace = TRUE),
#'                   variable1 = rnorm(n))
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name1",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name2",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name3",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'plot_merge = merge_plot(type = "pdf", ncol = 3)
#'ggsave2("Figure_Merge.pdf",height = 1,width = 3)

merge_plot <- function(type = "pdf", ncol = 1) {
  options(warn = -1)
  library(ggplotify)
  library(cowplot)
  library(magick)
  library(pdftools)
  fnames <- Sys.glob(paste0("*.",type))
  if(type == "pdf"){
    p <- lapply(fnames,function(i){
      pn <- as.ggplot(image_read_pdf(i))
    })
  }else if(type %in% c("jpg","png","tiff")){
    p <- lapply(fnames,function(i){
      pn <- as.ggplot(image_read(i))
    })
  }

  plot_grid(plotlist = p, ncol = ncol)
  return(p)
}

same_row_data1 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  return(data1)
}

same_row_data2 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data2 = data2[samesample,,drop=FALSE]
  return(data2)
}

