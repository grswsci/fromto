#' @title cor_test_row
#' @description Calculate the correlation between one gene and all genes (every row)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_row(df, "col1", methods = "pearson")
#' print(result)


cor_test_row <- function(df, varible, methods = "pearson"){
  x = as.numeric(df[varible,])
  varible_others = rownames(df)[rownames(df) != varible]
  outTab = data.frame()
  for (varible_another in varible_others) {
    y = as.numeric(df[varible_another,])
    corT = cor.test(x, y, method = methods)
    cor = corT$estimate
    pvalue = corT$p.value
    outTab = rbind(outTab,
                   data.frame(varible = varible,varible_others = varible_another,cor = cor,pvalue = pvalue)
    )
  }
  return(outTab)
}

#' @title cor_test_row_apply
#' @description Calculate the correlation between one gene and all genes (every row)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_row_apply(df, "col1", methods = "pearson")
#' print(result)

cor_test_row_apply <- function(df, varible, methods = "pearson"){
  suppressPackageStartupMessages(library(tidyverse))
  x = as.numeric(df[varible,])
  varible_others = rownames(df)[rownames(df)!= varible]
  y_df <- as.data.frame(df[varible_others,])
  cor_df <- apply(y_df, 1, function(y) cor.test(x, y, method = methods)$estimate)  %>% data.frame(cor = .)
  pvalue_df <- apply(y_df, 1, function(y) cor.test(x, y, method = methods)$p.value)  %>% data.frame(pvalue = .)
  outTab <- data.frame(varible = varible,
                       varible_others = varible_others,
                       cor = cor_df,
                       pvalue = pvalue_df)
  return(outTab)
}

#' @title cor_test_row_apply_all
#' @description Calculate the correlation between every gene and other genes (every row)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_row_apply_all(df, "col1", methods = "pearson")
#' print(result)

cor_test_row_apply_all <- function(df, varible, methods = "pearson"){
  result = lapply(rownames(df), function(x) cor_test_row_apply(df, varible = x, methods = methods))
  outTab = do.call(rbind, result)
  return(outTab)
}

#' @title cor_test_col
#' @description Calculate the correlation between one gene and other genes (every col)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_col(df, "col1", methods = "pearson")
#' print(result)

cor_test_col <- function(df, varible, methods = "pearson"){
x = as.numeric(df[,varible])
varible_others = colnames(df)[colnames(df) != varible]
outTab = data.frame()
for (varible_another in varible_others) {
  y = as.numeric(df[,varible_another])
  corT = cor.test(x, y, method = methods)
  cor = corT$estimate
  pvalue = corT$p.value
  outTab = rbind(outTab,
                 data.frame(varible = varible,varible_others = varible_another,cor = cor,pvalue = pvalue)
  )
}
 return(outTab)
}

#' @title cor_test_col_apply
#' @description Calculate the correlation between one gene and other genes (every col)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_col_apply(df, "col1", methods = "pearson")
#' print(result)

cor_test_col_apply <- function(df, varible, methods = "pearson"){
  suppressPackageStartupMessages(library(tidyverse))
  x = as.numeric(df[,varible])
  varible_others = colnames(df)[colnames(df)!= varible]
  y_df <- as.data.frame(df[,varible_others])
  cor_df <- cor(x, y_df, method = methods) %>% as.data.frame() %>% t()
  pvalue_df <- apply(y_df, 2, function(y) cor.test(x, y, method = methods)$p.value)  %>% data.frame(pvalue = .)
  outTab <- data.frame(varible = varible,
                       varible_others = varible_others,
                       cor = cor_df,
                       pvalue = pvalue_df)
  return(outTab)
}

#' @title cor_test_col_apply_all
#' @description Calculate the correlation between every gene and other genes (every col)
#' @param df data frame
#' @param varible Gene Name
#' @param methods pearson or spearman
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 100
#' n_cols = 100
#' df = data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
#' rownames(df) = paste0("row", 1:n_rows)
#' colnames(df) = paste0("col", 1:n_cols)
#' result = cor_test_col_apply_all(df, "col1", methods = "pearson")
#' print(result)

cor_test_col_apply_all <- function(df, varible, methods = "pearson"){
  result = lapply(colnames(df), function(x) cor_test_col_apply(df, varible = x, methods = methods))
  outTab = do.call(rbind, result)
  return(outTab)
}
