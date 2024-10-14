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

cor_test_row_apply_all <- function(df, varible, methods = "pearson"){
  result = lapply(rownames(df), function(x) cor_test_row_apply(df, varible = x, methods = methods))
  outTab = do.call(rbind, result)
  return(outTab)
}

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

cor_test_col_apply_all <- function(df, varible, methods = "pearson"){
  result = lapply(colnames(df), function(x) cor_test_col_apply(df, varible = x, methods = methods))
  outTab = do.call(rbind, result)
  return(outTab)
}
