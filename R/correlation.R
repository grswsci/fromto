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

cor_test_col_apply <- function(df, varible, methods = "pearson"){
  x = as.numeric(df[,varible])
  varible_others = colnames(df)[colnames(df)!= varible]
  y_matrix <- as.matrix(df[,varible_others])
  cor_matrix <- cor(x, y_matrix, method = methods)
  pvalue_matrix <- apply(y_matrix, 2, function(y) cor.test(x, y, method = methods)$p.value)
  outTab <- data.frame(varible = varible,
                       varible_others = varible_others,
                       cor = t(cor_matrix),
                       pvalue = pvalue_matrix)
  return(outTab)
}

