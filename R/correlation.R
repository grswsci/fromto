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
}
