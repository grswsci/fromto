quantiles <- function(x){
  ifelse(x > quantile(x,.75),"Q1",
         ifelse(x > quantile(x,.5),"Q2",
                ifelse(x > quantile(x,.25),"Q3","Q4")
                )
         )
}

quantiles_df <- function(data,variable){
  new_quantile = apply(data[,variable,drop=F], 2, quantiles)
  colnames(new_quantile) = "Quantile"
  ExpressionType = cbind(data,new_quantile)
  idx = order(ExpressionType$Quantile)
  ExpressionType = ExpressionType[idx,]
  return(ExpressionType)
}

