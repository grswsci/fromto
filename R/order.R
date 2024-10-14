order_fromto <- function(df,variable){
  idx = order(df[,variable])
  df = df[idx,,drop=FALSE]
  return(df)
}
