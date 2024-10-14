remove_distinct <- function(df,variable){
  df = df[!duplicated(df[,variable]),,drop=FALSE]
  return(df)
}
