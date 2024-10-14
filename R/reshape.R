shape_melt <- function(df,not_change_col){
  df = reshape2::melt(df, id.vars = not_change_col)
  return(df)
}

shape_dcast <- function(df,row_name,col_name,value_col){
  formula_name = as.formula(paste0(row_name," ~ ",col_name))
  df = reshape2::dcast(
    data = df,
    formula_name,
    value.var = value_col,
    fun.aggregate = mean
  )
  return(df)
}


