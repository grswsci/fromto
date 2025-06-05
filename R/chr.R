as_chr = function(df,col_use = NULL){
  if(is.null(col_use)){
    chr_columns = colnames(df)
    for (col in chr_columns) {
      if (exists(col, where = df)) {
        df[[col]] <- as.character(df[[col]])
      }
    }
  }else{
    chr_columns = col_use
    for (col in chr_columns) {
      if (exists(col, where = df)) {
        df[[col]] <- as.character(df[[col]])
      }
    }
  }
  return(df)
}
