read_fromto <- function(file,row_names = TRUE){
  data = data.table::fread(file)
  data = as.data.frame(data)
  if(row_names == TRUE){
    if(length(unique(data[,1])) < nrow(data)){
      data = as.matrix(data)
      rownames(data) = data[,1]
      data = data[,1]
    }else{
      rownames(data) = data[,1]
      data = data[,1]
    }
  }
  return(data)
}
