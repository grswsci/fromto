numericize_by_column <- function(data_frame,column = NULL) {
  if(is.null(column)){
  data_frame[,1:ncol(data_frame)] = as.numeric(unlist(data_frame[,1:ncol(data_frame)]))
  }else{
    data_frame[,column] = as.numeric(unlist(data_frame[,column]))
  }
  return(data_frame)
}

