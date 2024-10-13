merge_row <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  data2 = data2[samesample,,drop=FALSE]
  data3 = cbind(data1,data2)
  return(data3)
}

merge_col <- function(data1,data2){
  samesample = intersect(colnames(data1),colnames(data2))
  data1 = data1[,samesample,drop=FALSE]
  data2 = data2[,samesample,drop=FALSE]
  data3 = rbind(data1,data2)
  return(data3)
}

same_row_data1 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  return(data1)
}

same_row_data2 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data2 = data2[samesample,,drop=FALSE]
  return(data2)
}
