getmerge_row <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  data2 = data2[samesample,,drop=FALSE]
  data3 = cbind(data1,data2)
  return(data3)
}

getsame_row_data1 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  return(data1)
}

getsame_row_data2 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data2 = data2[samesample,,drop=FALSE]
  return(data2)
}
