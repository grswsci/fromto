minmax <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

minmax_row <- function(x) {
  apply(x, 1, function(row) {
    return((row - min(row))/(max(row)-min(row)))
  })
}

minmax_col <- function(x) {
  apply(x, 2, function(col) {
    return((col - min(col))/(max(col)-min(col)))
  })
}
