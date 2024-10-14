minmax <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#' @title minmax_row
#' @description Standardization of the matrix to minmax score (every row)
#' @param x matrix, rownames is genes, colnames is samples
#' @return minmax matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' result = minmax_row(counts_matrix)
#' print(result)
#' print(max(as.numeric(result[,1])))
#' print(min(as.numeric(result[,1])))
minmax_row <- function(x) {
  apply(x, 1, function(row) {
    return((row - min(row))/(max(row)-min(row)))
  })
}
#' @title minmax_col
#' @description Standardization of the matrix to minmax score (every col)
#' @param x matrix, rownames is samples, colnames is genes
#' @return minmax matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' counts_matrix = t(counts_matrix)
#' result = minmax_col(counts_matrix)
#' print(result)
#' print(max(as.numeric(result[,1])))
#' print(min(as.numeric(result[,1])))
minmax_col <- function(x) {
  apply(x, 2, function(col) {
    return((col - min(col))/(max(col)-min(col)))
  })
}
