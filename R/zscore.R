#' @title zscore_row
#' @description Standardization of the matrix to zscore (every row)
#' @param x matrix, rownames is genes, colnames is samples
#' @return zscore matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' result = zscore_row(counts_matrix)
#' print(result)
#' print(var(as.numeric(result[1,])))
#' print(mean(as.numeric(result[1,])))

zscore_row <- function(x) {
  rowmean = apply(x, 1, mean)
  rowsd = apply(x, 1, sd)
  rv = sweep(x, 1, rowmean,"-")
  rv = sweep(rv, 1, rowsd, "/")
  return(rv)
}

#' @title zscore_col
#' @description Standardization of the matrix to zscore (every col)
#' @param x matrix, rownames is samples, colnames is genes
#' @return zscore matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' counts_matrix = t(counts_matrix)
#' result = zscore_col(counts_matrix)
#' print(result)
#' print(var(as.numeric(result[,1])))
#' print(mean(as.numeric(result[,1])))

zscore_col <- function(x){
  colmean = apply(x, 2, mean)
  colsd = apply(x, 2, sd)
  cv = sweep(x, 2, colmean,"-")
  cv = sweep(cv, 2, colsd,"/")
  return(cv)
}

