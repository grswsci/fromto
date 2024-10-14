#' @title order_fromto
#' @description The data box is in ascending order according to one of the columns
#' @param x data frame, rownames is samples, colnames is variables
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' counts_matrix = t(counts_matrix)
#' result = order_fromto(df = counts_matrix,variable = "PDCD1")
#' print(result)
order_fromto <- function(df,variable){
  idx = order(df[,variable])
  df = df[idx,,drop=FALSE]
  return(df)
}
