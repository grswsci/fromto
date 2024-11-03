#' @title convert_to_factor_by_column
#' @description factor the data frame
#' @param data_frame data frame
#' @param columns data frame
#' @return numeric matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' result = convert_to_numeric_by_column(counts_matrix)
#' print(result)
convert_to_factor_by_column = function(data_frame, columns = NULL) {
  if (is.null(column)) {
    columns = 1:ncol(data_frame)
    for (col in columns) {
      data_frame[[col]] = factor(data_frame[[col]])
    }
  }else{
    for (col in columns) {
      data_frame[[col]] = factor(data_frame[[col]])
    }
  }
  return(data_frame)
}
#' @title convert_to_numeric_by_column
#' @description Standardization of the matrix to zscore (every col)
#' @param x matrix, rownames is samples, colnames is genes
#' @return numeric matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' result = convert_to_numeric_by_column(counts_matrix)
#' print(result)
convert_to_numeric_by_column = function(data_frame,column = NULL) {
  if(is.null(column)){
    data_frame[,1:ncol(data_frame)] = as.numeric(unlist(data_frame[,1:ncol(data_frame)]))
  }else{
    data_frame[,column] = as.numeric(unlist(data_frame[,column]))
  }
  return(data_frame)
}

