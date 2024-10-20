#' @title find_index
#' @description Is it indexs
#' @param x character
#' @param ys characters
#' @return index
#' @examples
#' # examples
#' ys = c("This is a sample text where we have a note saying 'replaced by ID12345'.",
#'       "This is a sample text where we have a note saying 'replaced by ID12346'.",
#'       "This is a sample text where we have a note saying 'replaced nby ID12345'.",
#'       "This is a sample text where we have a note saying 'replaced nby ID12346'.")
#' x = "replaced by ID"
#' find_index(x = x,ys = ys)
find_index = function(x,ys){
  logical_indices = grepl(x,ys)
  indices = which(logical_indices)
  return(indices)
}
#' @title find_indexs
#' @description Is it indexs
#' @param xs characters
#' @param ys characters
#' @return index
#' @examples
#' # examples
#' df = data.frame(
#' y = c("This is a sample text where we have a note saying 'replaced by ID12345'.",
#'       "This is a sample text where we have a note saying 'replaced by ID12346'.",
#'       "This is a sample text where we have a note saying 'replaced nby ID12345'.",
#'       "This is a sample text where we have a note saying 'replaced nby ID12346'.")
#' x = c("replaced by ID",
#'       "replaced nby ID",
#'       "replaced nby ID",
#'       "replaced by ID")
#' )
#' find_indexs(xs = df$x,ys = df$y)
find_indexs = function(xs,ys){
  indices = c()
  for (variable in 1:length(xs)) {
   res = grepl(xs[variable],ys[variable])
   if(res == TRUE){
     indices = c(indices,variable)
   }
  }
}
