#' @title not_in
#' @description Isn 't it inside
#' @param x character string
#' @param y character string
#' @return TRUE or FALSE
#' @examples
#' # examples
#' str = c("aaa","bBB","CcC","dDd")
#' if("PDCD1" not_in str){
#' print("not in")
#' }

not_in <- function(x, y) {
  !x %in% y
}

