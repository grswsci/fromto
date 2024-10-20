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
#' @title is_integer
#' @description Is it integer
#' @param x number
#' @return TRUE or FALSE
#' @examples
#' # examples
#' is_integer(5)
#' is_integer(5.5)

is_integer <- function(x) {
  return(x %% 1 == 0)
}
