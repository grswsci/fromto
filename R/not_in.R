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
#' @title is_integer
#' @description Is it integer
#' @param x number
#' @return TRUE or FALSE
#' @examples
#' # examples
#' y = "This is a sample text where we have a note saying 'replaced by ID12345'."
#' x = "replaced by ID"
#' is_in(x,y)
is_in = function(x,y){
  result = grepl(x, y)
  return(result)
}

