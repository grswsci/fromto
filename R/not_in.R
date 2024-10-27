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
#' @title is_in
#' @description Is it in
#' @param x character
#' @param ys character
#' @return TRUE or FALSE
#' @examples
#' # examples
#' y = "This is a sample text where we have a note saying 'replaced by ID12345'."
#' x = "replaced by ID"
#' is_in(x,y)
is_in = function(x,ys){
  result = grepl(x,ys)
  return(result)
}
#' @title is_ins
#' @description Is it ins
#' @param df data.frame
#' @param xs characters
#' @param ys characters
#' @return TRUE or FALSE
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
#' is_ins(df,xs = df$x,ys = df$y)
is_ins = function(df,xs,ys){
  result = c()
  for (variable in 1:length(xs)) {
    res = grepl(xs[variable],ys[variable])
    result = c(result,res)
  }
  result_df = df[result,]
  return(result_df)
}

