#' @title exp_fromto
#' @description computes the exponential function.
#' @param df data frame
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 5
#' n_cols = 3
#' df = data.frame(
#'  var1 = rnorm(n_rows),
#'  var2 = runif(n_rows),
#'  var3 = rpois(n_rows, lambda = 5)
#')
#'result_df = exp_fromto(df)
#'print(result_df)


exp_fromto <- function(df){
  suppressPackageStartupMessages(library(tidyverse))
  df = df %>% mutate(across(everything(), ~ (exp(.))))
  return(df)
}

