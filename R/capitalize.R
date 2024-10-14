#' @title capitalize_first_letter
#' @description capital the first letter, lowercase the rest of the letters
#' @param str character string
#' @return character string
#' @examples
#' # examples
#' str = c("aaa","bBB","CcC","dDd")
#' str = capitalize_first_letter(str)
#' print(str)

capitalize_first_letter <- function(str) {
  return(paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str)))))
}


#setwd("C:\\fromto")
#roxygen2::roxygenize()
