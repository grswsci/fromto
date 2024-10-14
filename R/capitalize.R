capitalize_first_letter <- function(str) {
  return(paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str)))))
}
