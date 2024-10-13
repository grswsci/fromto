capitalize_first_letter <- function(str) {  
  if (nchar(str) == 0) {  
    return(str)  
  }  
  return(paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str)))))  
}  