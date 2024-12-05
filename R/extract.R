#' @title extract_parenthesis
#' @description extract from parenthesis
#' @param str str
#' @return str
extract_parenthesis = function(str) {
  extract_match = gsub(".*\\((.*?)\\).*", "\\1", str)
  return(extract_match)
}
#' @title extract_parentheses
#' @description extract from parentheses
#' @param strs strs
#' @return strs
extract_parentheses = function(strs) {
  extract_matchs = lapply(strs, extract_parenthesis)
  return(extract_matchs)
}

