#' @title run_download
#' @description run_download
#' @param urls web urls
#' @return file
run_download = function(urls){
  for (url in urls){
    destfile = unlist(strsplit(url,"/",fixed = TRUE))[length(unlist(strsplit(url,"/",fixed = TRUE)))]
    download.file(url, destfile = destfile, mode = "wb")
  }
}
