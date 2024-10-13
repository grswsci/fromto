files_in_path <- function(path){
  filenames = list.files(path = path, full.names = FALSE)
  return(filenames)
}

files_be_filtered <- function(str,filenames){
  files = grep(str,filenames,value=T)
  return(files)
}
