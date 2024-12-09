#' @title geogpl
#' @description which gpl
#' @param GEO_ID GEO ID
geogpl = function (GEO_ID){
  clinicallines = readLines(paste0(GEO_ID, "_series_matrix.txt"))
  clinicallines_filtered = clinicallines[substr(clinicallines, 1, 19) == "!Series_platform_id"]
  platform_id = sub(".*?(GPL\\d+).*", "\\1", clinicallines_filtered)
  return(platform_id)
}
#' @title geoann
#' @description The probe name was annotated as gene name.
#' @param GPL_ID GPL ID
#' @param GEO_ID GEO ID
#' @return data frame
#' @examples
#' # examples
#' geoget(GEO_IDs = "GSE103668")
#' library(tidyverse)
#' library(R.utils)
#' list.files() %>% grepl(".txt.gz$", ., fixed = FALSE) %>% which() %>% list.files()[.] %>% gunzip(., remove = FALSE, overwrite = TRUE)
#' data = geoann(GPL_ID = "GPL570",GEO_ID = "GSE103668")
#' print(data)
geoann = function (GPL_ID,GEO_ID) {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(limma))
  suppressPackageStartupMessages(library(impute))
  data_file = system.file("data", paste0(GPL_ID, ".RDS"), package = "fromto")
  GPL = readRDS(data_file)
  GPL = GPL[GPL$`Gene Symbol` != "", ]
  GPL$ID = paste0("GPL", GPL$ID)
  GPL = as.data.frame(GPL)
  rownames(GPL) = GPL[,1]

  lines = readLines(paste0(GEO_ID, "_series_matrix.txt"))
  lines_filtered = lines[!grepl("!", lines)]
  #data = read.table(textConnection(paste(lines_filtered, collapse = "\n")), header = TRUE)
  data = data.table::fread(text = paste(lines_filtered, collapse = "\n"), header = TRUE)
  data$ID_REF = paste0("GPL", data$ID_REF)
  data = as.data.frame(data)
  rownames(data) = data[,1]

  SameProbes = intersect(rownames(GPL),rownames(data))
  data = cbind(GPL[SameProbes,],data[SameProbes,])
  data = as.matrix(data)
  rownames(data) = data[,2]
  rownames(data) = sapply(strsplit(rownames(data), " /// "), function(x) c(x[1]))
  data = data[,-c(1:3)]
  class(data) = "numeric"

  na_col_proportion = colSums(is.na(data)) / nrow(data)
  valid_cols = which(na_col_proportion <= 0.8)
  filtered_data = data[, valid_cols, drop = FALSE]

  na_proportion = colSums(is.na(filtered_data)) / ncol(filtered_data)
  valid_rows = which(na_proportion <= 0.8)
  if(length(valid_rows) == 0){
    final_data = filtered_data
  }else{
    final_data = filtered_data[valid_rows, , drop = FALSE]
  }

  if(any(is.na(final_data))){
    print("impute NA...")
    imputed_data = impute.knn(final_data)
    imputed_data_final = imputed_data$data
    final_data_return = avereps(imputed_data_final)
  }else{
    final_data_return = avereps(final_data)
  }
  return(final_data_return)
}

