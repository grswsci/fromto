geoann = function (GPL_ID,GEO_ID) {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(limma))
  data_file = system.file("data", paste0(GPL_ID, ".RDS"), package = "fromto")
  GPL = readRDS(data_file)
  GPL = GPL[GPL$`Gene Symbol` != "", ]
  GPL$ID = paste0("GPL", GPL$ID)
  GPL = as.data.frame(GPL)
  rownames(GPL) = GPL[,1]

  lines = readLines(paste0(GEO_ID, "_series_matrix.txt"))
  lines_filtered = lines[!grepl("!", lines)]
  data = read.table(textConnection(paste(lines_filtered, collapse = "\n")), header = TRUE)
  data$ID_REF = paste0("GPL", data$ID_REF)
  data = as.data.frame(data)
  rownames(data) = data[,1]

  SameProbes = intersect(rownames(GPL),rownames(data))
  data = cbind(GPL[SameProbes,],data[SameProbes,])
  data = na.omit(data) %>% as.matrix()
  rownames(data) = data[,2]
  rownames(data) = sapply(strsplit(rownames(data), " /// "), function(x) c(x[1]))
  data = data[,-c(1:3)]
  class(data) = "numeric"
  data = avereps(data)

  return(data)
}
