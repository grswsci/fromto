geocli <- function(GEO_ID){
  clinicallines = readLines(paste0(GEO_ID,"_series_matrix.txt"))
  clinicallines_filtered = clinicallines[substr(clinicallines,1,7)=="!Sample"]
  clinicaldata = read.table(textConnection(paste(clinicallines_filtered, collapse="\n")), header=TRUE)
  clinical = t(clinicaldata)

  rownames(clinical) = clinical[,1]
  colnames(clinical) = clinical[1,]
  clinical = as.data.frame(clinical[-1,-1])

  contains_colon = function(column) {
    any(grepl(": ", as.character(column)))
  }
  cols_with_colon_index = which(sapply(clinical, contains_colon))

  clinical = clinical[,cols_with_colon_index]
  colnames(clinical) = NULL

  for (col in 1:ncol(clinical)) {
    split_names = unique(sapply(strsplit(clinical[,col], ": ", fixed = TRUE), function(x) c(x[1])))
    print(split_names)
    split_data = sapply(strsplit(clinical[,col], ": ", fixed = TRUE), function(x) c(x[2]))
    colnames(clinical)[col] = split_names
    clinical[,col] = split_data
  }

  return(clinical)
}
