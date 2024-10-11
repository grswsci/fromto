geocli <- function(GEO_ID){
  clinicallines = readLines(paste0(GEO_ID,"_series_matrix.txt"))
  clinicallines_filtered = clinicallines[substr(clinicallines,1,7)=="!Sample"]
  contains_colon = function(column) {
    any(grepl(": ", as.character(column)))
  }
  clinicallines_filtered_with_colon_index = which(sapply(clinicallines_filtered, contains_colon))
  clinicallines_filtered =  clinicallines_filtered[c(1,2,clinicallines_filtered_with_colon_index)]
  clinicaldata = read.table(textConnection(paste(clinicallines_filtered, collapse="\n")), header=TRUE)
  clinicaldata["Samples_Original",] = colnames(clinicaldata)
  clinical = t(clinicaldata)

  rownames(clinical) = clinical[,1]
  colnames(clinical) = clinical[1,]
  clinical = as.data.frame(clinical[-1,-1])

  cols_with_colon_index = which(sapply(clinical, contains_colon))
  clinical = clinical[,c(cols_with_colon_index,ncol(clinical))]

  colnames(clinical) = NULL
  for (col in 1:(ncol(clinical)-1)) {
    split_names = unique(sapply(strsplit(clinical[,col], ": ", fixed = TRUE), function(x) c(x[1])))
    print(split_names)
    split_data = sapply(strsplit(clinical[,col], ": ", fixed = TRUE), function(x) c(x[2]))
    colnames(clinical)[col] = split_names
    clinical[,col] = split_data
  }
  colnames(clinical)[ncol(clinical)] = "Samples_Original"
  return(clinical)
}
