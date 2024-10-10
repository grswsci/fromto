geocli <- function(GEO_ID){
  clinicallines = readLines(paste0(GEO_ID,"_series_matrix.txt"))
  clinicallines_filtered = clinicallines[substr(clinicallines,1,7)=="!Sample"]
  clinicaldata = read.table(textConnection(paste(clinicallines_filtered, collapse="\n")), header=TRUE)
  clinicaldata = t(clinicaldata)
  return(clinicaldata)
}
