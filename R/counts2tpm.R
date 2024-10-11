counts2tpm <- function(counts){
  library(limma)
  data_file = system.file("data", "Human.GRCh38.p13.annot.RDS", package = "fromto")
  Gene_length = readRDS(data_file)
  Gene_length = as.data.frame(Gene_length)
  Gene_length = Gene_length[,c("Symbol","Length")]
  Gene_length = Gene_length[!duplicated(Gene_length$Symbol),]
  row.names(Gene_length) = Gene_length[,1]
  Gene_length_counts = getmerge_row(Gene_length,counts)
  Gene_length_counts = Gene_length_counts[,-1]
  Gene_length_counts = as.matrix(Gene_length_counts)
  class(Gene_length_counts) = "numeric"
  Gene_length_counts = as.data.frame(Gene_length_counts)

  kb = Gene_length_counts$Length / 1000
  kb
  Gene_counts = Gene_length_counts[,2:ncol(Gene_length_counts)]
  rpk = Gene_counts / kb
  rpk
  tpm = t(t(rpk)/colSums(rpk) * 1000000)
  print("sum(as.numeric(tpm[,1])): ",sum(as.numeric(tpm[,1])))
  return(tpm)
}
