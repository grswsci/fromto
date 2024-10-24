#' @title counts2tpm
#' @description Standardization of the counts matrix to TPM
#' @param counts counts matrix, rownames is genes, colnames is samples
#' @return TPM matrix
#' @examples
#' # examples
#' set.seed(123)
#' counts_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(counts_matrix) = paste0("sample", 1:ncol(counts_matrix))
#' rownames(counts_matrix) = c("PDCD1","CD274","MKI67")
#' result = counts2tpm(counts_matrix)
#' print(result)

counts2tpm <- function(counts){
  library(limma)
  data_file = system.file("data", "Human.GRCh38.p13.annot.RDS", package = "fromto")
  Gene_length = readRDS(data_file)
  Gene_length = as.data.frame(Gene_length)
  Gene_length = Gene_length[,c("Symbol","Length")]
  Gene_length = Gene_length[!duplicated(Gene_length$Symbol),]
  row.names(Gene_length) = Gene_length[,1]
  Gene_length_counts = merge_row(Gene_length,counts)
  Gene_length_counts = Gene_length_counts[,-1]
  Gene_length_counts = as.matrix(Gene_length_counts)
  class(Gene_length_counts) = "numeric"
  Gene_length_counts = as.data.frame(Gene_length_counts)
  Gene_counts = Gene_length_counts[,c(2:ncol(Gene_length_counts))]

  kb = Gene_length_counts$Length / 1000
  rpk = Gene_counts / kb
  sample_rpk_sums = colSums(rpk)
  sample_rpk_sums[sample_rpk_sums == 0] = 1
  tpm = t(t(rpk) / sample_rpk_sums * 1000000)

  print(paste0("sum(as.numeric(tpm[,1])): ",sum(as.numeric(tpm[,1]))))
  return(tpm)
}

