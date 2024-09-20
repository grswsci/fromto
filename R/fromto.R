# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

fromto <- function(genes,from,to){
  data_file = system.file("data", "trans2gene.RDS", package = "fromto")
  Gene_all = readRDS(data_file)
  Gene_intersect = intersect(genes,Gene_all[,from])
  Gene_subset = Gene_all[which(Gene_all[,from] %in% Gene_intersect),]
  Gene_output = Gene_subset[,c(from,to)]
  if(from != "Synonyms_GeneID" & to != "Synonyms_GeneID"){
    Gene_output = Gene_output[!duplicated(Gene_output[,from]),]
  }
  return(Gene_output)
}

fromtoupdate <- function(gene_matrix) {
  synonyms_df = system.file("data", "trans2gene.RDS", package = "fromto")
  synonyms_df$Synonyms_GeneID = as.character(synonyms_df$Synonyms_GeneID)
  new_rownames = rownames(gene_matrix)
  idx = match(rownames(gene_matrix), synonyms_df$Synonyms_GeneID)
  new_rownames[!is.na(idx)] = synonyms_df$Symbol[idx[!is.na(idx)]]
  rownames(gene_matrix) <- new_rownames
  return(gene_matrix)
}
