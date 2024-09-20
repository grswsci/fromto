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
    Gene_output = Gene_output[!duplicated(Gene_output[,"from"]),]
  }
  return(Gene_output)
}
colnames(Gene_all)
