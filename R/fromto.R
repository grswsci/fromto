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
  Gene_all = data.frame(lapply(Gene_all, as.character))
  Gene_intersect = intersect(genes,as.vector(Gene_all[,from]))
  Gene_subset = Gene_all[which(Gene_all[,from] %in% Gene_intersect),]
  Gene_output = Gene_subset[,c(from,to)]
  if(from != "Synonyms_GeneID" & to != "Synonyms_GeneID"){
    Gene_output = Gene_output[!duplicated(Gene_output[,from]),]
  }
  return(Gene_output)
}

fromto2 <- function(genes,from,to){
  data_file = system.file("data", "trans2gene_unique.RDS", package = "fromto")
  Gene_all = readRDS(data_file)
  Gene_all = data.frame(lapply(Gene_all, as.character))
  if(from == "Synonyms_GeneID"){
    Gene_all$Synonyms_GeneID2 = gsub("_non-unique","",Gene_all$Synonyms_GeneID)
    Gene_intersect = intersect(genes,Gene_all[,"Synonyms_GeneID2"])
    Gene_subset = Gene_all[which(Gene_all[,"Synonyms_GeneID2"] %in% Gene_intersect),]
  }else{
  Gene_intersect = intersect(genes,as.vector(Gene_all[,from]))
  Gene_subset = Gene_all[which(Gene_all[,from] %in% Gene_intersect),]
  }
  Gene_output = Gene_subset[,c(from,to)]
  if(from != "Synonyms_GeneID" & to != "Synonyms_GeneID"){
    Gene_output = Gene_output[!duplicated(Gene_output[,from]),]
  }
  return(Gene_output)
}


fromtoupdate <- function(gene_matrix) {
  data_file = system.file("data", "trans2gene_unique.RDS", package = "fromto")
  synonyms_df = readRDS(data_file)
  synonyms_df$Synonyms_GeneID = as.character(synonyms_df$Synonyms_GeneID)

  new_rownames = rownames(gene_matrix)
  updated_indices = integer(0)  # 用于存储已更新行名的索引
  # 遍历gene_matrix的行名
  for (i in seq_along(new_rownames)) {
    gene_id = new_rownames[i]
    if(gene_id %in% synonyms_df$Symbol){
      new_rownames[i] = gene_id
    }else{
    # 尝试在synonyms_df中找到匹配项
    match_idx = match(gene_id, synonyms_df$Synonyms_GeneID)
    if (!is.na(match_idx)) {
      # 如果找到匹配项，则更新行名
      new_rownames[i] = synonyms_df$Symbol[match_idx]
      updated_indices = c(updated_indices, i)  # 记录已更新的索引（可选）
    }
    }
    # 注意：这里没有处理未找到匹配项的情况
  }
  # 更新gene_matrix的行名
  gene_matrix = as.matrix(gene_matrix)
  rownames(gene_matrix) = new_rownames

  return(gene_matrix)
}

capitalize_first_letter <- function(str) {
  if (nchar(str) == 0) {
    return(str)
  }
  return(paste0(toupper(substr(str, 1, 1)), tolower(substr(str, 2, nchar(str)))))
}
