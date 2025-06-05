#' @title fromto
#' @description Converted gene names
#' @param genes gene names
#' @param from one type of gene name
#' @param to another type of gene name
#' @param You can freely convert the following GeneIDs!
#' Symbol
#' NCBI_GeneID
#' HGNC_GeneID
#' SwissProt_GeneID
#' Ensembl_GeneID
#' OMIM_GeneID
#' Proteins_GeneID
#' Description_GeneID
#' Transcripts_GeneID
#' Gene_Group_Identifier_GeneID
#' Synonyms_GeneID
#' @return data frame
#' @examples
#' # examples
#' GeneNames = c("PDCD1","CD274","MKI67")
#' df = fromto(genes = GeneNames, from = "Symbol",to = "NCBI_GeneID")
#' print(df)
#'
fromto <- function(genes,from,to){
  data_file = system.file("NCBI", "trans2gene.RDS", package = "fromto")
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

#' @title fromto2
#' @description Converted gene names
#' @param genes gene names
#' @param from one type of gene name
#' @param to another type of gene name
#' @param You can freely convert the following GeneIDs!
#' Symbol
#' NCBI_GeneID
#' HGNC_GeneID
#' SwissProt_GeneID
#' Ensembl_GeneID
#' OMIM_GeneID
#' Proteins_GeneID
#' Description_GeneID
#' Transcripts_GeneID
#' Gene_Group_Identifier_GeneID
#' Synonyms_GeneID
#' @return data frame
#' @examples
#' # examples
#' GeneNames = c("PDCD1","CD274","MKI67")
#' df = fromto2(genes = GeneNames, from = "Symbol",to = "NCBI_GeneID")
#' print(df)

fromto2 <- function(genes,from,to){
  data_file = system.file("NCBI", "trans2gene_unique.RDS", package = "fromto")
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

#' @title fromto_update_matrix
#' @description Update gene names
#' @param gene_matrix gene_matrix
#' @param avereps_use = TRUE or use FALSE
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' Oldname_matrix = matrix(rpois(500, lambda = 10), nrow = 3, ncol = 50)
#' colnames(Oldname_matrix) = paste0("sample", 1:ncol(Oldname_matrix))
#' rownames(Oldname_matrix) = c("hPD-1","PDL1","MIB-1")
#' Newname_matrix = fromto_update_matrix(Oldname_matrix)
#' print(Newname_matrix)

fromto_update_matrix = function(gene_matrix,avereps_use = TRUE) {
  data_file = system.file("NCBI", "trans2gene_unique.RDS", package = "fromto")
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

  if(avereps_use){
    library(limma)
    gene_matrix = as.matrix(gene_matrix)
    class(gene_matrix) = "numeric"
    gene_matrix = limma::avereps(gene_matrix)
    return(gene_matrix)
  }else{
    return(gene_matrix)
  }
}

fromto_update_vector = function(gene_vector) {
  data_file = system.file("NCBI", "trans2gene_unique.RDS", package = "fromto")
  synonyms_df = readRDS(data_file)
  synonyms_df$Synonyms_GeneID = as.character(synonyms_df$Synonyms_GeneID)

  new_rownames = gene_vector
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
  return(new_rownames)
}
