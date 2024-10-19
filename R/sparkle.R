#' @title rna_exp_pancancer
#' @description Read the pan-cancer matrix
#' @param not require
#' @return the data frame of pan-cancer matrix
rna_exp_pancancer = function(){
  panexpr = fread("/mnt/ExpBig/EBPlusPlusAdjustPANCAN_IlluminaHiSeq_RNASeqV2.geneExp.grsw.impute.log2(Xsum1).tsv",sep = "\t",stringsAsFactors = F,check.names = F,header = T)
  panexpr = as.data.frame(panexpr)
  rownames(panexpr) = panexpr[,1]
  panexpr = panexpr[,-1]
  colnames(panexpr) = substr(colnames(panexpr),1,15)
  return(panexpr)
}
#' @title rna_exp_singlecancer
#' @description Read the single cancer tumor expression of all gene
#' @param CancerType CancerType
#' @return the data frame of the single cancer tumor expression of all gene
rna_exp_singlecancer = function(CancerType){
  library(limma)
  library(data.table)
  data = fread(paste0("/mnt/ExpBig/Expression.Firehose.",CancerType,".txt"),sep="\t",header=TRUE,check.names=FALSE)
  data = as.matrix(data)
  rownames(data) = data[,1]
  data = data[,-1]
  class(data) = "numeric"
  return(data)
}
#' @title rna_exp_singlecancer_multigenes
#' @description Read the single cancer tumor expression of multigenes
#' @param CancerType CancerType
#' @param multigenes multigenes
#' @return the data frame of the single cancer tumor expression of multigenes
rna_exp_singlecancer_multigenes = function(CancerType,multigenes){
  library(limma)
  library(data.table)
  data = fread(paste0("/mnt/ExpBig/Expression.Firehose.",CancerType,".txt"),sep="\t",header=TRUE,check.names=FALSE)
  data = as.matrix(data)
  rownames(data) = data[,1]
  data = data[,-1]
  class(data) = "numeric"
  data = data[intersect(as.vector(multigenes),rownames(data)),]
  return(data)
}
#' @title rna_exp_geo_singledataset_tumor
#' @description Read the single dataset tumor expression of all genes
#' @param CancerType CancerType
#' @param DatasetName DatasetName
#' @return data frame of the single dataset tumor expression of all genes
rna_exp_geo_singledataset_tumor = function(CancerType,DatasetName){
  load(paste0("/mnt/Best/",CancerName,"/symbol.rda"))
  library(limma)
  library(data.table)
  data = total_expr_list[[DatasetName]]
  clinical = total_clin_list[[DatasetName]]
  rownames(clinical) = clinical[,1]
  if("Tissue" %in% colnames(clinical)){
    clinical = clinical[which(clinical$Tissue == "Tumor"),]
  }
  data_Tumor = data[,rownames(clinical)]
  return(data_Tumor)
}
#' @title rna_exp_st
#' @description Read the st rna expression of all genes
#' @param CancerType CancerType
#' @return data frame of the st rna expression of all genes
rna_exp_st = function(CancerType){
  stRNA = readRDS(paste0("/mnt/ST/",CancerType,".stRNA.RDS"))
  return(stRNA)
}
#' @title rna_exp_sc
#' @description Read the sc rna expression of all genes
#' @param CancerType CancerType
#' @return data frame of the sc rna expression of all genes
rna_exp_sc = function(CancerType){
  scRNA = readRDS(paste0("/mnt/TISCH/",CancerType,".RDS"))
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data)=="Celltype (major-lineage)"] = "CellType_MainLabel"
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data)=="Celltype (minor-lineage)"] = "CellType_FineLabel"
  Idents(scRNA) = scRNA$CellType_MainLabel
  return(scRNA)
}
#' @title rna_exp_sc_tumor
#' @description Read the sc rna tumor expression of all genes
#' @param CancerType CancerType
#' @return data frame of the sc rna tumor expression of all genes
rna_exp_sc_tumor = function(CancerType){
  scRNA = readRDS(paste0("/mnt/TISCH/",CancerType,".RDS"))
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data)=="Celltype (major-lineage)"] = "CellType_MainLabel"
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data)=="Celltype (minor-lineage)"] = "CellType_FineLabel"
  Idents(scRNA) = scRNA$CellType_MainLabel
  if("Tissue" %in% colnames(scRNA@meta.data)){
    scRNA@meta.data$Tissue = gsub("tumor","Tumor",scRNA@meta.data$Tissue)
    if("Tumor" %in% unique(scRNA@meta.data$Tissue)){
      scRNA = scRNA[,scRNA@meta.data$Tissue== "Tumor"]
    }
    gc()
  }else if("Source" %in% colnames(scRNA@meta.data)){
    scRNA@meta.data$Source = gsub("tumor","Tumor",scRNA@meta.data$Source)
    if("Tumor" %in% unique(scRNA@meta.data$Source)){
      scRNA = scRNA[,scRNA@meta.data$Source== "Tumor"]
    }
    gc()
  }else{
    scRNA = scRNA
    print(c("Nothing"))
  }
  return(scRNA)
}
#' @title rna_exp_sc_gene
#' @description get sc rna data of selected genes
#' @param scRNA seurat object
#' @param GeneName One Gene or multi-Genes
#' @return data frame of the sc rna expression of One Gene or multi-Genes
rna_exp_sc_gene <- function(scRNA,GeneName){
  library(tidyverse)
  expression_data = GetAssayData(object = scRNA, assay = "RNA") %>% .[c(GeneName),] %>% as.matrix()
  if(length(GeneName) == 1){
    colnames(expression_data) = GeneName
    expression_data = as.data.frame(expression_data)
  }else{
    expression_data = t(expression_data)
    expression_data = as.data.frame(expression_data)
  }
  return(expression_data)
}
#' @title rna_exp_st_gene
#' @description get st rna data of selected genes
#' @param scRNA seurat object
#' @param GeneName One Gene or multi-Genes
#' @return data frame of the st rna expression of One Gene or multi-Genes
rna_exp_st_gene = function(stRNA,GeneName){
  expression_data = GetAssayData(object = scRNA, assay = "Spatial") %>% .[c(GeneName),] %>% as.matrix()
  if(length(GeneName) == 1){
    colnames(expression_data) = GeneName
    expression_data = as.data.frame(expression_data)
  }else{
    expression_data = t(expression_data)
    expression_data = as.data.frame(expression_data)
  }
  return(expression_data)
}
#' @title protein_exp_cptac
#' @description Read the cptac protein expression of all genes without impute NA value
#' @param CancerType CancerType
#' @return data frame of the cptac protein expression of all genes without impute NA value
protein_exp_cptac = function(CancerType){
  library(data.table)
  data = fread(paste0("/mnt/Proteomics/expression_",CancerType,"_protein.csv"))
  data = as.matrix(data)
  rownames(data) = data[,1]
  data = data[,-1]
  return(data)
}
#' @title protein_exp_cptac_impute
#' @description Read the cptac protein expression of all genes with impute NA value
#' @param CancerType CancerType
#' @return data frame of the cptac protein expression of all genes with impute NA value
protein_exp_cptac_impute = function(CancerType){
  library(data.table)
  library(limma)
  library(impute)
  data = fread(paste0("/mnt/Proteomics/expression_",CancerType,"_protein.csv"))
  data = as.matrix(data)
  rownames(data) = data[,1]
  data = data[,-1]
  class(data) = "numeric"
  mat=impute.knn(data)
  data = mat$data
  return(data)
}
#' @title rna_sample_pancancer
#' @description Read the pan-cancer sample
#' @param not require
#' @return the data frame of pan-cancer sample
rna_sample_pancancer = function(){
  rawAnno = read.delim("/mnt/Merge/merged_sample_quality_annotations.tsv",sep = "\t",row.names = NULL,check.names = F,stringsAsFactors = F,header = T)
  #rawAnno = rawAnno[rawAnno$Do_not_use == "False",]
  rawAnno$simple_barcode = substr(rawAnno$aliquot_barcode,1,15)
  samAnno = rawAnno[!duplicated(rawAnno$simple_barcode),c("cancer type", "simple_barcode")]
  samAnno = samAnno[which(samAnno$`cancer type` != ""),]
  return(samAnno)
}
#' @title rna_survival_pancancer
#' @description Read the pan-cancer survival
#' @param not require
#' @return the data frame of pan-cancer survival
rna_survival_pancancer = function(){
  library(data.table)
  data = fread("/mnt/Merge/Survival_SupplementalTable_S1_20171025_xena_sp")
  data = as.data.frame(data)
  data = data[which(substr(data$sample,14,14) == "0"),]
  data = data[,c("sample","cancer type abbreviation","OS.time","OS","DSS.time","DSS","PFI.time","PFI","DFI.time","DFI")]
  colnames(data)[2] = "CancerType"
  rownames(data) = data[,1]
  return(data)
}
#' @title rna_gene_exp_pancancer
#' @description Read the pan-cancer expression of one gene
#' @param GeneName GeneName
#' @return the data frame of pan-cancer expression of one gene
rna_gene_exp_pancancer = function(GeneName){
  data = read.table(paste0("/mnt/expTime/Expression.",GeneName,".txt"), header=T, sep="\t", check.names=F,row.names = 1)
  return(data)
}
#' @title rna_gene_exp_pancancer_zscore
#' @description Read the pan-cancer zscore expression of one gene
#' @param GeneName GeneName
#' @return the data frame of pan-cancer zscore expression of one gene
rna_gene_exp_pancancer_zscore = function(GeneName){
  data = read.table(paste0("/mnt/expTime/Expression.",GeneName,".zscore.txt"),sep="\t",header=T,check.names=F, row.names=1)
  return(data)
}
#' @title rna_gene_exp_singlecancer
#' @description Read the single cancer expression of one gene
#' @param GeneName GeneName
#' @param CancerType CancerType
#' @return the data frame of single cancer expression of one gene
rna_gene_exp_singlecancer = function(GeneName,CancerType){
  data = read.table(paste0("/mnt/expTime/Expression.",GeneName,".txt"), header=T, sep="\t", check.names=F,row.names = 1)
  data = data[which(data$CancerType == CancerType),]
  return(data)
}
#' @title rna_gene_exp_singlecancer_zscore
#' @description Read the single cancer zscore expression of one gene
#' @param GeneName GeneName
#' @param CancerType CancerType
#' @return the data frame of single cancer zscore expression of one gene
rna_gene_exp_singlecancer_zscore = function(GeneName,CancerType){
  data = read.table(paste0("/mnt/expTime/Expression.",GeneName,".zscore.txt"), header=T, sep="\t", check.names=F,row.names = 1)
  data = data[which(data$CancerType == CancerType),]
  return(data)
}
#' @title rna_gene_survival_pancancer
#' @description Read the pan-cancer expression and survival information of one gene
#' @param GeneName GeneName
#' @return the data frame of single cancer zscore expression of one gene
rna_gene_survival_pancancer = function(GeneName){
  data = read.table(paste0("/mnt/expTime/expTime.",GeneName,".txt"), header=T, sep="\t", check.names=F,row.names = 1)
  return(data)
}
#' @title rna_gene_survival_singlecancer
#' @description Read the single cancer tumor expression and survival information of one gene
#' @param GeneName GeneName
#' @param CancerType CancerType
#' @return data frame of the single cancer tumor expression and survival information of one gene
rna_gene_survival_singlecancer = function(GeneName,CancerType){
  data = read.table(paste0("/mnt/expTime/expTime.",GeneName,".txt"), header=T, sep="\t", check.names=F,row.names = 1)
  data = data[which(data$CancerType == CancerType),]
  return(data)
}
#' @title sparkle_start_time
#' @description sparkle start time
#' @param not require
#' @return sparkle start time
sparkle_start_time = function(){
  startTime = Sys.time()
  startTime = as.character(startTime)
  startTime = gsub(" " ,"-",startTime)
  startTime = gsub(":" ,"-",startTime)
  return(startTime)
}
#' @title zscore_col_genename
#' @description zscore Designated Genes
#' @param data data frame
#' @param GeneName GeneName
#' @param FilterOut TRUE is to remove outliers, or use False
#' @return zscore result
zscore_col_genename = function(data,GeneName,FilterOut = TRUE) {
  data$Zscore = as.numeric(scale(data[,GeneName], center = TRUE, scale = TRUE))
  if(FilterOut == TRUE){
    data = data[which(data$Zscore <3 & data$Zscore > -3),]
    return(data)
  }else{
    return(data)
  }
}
#' @title survival_filter_year
#' @description survival filter year
#' @param data data frame
#' @param SurvivalType SurvivalType OS DSS DFI PFI
#' @return zscore result
survival_filter_year = function(data,SurvivalType){
  if(SurvivalType == "OS"){
    data$OS.time = data$OS.time/365
    data = as.data.frame(data)
    data = data[data$OS.time>0,]
    data = data[!(is.na(data$OS.time)),]
    data = data[!(is.na(data$OS)),]
    return(data)
  }else if(SurvivalType == "DSS"){
    data$DSS.time = data$DSS.time/365
    data = as.data.frame(data)
    data = data[data$DSS.time>0,]
    data = data[!(is.na(data$DSS.time)),]
    data = data[!(is.na(data$DSS)),]
    return(data)
  }else if(SurvivalType == "DFI"){
    data$DFI.time = data$DFI.time/365
    data = as.data.frame(data)
    data = data[data$DFI.time>0,]
    data = data[!(is.na(data$DFI.time)),]
    data = data[!(is.na(data$DFI)),]
    return(data)
  }else if(SurvivalType == "PFI"){
    data$PFI.time = data$PFI.time/365
    data = as.data.frame(data)
    data = data[data$PFI.time>0,]
    data = data[!(is.na(data$PFI.time)),]
    data = data[!(is.na(data$PFI)),]
    return(data)
  }
}

#' @title survival_filter_month
#' @description survival filter month
#' @param data data frame
#' @param SurvivalType SurvivalType OS DSS DFI PFI
#' @return zscore result
survival_filter_month = function(data,SurvivalType){
  if(SurvivalType == "OS"){
    data$OS.time = data$OS.time/30.4
    data = as.data.frame(data)
    data = data[data$OS.time>0,]
    data = data[!(is.na(data$OS.time)),]
    data = data[!(is.na(data$OS)),]
    return(data)
  }else if(SurvivalType == "DSS"){
    data$DSS.time = data$DSS.time/30.4
    data = as.data.frame(data)
    data = data[data$DSS.time>0,]
    data = data[!(is.na(data$DSS.time)),]
    data = data[!(is.na(data$DSS)),]
    return(data)
  }else if(SurvivalType == "DFI"){
    data$DFI.time = data$DFI.time/30.4
    data = as.data.frame(data)
    data = data[data$DFI.time>0,]
    data = data[!(is.na(data$DFI.time)),]
    data = data[!(is.na(data$DFI)),]
    return(data)
  }else if(SurvivalType == "PFI"){
    data$PFI.time = data$PFI.time/30.4
    data = as.data.frame(data)
    data = data[data$PFI.time>0,]
    data = data[!(is.na(data$PFI.time)),]
    data = data[!(is.na(data$PFI)),]
    return(data)
  }
}
