#' @title limma_gene
#' @description limma gene
#' @param data row is gene, column name is sample.
#' @param GeneName Gene Name/symbol
#' @param pct proportion
#' @param DatasetName Custom output file name
#' @return csv and limma results
limma_gene = function(data,
                      GeneName,
                      pct = 0.3,
                      DatasetName = "DatasetName"){
  if(GeneName %in% rownames(data)){
    suppressPackageStartupMessages(library(limma))
    GeneData = data[GeneName,,drop = F]
    GeneData = t(GeneData)
    GeneData = GeneData[order(GeneData[,1],decreasing = T),,drop = F]
    GeneData_high_samples = rownames(GeneData[1:(nrow(GeneData) * pct),,drop = F])
    GeneData_low_samples  = rownames(GeneData[nrow(GeneData):(nrow(GeneData) - nrow(GeneData) * pct + 1),,drop = F])
    data_high_samples = data[,GeneData_high_samples]
    case_number = ncol(data_high_samples)
    data_low_samples = data[,GeneData_low_samples]
    control_number = ncol(data_low_samples)
    data_high_low = cbind(data_high_samples,data_low_samples)
    Type =  factor(c(rep("case",case_number),rep("control",control_number)))
    design = model.matrix(~0 + factor(Type))
    colnames(design) = levels(Type)
    rownames(design) = colnames(data_high_low)
    contrast.matrix = makeContrasts(case-control, levels = design)
    fit = lmFit(data_high_low, design)
    fit2 = contrasts.fit(fit, contrast.matrix)
    fit2 = eBayes(fit2)
    limmma_results = topTable(fit2, coef = 1, n = Inf, adjust.method = "BH", sort.by = "P")
    write.csv(limmma_results, paste0("easy_input_limma_",GeneName,"_",DatasetName,"_",pct,".csv"), quote = F)
    return(limmma_results)
  }
}
#' @title limma_gene_gsea
#' @description limma_gene_gsea
#' @param data row is gene, column name is sample.
#' @param GeneName Gene Name/symbol
#' @param pct proportion
#' @param gmt gmt file
#' @param gmtrds gmtrds file
#' @param DatasetName Custom output file name
#' @return csv and limma results
limma_gene_gsea = function(data,
                           GeneName,
                           DatasetName = "DatasetName",
                           pct = 0.3,
                           gmt = NULL,
                           gmtrds){
  if(GeneName %in% rownames(data)){
  suppressPackageStartupMessages(library(limma))
  suppressPackageStartupMessages(library(clusterProfiler))
  GeneData = data[GeneName,,drop = F]
  GeneData = t(GeneData)
  GeneData = GeneData[order(GeneData[,1],decreasing = T),,drop = F]
  GeneData_high_samples = rownames(GeneData[1:(nrow(GeneData) * pct),,drop = F])
  GeneData_low_samples  = rownames(GeneData[nrow(GeneData):(nrow(GeneData) - nrow(GeneData) * pct + 1),,drop = F])
  data_high_samples = data[,GeneData_high_samples]
  case_number = ncol(data_high_samples)
  data_low_samples = data[,GeneData_low_samples]
  control_number = ncol(data_low_samples)
  data_high_low = cbind(data_high_samples,data_low_samples)
  Type =  factor(c(rep("case",case_number),rep("control",control_number)))
  design = model.matrix(~0 + factor(Type))
  colnames(design) = levels(Type)
  rownames(design) = colnames(data_high_low)
  contrast.matrix = makeContrasts(case-control, levels = design)
  fit = lmFit(data_high_low, design)
  fit2 = contrasts.fit(fit, contrast.matrix)
  fit2 = eBayes(fit2)
  limmma_results = topTable(fit2, coef = 1, n = Inf, adjust.method = "BH", sort.by = "P")
  write.csv(limmma_results, paste0("easy_input_limma_",GeneName,"_",DatasetName,"_",pct,".csv"), quote = F)
  limmma_results$logFC = as.numeric(as.character(limmma_results$logFC))
  limmma_results_order = limmma_results[order(limmma_results$logFC, decreasing = T), ]
  GSEA_input = limmma_results_order$logFC
  names(GSEA_input) = rownames(limmma_results_order)
  if(!is.null(gmt)){
    gmtList = read.gmt(gmt)
  }else{
    gmtList = readRDS(gmtrds)
  }

  gseaRes = GSEA(geneList = GSEA_input,
                 minGSSize = 1,
                 maxGSSize = 500,
                 pvalueCutoff = 1,
                 seed = FALSE,
                 TERM2GENE = gmtList)
  return(gseaRes)
  }
}
#' @title limma_gene_gsea_customized
#' @description limma_gene_gsea_customized
#' @param data row is gene, column name is sample.
#' @param GeneName Gene Name/symbol
#' @param pct proportion
#' @param customized customized file
#' @param DatasetName Custom output file name
#' @return csv and limma results
limma_gene_gsea_customized = function(data,
                                      GeneName,
                                      customized,
                                      pct = 0.3,
                                      DatasetName = "DatasetName"){
  if(GeneName %in% rownames(data)){
  suppressPackageStartupMessages(library(limma))
  suppressPackageStartupMessages(library(clusterProfiler))
  suppressPackageStartupMessages(library(msigdbr))
  suppressPackageStartupMessages(library(DOSE))
  suppressPackageStartupMessages(library(enrichplot))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(RColorBrewer))
  suppressPackageStartupMessages(library(gridExtra))
  suppressPackageStartupMessages(library(ggrepel))
  limmma_results = limma_gene(data = data, GeneName = GeneName, pct = pct,DatasetName = DatasetName)
  limmma_results = na.omit(limmma_results)
  limmma_results$logFC = as.numeric(as.character(limmma_results$logFC))
  limmma_results_order = limmma_results[order(limmma_results$logFC, decreasing = T), ]
  GSEA_input = limmma_results_order$logFC
  names(GSEA_input) = rownames(limmma_results_order)
  kk = GSEA(geneList = GSEA_input ,
            TERM2GENE = customized,
            verbose = F,
            minGSSize = 2,
            maxGSSize = 500,
            nPerm = 10000,
            pvalueCutoff = 1
  )
  write.csv(kk, paste0(GeneName,"_",DatasetName,"_",pct,"_GSEA.csv"))
  plot = gseaplot2(kk, geneSetID=c(1:dim(kk)[1]),
                   color = c("#E64B35FF", "#4DBBD5FF","#3C5488FF","#00A087FF","#F39B7FFF","#8491B4FF","#91D1C2FF","#DC0000FF","#7E6148FF","#B09C85FF")[1:dim(kk)[1]])
  addTab = as.data.frame(as.matrix(ifelse(round(kk@result[["pvalue"]], 3) < 0.001, "<0.001", paste0(round(kk@result[["pvalue"]], 3)))),
                         as.matrix(paste0(round(kk@result$NES,3))))
  addTab_next = data.frame(row.names(addTab), addTab$V1)
  colnames(addTab_next) = c("NES","pvalue")
  rownames(addTab_next) = "GSEA"
  df = tibble(x = 1, y = 1, tb = list(addTab_next))
  suppressPackageStartupMessages(library(tibble))
  suppressPackageStartupMessages(library(ggpp))
  plot[[1]] = plot[[1]] + geom_table(data = df, aes(x = x, y = y, label = tb), table.rownames = TRUE)
  ggsave(paste0(GeneName,"_",DatasetName,"_",pct,"_GSEA.pdf"), plot = plot ,width=5, height=6)
  }
}
#' @title customized
#' @description customized
#' @param geneset Gene Names/symbols
#' @return customized results
customized = function(geneset = c('IL2','GZMA','GNLY','PRF1','GZMB','GZMK','IFNG','NKG7')){
  customized = data.frame(geneset)
  customized$term = "Custom"
  colnames(customized) = c("gene","term")
  customized = customized[,c(2,1),drop=F]
  return(customized)
}
