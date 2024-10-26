#' @title sc_run_AUCell
#' @description sc run AUCell
#' @param scRNA Seurat Object
#' @param markers markers
#' @param cores_use default = 2
sc_run_AUCell = function(scRNA,markers,cores_use = 2){
  library(Seurat)
  library(AUCell)
  markers = intersect(markers,rownames(scRNA))
  geneSets = list(Custom = markers)
  cells_rankings = AUCell_buildRankings(as.matrix(GetAssayData(object = scRNA, assay = "RNA")),
                                        nCores = cores_use)
  cells_AUC = AUCell_calcAUC(geneSets,
                             cells_rankings,
                             nCores = cores_use,
                             aucMaxRank = nrow(cells_rankings)*0.05)
  #AUC_Exp = as.numeric(getAUC(cells_AUC)["Custom",])
  #scRNA$AUC_Exp = AUC_Exp
  AUC_Score = cells_AUC@assays@data@listData[["AUC"]]
  AUC_Score = t(AUC_Score)
  colnames(AUC_Score) = "AUC"
  scRNA@meta.data = merge_row(scRNA@meta.data,AUC_Score)
  return(scRNA)
}
#' @title sc_run_UCell
#' @description sc run UCell
#' @param scRNA Seurat Object
#' @param markers markers
#' @param cores_use default = 2
#' @param name_use  default = ""
sc_run_UCell = function(scRNA,markers,cores_use = 2, name_use = ""){
  library(Seurat)
  library(UCell)
  markers = intersect(markers,rownames(scRNA))
  geneSets = list(UCell = markers)
  scRNA = AddModuleScore_UCell(scRNA,features = geneSets,ncores = cores_use,name = name_use)
  return(scRNA)
}

#' @title sc_run_AddModuleScore
#' @description sc run AddModuleScore
#' @param scRNA Seurat Object
#' @param markers markers
#' @param name_use default = 'AddModuleScore'
sc_run_AddModuleScore = function(scRNA,markers,name_use = 'AddModuleScore'){
  library(Seurat)
  markers = intersect(markers,rownames(scRNA))
  geneSets = list(Custom = markers)
  scRNA = AddModuleScore(object = scRNA,features = geneSets, ctrl = 100, name = name_use)
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data) == paste0(name_use,"1")] = name_use
  return(scRNA)
}


