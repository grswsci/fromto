#' @title sc_10x
#' @description One key to prepare multiple samples of 10x three files
#' @param path working directory
#' @param dir_name_idx Sample name index
#' @param sc_10x_name_idx 10x three files index
#' @param str What to divide it by
#' @return 10x three files
sc_10x = function(path,
                  dir_name_idx = 2,
                  sc_10x_name_idx = 4,
                  str = "_"){
  setwd(path)
  options(warn = -1)
  library(R.utils)
  list_files = list.files()
  list_files = list_files[is_in(".gz",list_files)]
  list_files_sample = unique(strsplit_fromto(list_files,str,dir_name_idx))
  for (sample_name in list_files_sample) {
    dir.create(sample_name, recursive = TRUE, showWarnings = TRUE)
    }
  for (variable in list_files) {
    dir_include = strsplit_fromto(variable,str,2)
    file.copy(from = variable,
              to = paste0(dir_include,"/",variable))
  }
  for (dir_sample in list_files_sample) {
    setwd(paste0(path,"/",dir_sample))
    list_files_sample = list.files()
  for (variable in list_files_sample) {
    gunzip(variable, remove = FALSE, overwrite = TRUE)
    file.remove(variable)
    file.rename(gsub(".gz","",variable),
                strsplit_fromto(gsub(".gz","",variable),str,sc_10x_name_idx))
    file.rename("features.tsv", "genes.tsv")
  }
}
}
#' @title sc_10x_seurat
#' @description One key to Create Seurat Object
#' @param path working directory
#' @param if_join if Join Layers default = TRUE
#' @param min.cells min.cells default = 3
#' @param min.features min.features default = 200
#' @return Seurat Object
sc_10x_seurat = function(path,
                         if_join = TRUE,
                         min.cells = 3,
                         min.features = 200){
  library(Seurat)
  samples = list.dirs(path)[2:length(list.dirs(path))]
  samples = gsub(paste0(path,"/"),"",samples,fixed = TRUE)
  scRNAlist = lapply(samples,function(samples){
    folder = file.path(path,samples)
    sce = CreateSeuratObject(counts = Read10X(folder),
                             project = samples,
                             min.cells = min.cells,
                             min.features = min.features)
    colnames(sce) = paste0(samples,"_",colnames(sce))
    return(sce)
  })
  scRNA = merge(scRNAlist[[1]], scRNAlist[2:length(scRNAlist)])
  if(if_join == FALSE){
    return(scRNA)
  }else{
    scRNA_combined = JoinLayers(scRNA)
    return(scRNA_combined)
  }
}
