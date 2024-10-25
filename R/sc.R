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
#' @title sc_qc_mouse
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param min_Gene min_Gene default = 500
#' @param max_Gene max_Gene default = 4000
#' @param max_UMI max_UMI default = 15000
#' @param pct_mt pct_mt default = 10
#' @param pct_rb pct_rb default = 1
#' @param plot_featrures plot_featrures default = c("nFeature_RNA","nCount_RNA","percent.MT","percent.RB")
#' @param group group default = "orig.ident"
#' @param width width = 28
#' @param height height = 8
#' @param mycolor mycolor
#' @param alphas alphas default = 0.8
#' @return Seurat Object

sc_qc_mouse = function(scRNA,
                       min_Gene = 500,
                       max_Gene = 4000,
                       max_UMI = 15000,
                       pct_MT = 10,
                       pct_RB = 100,
                       plot_featrures = c("nFeature_RNA","nCount_RNA","percent.MT"),
                       group = "orig.ident",
                       width = 28,
                       height = 8,
                       alphas = 0.8,
                       mycolor = c("#BC3C29FF","#0072B5FF","#E18727FF",
                                   "#20854EFF","#7876B1FF","#6F99ADFF",
                                   "#FFDC91FF","#EE4C97FF","#E64B35FF",
                                   "#4DBBD5FF","#00A087FF","#3C5488FF",
                                   "#F39B7FFF","#8491B4FF","#91D1C2FF",
                                   "#DC0000FF","#7E6148FF","#B09C85FF",
                                   "#3B4992FF","#EE0000FF","#008B45FF",
                                   "#631879FF","#008280FF","#BB0021FF",
                                   "#5F559BFF","#A20056FF","#808180FF",
                                   "#00468BFF","#ED0000FF","#42B540FF",
                                   "#0099B4FF","#925E9FFF","#FDAF91FF",
                                   "#AD002AFF","#ADB6B6FF","#374E55FF",
                                   "#DF8F44FF","#00A1D5FF","#B24745FF",
                                   "#79AF97FF","#6A6599FF","#80796BFF",
                                   "#1f77b4",  "#ff7f0e",  "#279e68",
                                   "#d62728",  "#aa40fc",  "#8c564b",
                                   "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")){
  library(ggplot2)
  library(patchwork)
  scRNA[["percent.MT"]] = PercentageFeatureSet(scRNA, pattern = "^mt-")
  scRNA[["percent.RB"]] = PercentageFeatureSet(scRNA, pattern = "^Rp[sl]")

  plots_qc = list()
  for(i in seq_along(plot_featrures)){
    plots_qc[[paste0("before_",i)]] = VlnPlot(scRNA,
                         cols = alpha(mycolor,alphas),
                         group.by = group,
                         pt.size = 0,
                         features = plot_featrures[i]) +
      theme(axis.title.x = element_blank()) +
      NoLegend()
  }

  scRNA = subset(scRNA,
                 subset = percent.MT < pct_MT &
                          nCount_RNA < max_UMI &
                          nFeature_RNA > min_Gene &
                          nFeature_RNA < max_Gene &
                          percent.RB < pct_RB)

  for(i in seq_along(plot_featrures)){
    plots_qc[[paste0("after_",i)]] = VlnPlot(scRNA,
                         cols = alpha(mycolor,alphas),
                         group.by = group,
                         pt.size = 0,
                         features = plot_featrures[i]) +
      theme(axis.title.x=element_blank()) +
      NoLegend()
  }
  violin_qc = wrap_plots(plots = plots_qc, nrow = 2)

  pdf(file = "qc_before_and_after.pdf", width = width, height = height)
  print(violin_qc)
  dev.off()

  return(scRNA)
}
#' @title sc_qc_human
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param min_Gene min_Gene default = 500
#' @param max_Gene max_Gene default = 4000
#' @param max_UMI max_UMI default = 15000
#' @param pct_mt pct_mt default = 10
#' @param pct_rb pct_rb default = 1
#' @param plot_featrures plot_featrures default = c("nFeature_RNA","nCount_RNA","percent.MT","percent.RB")
#' @param group group default = "orig.ident"
#' @param width width = 28
#' @param height height = 8
#' @param alphas alphas default = 0.8
#' @param mycolor mycolor
#' @return Seurat Object

sc_qc_human = function(scRNA,
                       min_Gene = 500,
                       max_Gene = 4000,
                       max_UMI = 15000,
                       pct_MT = 10,
                       pct_RB = 100,
                       plot_featrures = c("nFeature_RNA","nCount_RNA","percent.MT"),
                       group = "orig.ident",
                       width = 28,
                       height = 8,
                       alphas = 0.8,
                       mycolor = c("#BC3C29FF","#0072B5FF","#E18727FF",
                                   "#20854EFF","#7876B1FF","#6F99ADFF",
                                   "#FFDC91FF","#EE4C97FF","#E64B35FF",
                                   "#4DBBD5FF","#00A087FF","#3C5488FF",
                                   "#F39B7FFF","#8491B4FF","#91D1C2FF",
                                   "#DC0000FF","#7E6148FF","#B09C85FF",
                                   "#3B4992FF","#EE0000FF","#008B45FF",
                                   "#631879FF","#008280FF","#BB0021FF",
                                   "#5F559BFF","#A20056FF","#808180FF",
                                   "#00468BFF","#ED0000FF","#42B540FF",
                                   "#0099B4FF","#925E9FFF","#FDAF91FF",
                                   "#AD002AFF","#ADB6B6FF","#374E55FF",
                                   "#DF8F44FF","#00A1D5FF","#B24745FF",
                                   "#79AF97FF","#6A6599FF","#80796BFF",
                                   "#1f77b4",  "#ff7f0e",  "#279e68",
                                   "#d62728",  "#aa40fc",  "#8c564b",
                                   "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")){
  library(ggplot2)
  library(patchwork)
  scRNA[["percent.MT"]] = PercentageFeatureSet(scRNA, pattern = "^mt-")
  scRNA[["percent.RB"]] = PercentageFeatureSet(scRNA, pattern = "^Rp[sl]")

  plots_qc = list()
  for(i in seq_along(plot_featrures)){
    plots_qc[[paste0("before_",i)]] = VlnPlot(scRNA,
                                              cols = alpha(mycolor,alphas),
                                              group.by = group,
                                              pt.size = 0,
                                              features = plot_featrures[i]) +
      theme(axis.title.x = element_blank()) +
      NoLegend()
  }

  scRNA = subset(scRNA,
                 subset = percent.MT < pct_MT &
                   nCount_RNA < max_UMI &
                   nFeature_RNA > min_Gene &
                   nFeature_RNA < max_Gene &
                   percent.RB < pct_RB)

  for(i in seq_along(plot_featrures)){
    plots_qc[[paste0("after_",i)]] = VlnPlot(scRNA,
                                             cols = alpha(mycolor,alphas),
                                             group.by = group,
                                             pt.size = 0,
                                             features = plot_featrures[i]) +
      theme(axis.title.x=element_blank()) +
      NoLegend()
  }
  violin_qc = wrap_plots(plots = plots_qc, nrow = 2)

  pdf(file = "qc_before_and_after.pdf", width = width, height = height)
  print(violin_qc)
  dev.off()

  return(scRNA)
}

#' @title sc_cellcycle_adjust
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param is_v5 is v5 Seurat Object default = TRUE
#' @param species species default = human or u use mouse
#' @param width width default = 6
#' @param height height default = 3
#' @param alphas alphas default = 0.8
#' @param mycolor mycolor
#' @return Seurat Object
sc_cellcycle_adjust = function(scRNA,
                            species = "human",
                            is_v5 = TRUE,
                            width = 6,
                            height = 3,
                            alphas = 0.8,
                            mycolor = c("#BC3C29FF","#0072B5FF","#E18727FF",
                                        "#20854EFF","#7876B1FF","#6F99ADFF",
                                        "#FFDC91FF","#EE4C97FF","#E64B35FF",
                                        "#4DBBD5FF","#00A087FF","#3C5488FF",
                                        "#F39B7FFF","#8491B4FF","#91D1C2FF",
                                        "#DC0000FF","#7E6148FF","#B09C85FF",
                                        "#3B4992FF","#EE0000FF","#008B45FF",
                                        "#631879FF","#008280FF","#BB0021FF",
                                        "#5F559BFF","#A20056FF","#808180FF",
                                        "#00468BFF","#ED0000FF","#42B540FF",
                                        "#0099B4FF","#925E9FFF","#FDAF91FF",
                                        "#AD002AFF","#ADB6B6FF","#374E55FF",
                                        "#DF8F44FF","#00A1D5FF","#B24745FF",
                                        "#79AF97FF","#6A6599FF","#80796BFF",
                                        "#1f77b4",  "#ff7f0e",  "#279e68",
                                        "#d62728",  "#aa40fc",  "#8c564b",
                                        "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")){
  if(is_v5 == TRUE){
  scRNA[["RNA"]] = as(scRNA[["RNA"]], "Assay")
  }
  if(species == "mouse"){
  data("mouse_cell_cycle_genes")
  mouse.s.genes = mouse_cell_cycle_genes[[1]]
  mouse.g2m.genes = mouse_cell_cycle_genes[[2]]
  scRNA = CellCycleScoring(object = scRNA,
                           g2m.features = mouse.g2m.genes,
                           s.features = mouse.s.genes)
  plot_S.Score = VlnPlot(scRNA,
                         features = c("S.Score"),
                         cols = alpha(mycolor,alphas)
                         ) + NoLegend()
  plot_G2M.Score = VlnPlot(scRNA,
                           features = c("G2M.Score"),
                           cols = alpha(mycolor,alphas)
                           ) + NoLegend()

  violin_cellcycle = wrap_plots(plots = list(plot_S.Score,plot_G2M.Score), nrow = 1)
  pdf(file = "sc_cellcycle_adjust_mouse.pdf", width = width, height = height)
  print(violin_cellcycle)
  dev.off()
  }else if(species == "human"){
    scRNA = CellCycleScoring(object = scRNA,
                             g2m.features = cc.genes$g2m.genes,
                             s.features = cc.genes$s.genes)
    plot_S.Score = VlnPlot(scRNA,
                           features = c("S.Score"),
                           cols = alpha(mycolor,alphas)
    ) + NoLegend()
    plot_G2M.Score = VlnPlot(scRNA,
                             features = c("G2M.Score"),
                             cols = alpha(mycolor,alphas)
    ) + NoLegend()
    violin_cellcycle = wrap_plots(plots = list(plot_S.Score,plot_G2M.Score), nrow = 1)
    pdf(file = "sc_cellcycle_adjust_human.pdf", width = width, height = height)
    print(violin_cellcycle)
    dev.off()
  }
  return(scRNA)
}

