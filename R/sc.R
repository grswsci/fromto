#' @title sc_10x
#' @description One key to prepare multiple samples of 10x three files
#' @param path working directory
#' @param dir_name_idx Sample name index
#' @param sc_10x_name_idx 10x three files index
#' @param str What to divide it by
#' @return 10x three files
sc_10x = function(path,
                  dir_name_idx = 1,
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
    dir_include = strsplit_fromto(variable,str, dir_name_idx)
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
  if(length(samples) > 1){
  scRNA = merge(scRNAlist[[1]], scRNAlist[2:length(scRNAlist)])
  }else{
    scRNA = scRNAlist[[1]]
  }
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
  scRNA[["percent.MT"]] = PercentageFeatureSet(scRNA, pattern = "^MT-")
  scRNA[["percent.RB"]] = PercentageFeatureSet(scRNA, pattern = "^RP[SL]")

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
  library(ggplot2)
  library(patchwork)
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
                         pt.size = 0,
                         cols = alpha(mycolor,alphas)
                         ) + NoLegend()
  plot_G2M.Score = VlnPlot(scRNA,
                           features = c("G2M.Score"),
                           pt.size = 0,
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
#' @title sc_scale
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param methods_my methods_my default = NA, or set as "sct"
#' @param if_vars_to_regress if_vars_to_regress default = "TRUE", or set as "FALSE"
sc_scale = function(scRNA,
                    methods_my = NA,
                    if_vars_to_regress = "TRUE") {
  if(is.na(methods_my)){
    scRNA = NormalizeData(object = scRNA,
                          normalization.method = "LogNormalize",
                          scale.factor = 10000)
    scRNA = FindVariableFeatures(object = scRNA,
                                 selection.method = "vst",
                                 nfeatures = 2000)
    if(if_vars_to_regress == FALSE){
      scRNA = ScaleData(scRNA)
    }else{
      scRNA = ScaleData(scRNA,vars.to.regress = c("S.Score", "G2M.Score"))
    }

  }else if(methods_my == "sct"){
    if(if_vars_to_regress == FALSE){
      scRNA = SCTransform(scRNA)
    }else{
      scRNA = SCTransform(scRNA,vars.to.regress = c("S.Score", "G2M.Score"))
    }
  }
  return(scRNA)
}

#' @title sc_select_pc
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param npcs npcs default = 20
#' @param width width default = 8
#' @param height height default = 2
sc_select_pc = function(scRNA,
                        npcs = 30,
                        width = 12,
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
                                    "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")) {
  library(ggplot2)
  library(patchwork)
  scRNA = RunPCA(scRNA,
                 npcs = npcs,
                 pc.genes = VariableFeatures(object = scRNA),
                 verbose = FALSE)
  plot_Variable = VariableFeaturePlot(object = scRNA)+ NoLegend()
  plot_Variable_top = LabelPoints(plot = plot_Variable,
                       points = head(x = VariableFeatures(object = scRNA), 3),
                       repel = TRUE) + NoLegend()
  plot_DimPlot = DimPlot(object = scRNA, reduction = "pca",cols = alpha(mycolor,alphas))
  plot_ElbowPlot = ElbowPlot(scRNA,ndims = npcs)

  plot_RunPCA = wrap_plots(plots = list(plot_Variable,
                                        plot_Variable_top,
                                        plot_DimPlot,
                                        plot_ElbowPlot), nrow = 1)
  pdf(file = "sc_RunPCA.pdf", width = width, height = height)
  print(plot_RunPCA)
  dev.off()
  # Determine percent of variation associated with each PC
  pct = scRNA[["pca"]]@stdev / sum(scRNA[["pca"]]@stdev) * 100
  # Calculate cumulative percents for each PC
  cumu = cumsum(pct)
  # Determine which PC exhibits cumulative percent greater than 90% and % variation associated with the PC as less than 5
  co1 = which(cumu > 90 & pct < 5)[1]
  # Determine the difference between variation of PC and subsequent PC，last point where change of % of variation is more than 0.1
  co2 = sort(which((pct[1:length(pct) - 1] - pct[2:length(pct)]) > 0.1), decreasing = T)[1] + 1
  # Max of the two calculation
  pc_select = max(co1, co2)
  print(pc_select)

  scRNA = RunPCA(scRNA,
                 npcs = pc_select,
                 pc.genes = VariableFeatures(object = scRNA),
                 verbose = FALSE)
  return(scRNA)
}

#' @title sc_run_harmony
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param group_by_vars default = "orig.ident"
#' @param reduction_use default = "pca"
#' @param assay_use default = "RNA"
#' @param max_iter_harmony default = 30
#' @param width width default = 6
#' @param height height default = 3
sc_run_harmony = function(scRNA,
                        group_by_vars = "orig.ident",
                        reduction_use = "pca",
                        assay_use = "RNA",
                        max_iter_harmony = 30,
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
                                    "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")) {
  library(ggplot2)
  library(patchwork)
  library(harmony)
  scRNA = RunHarmony(object = scRNA,
                     group.by.vars = group_by_vars,
                     reduction.use = reduction_use,
                     assay.use = assay_use,
                     max.iter.harmony = max_iter_harmony)
  plot_Dim = DimPlot(object = scRNA,
                     reduction = "harmony",
                     cols = alpha(mycolor,alphas)) + NoLegend()
  plot_Elbow = ElbowPlot(scRNA,
                         reduction = "harmony",
                         ndims = ncol(scRNA@reductions[["pca"]]@cell.embeddings))
  plot_RunHarmony = wrap_plots(plots = list(plot_Dim,plot_Elbow), nrow = 1)
  pdf(file = "sc_RunHarmony.pdf", width = width, height = height)
  print(plot_RunHarmony)
  dev.off()
  return(scRNA)
}
#' @title sc_run_umap_tsne
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param resolution default = 0.8
#' @param reduction_use default = "pca"
sc_run_umap_tsne = function(scRNA,
                            reduction_use = "harmony",
                            resolution = 0.8) {
  scRNA = FindNeighbors(object = scRNA, reduction.use = reduction_use)
  scRNA = FindClusters(object = scRNA, resolution = resolution)
  scRNA = RunUMAP(object = scRNA, dims = 1:ncol(scRNA@reductions[[reduction_use]]),reduction = reduction_use)
  scRNA = RunTSNE(object = scRNA, reduction = reduction_use)
  return(scRNA)
}
#' @title sc_run_DoubletFinder
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param if_sct default = FALSE
sc_run_DoubletFinder = function(scRNA,if_sct = FALSE){
  library(DoubletFinder)
  sweep.res.list = paramSweep(scRNA, PCs = 1:ncol(scRNA@reductions[["pca"]]), sct = if_sct)
  sweep.stats = summarizeSweep(sweep.res.list, GT = FALSE)
  sweep.stats[order(sweep.stats$BCreal),]
  bcmvn = find.pK(sweep.stats)
  pK_bcmvn = as.numeric(as.vector(bcmvn$pK[which.max(bcmvn$BCmetric)]))
  DoubletRate = ncol(scRNA)*8*1e-6
  homotypic.prop = modelHomotypic(scRNA$seurat_clusters)
  nExp_poi = round(DoubletRate*nrow(scRNA@meta.data))
  nExp_poi.adj = round(nExp_poi*(1-homotypic.prop))
  scRNA = doubletFinder(seu = scRNA,
                        PCs = 1:ncol(scRNA@reductions[["pca"]]),
                        pN = 0.25,
                        pK = pK_bcmvn,
                        nExp = nExp_poi.adj,
                        reuse.pANN = FALSE,
                        sct = if_sct)
  return(scRNA)
}
#' @title sc_run_clustree
#' @description sc_run_clustree
#' @param scRNA Seurat Object
#' @param from default = 0.1
#' @param to default = 1
#' @param seqs default = 0.1
#' @param reduction_use default = 0.1
#' @param height default = 15
#' @param width default = 10
sc_run_clustree = function(scRNA,
                           from = 0.1,
                           to = 1,
                           seqs = 0.1,
                           reduction_use = "harmony",
                           height = 15,
                           width = 10) {
  library(clustree)
  scRNA = FindNeighbors(scRNA, reduction = reduction_use,dims = 1:ncol(scRNA@reductions[[reduction_use]]))
  scRNA = FindClusters(object = scRNA,resolution = c(seq(from,to,seqs)))
  plot_clustree = clustree(scRNA@meta.data,prefix = "RNA_snn_res.")
  pdf(file="sc_clustree.pdf",height = height, width = width)
  print(plot_clustree)
  dev.off()
  return(scRNA)
}
#' @title sc_call_back
#' @description sc call back
#' @param scRNA Seurat Object with main label
#' @param scRNA_subset Subset Seurat Object with fine label
#' @param label_scRNA default = "CellType_FineLabel"
#' @param label_scRNA default = "CellType"
sc_call_back = function(scRNA,
                        scRNA_subset,
                        label_scRNA = "CellType_FineLabel",
                        label_scRNA_subset = "CellType") {
  scRNA@meta.data[,label_scRNA][match(colnames(scRNA_subset), colnames(scRNA))] = scRNA_subset@meta.data[,label_scRNA_subset]
  return(scRNA)
}

#' @title sc_name_cell_with_gene
#' @description name cell with gene
#' @param scRNA Seurat Object with main label
#' @param slot default = "counts", or use "data"
#' @param gene_use gene symbol
#' @param cell_use cell name
#' @param label_use label in colnames(scRNA@meta.data)
#' @param label_new label_new in colnames(scRNA@meta.data)
#' @param method_use default =  "pn", means positive and negative. or use "med", means median
sc_name_cell_with_gene = function(scRNA,
                                  slot = "counts",
                                  gene_use,
                                  cell_use,
                                  label_use = "celltype",
                                  label_new = "CellType_New",
                                  method_use = "pn"){
  library(tidyverse)
  GeneData = GetAssayData(object = scRNA, assay = "RNA",slot = slot) %>% .[c(gene_use),] %>%
    as.matrix() %>%
    as.data.frame()

  if(method_use == "pn"){
    GeneData$GeneType = ifelse(GeneData$V1 > 0,"Positive","Negative")
    colnames(GeneData)[1] = gene_use

    scRNA@meta.data = merge_row(scRNA@meta.data,GeneData)
    scRNA@meta.data[,label_new] = scRNA@meta.data[,label_use]
    scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                            scRNA@meta.data$GeneType == "Positive",
                                         paste0(gene_use,"+ ",cell_use),scRNA@meta.data[,label_new])
    scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                           scRNA@meta.data$GeneType == "Negative",
                                         paste0(gene_use,"- ",cell_use),scRNA@meta.data[,label_new])
  }else if(method_use == "med"){
    if(median(GeneData$V1) == 0){
      message("median value is 0, use pn param automaticly")
      GeneData$GeneType = ifelse(GeneData$V1 > 0,"Positive","Negative")
      colnames(GeneData)[1] = gene_use

      scRNA@meta.data = merge_row(scRNA@meta.data,GeneData)
      scRNA@meta.data[,label_new] = scRNA@meta.data[,label_use]
      scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                             scRNA@meta.data$GeneType == "Positive",
                                           paste0(gene_use,"+ ",cell_use),scRNA@meta.data[,label_new])
      scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                             scRNA@meta.data$GeneType == "Negative",
                                           paste0(gene_use,"- ",cell_use),scRNA@meta.data[,label_new])
    }else{
      GeneData$GeneType = ifelse(GeneData$V1 > median(GeneData$V1),"High","Low")
      colnames(GeneData)[1] = gene_use

      scRNA@meta.data = merge_row(scRNA@meta.data,GeneData)
      scRNA@meta.data[,label_new] = scRNA@meta.data[,label_use]
      scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                             scRNA@meta.data$GeneType == "High",
                                           paste0(gene_use,"+ ",cell_use),scRNA@meta.data[,label_new])
      scRNA@meta.data[,label_new] = ifelse(scRNA@meta.data[,label_new] == cell_use &
                                             scRNA@meta.data$GeneType == "Low",
                                           paste0(gene_use,"- ",cell_use),scRNA@meta.data[,label_new])
    }
  }
  return(scRNA)
}
#' @title sc_tisch2_reanalysis
#' @description name cell with gene
#' @param scRNA Seurat Object with main label
#' @param nfeatures default = 2000
#' @param cell_use cell name
#' @param label_use label in colnames(scRNA@meta.data)
#' @param resolution_use default = 1
#' @param reduction_use default = "pca"
sc_tisch2_reanalysis = function(scRNA,
                                nfeatures = 2000,
                                cell_use,
                                label_use = "CellType_FineLabel",
                                resolution_use = 1,
                                reduction_use = "pca"){
  scRNA_subset = scRNA[,scRNA@meta.data[,label_use] %in% cell_use]
  scRNA_subset[["RNA"]] = as(scRNA_subset[["RNA"]], "Assay")
  scRNA_subset = FindVariableFeatures(object = scRNA_subset, selection.method = "vst", nfeatures = nfeatures)
  scRNA_subset = ScaleData(scRNA_subset)
  scRNA_subset = sc_select_pc(scRNA = scRNA_subset,npcs = 30,width = 12,height = 3)
  scRNA_subset = sc_run_clustree(scRNA_subset,reduction_use = reduction_use)
  scRNA_subset = sc_run_umap_tsne(scRNA = scRNA_subset, reduction_use = reduction_use, resolution = resolution_use)
  return(scRNA_subset)
}

#' @title sc_run_nmf
#' @description sc_run_nmf
#' @param scRNA Seurat Object with main label
sc_run_nmf = function(scRNA, nfeatures = 2000,rank_use = 12){
  library(Seurat)
  library(NMF)
  library(tidyverse)
  scRNA = FindVariableFeatures(object = scRNA, selection.method = "vst", nfeatures = nfeatures)
  scRNA = ScaleData(scRNA,do.center = FALSE)
  vm = scRNA@assays$RNA$scale.data
  res = nmf(vm,rank_use,method = "snmf/r")

  fs = extractFeatures(res, 30L)
  fs = lapply(fs, function(x) rownames(res)[x])
  fs = do.call("rbind", fs)

  rownames(fs) = paste0("cluster",1:rank_use)
  write.csv(t(fs), "c_NMF_TopGenes.csv")
  DT::datatable(t(fs))
  scRNA = RunPCA(scRNA,verbose = F)
  scRNA@reductions$nmf = scRNA@reductions$pca
  scRNA@reductions$nmf@cell.embeddings = t(coef(res))
  scRNA@reductions$nmf@feature.loadings = basis(res)
  scRNA = RunUMAP(scRNA, reduction = 'nmf', dims = 1:rank_use)

  ## 基于NMF降维矩阵的聚类
  scRNA = FindNeighbors(scRNA, reduction='nmf', dims = 1:rank_use) %>% FindClusters()

  ## 基于因子最大载荷分类
  scRNA$nmf_cluster = apply(NMF::coefficients(res)[1:rank_use,], 2, which.max)

  return(scRNA)
}
