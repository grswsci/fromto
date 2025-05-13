#' @title bplot_scmarkers
#' @description bplot scmarkers
#' @param scRNA Seurat object
#' @param label celltype or seurat_clusters or others
#' @param width pdf width
#' @param height pdfheight
#' @param mycolor mycolor
#' @return pdf and ggplot object
#' @export
bplot_scmarkers = function(scRNA,
                           label = "seurat_clusters",
                            width = 20,
                           height = 8,
                      Neutrophils = NA,
                        Basophils = NA,
                      Eosinophils = NA,
                            Masts = c('TPSAB1','TPSB2','CPA3','FCER1A','KIT','MS4A2','GATA2'),
                              DCs = c('CLEC10A','LAMP3','ITGAX'),
                            cDC1s = c('CLEC9A'),
                            cDC2s = c('CD1C'),
                             pDCs = c('CLEC4C'),
                          Plasmas = c('IGHG1',"MZB1","TNFRSF17","SLAMF7","XBP1"),
                               Bs = c('CD79A','CD79B'),
                               Ts = c('CD3D','CD3E','CD3G'),
                            MAITs = c("TRAV1-2", "KLRB1", "ZBTB16"),
                             gdTs = c("TRDC", "TRGC") ,
                             CD4s = c('CD4'),
                          CD4Tfhs = c("CXCR5", "ICOS", "BCL6"),
                          CD4Th1s = c("TBX21"),#"IFNG", "CXCR3"
                          CD4Th2s = c("GATA3", "IL4", "IL5", "IL13"),
                          CD4Th9s = c("SPI1", "IL9"),
                         CD4Th17s = c("RORC", "IL17A", "IL17F", "IL23R"),
                          CD4Tr1s = c("IL10"),
                             CD8s = c('CD8A','CD8B'),
                             Texs = c('LAG3','TIGIT','PDCD1','HAVCR2'),
                            Tregs = c('FOXP3','CTLA4','IL2RA'),
                              Tns = c('TCF7','SELL','LEF1','CCR7'),
                            Teffs = c("KLRG1","NKG7", "GNLY","GZMB","PRF1","IFNG"),
                             Tsts = c('HSPA1A','HSPA1B','HSPH1'),
                             Tcms = c('FAS','CXCR3'),
                             Tems = NA,
                              NKs = c("NCR1","KLRC1","KIR2DL4","XCL1","XCL2","IL18R1",'KLRD1','NCAM1'),
                        Monocytes = c('CD14','FCGR3A','S100A12','CD300E','FCN1'),
                      Macrophages = c('LYZ',"CD68","C1QC","CSF1R",'AIF1','ALB'),
                              M1s = c('NOS2'),
                              M2s = c('MCR1','ARG1'),
                   Cardiomyocytes = c('ACTC1','MYH7','TNNT2','TNNI3','TTN','ACTN2','MYH6'),
                      Fibroblasts = c('DCN','COL1A1','COL1A2','THY1'),
                     Endothelials = c('TEK','PECAM1','FLT1','VWF'),
                     Mesothelials = c('MSLN','UPK3B','CALB2','WT1'),
                             SMCs = c('ACTA2','MYH11'),
                      Epithelials = c('EPCAM','KRT19','CDH1','KRT18'),
                          Prolifs = c('MKI67'),
                        Mal_PNETs = c('SYP','CHGA'),
                             Mals = NA,
                         name_use = "Cell",
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
 suppressPackageStartupMessages(library(ggplot2))
 suppressPackageStartupMessages(library(tidyverse))
  if(length(intersect(row.names(scRNA),Neutrophils)) == 0){
    Neutrophil = ""
    Neutrophil_rep = ""
  }else{
    Neutrophil = c(intersect(row.names(scRNA),Neutrophils))
    Neutrophil_rep = rep("Neutro",length(Neutrophil))
  }

  if(length(intersect(row.names(scRNA),Basophils)) == 0){
    Basophil = ""
    Basophil_rep = ""
  }else{
    Basophil = c(intersect(row.names(scRNA),Basophils))
    Basophil_rep = rep("Baso",length(Basophil))
  }

  if(length(intersect(row.names(scRNA),Eosinophils)) == 0){
    Eosinophil = ""
    Eosinophil_rep = ""
  }else{
    Eosinophil = c(intersect(row.names(scRNA),Eosinophils))
    Eosinophil_rep = rep("Eosino",length(Eosinophil))
  }

  if(length(intersect(row.names(scRNA),Masts)) == 0){
    Mast = ""
    Mast_rep = ""
  }else{
    Mast = c(intersect(row.names(scRNA),Masts))
    Mast_rep = rep("Mast",length(Mast))
  }

  if(length(intersect(row.names(scRNA),DCs)) == 0){
    DC = ""
    DC_rep = ""
  }else{
    DC = c(intersect(row.names(scRNA),DCs))
    DC_rep = rep("DC",length(DC))
  }

  if(length(intersect(row.names(scRNA),cDC1s)) == 0){
    cDC1 = ""
    cDC1_rep = ""
  }else{
    cDC1 = c(intersect(row.names(scRNA),cDC1s))
    cDC1_rep = rep("cDC1",length(cDC1))
  }

  if(length(intersect(row.names(scRNA),cDC2s)) == 0){
    cDC2 = ""
    cDC2_rep = ""
  }else{
    cDC2 = c(intersect(row.names(scRNA),cDC2s))
    cDC2_rep = rep("cDC2",length(cDC2))
  }

  if(length(intersect(row.names(scRNA),pDCs)) == 0){
    pDC = ""
    pDC_rep = ""
  }else{
    pDC = c(intersect(row.names(scRNA),pDCs))
    pDC_rep = rep("pDC",length(pDC))
  }

  if(length(intersect(row.names(scRNA),Plasmas)) == 0){
    Plasma = ""
    Plasma_rep = ""
  }else{
    Plasma = c(intersect(row.names(scRNA),Plasmas))
    Plasma_rep = rep("Plasma",length(Plasma))
  }

  if(length(intersect(row.names(scRNA),Bs)) == 0){
    B = ""
    B_rep = ""
  }else{
    B = c(intersect(row.names(scRNA),Bs))
    B_rep = rep("B",length(B))
  }

  if(length(intersect(row.names(scRNA),Ts)) == 0){
    T = ""
    T_rep = ""
  }else{
    T = c(intersect(row.names(scRNA),Ts))
    T_rep = rep("T",length(T))
  }

  if(length(intersect(row.names(scRNA),CD4s)) == 0){
    CD4 = ""
    CD4_rep = ""
  }else{
    CD4 = c(intersect(row.names(scRNA),CD4s))
    CD4_rep = rep("CD4",length(CD4))
  }

  if(length(intersect(row.names(scRNA),CD8s)) == 0){
    CD8 = ""
    CD8_rep = ""
  }else{
    CD8 = c(intersect(row.names(scRNA),CD8s))
    CD8_rep = rep("CD8",length(CD8))
  }

  if(length(intersect(row.names(scRNA),Texs)) == 0){
    Tex = ""
    Tex_rep = ""
  }else{
    Tex = c(intersect(row.names(scRNA),Texs))
    Tex_rep = rep("Tex",length(Tex))
  }

  if(length(intersect(row.names(scRNA),Tregs)) == 0){
    Treg = ""
    Treg_rep = ""
  }else{
    Treg = c(intersect(row.names(scRNA),Tregs))
    Treg_rep = rep("Treg",length(Treg))
  }

  if(length(intersect(row.names(scRNA),Tns)) == 0){
    Tn = ""
    Tn_rep = ""
  }else{
    Tn = c(intersect(row.names(scRNA),Tns))
    Tn_rep = rep("Tn",length(Tn))
  }

  if(length(intersect(row.names(scRNA),Teffs)) == 0){
    Teff = ""
    Teff_rep = ""
  }else{
    Teff = c(intersect(row.names(scRNA),Teffs))
    Teff_rep = rep("Teff",length(Teff))
  }

 if(length(intersect(row.names(scRNA),Tsts)) == 0){
   Tst = ""
   Tst_rep = ""
 }else{
   Tst = c(intersect(row.names(scRNA),Tsts))
   Tst_rep = rep("Tst",length(Tst))
 }

  if(length(intersect(row.names(scRNA),NKs)) == 0){
    NK = ""
    NK_rep = ""
  }else{
    NK = c(intersect(row.names(scRNA),NKs))
    NK_rep = rep("NK",length(NK))
  }

  if(length(intersect(row.names(scRNA),Monocytes)) == 0){
    Monocyte = ""
    Monocyte_rep = ""
  }else{
    Monocyte = c(intersect(row.names(scRNA),Monocytes))
    Monocyte_rep = rep("Mono",length(Monocyte))
  }

  if(length(intersect(row.names(scRNA),Macrophages)) == 0){
    Macrophage = ""
    Macrophage_rep = ""
  }else{
    Macrophage = c(intersect(row.names(scRNA),Macrophages))
    Macrophage_rep = rep("Macro",length(Macrophage))
  }

  if(length(intersect(row.names(scRNA),M1s)) == 0){
    M1 = ""
    M1_rep = ""
  }else{
    M1 = c(intersect(row.names(scRNA),M1s))
    M1_rep = rep("M1",length(M1))
  }

  if(length(intersect(row.names(scRNA),M2s)) == 0){
    M2 = ""
    M2_rep = ""
  }else{
    M2 = c(intersect(row.names(scRNA),M2s))
    M2_rep = rep("M2",length(M2))
  }

  if(length(intersect(row.names(scRNA),Cardiomyocytes)) == 0){
    Cardiomyocyte = ""
    Cardiomyocyte_rep = ""
  }else{
    Cardiomyocyte = c(intersect(row.names(scRNA),Cardiomyocytes))
    Cardiomyocyte_rep = rep("Cardio",length(Cardiomyocyte))
  }

  if(length(intersect(row.names(scRNA),Fibroblasts)) == 0){
    Fibroblast = ""
    Fibroblast_rep = ""
  }else{
    Fibroblast = c(intersect(row.names(scRNA),Fibroblasts))
    Fibroblast_rep = rep("Fibro",length(Fibroblast))
  }

  if(length(intersect(row.names(scRNA),Endothelials)) == 0){
    Endothelial = ""
    Endothelial_rep = ""
  }else{
    Endothelial = c(intersect(row.names(scRNA),Endothelials))
    Endothelial_rep = rep("Endo",length(Endothelial))
  }

  if(length(intersect(row.names(scRNA),Mesothelials)) == 0){
    Mesothelial = ""
    Mesothelial_rep = ""
  }else{
    Mesothelial = c(intersect(row.names(scRNA),Mesothelials))
    Mesothelial_rep = rep("Meso",length(Mesothelial))
  }

  if(length(intersect(row.names(scRNA),SMCs)) == 0){
    SMC = ""
    SMC_rep = ""
  }else{
    SMC = c(intersect(row.names(scRNA),SMCs))
    SMC_rep = rep("SMC",length(SMC))
  }

  if(length(intersect(row.names(scRNA),Epithelials)) == 0){
    Epithelial = ""
    Epithelial_rep = ""
  }else{
    Epithelial = c(intersect(row.names(scRNA),Epithelials))
    Epithelial_rep = rep("Epith",length(Epithelial))
  }

 if(length(intersect(row.names(scRNA),Prolifs)) == 0){
   Prolif = ""
   Prolif_rep = ""
 }else{
   Prolif = c(intersect(row.names(scRNA),Prolifs))
   Prolif_rep = rep("Prolif",length(Prolif))
 }

 if(length(intersect(row.names(scRNA),Mal_PNETs)) == 0){
   Mal_PNET = ""
   Mal_PNET_rep = ""
 }else{
   Mal_PNET = c(intersect(row.names(scRNA),Mal_PNETs))
   Mal_PNET_rep = rep("PNET",length(Mal_PNET))
 }

 if(length(intersect(row.names(scRNA),Mals)) == 0){
   Mal = ""
   Mal_rep = ""
 }else{
   Mal = c(intersect(row.names(scRNA),Mals))
   Mal_rep = rep("Mal",length(Mal))
 }

 if(length(intersect(row.names(scRNA),MAITs)) == 0){
   MAIT = ""
   MAIT_rep = ""
 }else{
   MAIT = c(intersect(row.names(scRNA),MAITs))
   MAIT_rep = rep("MAIT",length(MAIT))
 }

 if(length(intersect(row.names(scRNA),gdTs)) == 0){
   gdT = ""
   gdT_rep = ""
 }else{
   gdT = c(intersect(row.names(scRNA),gdTs))
   gdT_rep = rep("gdT",length(gdT))
 }
 if(length(intersect(row.names(scRNA),CD4Tfhs)) == 0){
   CD4Tfh = ""
   CD4Tfh_rep = ""
 }else{
   CD4Tfh = c(intersect(row.names(scRNA),CD4Tfhs))
   CD4Tfh_rep = rep("CD4Tfh",length(CD4Tfh))
 }
 if(length(intersect(row.names(scRNA),CD4Th1s)) == 0){
   CD4Th1 = ""
   CD4Th1_rep = ""
 }else{
   CD4Th1 = c(intersect(row.names(scRNA),CD4Th1s))
   CD4Th1_rep = rep("CD4Th1",length(CD4Th1))
 }
 if(length(intersect(row.names(scRNA),CD4Th2s)) == 0){
   CD4Th2 = ""
   CD4Th2_rep = ""
 }else{
   CD4Th2 = c(intersect(row.names(scRNA),CD4Th2s))
   CD4Th2_rep = rep("CD4Th2",length(CD4Th2))
 }
 if(length(intersect(row.names(scRNA),CD4Th9s)) == 0){
   CD4Th9 = ""
   CD4Th9_rep = ""
 }else{
   CD4Th9 = c(intersect(row.names(scRNA),CD4Th9s))
   CD4Th9_rep = rep("CD4Th9",length(CD4Th9))
 }
 if(length(intersect(row.names(scRNA),CD4Th17s)) == 0){
   CD4Th17 = ""
   CD4Th17_rep = ""
 }else{
   CD4Th17 = c(intersect(row.names(scRNA),CD4Th17s))
   CD4Th17_rep = rep("CD4Th17",length(CD4Th17))
 }
 if(length(intersect(row.names(scRNA),CD4Tr1s)) == 0){
   CD4Tr1 = ""
   CD4Tr1_rep = ""
 }else{
   CD4Tr1 = c(intersect(row.names(scRNA),CD4Tr1s))
   CD4Tr1_rep = rep("CD4Tr1",length(CD4Tr1))
 }
 if(length(intersect(row.names(scRNA),Tcms)) == 0){
   Tcm = ""
   Tcm_rep = ""
 }else{
   Tcm = c(intersect(row.names(scRNA),Tcms))
   Tcm_rep = rep("Tcm",length(Tcm))
 }
 if(length(intersect(row.names(scRNA),Tems)) == 0){
   Tem = ""
   Tem_rep = ""
 }else{
   Tem = c(intersect(row.names(scRNA),Tems))
   Tem_rep = rep("Tem",length(Tem))
 }

  selected_markers = c(Neutrophil,
                       Basophil,
                       Eosinophil,
                       Mast,
                       DC,
                       cDC1,
                       cDC2,
                       pDC,
                       Plasma,
                       B,
                       T,
                       MAIT,
                       gdT,
                       CD4,
                       CD4Tfh,
                       CD4Th1,
                       CD4Th2,
                       CD4Th9,
                       CD4Th17,
                       CD4Tr1,
                       CD8,
                       Tex,
                       Treg,
                       Tn,
                       Teff,
                       Tst,
                       Tcm,
                       Tem,
                       NK,
                       Monocyte,
                       Macrophage,
                       M1,
                       M2,
                       Cardiomyocyte,
                       Fibroblast,
                       Endothelial,
                       Mesothelial,
                       SMC,
                       Epithelial,
                       Prolif,
                       Mal_PNET,
                       Mal
  )

  selected_labels = c(Neutrophil_rep,
                   Basophil_rep,
                   Eosinophil_rep,
                   Mast_rep,
                   DC_rep,
                   cDC1_rep,
                   cDC2_rep,
                   pDC_rep,
                   Plasma_rep,
                   B_rep,
                   T_rep,
                   MAIT_rep,
                   gdT_rep,
                   CD4_rep,
                   CD4Tfh_rep,
                   CD4Th1_rep,
                   CD4Th2_rep,
                   CD4Th9_rep,
                   CD4Th17_rep,
                   CD4Tr1_rep,
                   CD8_rep,
                   Tex_rep,
                   Treg_rep,
                   Tn_rep,
                   Teff_rep,
                   Tst_rep,
                   Tcm_rep,
                   Tem_rep,
                   NK_rep,
                   Monocyte_rep,
                   Macrophage_rep,
                   M1_rep,
                   M2_rep,
                   Cardiomyocyte_rep,
                   Fibroblast_rep,
                   Endothelial_rep,
                   Mesothelial_rep,
                   SMC_rep,
                   Epithelial_rep,
                   Prolif_rep,
                   Mal_PNET_rep,
                   Mal_rep
  )

  selected_markers = selected_markers[selected_markers != ""]
  selected_labels = selected_labels[selected_labels != ""]

  data.usage = DotPlot(scRNA,features = selected_markers, group.by = label)$data
  data.anno = data.frame(features.plot = unique(data.usage$features.plot),label = selected_labels)
  df.plot = plyr::join(data.usage,data.anno)

  p = ggplot(df.plot,
             aes(x = features.plot,
                 y = as.numeric(id),
                 size = pct.exp,
                 color = avg.exp.scaled)
             ) +
    geom_point() +
    scale_size("% detected", range = c(0,10)) +
    scale_colour_gradient2("Average\nexpression",
                           low = c("#2F5B89","#6B9AB7","lightblue"),
                           mid = "white",
                           high = c("#F39B7FFF","#ED5E57","#A80C3A"),
                           midpoint = 0) +
    #scale_color_gradientn(colours = c("black",viridis::viridis(20), "#FFB600FF", "#FF9200FF", "#FF6D00FF","#FF4900FF","#FF2400FF" ,"#FF0000FF"),
    #                      guide = guide_colorbar(ticks.colour = "black",frame.colour = "black"),
    #                      name = "Average\nexpression") +
    cowplot::theme_cowplot() +
    ylab("") +
    xlab("Markers") +
    theme_bw() +
    scale_y_continuous(breaks = 1:length(levels(df.plot$id)),
                       labels = levels(df.plot$id),sec.axis = dup_axis())+
    facet_grid(~label, scales="free_x",space = "free")+
    theme_classic() +
    theme(axis.text.x = element_text(size=12, angle = 90, hjust= 1, vjust= 0.5,color="black",face="bold"),
          axis.text.y = element_text(size=12, color="skyblue",face="bold"),
          axis.title.x = element_text(size=14,colour = 'black',#vjust = -0.8,
                                      hjust = 0.5),
          axis.ticks.y = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = 'grey30',size = 0.2), panel.spacing=unit(0, "mm"),
          strip.text.x = element_text(size=15,
                                      face="bold",
                                      color = "#FFFFFF",
                                      vjust = 0.5,
                                      margin = margin(b = 3,t=3)),
          strip.background = element_rect(colour="grey30", fill="grey60",size = 1)
          )

  g = ggplot_gtable(ggplot_build(p))
  strips = which(grepl('strip-', g$layout$name))
  cols = mycolor[1:length(unique(df.plot$label))] %>% as.vector()
  for (i in seq_along(strips)) {
    k = which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l = which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill = cols[i]
    }
  plot(g)
  ggsave(g, file = paste0(name_use,"_Markers.pdf"),width = width, height = height)
  return(g)
}



#' @title bplot_scmarkers_mouse
#' @description bplot_scmarkers_mouse
#' @param scRNA Seurat object
#' @param label celltype or seurat_clusters or others
#' @param width pdf width
#' @param height pdfheight
#' @param mycolor mycolor
#' @return pdf and ggplot object
#' @export
bplot_scmarkers_mouse = function(scRNA,
                           label = "seurat_clusters",
                           width = 20,
                           height = 8,
                           Neutrophils = c("Ly6g","S100a8","S100a9","Elane"),
                           Basophils = c("Prss34","Mcpt8"),
                           Eosinophils = c("Prg2","Epx"),
                           Masts = c('Cpa3','Mcpt4','Fcer1a','Kit'),
                           DCs = c('Cd209a','Zbtb46','Flt3','Itgax','Itgae'),
                           cDC1s = c('Batf3','Clec9a','Irf8','Xcr1'),
                           cDC2s = c('Sirpa','Itgam'),
                           pDCs = c('Siglech','Bst2'),
                           Plasmas = c('Sdc1',"Jchain",'Tnfrsf17','Xbp1'),
                           Bs = c('Cd79a','Cd79b','Cd19','Ms4a1'),
                           Ts = c('Cd3d','Cd3e','Cd3g'),
                           CD4s = c('Cd4'),
                           CD8s = c('Cd8a','Cd8b1'),
                           Texs = c('Lag3','Tigit','Havcr2','Pdcd1'),
                           Tregs = c('Foxp3','Ctla4'),
                           Tns = c('Tcf7','Sell','Lef1','Ccr7'),
                           Teffs = c('Ifng','Tnf','Prf1','Gzma','Gzmb','Il2ra','Il2rb'),
                           NKs = c('Ncr1','Ncam1','Klrb1c','Klrd1','Klra8','Klrk1'),
                           Monocytes = c('Ly6c2','Ly6c1','Lyz2','Csf1r','Cd14','Fcgr3'),
                           Macrophages = c('Adgre1',"Cd68",'C1qa','C1qb','C1qc'),
                           M1s = c('Nos2'),
                           M2s = c('Mrc1','Arg1'),
                           Cardiomyocytes = c('Tnnt2','Myh6','Tnnc1','Tnni3'),
                           Fibroblasts = c('DCN','COL1A1','COL1A2','THY1'),
                           Endothelials = c('TEK','PECAM1','FLT1','VWF'),
                           Mesothelials = c('MSLN','UPK3B','CALB2','WT1'),
                           SMCs = c('ACTA2','TAGLN'),
                           Epithelials = c('EPCAM','KRT19','CDH1','KRT18'),
                           Prolifs = c('MKI67'),
                           name_use = "name_use",
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
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(tidyverse))
  if(length(intersect(row.names(scRNA),Neutrophils)) == 0){
    Neutrophil = ""
    Neutrophil_rep = ""
  }else{
    Neutrophil = c(intersect(row.names(scRNA),Neutrophils))
    Neutrophil_rep = rep("Neutro",length(Neutrophil))
  }

  if(length(intersect(row.names(scRNA),Basophils)) == 0){
    Basophil = ""
    Basophil_rep = ""
  }else{
    Basophil = c(intersect(row.names(scRNA),Basophils))
    Basophil_rep = rep("Baso",length(Basophil))
  }

  if(length(intersect(row.names(scRNA),Eosinophils)) == 0){
    Eosinophil = ""
    Eosinophil_rep = ""
  }else{
    Eosinophil = c(intersect(row.names(scRNA),Eosinophils))
    Eosinophil_rep = rep("Eosino",length(Eosinophil))
  }

  if(length(intersect(row.names(scRNA),Masts)) == 0){
    Mast = ""
    Mast_rep = ""
  }else{
    Mast = c(intersect(row.names(scRNA),Masts))
    Mast_rep = rep("Mast",length(Mast))
  }

  if(length(intersect(row.names(scRNA),DCs)) == 0){
    DC = ""
    DC_rep = ""
  }else{
    DC = c(intersect(row.names(scRNA),DCs))
    DC_rep = rep("DC",length(DC))
  }

  if(length(intersect(row.names(scRNA),cDC1s)) == 0){
    cDC1 = ""
    cDC1_rep = ""
  }else{
    cDC1 = c(intersect(row.names(scRNA),cDC1s))
    cDC1_rep = rep("cDC1",length(cDC1))
  }

  if(length(intersect(row.names(scRNA),cDC2s)) == 0){
    cDC2 = ""
    cDC2_rep = ""
  }else{
    cDC2 = c(intersect(row.names(scRNA),cDC2s))
    cDC2_rep = rep("cDC2",length(cDC2))
  }

  if(length(intersect(row.names(scRNA),pDCs)) == 0){
    pDC = ""
    pDC_rep = ""
  }else{
    pDC = c(intersect(row.names(scRNA),pDCs))
    pDC_rep = rep("pDC",length(pDC))
  }

  if(length(intersect(row.names(scRNA),Plasmas)) == 0){
    Plasma = ""
    Plasma_rep = ""
  }else{
    Plasma = c(intersect(row.names(scRNA),Plasmas))
    Plasma_rep = rep("Plasma",length(Plasma))
  }

  if(length(intersect(row.names(scRNA),Bs)) == 0){
    B = ""
    B_rep = ""
  }else{
    B = c(intersect(row.names(scRNA),Bs))
    B_rep = rep("B",length(B))
  }

  if(length(intersect(row.names(scRNA),Ts)) == 0){
    T = ""
    T_rep = ""
  }else{
    T = c(intersect(row.names(scRNA),Ts))
    T_rep = rep("T",length(T))
  }

  if(length(intersect(row.names(scRNA),CD4s)) == 0){
    CD4 = ""
    CD4_rep = ""
  }else{
    CD4 = c(intersect(row.names(scRNA),CD4s))
    CD4_rep = rep("CD4",length(CD4))
  }

  if(length(intersect(row.names(scRNA),CD8s)) == 0){
    CD8 = ""
    CD8_rep = ""
  }else{
    CD8 = c(intersect(row.names(scRNA),CD8s))
    CD8_rep = rep("CD8",length(CD8))
  }

  if(length(intersect(row.names(scRNA),Texs)) == 0){
    Tex = ""
    Tex_rep = ""
  }else{
    Tex = c(intersect(row.names(scRNA),Texs))
    Tex_rep = rep("Tex",length(Tex))
  }

  if(length(intersect(row.names(scRNA),Tregs)) == 0){
    Treg = ""
    Treg_rep = ""
  }else{
    Treg = c(intersect(row.names(scRNA),Tregs))
    Treg_rep = rep("Treg",length(Treg))
  }

  if(length(intersect(row.names(scRNA),Tns)) == 0){
    Tn = ""
    Tn_rep = ""
  }else{
    Tn = c(intersect(row.names(scRNA),Tns))
    Tn_rep = rep("Tn",length(Tn))
  }

  if(length(intersect(row.names(scRNA),Teffs)) == 0){
    Teff = ""
    Teff_rep = ""
  }else{
    Teff = c(intersect(row.names(scRNA),Teffs))
    Teff_rep = rep("Teff",length(Teff))
  }

  if(length(intersect(row.names(scRNA),NKs)) == 0){
    NK = ""
    NK_rep = ""
  }else{
    NK = c(intersect(row.names(scRNA),NKs))
    NK_rep = rep("NK",length(NK))
  }

  if(length(intersect(row.names(scRNA),Monocytes)) == 0){
    Monocyte = ""
    Monocyte_rep = ""
  }else{
    Monocyte = c(intersect(row.names(scRNA),Monocytes))
    Monocyte_rep = rep("Mono",length(Monocyte))
  }

  if(length(intersect(row.names(scRNA),Macrophages)) == 0){
    Macrophage = ""
    Macrophage_rep = ""
  }else{
    Macrophage = c(intersect(row.names(scRNA),Macrophages))
    Macrophage_rep = rep("Macro",length(Macrophage))
  }

  if(length(intersect(row.names(scRNA),M1s)) == 0){
    M1 = ""
    M1_rep = ""
  }else{
    M1 = c(intersect(row.names(scRNA),M1s))
    M1_rep = rep("M1",length(M1))
  }

  if(length(intersect(row.names(scRNA),M2s)) == 0){
    M2 = ""
    M2_rep = ""
  }else{
    M2 = c(intersect(row.names(scRNA),M2s))
    M2_rep = rep("M2",length(M2))
  }

  if(length(intersect(row.names(scRNA),Cardiomyocytes)) == 0){
    Cardiomyocyte = ""
    Cardiomyocyte_rep = ""
  }else{
    Cardiomyocyte = c(intersect(row.names(scRNA),Cardiomyocytes))
    Cardiomyocyte_rep = rep("Cardio",length(Cardiomyocyte))
  }

  if(length(intersect(row.names(scRNA),Fibroblasts)) == 0){
    Fibroblast = ""
    Fibroblast_rep = ""
  }else{
    Fibroblast = c(intersect(row.names(scRNA),Fibroblasts))
    Fibroblast_rep = rep("Fibro",length(Fibroblast))
  }

  if(length(intersect(row.names(scRNA),Endothelials)) == 0){
    Endothelial = ""
    Endothelial_rep = ""
  }else{
    Endothelial = c(intersect(row.names(scRNA),Endothelials))
    Endothelial_rep = rep("Endo",length(Endothelial))
  }

  if(length(intersect(row.names(scRNA),Mesothelials)) == 0){
    Mesothelial = ""
    Mesothelial_rep = ""
  }else{
    Mesothelial = c(intersect(row.names(scRNA),Mesothelials))
    Mesothelial_rep = rep("Meso",length(Mesothelial))
  }

  if(length(intersect(row.names(scRNA),SMCs)) == 0){
    SMC = ""
    SMC_rep = ""
  }else{
    SMC = c(intersect(row.names(scRNA),SMCs))
    SMC_rep = rep("Meso",length(SMC))
  }

  if(length(intersect(row.names(scRNA),Epithelials)) == 0){
    Epithelial = ""
    Epithelial_rep = ""
  }else{
    Epithelial = c(intersect(row.names(scRNA),Epithelials))
    Epithelial_rep = rep("Epi",length(Epithelial))
  }

  if(length(intersect(row.names(scRNA),Prolifs)) == 0){
    Prolif = ""
    Prolif_rep = ""
  }else{
    Prolif = c(intersect(row.names(scRNA),Prolifs))
    Prolif_rep = rep("Prolif",length(Prolif))
  }

  selected_markers = c(Neutrophil,
                       Basophil,
                       Eosinophil,
                       Mast,
                       DC,
                       cDC1,
                       cDC2,
                       pDC,
                       Plasma,
                       B,
                       T,
                       CD4,
                       CD8,
                       Tex,
                       Treg,
                       Tn,
                       Teff,
                       NK,
                       Monocyte,
                       Macrophage,
                       M1,
                       M2,
                       Cardiomyocyte,
                       Fibroblast,
                       Endothelial,
                       Mesothelial,
                       SMC,
                       Epithelial,
                       Prolif
  )

  selected_labels = c(Neutrophil_rep,
                      Basophil_rep,
                      Eosinophil_rep,
                      Mast_rep,
                      DC_rep,
                      cDC1_rep,
                      cDC2_rep,
                      pDC_rep,
                      Plasma_rep,
                      B_rep,
                      T_rep,
                      CD4_rep,
                      CD8_rep,
                      Tex_rep,
                      Treg_rep,
                      Tn_rep,
                      Teff_rep,
                      NK_rep,
                      Monocyte_rep,
                      Macrophage_rep,
                      M1_rep,
                      M2_rep,
                      Cardiomyocyte_rep,
                      Fibroblast_rep,
                      Endothelial_rep,
                      Mesothelial_rep,
                      SMC_rep,
                      Epithelial_rep,
                      Prolif_rep
  )

  selected_markers = selected_markers[selected_markers != ""]
  selected_labels = selected_labels[selected_labels != ""]

  data.usage = DotPlot(scRNA,features = selected_markers, group.by = label)$data
  data.anno = data.frame(features.plot = unique(data.usage$features.plot),label = selected_labels)
  df.plot = plyr::join(data.usage,data.anno)

  p = ggplot(df.plot,
             aes(x = features.plot,
                 y = as.numeric(id),
                 size = pct.exp,
                 color = avg.exp.scaled)
  ) +
    geom_point() +
    scale_size("% detected", range = c(0,10)) +
    scale_colour_gradient2("Average\nexpression",
                           low = c("#2F5B89","#6B9AB7","lightblue"),
                           mid = "white",
                           high = c("#F39B7FFF","#ED5E57","#A80C3A"),
                           midpoint = 0) +
    #scale_color_gradientn(colours = c("black",viridis::viridis(20), "#FFB600FF", "#FF9200FF", "#FF6D00FF","#FF4900FF","#FF2400FF" ,"#FF0000FF"),
    #                      guide = guide_colorbar(ticks.colour = "black",frame.colour = "black"),
    #                      name = "Average\nexpression") +
    cowplot::theme_cowplot() +
    ylab("") +
    xlab("Markers") +
    theme_bw() +
    scale_y_continuous(breaks = 1:length(levels(df.plot$id)),
                       labels = levels(df.plot$id),sec.axis = dup_axis())+
    facet_grid(~label, scales="free_x",space = "free")+
    theme_classic() +
    theme(axis.text.x = element_text(size=12, angle = 90, hjust= 1, vjust= 0.5,color="black",face="bold"),
          axis.text.y = element_text(size=12, color="skyblue",face="bold"),
          axis.title.x = element_text(size=14,colour = 'black',#vjust = -0.8,
                                      hjust = 0.5),
          axis.ticks.y = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = 'grey30',size = 0.2), panel.spacing=unit(0, "mm"),
          strip.text.x = element_text(size=15,
                                      face="bold",
                                      color = "#FFFFFF",
                                      vjust = 0.5,
                                      margin = margin(b = 3,t=3)),
          strip.background = element_rect(colour="grey30", fill="grey60",size = 1)
    )

  g = ggplot_gtable(ggplot_build(p))
  strips = which(grepl('strip-', g$layout$name))
  cols = mycolor[1:length(unique(df.plot$label))] %>% as.vector()
  for (i in seq_along(strips)) {
    k = which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l = which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill = cols[i]
  }
  plot(g)
  ggsave(g, file = paste0(name_use,"_Markers.pdf"), width = width, height = height)
  return(g)
}
