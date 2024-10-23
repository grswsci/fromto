#' @title uplot_cell
#' @description umap or tsne
#' @param scRNA Seurat object
#' @param plotname plot name
#' @param reduction umap or tsne visualization
#' @param label celltype seurat_clusters
#' @param alphas transparency 0-1
#' @param mycolor colors
#' @return pdf and ggplot
uplot_cell = function(scRNA, plotname = "CellType", reduction = "umap", label = "celltype", alphas = 0.8, mycolor = c("#BC3C29FF","#0072B5FF","#E18727FF",
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
  library(tidyverse)
  umap = scRNA@reductions[[reduction]]@cell.embeddings %>% as.data.frame() %>% cbind(Cluster = scRNA@meta.data[,label])
  colnames(umap) = c("UMAP_1","UMAP_2","Cluster")
  p = ggplot(umap,aes(x= UMAP_1 , y = UMAP_2 ,color = Cluster)) +
    geom_point(size = 0.1 , alpha = alphas)  +
    scale_color_manual(values = mycolor)
  p2 = p + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = 'white'),
                 plot.background=element_rect(fill="white"))
  p3 = p2 + theme(legend.title = element_blank(),
                legend.key=element_rect(fill='white'),
                legend.text = element_text(size=20),
                legend.key.size=unit(1,'cm') ) +
  guides(color = guide_legend(override.aes = list(size=5)))
  p4 = p3 + geom_segment(aes(x = min(umap$UMAP_1) , y = min(umap$UMAP_2),
                         xend = min(umap$UMAP_1) +3,
                         yend = min(umap$UMAP_2) ),
                         colour = "black", size=1,
                         arrow = arrow(length = unit(0.3,"cm")))+
  geom_segment(aes(x = min(umap$UMAP_1)  ,
                   y = min(umap$UMAP_2),
                   xend = min(umap$UMAP_1) ,
                   yend = min(umap$UMAP_2) + 3),
               colour = "black", size=1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  annotate("text",x = min(umap$UMAP_1) + 1.5,
           y = min(umap$UMAP_2) -1,
           label = "UMAP_1",
           color = "black",
           size = 3,
           fontface = "bold" ) +
  annotate("text",x = min(umap$UMAP_1) - 1,
           y = min(umap$UMAP_2) + 1.5,
           label = "UMAP_2",
           color = "black",size = 3,
           fontface = "bold" ,angle = 90)
  cell_type_med = umap %>% group_by(Cluster) %>%
    summarise(UMAP_1 = median(UMAP_1),UMAP_2 = median(UMAP_2))
  p6 = p4 + geom_label_repel(aes(label = Cluster),
                          fontface="bold",
                          data = cell_type_med,
                          point.padding=unit(0.5, "lines")) +
  theme(legend.position = "none")
  ggsave(paste0(plotname,".pdf"), plot = p6, width = 6, height = 6)
  return(p6)
}
