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
  library(ggrepel)
  umap = scRNA@reductions[[reduction]]@cell.embeddings %>% as.data.frame() %>% cbind(Cluster = scRNA@meta.data[,label])
  colnames(umap) = c("UMAP_1","UMAP_2","Cluster")

  cell_type_med = umap %>% group_by(Cluster) %>% summarise(UMAP_1 = median(UMAP_1),UMAP_2 = median(UMAP_2))

  UMAP_1_min = min(umap$UMAP_1)
  UMAP_2_min = min(umap$UMAP_2)

  p = ggplot(umap,aes(x= UMAP_1 , y = UMAP_2 ,color = Cluster)) +
    geom_point(size = 0.1 , alpha = alphas)  +
    scale_color_manual(values = mycolor) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill="white")
          ) +
    theme(legend.title = element_blank(),
          legend.key = element_rect(fill='white'),
          legend.text = element_text(size=20),
          legend.key.size = unit(1,'cm')
          ) +
  guides(color = guide_legend(override.aes = list(size = 5))
         ) +
    annotate("segment", x = UMAP_1_min ,
                     y = UMAP_2_min,
                     xend = UMAP_1_min + 3,
                     yend = UMAP_2_min,
                     colour = "black",
                     size = 1,
                     arrow = arrow(length = unit(0.3,"cm"))
                 ) +
    annotate("segment", x = UMAP_1_min,
                   y = UMAP_2_min,
                   xend = UMAP_1_min ,
                   yend = UMAP_2_min + 3,
                   colour = "black",
                   size = 1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  annotate("text",
           x = UMAP_1_min + 1.5,
           y = UMAP_2_min - 1,
           label = "UMAP_1",
           color = "black",
           size = 3,
           fontface = "bold" ) +
  annotate("text",
           x = UMAP_1_min - 1,
           y = UMAP_2_min + 1.5,
           label = "UMAP_2",
           color = "black",
           size = 3,
           fontface = "bold" ,angle = 90) +
    geom_label_repel(aes(label = Cluster),
                          fontface = "bold",
                          data = cell_type_med,
                          point.padding = unit(0.5, "lines")
                     ) +
  theme(legend.position = "none")
  ggsave(paste0(plotname,".pdf"), plot = p, width = 6, height = 6)
  return(p)
}

#' @title uplot_gene1
#' @description umap or tsne
#' @param scRNA Seurat object
#' @param marker one gene or many genes
#' @param dim umap or tsne visualization
#' @param size dot size
#' @param ncol Multiple genes are arranged in columns and how many columns u want
#' @return ggplot object
uplot_gene1  = function(scRNA,
                        marker,
                        dim = "umap",
                        size = 0.8,
                        slot_use = "counts",
                        nrow = 1,
                        height = 6,
                        width = 13){
  require(ggplot2)
  require(ggrastr)
  require(Seurat)
  require(patchwork)
  cold = colorRampPalette(c('#f7fcf0','#41b6c4','#253494'))
  warm = colorRampPalette(c('#ffffb2','#fecc5c','#e31a1c'))
  mypalette = c(rev(cold(11)), warm(10))
  mypalette = c('lightgrey','#330066','#336699','#66CC66','#FFCC33',"red")

  if(dim == "tsne"){
    xtitle = "tSNE1"
    ytitle = "tSNE2"
  }

  if(dim == "umap"){
    xtitle = "UMAP1"
    ytitle = "UMAP2"
  }

  if(length(marker) == 1){
    plot = FeaturePlot(scRNA, features = marker,slot = slot_use,raster = FALSE)
    data = plot$data

    if(dim=="umap"){
      colnames(data) = c("x","y","ident","gene")
    }

    if(dim == "tsne"){
      colnames(data) = c("x","y","ident","gene")
    }

    p = ggplot(data, aes(x, y)) +
      geom_point_rast(shape = 21, stroke = 0.25,
                      aes(colour = gene,
                          fill = gene), size = size) +
      scale_fill_gradientn(colours = mypalette)+
      scale_colour_gradientn(colours = mypalette)+
      theme_bw() +
      ggtitle(marker) +
      labs(x= xtitle, y= ytitle)+
      theme(
        plot.title = element_text(size=12, face="bold.italic", hjust = 0),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_text(size=12),
        legend.text = element_text(size =10),
        legend.title = element_blank(),
        aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    pdf(file=paste0("sc_",marker,"_uplot.pdf"),height = 6, width = 6.5)
    print(p)
    dev.off()
    return(p)
  }else{
    gene_list = list()
    for (i in 1:length(marker)) {
      plot = FeaturePlot(scRNA, features = marker[i],slot = slot_use,raster = FALSE)
      data = plot$data

      if(dim == "tsne"){
        colnames(data) = c("x","y","ident","gene")
      }

      if(dim == "umap"){
        colnames(data) = c("x","y","ident","gene")
      }

      gene_list[[i]] = data
      names(gene_list) = marker[i]
    }

    plot_list = list()

    for (i in 1:length(marker)) {

      p = ggplot(gene_list[[i]], aes(x, y)) +
        geom_point_rast(shape = 21, stroke=0.25,
                        aes(colour = gene,
                            fill = gene), size = size) +
        scale_fill_gradientn(colours = mypalette)+
        scale_colour_gradientn(colours = mypalette)+
        theme_bw() +
        ggtitle(marker[i]) +
        labs(x = xtitle, y = ytitle) +
        theme(
          plot.title = element_text(size=12, face="bold.italic", hjust = 0),
          axis.text = element_text(size=8, colour = "black"),
          axis.title = element_text(size=12),
          legend.text = element_text(size =10),
          legend.title = element_blank(),
          aspect.ratio = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      plot_list[[i]] <- p
    }
    p_all = wrap_plots(plots = plot_list, nrow = nrow)
    pdf(file=paste0("sc_merge_uplot.pdf"),height = height, width = width)
    print(p_all)
    dev.off()
    return(p_all)
  }
}

#' @title uplot_gene2
#' @description umap or tsne
#' @param scRNA Seurat object
#' @param marker one gene or many genes
#' @param dim umap or tsne visualization
#' @param size dot size
#' @param ncol Multiple genes are arranged in columns and how many columns u want
#' @return ggplot object
uplot_gene2  = function(scRNA,
                        marker,
                        dim = "umap",
                        slot_use = "counts",
                        size = 0.8,
                        nrow = 1,
                        height = 6,
                        width = 13){
  require(ggplot2)
  require(ggrastr)
  require(Seurat)
  require(patchwork)

  cold = colorRampPalette(c('#f7fcf0','#41b6c4','#253494'))
  warm = colorRampPalette(c('#ffffb2','#fecc5c','#e31a1c'))
  mypalette = c(rev(cold(11)), warm(10))
  mypalette = c('lightgrey','#330066','#336699','#66CC66','#FFCC33',"red")

  if(dim == "tsne"){
    xtitle = "tSNE1"
    ytitle = "tSNE2"
  }

  if(dim == "umap"){
    xtitle = "UMAP1"
    ytitle = "UMAP2"
  }

  if(length(marker) == 1){
    plot = FeaturePlot(scRNA, features = marker,slot = slot_use,raster = FALSE)
    data = plot$data

    if(dim=="tsne"){
      colnames(data) = c("x","y","ident","gene")
    }

    if(dim=="umap"){
      colnames(data) = c("x","y","ident","gene")
    }

    p = ggplot(data, aes(x, y)) +
      geom_point_rast(shape = 21, stroke = 0.25,
                      aes(colour = gene,
                          fill = gene), size = size) +
      geom_density_2d(data = data[data$gene > 0,],
                      aes(x = x, y = y),
                      bins = 5, colour="black") +
      scale_fill_gradientn(colours = mypalette)+
      scale_colour_gradientn(colours = mypalette)+
      theme_bw() +
      ggtitle(marker) +
      labs(x= xtitle, y= ytitle)+
      theme(
        plot.title = element_text(size=12, face="bold.italic", hjust = 0),
        axis.text = element_text(size=8, colour = "black"),
        axis.title = element_text(size=12),
        legend.text = element_text(size =10),
        legend.title = element_blank(),
        aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    pdf(file=paste0("sc_",marker,"_uplot.pdf"),height = 6, width = 6.5)
    print(p)
    dev.off()
    return(p)
  }else{
    gene_list = list()
    for (i in 1:length(marker)) {
      plot = FeaturePlot(scRNA, features = marker[i],slot = slot_use,raster = FALSE)
      data = plot$data

      if(dim == "tsne"){
        colnames(data) = c("x","y","ident","gene")
      }

      if(dim == "umap"){
        colnames(data) = c("x","y","ident","gene")
      }

      gene_list[[i]] = data
      names(gene_list) = marker[i]
    }

    plot_list = list()

    for (i in 1:length(marker)) {

      p = ggplot(gene_list[[i]], aes(x, y)) +
        geom_point_rast(shape = 21, stroke=0.25,
                        aes(colour = gene,
                            fill = gene), size = size) +
        geom_density_2d(data = data[data$gene > 0,],
                        aes(x = x, y = y),
                        bins = 5, colour="black") +
        scale_fill_gradientn(colours = mypalette)+
        scale_colour_gradientn(colours = mypalette)+
        theme_bw() +
        ggtitle(marker[i]) +
        labs(x = xtitle, y = ytitle) +
        theme(
          plot.title = element_text(size = 12, face="bold.italic", hjust = 0),
          axis.text = element_text(size = 8, colour = "black"),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.title = element_blank(),
          aspect.ratio = 1,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      plot_list[[i]] <- p
    }
    p_all = wrap_plots(plots = plot_list, nrow = nrow)
    pdf(file = paste0("sc_merge_uplot.pdf"),height = height, width = width)
    print(p_all)
    dev.off()
    return(p_all)
  }
}

#' @title uplot_gene3
#' @description umap or tsne
#' @param scRNA Seurat object
#' @param marker one gene
#' @param slot counts or data
#' @param reduction_use umap or tsne
#' @param ncol Multiple genes are arranged in columns and how many columns u want
#' @return ggplot object
#' devtools::install_github("powellgenomicslab/Nebulosa")
uplot_gene3  = function(scRNA,
                        marker,
                        slot_use = "counts",
                        reduction_use = "umap",
                        size = 0.8,
                        nameplot = "name"){
  library(Seurat)
  library(ggplot2)
  library(Nebulosa)
  library(ggnetwork)
  color = c(alpha('lightgrey',0.1),'#330066','#336699','#66CC66','#FFCC33',"red")
  plot = plot_density(scRNA,
                      features = marker,
                      slot = slot_use,
                      reduction = reduction_use,
                      pal = 'magma',
                      raster = TRUE,
                      size = size) +
      theme_blank() +
      theme(panel.background = element_rect(fill = "black"))&
      theme(legend.frame = element_rect(colour = "black"),
            legend.ticks = element_line(colour = "black", linewidth  = 0),
            legend.key.width = unit(0.3, "cm"),
            legend.key.height= unit(0.8,"cm"),
            legend.title = element_text(color = 'black', face = "bold", size = 8)
      )
    pdf(file = paste0(nameplot,"_",marker,"_Density_black_background.pdf"),width = 6.5,height = 6)
    print(plot)
    dev.off()

    plot2 = plot_density(scRNA,
                         features = marker,
                         slot = slot_use,
                         reduction = reduction_use,
                         pal = 'magma',
                         raster = TRUE,
                         size = size) +
      theme_blank() +
      theme(legend.frame = element_rect(colour = "black"),
            legend.ticks = element_line(colour = "black", linewidth  = 0),
            legend.key.width = unit(0.3, "cm"),
            legend.key.height= unit(0.8,"cm"),
            legend.title = element_text(color = 'black', face = "bold", size=8)
      )
    pdf(file = paste0(nameplot,"_",marker,"_Density.pdf"),width = 6.5,height = 6)
    print(plot2)
    dev.off()
  }

