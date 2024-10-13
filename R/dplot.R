dplot1 <- function(data,
                   Type = "Type",
                   variable,
                   levels = NULL,
                   test.methods = "wilcox.test",
                   DatasetName = "DatasetName",
                   width = 4.5,
                   height = 4,
                   alphas = 0.5,
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
                               "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")
                   ){
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))
  options(warn = -1)
  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  if(test.methods == "wilcox.test"){
    p = wilcox.test(expression ~ Type, data = data)$p.value
  }else if(test.methods == "kruskal.test"){
    p = kruskal.test(expression ~ Type, data = data)$p.value
  }

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }

  plot = ggplot(data = data,aes(x = Type,y = expression, fill = Type)) +
    scale_color_manual(values = alpha(mycolor,alphas)) +
    scale_fill_manual(values = alpha(mycolor,alphas)) +
    geom_violin(alpha = 0.4, position = position_dodge(width = .75), size=0.8, color="black") +
    geom_boxplot(notch = TRUE, outlier.size = -1, color="black", lwd=0.8, alpha = 0.5) +
    geom_point(shape = 21, size = 2, position = position_jitterdodge(), color="black", alpha=0.3) +
    theme_classic() +
    ylab(variable) +
    xlab("") +
    ylim(c(ifelse(min(data[,"expression"]) > 0, min(data[,"expression"])*0.9, min(data[,"expression"])*1.1),
           max(data[,"expression"])*1.1)
         ) +
    annotate(geom="text",
             cex = 6,
             x = (length(unique(data[,"Type"]))/2) + 0.5,
             y = max(data[,variable])*1.05,
             label=paste0("P ", ifelse(p < 0.001, "< 0.001", paste0("= ",round(p,3)))), color="black") +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
      axis.ticks = element_line(size=0.2, color="black"),
      axis.ticks.length = unit(0.2,"cm"),
      legend.position = "none",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10))
  ggsave(paste0(DatasetName,"_",variable,"_",Type,"_boxplot.pdf"), plot = plot, width = width, height = height)
}

dplot2 <- function(data,
                   Type = "Type",
                   variable,
                   levels = NULL,
                   test.methods = "wilcox.test",
                   DatasetName = "DatasetName",
                   width = 6,
                   height = 4,
                   alphas = 0.5,
                   title = "",
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
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))
  suppressPackageStartupMessages(library(ggpubr,quietly = TRUE))
  suppressPackageStartupMessages(library(ggExtra,quietly = TRUE))
  suppressPackageStartupMessages(library(aplot,quietly = TRUE))
  suppressPackageStartupMessages(library(hrbrthemes,quietly = TRUE))
  suppressPackageStartupMessages(library(ggtext,quietly = TRUE))

  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  if(test.methods == "wilcox.test"){
    p = wilcox.test(expression ~ Type, data = data)$p.value
  }else if(test.methods == "kruskal.test"){
    p = kruskal.test(expression ~ Type, data = data)$p.value
  }

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }

  pdf(paste0(DatasetName,"_",variable,"_",Type,"_cloudrainplot.pdf"), width = width, height = height)
  plot = ggplot(data, aes(x = Type,y = expression)) +
    ggdist::stat_halfeye(aes(color = Type, fill = Type),
                         adjust = .5,
                         width = .7,
                         .width = 0,
                         justification = -.2,
                         point_colour = NA) +
    geom_boxplot(aes(color = Type),width = .2, outlier.shape = NA) +
    geom_jitter(aes(color = Type),width = .05, alpha = .3) +
    scale_color_manual(values = alpha(mycolor,alphas)) +
    scale_fill_manual(values = alpha(mycolor,alphas)) +
    ylab(variable) +
    xlab("") +
    coord_flip() +
    labs(title = title,
         subtitle = paste0(ifelse(test.methods == "wilcox.test","Wilcoxon Rank Sum Test ",
                                  "Kruskal-Wallis Rank Sum Test "),
                           "P Value ",ifelse(p<0.001, "< 0.001",
                                             paste0("= ",round(p,3))
                                             )
                           )
    ) +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.4),
          plot.title = element_markdown(hjust = 0.5,vjust = .5,color = "black",
                                        size = 20, margin = margin(t = 1, b = 12)),
          plot.subtitle = element_markdown(hjust = 0,vjust = .5,size=15),
          plot.caption = element_markdown(face = 'bold',size = 12),
          legend.position = "none")
  print(plot)
  dev.off()
}

dplot3 <- function(data,
                   Type = "Type",
                   variable,
                   levels = NULL,
                   test.methods = "wilcox.test",
                   DatasetName = "DatasetName",
                   width = 6,
                   height = 3,
                   alphas = 0.5,
                   title = "",
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
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))
  suppressPackageStartupMessages(library(ggpubr,quietly = TRUE))
  suppressPackageStartupMessages(library(ggExtra,quietly = TRUE))
  suppressPackageStartupMessages(library(aplot,quietly = TRUE))
  suppressPackageStartupMessages(library(tidyverse,quietly = TRUE))

  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  if(test.methods == "wilcox.test"){
    p.val = wilcox.test(expression ~ Type, data = data)
  }else if(test.methods == "kruskal.test"){
    p.val = kruskal.test(expression ~ Type, data = data)
  }
  p.lab = paste0("P",ifelse(p.val$p.value < 0.001,
                            " < 0.001",paste0(" = ",round(p.val$p.value, 3))
                            )
                 )

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }

  p_top = ggplot(data,
                  aes(x = expression,
                      color = Type,
                      fill = Type)
  ) +
    geom_density() +
    scale_color_manual(values = alpha(mycolor,alphas)) +
    scale_fill_manual(values = alpha(mycolor,alphas)) +
    theme_classic() +
    xlab(paste0("Estimated Expression of ", variable)) +
    ylab(NULL) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 12,color = "black"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    ) +
    geom_rug()

  p_bot = ggplot(data, aes(Type, expression, fill = Type)) +
    geom_boxplot(aes(col = Type)) +
    scale_color_manual(values = alpha(mycolor,alphas)) +
    scale_fill_manual(values = alpha(mycolor,alphas)) +
    xlab(NULL) +
    ylab("Estimated Expression") +
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 11,color = "black"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    ) +
    annotate(geom="text",x = 0.2,
             hjust = 1,
             y = max(data[,"expression"])*1,
             size = 4,
             angle = 270,
             fontface = "bold",
             label = p.lab) +
    coord_flip()
  dat = ggplot_build(p_bot)$data[[1]]
  p_bot = p_bot +
    geom_segment(data = dat,
                 aes(x = xmin,xend = xmax,
                     y = middle,yend = middle),
                 color="white",
                 inherit.aes = F)
  p_bot
  p = p_top %>% insert_bottom(p_bot, height = 0.4)
  pdf(paste0(DatasetName,"_",variable,"_",Type,"_densityplot.pdf"), width = width,height = height)
  print(p)
  invisible(dev.off())
}

dplot4 <- function(data,
                   Type = "Type",
                   variable,
                   levels = NULL,
                   test.methods = "wilcox.test",
                   DatasetName = "DatasetName",
                   width = 4.5,
                   height = 4.5,
                   alphas = 0.5,
                   title = "",
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
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))
  suppressPackageStartupMessages(library(beeswarm,quietly = TRUE))
  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  if(test.methods == "wilcox.test"){
    p = wilcox.test(expression ~ Type, data = data)$p.value
  }else if(test.methods == "kruskal.test"){
    p = kruskal.test(expression ~ Type, data = data)$p.value
  }

  if(p < 0.001){
    pval = signif(p,4)
    pval = format(pval, scientific = TRUE)
  }else{
    pval = sprintf("%.03f",p)
  }

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }
  if(is.null(levels)){
  data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
  data[,"Type"] = factor(data[,"Type"], levels = levels)
  }
  box_plot = boxplot(expression ~ Type, data = data,outline = FALSE, plot=F)
  yMin = min(box_plot$stats)
  yMax = max(box_plot$stats/5 + box_plot$stats)
  n = ncol(box_plot$stats)
  pdf(file = paste0(DatasetName,"_",variable,"_",Type,"_beeswarmplot.pdf"), width = width, height = height)
  par(mar = c(2,4,2,1))
  boxplot(expression ~ Type,
          data = data,
          ylab = variable,
          xlab = "",
          main = paste0(ifelse(test.methods == "wilcox.test","Wilcoxon Rank Sum Test ",
                               "Kruskal-Wallis Rank Sum Test "), "(p = ",pval,")"),
          cex.main = 1,
          cex.lab = 1,
          cex.axis = 1,
          ylim = c(yMin,yMax),
          outline = FALSE)
  beeswarm(expression ~ Type,
         data = data,
         col = alpha(mycolor,alphas),
         lwd = 0.1,
         pch = 16,
         add = TRUE,
         corral="wrap")
  dev.off()
}
