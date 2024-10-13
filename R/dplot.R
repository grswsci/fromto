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
