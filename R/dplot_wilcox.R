dplot_wilcox_1 <- function(data,
                           Type = "Type",
                           variable,
                           levels = NULL,
                           mycolor = c("#0072B5FF","#BC3C29FF"),
                           DatasetName = "DatasetName"){
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))

  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  p = wilcox.test(expression ~ Type, data = data)$p.value

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }

  plot = ggplot(data = data,aes(x = Type,y = expression, fill = Type)) +
    scale_color_manual(values = alpha(mycolor,0.5)) +
    scale_fill_manual(values = alpha(mycolor,0.5)) +
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
             x = 1.5,
             y = max(data[,variable])*1.05,
             label=paste0("P ", ifelse(p < 0.001, "< 0.001", paste0("= ",round(p,3)))), color="black") +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
      axis.ticks = element_line(size=0.2, color="black"),
      axis.ticks.length = unit(0.2,"cm"),
      legend.position = "none",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10))
  ggsave(paste0(DatasetName,"_",variable,"_",Type,"_wilcox.pdf"), plot = plot, width = 4.5, height = 4)
}
