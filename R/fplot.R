#' @title fplot1
#' @description forest plot
#' @param data data frame
#' @param nameplot plot name
#' @param height pdf height
#' @return forest plot
fplot1 = function(data,nameplot,height = 10){
  suppressPackageStartupMessages(library(grid))
  suppressPackageStartupMessages(library(readr))
  suppressPackageStartupMessages(library(forestploter))
  suppressPackageStartupMessages(library(ggplot2))

  data$HR = round(unlist(as.numeric(data$HR)),3)
  data$HR.95L = round(unlist(as.numeric(data$HR.95L)),3)
  data$HR.95H = round(unlist(as.numeric(data$HR.95H)),3)
  data$pvalue = round(unlist(as.numeric(data$pvalue)),3)
  data$pvalue = ifelse(data$pvalue < 0.001,"<0.001", data$pvalue)
  lineVec = nrow(data) + 1
  data$' ' = paste(rep(" ", 10), collapse = " ")
  if(nrow(data) <= 10){
  list_color = list(fill = c(alpha(c("#e41a1c",
                            "#377eb8",
                            "#4daf4a",
                            "#984ea3",
                            "#ff7f00",
                            "#ffff33",
                            "#7E6148",
                            "#a65628",
                            "#f781bf",
                            "#999999")[1:nrow(data)],0.05))))
  }else{
    list_color = list(fill = c(rep(alpha(c("white",0.05)
                                   ,100)
    )
    )
  }
  tm = forest_theme(base_size = 18,
                    ci_pch = 16,
                    ci_lty = 1,
                    ci_lwd = 1.5,
                    ci_col = "black",
                    ci_Theight = 0.2,
                    refline_lty="dashed",
                    refline_lwd = 1,
                    refline_col="grey20",
                    xaxis_cex = 0.8,
                    core = list(bg_params = list_color),
                    footnote_cex = 0.6,
                    footnote_col = "blue")
trace(forestploter::forest_theme,edit = TRUE)
  if(max(data$HR.95H) < 1){
    xlim = c(min(data$HR.95L),1.2)
  }else if(max(data$HR.95H) < 2){
    xlim = c(min(data$HR.95L),2)
  }else if(max(data$HR.95H) < 3){
    xlim = c(min(data$HR.95L),3)
  }else if(max(data$HR.95H) < 4){
    xlim = c(min(data$HR.95L),4)
  }else{
    xlim = c(min(data$HR.95L),5)
  }
  plot = forestploter::forest(data[,c(1:(ncol(data)-2),ncol(data),(ncol(data)-1))],
                              est = as.numeric(data$HR),
                              lower = as.numeric(data$HR.95L),
                              upper = as.numeric(data$HR.95H),
                              ci_column = 5,
                              ref_line = 1,
                              xlim = xlim,
                              theme = tm
  )

  boxcolor = c("#E64B35","#4DBBD5")
  boxcolor = ifelse(data$HR > 1,boxcolor[1],boxcolor[2])
  for(i in 1:nrow(data)){
    plot = edit_plot(plot, col=5,row = i, which = "ci", gp = gpar(fill = boxcolor[i],fontsize=25))
  }

  pos_bold_pval = which(as.numeric(gsub('<',"",data$pvalue)) < 0.05)
  if(length(pos_bold_pval) > 0){
    for(i in pos_bold_pval){
      plot <- edit_plot(plot, col=c(1,2,3,4,6),row = i, which = "text", gp = gpar(fontface="bold"))
    }
  }
  plot = add_border(plot, part = "header", row =1,where = "top",gp = gpar(lwd =2))
  plot = add_border(plot, part = "header", row = c(lineVec), gp = gpar(lwd =1))

  plot = edit_plot(plot, col= 1:ncol(data),row = 1:nrow(data), which = "text", gp = gpar(fontsize=12))
  plot = edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),part="header",
                    x = unit(0.5, "npc"))
  plot = edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),
                    x = unit(0.5, "npc"))
  pdf(paste0(nameplot,".pdf"), width=12, height = height)
  print(plot)
  dev.off()
}
#' @title fplot2
#' @description forest plot
#' @param data data frame
#' @param nameplot plot name
#' @param height pdf height
#' @return forest plot
fplot2 = function(data,nameplot,height = 10){
  suppressPackageStartupMessages(library(grid))
  suppressPackageStartupMessages(library(readr))
  suppressPackageStartupMessages(library(forestploter))
  suppressPackageStartupMessages(library(ggplot2))

  data$HR = round(unlist(as.numeric(data$HR)),3)
  data$HR.95L = round(unlist(as.numeric(data$HR.95L)),3)
  data$HR.95H = round(unlist(as.numeric(data$HR.95H)),3)
  data$pvalue = round(unlist(as.numeric(data$pvalue)),3)
  data$pvalue = ifelse(data$pvalue<0.001,"<0.001",data$pvalue)
  lineVec = nrow(data)+1
  data$' ' = paste(rep(" ", 10), collapse = " ")
  list_color = list(fill = c(alpha(c("#e41a1c",
                                     "#377eb8",
                                     "#4daf4a",
                                     "#984ea3",
                                     "#ff7f00",
                                     "#ffff33",
                                     "#7E6148",
                                     "#a65628",
                                     "#f781bf",
                                     "#999999")[1:nrow(data)],0.05))))
    }else{
      list_color = list(fill = c(rep(alpha(c("white",0.05)
                                       ,100)
                                     )
      )
                        }
  tm = forest_theme(base_size = 18,
                     ci_pch = 16,
                     ci_lty = 1,
                     ci_lwd = 1.5,
                     ci_col = "black",
                     ci_Theight = 0.2,
                     refline_lty = "dashed",
                     refline_lwd = 1,
                     refline_col = "grey20",
                     xaxis_cex = 0.8,
                     core = list(bg_params = list_color),
                     footnote_cex = 0.6,
                     footnote_col = "blue")
  if(max(data$HR.95H) < 1){
    xlim = c(min(data$HR.95L),1.2)
  }else if(max(data$HR.95H) < 2){
    xlim = c(min(data$HR.95L),2)
  }else if(max(data$HR.95H) < 3){
    xlim = c(min(data$HR.95L),3)
  }else if(max(data$HR.95H) < 4){
    xlim = c(min(data$HR.95L),4)
  }else{
    xlim = c(min(data$HR.95L),5)
  }
  plot = forestploter::forest(data[,c(1:(ncol(data)-3),ncol(data),(ncol(data)-2):(ncol(data)-1))],
                              est = as.numeric(data$HR),
                              lower = as.numeric(data$HR.95L),
                              upper = as.numeric(data$HR.95H),
                              ci_column = 5,
                              ref_line = 1,
                              xlim = xlim,
                              theme = tm
  )

  boxcolor = c("#E64B35","#4DBBD5","#00A087","#3C5488","#F39B7F","#8491B4","#91D1C2","#DC0000","#7E6148")

  boxcolor = ifelse(data$HR > 1,boxcolor[1],boxcolor[2])
  for(i in 1:nrow(data)){
    plot <- edit_plot(plot, col=5,row = i, which = "ci", gp = gpar(fill = boxcolor[i],fontsize=25)) # 改col，box的列
  }
  pos_bold_pval = which(as.numeric(gsub('<',"",data$pvalue))<0.05)
  if(length(pos_bold_pval) > 0){
    for(i in pos_bold_pval){
      plot = edit_plot(plot, col=c(1,2,3,4,6),row = i, which = "text", gp = gpar(fontface="bold"))  # 改col pvalue的列
    }
  }
  plot = add_border(plot, part = "header", row =1,where = "top",gp = gpar(lwd =2))
  plot = add_border(plot, part = "header", row = c(lineVec), gp = gpar(lwd =1))
  plot = edit_plot(plot, col=1:ncol(data),row = 1:nrow(data), which = "text", gp = gpar(fontsize=12))
  plot = edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),part="header",
                    x = unit(0.5, "npc"))
  plot = edit_plot(plot, col = 1:ncol(data), which = "text",hjust = unit(0.5, "npc"),
                    x = unit(0.5, "npc"))
  pdf(paste0(nameplot,".pdf"), width = 12, height = height)
  print(plot)
  dev.off()
}
