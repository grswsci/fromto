#' @title rplot
#' @description rplot
#' @param df data frame
#' @param group default = "Response_NR"
#' @param varible_analysis analysis which varible
#' @param varible_control in varible_analysis, control group name is what
#' @param main_name_use main name use
#' @param plot_name_use default = "plotname_use"
#' @return pdf
rplot = function(df,
                 group = "Response_NR",
                 varible_analysis,
                 varible_control = "NR",
                 main_name_use = "Immunotherapy response prediction",
                 plot_name_use = "plotname_use"){
  library(pROC)
  y = ifelse(df[,group] == varible_control, 0, 1)
  roc1 = roc(y, as.numeric(df[,varible_analysis]))
  ci1 = ci.auc(roc1, method = "bootstrap")
  ciVec = as.numeric(ci1)
  pdf(file = paste0(plot_name_use,"_ROC.pdf"), width = 5, height = 5)
  plot(roc1, print.auc = TRUE, col="#E64B35FF", legacy.axes = TRUE, main = main_name_use)
  text(0.39,
       0.43,
       paste0("95% CI: ",
              sprintf("%.03f",ciVec[1]),
              "-",sprintf("%.03f",ciVec[3])
       ),
       col = "#4DBBD5FF")
  dev.off()
}
