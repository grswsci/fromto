#' @title kmplot1
#' @description km plot and log-rank test
#' @param data data frame
#' @param minprop mini-proportion
#' @param GeneName GeneName
#' @param CancerType CancerType
#' @param Timeunit year month data
#' @return km plot
kmplot1 = function(data,
                   minprop = 0.3,
                   GeneName,
                   CancerType,
                   Timeunit = "year"){
  suppressPackageStartupMessages(library(survival))
  suppressPackageStartupMessages(library(survminer))
  suppressPackageStartupMessages(library(dplyr))

  bestcut = surv_cutpoint(data,
                          time = colnames(data)[1],
                          event = colnames(data)[2],
                          variables = GeneName,
                          minprop = minprop)
  cutoff = bestcut$cutpoint[1,1]
  data$group = factor(ifelse(data[,GeneName] > cutoff, "High","Low"), levels = c("High","Low"))
  data$group2 = factor(ifelse(data[,GeneName] > median(data[,GeneName]),"High","Low"), levels = c("High","Low"))

  SurvivalType = ifelse(colnames(data)[2] == "OS","Overall Survival",
                        ifelse(colnames(data)[2] == "DSS","Disease-specific Survival",
                               ifelse(colnames(data)[2] == "PFS","Progression-free Survival",
                                      ifelse(colnames(data)[2] == "DFS","Disease-free Survival",
                                             ifelse(colnames(data)[2] == "RFS","Relapse-free Survival",
                                                    ifelse(colnames(data)[2] == "DFI","Disease-free Interval",
                                                           ifelse(colnames(data)[2] == "PFI","Progression-free Interval","Survival")
                                                    )
                                             )
                                      )
                               )
                        )
  )

  if(substr(rownames(data)[1],1,3) == "GSM"){
    rownames(data) = paste0(CancerType,"_",substr(gsub("GSM","",rownames(data)),nchar(gsub("GSM","",rownames(data)))-3,nchar(gsub("GSM","",rownames(data)))),"_Rename")
    data2 = data[order(data[,GeneName],decreasing = T),,drop = F]
    write.csv(data2,paste0(GeneName,"_",CancerType,"_",SurvivalType,"_KM.csv"))
  }else{
    write.csv(data,paste0(GeneName,"_",CancerType,"_",SurvivalType,"_KM.csv"))
  }

  survdiff_formula1 = as.formula(paste0("Surv(",colnames(data)[1],",",colnames(data)[2],")","~","group"))
  diff = survdiff(survdiff_formula1,data = data, na.action = na.exclude)
  pValue = 1 - pchisq(diff$chisq, length(diff$n) - 1)
  if(pValue < 0.001){
    pValue = "< 0.001"
  }else{
    pValue = paste0("= ",sprintf("%.03f",pValue))
  }
  fit = survfit(survdiff_formula1, data = data)
  pdf(paste0(GeneName,"_",CancerType,"_",SurvivalType,"_The_Best_Cutoff.pdf"),onefile = FALSE,width = 5,height =5)
  plot(fit,
       lty = 2:3,
       lwd = 2,
       col = c("#E64B35FF", "#4DBBD5FF"),
       xlab = paste0("Time (",Timeunit,")"),
       ylab = paste0("Survival Rate (",SurvivalType,")"),
       main = paste("log-rank test p", pValue , "(",CancerType,")"),
       mark.time=T)
  legend("bottomright",
         lty = 2:3,
         c(paste0("High ", GeneName," (n= ",nrow(data[data$group=="High",]),")"),
           paste0("Low  ", GeneName," (n= ",nrow(data[data$group=="Low",]), ")")),
         lwd = 2,
         col=c("#E64B35FF", "#4DBBD5FF"))
  dev.off()

  survdiff_formula2 = as.formula(paste0("Surv(",colnames(data)[1],",",colnames(data)[2],")","~","group2"))
  diff2 = survdiff(survdiff_formula2, data = data, na.action = na.exclude)
  pValue2 = 1 - pchisq(diff2$chisq, length(diff2$n) - 1)
  if(pValue2 < 0.001){
    pValue2 = "< 0.001"
  }else{
    pValue2 = paste0("= ",sprintf("%.03f",pValue2))
  }
  fit2 = survfit(survdiff_formula2, data = data)
  pdf(paste0(GeneName,"_",CancerType,"_",SurvivalType,"_The_median_cutoff.pdf"),onefile = FALSE,width = 5,height =5)
  plot(fit2,
       lty = 2:3,
       lwd = 2,
       col = c("#E64B35FF", "#4DBBD5FF"),
       xlab = paste0("Time (",Timeunit,")"),
       ylab = paste0("Survival Rate (",SurvivalType,")"),
       main = paste("log-rank test p", pValue2 , "(",CancerType,")"),
       mark.time=T)
  legend("bottomright",
         lty=2:3,
         c(paste0("High ", GeneName," (n= ",nrow(data[data$group2 == "High",]),")"),
           paste0("Low  ", GeneName," (n= ",nrow(data[data$group2 == "Low",]), ")")),
         lwd = 2,
         col = c("#E64B35FF", "#4DBBD5FF"))
  dev.off()
}
