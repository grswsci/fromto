#' @title survival_unicox
#' @description The first column is the survival time, the second is the survival state, and the third and all subsequent columns are the variables to be analyzed.
#' @param data data frame
#' @return unicox results
survival_unicox = function(data){
  unicox_merge = data.frame()
  library(survival)
  for (variable in colnames(data)[3:ncol(data)]){
    data_subset = data[,c(colnames(data)[1:2],variable)]
    unicox = as.formula(paste0("Surv(",colnames(data_subset)[1],",",colnames(data_subset)[2],")","~",variable))
    myunicox = coxph(unicox, data = data_subset)
    unicoxResult = summary(myunicox)
    unicox_merge = rbind(unicox_merge,
                         cbind(id = variable,
                               HR = round(unicoxResult$conf.int[,"exp(coef)"],3),
                               HR.95L = round(unicoxResult$conf.int[,"lower .95"],3),
                               HR.95H = round(unicoxResult$conf.int[,"upper .95"],3),
                               pvalue = unicoxResult$coefficients[,"Pr(>|z|)"])
    )
  }
  return(unicox_merge)
}
#' @title survival_multicox
#' @description The first column is the survival time, the second is the survival state, and the third and all subsequent columns are the variables to be analyzed.
#' @param data data frame
#' @return multicox results
survival_multicox = function(data){
  library(survival)
  multicoxTable = data.frame()
  multicox = as.formula(paste0("Surv(",colnames(data)[1],",",colnames(data)[2],")","~","."))
  multicox = coxph(multicox, data = data)
  multicoxsummary = summary(multicox)
  multicoxTable = rbind(multicoxTable,
                        cbind(id = rownames(multicoxsummary[["coefficients"]]),
                              HR = round(multicoxsummary$conf.int[,"exp(coef)"],3),
                              HR.95L = round(multicoxsummary$conf.int[,"lower .95"],3),
                              HR.95H = round(multicoxsummary$conf.int[,"upper .95"],3),
                              pvalue = multicoxsummary$coefficients[,"Pr(>|z|)"])
  )
  multicoxTable = as.data.frame(multicoxTable)
  return(multicoxTable)
}

survival_logrank = function(data,
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

  survdiff_formula1 = as.formula(paste0("Surv(",colnames(data)[1],",",colnames(data)[2],")","~","group"))
  diff = survdiff(survdiff_formula1,data = data, na.action = na.exclude)
  pValue = 1 - pchisq(diff$chisq, length(diff$n) - 1)
  return(pValue)
}
