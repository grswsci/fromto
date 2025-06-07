plot_diff_boxplot = function(data,
                             tumor_colnames,
                             group_use,
                             var_use,
                             freq_use = 3,
                             color_use = c("#88c4e8","#db6968")){
  #构造临时对象
  data$expr = data[,var_use]
  data$Type = data[,group_use]
  data$CancerType = data[,tumor_colnames]
  
  #删除正常组过少的肿瘤
  df_delete = data.frame(table(data$CancerType,data$Type))
  df_delete = df_delete[which(df_delete$Freq < freq_use),]
  data = data[which(!data[,"CancerType"] %in% unique(df_delete$Var1)),]
  
  #计算显著性
  pvalues = sapply(unique(data$CancerType), function(x) {
    res = wilcox.test(as.numeric(expr) ~ Type, data = subset(data, CancerType == x)) 
    res$p.value
  })
  
  #准备可视化
  pv = data.frame(cancer_type = unique(data$CancerType), pvalue = pvalues)
  pv$sigcode = cut(pv$pvalue, c(0,0.0001, 0.001, 0.01, 0.05, 1),
                   labels=c('****','***', '**', '*', '')
  )
  
  #可视化
  count_N = data %>% group_by(CancerType, Type) %>% tally
  count_N$n = paste("n =",count_N$n)
  ylabname = var_use
  
  boxplot_res = ggplot(data, 
                    aes(x = CancerType, 
                        y = expr, 
                        color = Type, 
                        fill = Type)
  ) +
    geom_point(shape = 21, 
               size = 2, 
               position = position_jitterdodge(),  
               alpha = 0.35) +
    geom_boxplot(notch = F, 
                 outlier.size = -1, 
                 color = "black", 
                 lwd = 0.8, 
                 alpha = 0.7) +
    theme_classic() +
    scale_color_manual(values = color_use)+
    scale_fill_manual(values = color_use)+
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.8)
          )+
    theme(axis.text.x = element_text(colour="black", 
                                     size = 11,
                                     angle = 45, 
                                     hjust = .5, 
                                     vjust = .5),
          legend.position = "bottom") +
    geom_text(aes(x = cancer_type,
                  y = max(data[,var_use]),
                  label = pv$sigcode),
              data = pv,
              inherit.aes = F) +
    ylab(ylabname) +
    geom_text(data = count_N,
              aes(label = n, 
                  y = min(data[,var_use]) - 2 ,
                  color = Type),
              position = position_dodge2(0.9),
              size = 3,
              angle = 90, 
              hjust = 0) +
    ylim(min(data[,var_use]) - 2,
         max(data[,var_use])) +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          legend.position = "bottom",
          panel.border = element_rect(colour = "black", fill=NA, size=0.6))
  
  res_diff_pancancer = list(original_data = data,
                            pvalue_res = pv,
                            boxplot_res = boxplot_res
                            )
  
}








  