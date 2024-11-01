#' @title pplot
#' @description pplot
#' @param df data frame
#' @param variable default = c("Quantile","Response_NR")
#' @param levels default = c("R","NR")
#' @param legend_use default = "Response"
#' @param plot_name_use default = "plotname_use"
#' @return pdf
pplot = function(df,
                 variable = c("Quantile","Response_NR"),
                 levels = c("R","NR"),
                 legend_use = "Response",
                 plot_name_use = "plotname_use"){
  library(plyr)
  library(ggplot2)
  df = df[,variable]
  colnames(df) = c("Group",legend_use)
  table_df = table(df)
  df_table_df = as.data.frame(table_df)
  chisq.test_result = chisq.test(table_df)
  pvalue = chisq.test_result$p.value

  if(pvalue < 0.001){
    pvalue = "p < 0.001"
  }else{
    pvalue = paste0("p = ",round(pvalue,3))
  }

  df = ddply(df_table_df, .(Group), transform, percent = Freq/sum(Freq) * 100)
  df = ddply(df, .(Group), transform, pos = (cumsum(Freq) - 0.5 * Freq))
  df$label = paste0(sprintf("%.0f", df$percent), "%")
  df[,legend_use] = factor(df[,legend_use], levels = levels)

  p = ggplot(df,
             aes(x = factor(Group),
                 y = percent,
                 fill = df[,legend_use])
  ) +
    geom_bar(position = position_stack(), stat = "identity", width = 0.7) +
    scale_fill_manual(values = c("#E64B35FF",
                                 "#4DBBD5FF")
    ) +
    ggtitle(paste0("Chi-Square test ",pvalue)) +
    xlab("") +
    ylab("Percent") +
    guides(fill = guide_legend(title = legend_use)
    ) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 3) +
    theme_bw() +
    theme(axis.text.x = ggtext::element_markdown(angle =90,
                                                 size = 12,
                                                 hjust = 1,
                                                 vjust = 0.5),
          plot.margin = unit(c(1,1,1,1), "lines")
    )

  pdf(file=paste0(plot_name_use,"_Chi-Square_test.pdf"), width=5, height=5)
  print(p)
  dev.off()
}
