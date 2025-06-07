plot_diff_histogram = function(data,
                               Type = "Type",
                               variable,
                               levels = NULL,
                               dataset_name = "dataset_name",
                               width = 5,
                               height = 4.5,
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
                                           "#e377c2",  "#b5bd61",  "#17becf","#aec7e8")){
  suppressPackageStartupMessages(library(ggplot2,quietly = TRUE))
  suppressPackageStartupMessages(library(tidyverse,quietly = TRUE))
  data[,variable] = unlist(as.numeric(data[,variable]))
  data[,"expression"] = data[,variable]
  colnames(data)[colnames(data) == Type] = "Type"

  if(is.null(levels)){
    data[,"Type"] = factor(data[,"Type"], levels = unique(data[,"Type"]))
  }else{
    data[,"Type"] = factor(data[,"Type"], levels = levels)
  }

  if(length(unique(data[,"Type"])) == 2){
    p_wilcox = wilcox.test(data[which(data$Type == levels(data[,"Type"])[1]),"expression"],
                           data[which(data$Type == levels(data[,"Type"])[2]),"expression"])$p.value
    p_wilcox = ifelse(p_wilcox < 0.001,"p<0.001",round(p_wilcox,3))

    data_summary = data %>% group_by(Type) %>%
      summarise(
        mean_expression = mean(expression, na.rm = TRUE),
        se_expression = sd(expression, na.rm = TRUE) / sqrt(n()),
        n = n()
      )

    p_wilcox_df = data.frame(Group1 = levels(data[,"Type"])[1],
                             Group2 = levels(data[,"Type"])[2],
                             p_value = c(p_wilcox))
    p_wilcox_df$x_mid1 = c(1)
    p_wilcox_df$x_mid2 = c(2)
    p_wilcox_df$y_value = c(max(data_summary$mean_expression) * 1.05)

    histogram = ggplot(data_summary,
                       aes(x = Type,
                           y = mean_expression,
                           fill = Type)
    ) +
      geom_col(position = "dodge",width = 0.4) +
      geom_errorbar(aes(ymin = mean_expression - se_expression,
                        ymax = mean_expression + se_expression),
                    width = 0.2,
                    position = position_dodge(width = 0.9)) +
      labs(title = "Wilcoxon Rank Sum Tests")+
      ylim(0,max(data_summary$mean_expression + data_summary$se_expression) * 1.35) +
      xlab(dataset_name) +
      ylab(paste0(variable," (Mean Expression)")) +
      labs(fill = "Group") +
      theme_minimal() +
      scale_color_manual(values = alpha(mycolor,alphas))+
      scale_fill_manual(values = alpha(mycolor,alphas))+
      geom_segment(data = p_wilcox_df,
                   aes(x = x_mid1, xend = x_mid2, y = y_value, yend = y_value),
                   colour = "black", size = 0.5, linetype = "dashed", inherit.aes = FALSE) +
      geom_text(data = p_wilcox_df,
                aes(x = (x_mid1 + x_mid2) / 2, y = y_value, label = p_value),
                vjust = -0.5, size = 3.5, check_overlap = TRUE, inherit.aes = FALSE)

    pdf(file = paste0(dataset_name,"_",variable, ".pdf"), width = 5, height = 4.5)
    print(histogram)
    dev.off()

  }else if(length(unique(data[,"Type"])) == 3){
    p_wilcox_1v2 = wilcox.test(data[which(data$Type == levels(data[,"Type"])[1]),"expression"],
                               data[which(data$Type == levels(data[,"Type"])[2]),"expression"])$p.value
    p_wilcox_1v2 = ifelse(p_wilcox_1v2 < 0.001,"p<0.001",round(p_wilcox_1v2,3))

    p_wilcox_1v3 = wilcox.test(data[which(data$Type == levels(data[,"Type"])[1]),"expression"],
                               data[which(data$Type == levels(data[,"Type"])[3]),"expression"])$p.value
    p_wilcox_1v3 = ifelse(p_wilcox_1v3 < 0.001,"p<0.001",round(p_wilcox_1v3,3))

    p_wilcox_2v3 = wilcox.test(data[which(data$Type == levels(data[,"Type"])[2]),"expression"],
                               data[which(data$Type == levels(data[,"Type"])[3]),"expression"])$p.value
    p_wilcox_2v3 = ifelse(p_wilcox_2v3 < 0.001,"p<0.001",round(p_wilcox_2v3,3))

    data_summary = data %>%
      group_by(Type) %>%
      summarise(mean_expression = mean(expression, na.rm = TRUE),
                se_expression = sd(expression, na.rm = TRUE) / sqrt(n()),
                n = n())

    p_wilcox_df = data.frame(Group1 = c(levels(data[,"Type"])[1],levels(data[,"Type"])[1],levels(data[,"Type"])[2]),
                             Group2 = c(levels(data[,"Type"])[2],levels(data[,"Type"])[3],levels(data[,"Type"])[3]),
                             p_value = c(p_wilcox_1v2,p_wilcox_1v3,p_wilcox_2v3))
    p_wilcox_df$x_mid1 = c(1,1,2)
    p_wilcox_df$x_mid2 = c(2,3,3)
    p_wilcox_df$y_value = c(max(data_summary$mean_expression) * 1.05,
                            max(data_summary$mean_expression) * 1.20,
                            max(data_summary$mean_expression) * 1.35)


    histogram = ggplot(data_summary,aes(x = Type,y = mean_expression,fill = Type)) +
      geom_col(position = "dodge",width = 0.4) +
      geom_errorbar(aes(ymin = mean_expression - se_expression,
                        ymax = mean_expression + se_expression),
                    width = 0.2,
                    position = position_dodge(width = 0.9)) +
      labs(title = "Wilcoxon Rank Sum Tests")+
      ylim(0, max(data_summary$mean_expression + data_summary$se_expression) * 1.35) +
      xlab(dataset_name) +
      ylab(paste0(variable," (Mean Expression)")) +
      labs(fill = "Group") +
      theme_minimal() +
      scale_color_manual(values = alpha(mycolor,alphas))+
      scale_fill_manual(values = alpha(mycolor,alphas))+
      geom_segment(data = p_wilcox_df,
                   aes(x = x_mid1, xend = x_mid2, y = y_value, yend = y_value),
                   colour = "black", size = 0.5, linetype = "dashed", inherit.aes = FALSE) +
      geom_text(data = p_wilcox_df,
                aes(x = (x_mid1 + x_mid2) / 2,
                    y = y_value,
                    label = p_value),
                vjust = -0.5,
                size = 3.5,
                check_overlap = TRUE,
                inherit.aes = FALSE)
    pdf(file = paste0(dataset_name,"_",variable, ".pdf"), width = 5, height = 4.5)
    print(histogram)
    dev.off()
  }else{
    p_kruskal = kruskal.test(expression ~ Type, data = data)$p.value
    p_kruskal = ifelse(p_kruskal < 0.001,"p<0.001",round(p_kruskal,3))
    data_summary = data %>%
      group_by(Type) %>%
      summarise(mean_expression = mean(expression, na.rm = TRUE),
                se_expression = sd(expression, na.rm = TRUE) / sqrt(n()),
                n = n())
    histogram = ggplot(data_summary,aes(x = Type,y = mean_expression,fill = Type)) +
      geom_col(position = "dodge",width = 0.4) +
      geom_errorbar(aes(ymin = mean_expression - se_expression,
                        ymax = mean_expression + se_expression),
                    width = 0.2,
                    position = position_dodge(width = 0.9)) +
      labs(title = "Wilcoxon Rank Sum Tests")+
      ylim(0, max(data_summary$mean_expression + data_summary$se_expression) * 1.35) +
      xlab(dataset_name) +
      ylab(paste0(variable," (Mean Expression)")) +
      labs(fill = "Group") +
      theme_minimal() +
      scale_color_manual(values = alpha(mycolor,alphas)) +
      scale_fill_manual(values = alpha(mycolor,alphas)) +
      geom_text(aes(x = (1 + length(unique(data[,"Type"])))/2,
                    y = max(data_summary$mean_expression) * 1.05,
                    label = p_kruskal),
                vjust = -0.5,
                size = 3.5,
                check_overlap = TRUE,
                inherit.aes = FALSE)
    pdf(file = paste0(dataset_name,"_",variable, ".pdf"), width = width, height = height)
    print(histogram)
    dev.off()
  }
  return(histogram)
}
