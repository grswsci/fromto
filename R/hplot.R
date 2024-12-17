hplot = function(hplot_input,
                 right_annotation_use,
                 topann_color_use,
                 bar_color_use = "steelblue",
                 cellwidth,
                 cellheight,
                 legend_heatmap_use,
                 legend_topann_use,
                 fontsize_use,
                 cluster_columns,
                 cluster_rows){
  suppressPackageStartupMessages(library(ComplexHeatmap))
  col_ha = columnAnnotation(legend_topann_use = colnames(hplot_input),
                            col = list(legend_topann_use = topann_color_use),
                            show_annotation_name = F)

  row_ha = rowAnnotation(bar = anno_barplot(right_annotation_use,
                                            bar_width = 0.8,
                                            border = FALSE,
                                            gp = gpar(fill = bar_color_use, col = NA),
                                            add_numbers = T,
                                            numbers_offset = unit(-10, "mm"),
                                            axis_param = list("labels_rot" = 0),
                                            numbers_gp = gpar(fontsize = fontsize_use, col = "white"),
                                            width = unit(3, "cm")),
                         show_annotation_name = F)

  Heatmap(as.matrix(hplot_input),
          name = legend_heatmap_use,
          right_annotation = row_ha,
          top_annotation = col_ha,
          col = c("#1CB8B2", "#FFFFFF", "#EEB849"), # 黄绿配色
          rect_gp = gpar(col = "black", lwd = 1), # 边框设置为黑色
          cluster_columns = cluster_columns,
          cluster_rows = cluster_rows, # 不进行聚类，无意义
          show_column_names = FALSE,
          show_row_names = TRUE,
          row_names_side = "left",
          width = unit(cellwidth * ncol(hplot_input) + 2, "cm"),
          height = unit(cellheight * nrow(hplot_input), "cm"),
          column_split = factor(colnames(hplot_input), levels = colnames(hplot_input)),
          column_title = NULL,
          cell_fun = function(j, i, x, y, w, h, col) { # add text to each grid
            grid.text(label = format(hplot_input[i, j],
                                     digits = 3,
                                     nsmall = 3),
                      x, y, gp = gpar(fontsize = 10))
          }
  )
}

hplot1 = function(hplot_input,
                  bar_color_use = "steelblue",
                  legend_heatmap_use ,
                  legend_topann_use ,
                  cellwidth_use = 1,
                  cellheight_use = 0.5,
                  fontsize_use = 10,
                  round_use = 3,
                  topann_color_use = c("#BC3C29FF","#0072B5FF","#E18727FF",
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
                              "#e377c2",  "#b5bd61",  "#17becf","#aec7e8"),
                  heatmap_name_use = "heatmap_name_use"
){
  hplot_input = fromto::convert_to_factor_by_column(hplot_input)
  right_annotation_use = sort(apply(hplot_input, 1, mean), decreasing = T)
  right_annotation_use = round(right_annotation_use,round_use)
  hplot_input = hplot_input[names(right_annotation_use), ]
  topann_color_use = setNames(topann_color_use[1:length(colnames(hplot_input))], colnames(hplot_input))

  hm = hplot(hplot_input = hplot_input,
             right_annotation_use = right_annotation_use,
             topann_color_use = topann_color_use,
             bar_color_use = bar_color_use,
             cellwidth = cellwidth_use,
             cellheight = cellheight_use,
             cluster_columns = F,
             cluster_rows = F,
             fontsize_use = fontsize_use,
             legend_heatmap_use = legend_heatmap_use,
             legend_topann_use = legend_topann_use
  )

  existing_color_mapping = hm@top_annotation@anno_list[["legend_topann_use"]]@color_mapping
  new_order = colnames(hplot_input)
  colors = existing_color_mapping@colors
  new_color_mapping = ColorMapping(
    levels = new_order,
    colors = colors[new_order]
  )
  new_color_mapping@name = legend_topann_use
  hm@top_annotation@anno_list[["legend_topann_use"]]@color_mapping = new_color_mapping
  pdf(paste0(heatmap_name_use,".pdf"), width = 8 + ncol(hplot_input)/2, height = 5 + nrow(hplot_input)/6)
  draw(hm,
       heatmap_legend_side = "right",
       annotation_legend_side = "right")
  invisible(dev.off())
}

