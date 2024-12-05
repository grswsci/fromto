#' @title merge_col_add
#' @description Merge add line from another data frame
#' @param data1 data frame
#' @param data2 data frame
#' @param data1_var data1_var
#' @param data2_var_same_data1 data2_var_same_data1
#' @param data2_var_add_data1 data2_var_add_data1
merge_col_add = function(data1,
                         data2,
                         data1_var,
                         data2_var_same_data1,
                         data2_var_add_data1
){
  data1 = as.data.frame(data1)
  data2 = as.data.frame(data2)
  add_col = c()
  for (variable in data1[,data1_var]) {
    if(variable %in% data2[,data2_var_same_data1]){
      data2_subset = data2[which(data2[,data2_var_same_data1] == variable),,drop = FALSE]
      add_col = c(add_col,data2_subset[,data2_var_add_data1])
    }else{
      add_col = c(add_col,NA)
    }
  }
  data1 = cbind(data1,add_col)
  return(data1)
}
#' @title merge_row
#' @description Merge by rownames
#' @param data1 data frame
#' @param data2 data frame
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 10
#' n_cols1 = 3
#' data1 = data.frame(matrix(rnorm(n_rows * n_cols1), nrow = n_rows))
#' rownames(data1) = paste0("sample", 1:n_rows)
#' colnames(data1) = paste0("var", 1:n_cols1)
#' set.seed(456)
#' n_cols2 = 2
#' data2 = data.frame(matrix(runif(n_rows * n_cols2), nrow = n_rows))
#' rownames(data2) = paste0("sample", 1:n_rows)
#' colnames(data2) = paste0("var", (n_cols1 + 1):(n_cols1 + n_cols2))
#' result = merge_row(data1, data2)
#' print(result)

merge_row <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  data2 = data2[samesample,,drop=FALSE]
  data3 = cbind(data1,data2)
  return(data3)
}

#' @title merge_col
#' @description Merge by colnames
#' @param data1 data frame
#' @param data2 data frame
#' @return data frame
#' @examples
#' # examples
#' set.seed(123)
#' n_rows = 10
#' n_cols1 = 3
#' data1 = data.frame(matrix(rnorm(n_rows * n_cols1), nrow = n_rows))
#' rownames(data1) = paste0("sample", 1:n_rows)
#' colnames(data1) = paste0("var", 1:n_cols1)
#' data1 = t(data1)
#'
#' set.seed(456)
#' n_cols2 = 2
#' data2 = data.frame(matrix(runif(n_rows * n_cols2), nrow = n_rows))
#' rownames(data2) = paste0("sample", 1:n_rows)
#' colnames(data2) = paste0("var", (n_cols1 + 1):(n_cols1 + n_cols2))
#' data2 = t(data2)
#'
#' result = merge_col(data1, data2)
#' print(result)

merge_col <- function(data1,data2){
  samesample = intersect(colnames(data1),colnames(data2))
  data1 = data1[,samesample,drop=FALSE]
  data2 = data2[,samesample,drop=FALSE]
  data3 = rbind(data1,data2)
  return(data3)
}

#' @title merge_plot
#' @description Merge by plot
#' @param type plot type, you can choose "pdf","jpg","png" and "tiff"
#' @param ncol How many pictures are there in each column
#' @return ggplot object
#' @examples
#' # examples
#' set.seed(123)
#' n = 150
#' data = data.frame(Type = sample(c("A", "B", "C"), n, replace = TRUE),
#'                   variable1 = rnorm(n))
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name1",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name2",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'    dplot1(data,
#'           Type = "Type",
#'           variable = "variable1",
#'           test.methods = "kruskal.test",
#'           DatasetName = "Name3",
#'           levels = c("C","B","A"),
#'           width = 6,
#'           height = 5)
#'plot_merge = merge_plot(type = "pdf", ncol = 3)
#'ggsave2("Figure_Merge.pdf",height = 1,width = 3)

merge_plot <- function(type = "pdf", ncol = 1) {
  options(warn = -1)
  library(ggplotify)
  library(cowplot)
  library(magick)
  library(pdftools)
  fnames <- Sys.glob(paste0("*.",type))
  if(type == "pdf"){
    p <- lapply(fnames,function(i){
      pn <- as.ggplot(image_read_pdf(i))
    })
  }else if(type %in% c("jpg","png","tiff")){
    p <- lapply(fnames,function(i){
      pn <- as.ggplot(image_read(i))
    })
  }

  plot_grid(plotlist = p, ncol = ncol)
  return(p)
}

same_row_data1 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  return(data1)
}

same_row_data2 <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data2 = data2[samesample,,drop=FALSE]
  return(data2)
}

