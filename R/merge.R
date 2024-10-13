merge_row <- function(data1,data2){
  samesample = intersect(rownames(data1),rownames(data2))
  data1 = data1[samesample,,drop=FALSE]
  data2 = data2[samesample,,drop=FALSE]
  data3 = cbind(data1,data2)
  return(data3)
}

merge_col <- function(data1,data2){
  samesample = intersect(colnames(data1),colnames(data2))
  data1 = data1[,samesample,drop=FALSE]
  data2 = data2[,samesample,drop=FALSE]
  data3 = rbind(data1,data2)
  return(data3)
}

merge_plot <- function(type = "pdf", ncol = 1) {
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
