#' @title st_read_10x_h5_lowres
#' @description st_read_10x_h5_lowres
#' @param CancerName CancerName
#' @param path path
#' @return seurat object
st_read_10x_h5_lowres = function(CancerName,path = "/DL"){
  filenames = list.files(path = path, full.names = FALSE)
  meta.data = grep("csv$",filenames,value=T)
  print(meta.data)
  h5File = grep("h5$",filenames,value=T)
  print(h5File)
  rm(filenames)
  library(Seurat)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(Matrix)
  work.path = path
  setwd(work.path)

  filenames = list.files(path = paste0(path,"/spatial"), full.names = FALSE)

  jsonFile = grep("json$",filenames,value=T)
  if(jsonFile != "scalefactors_json.json"){
    file.copy(from = paste0(path,"/spatial/",jsonFile),
              to = paste0(path,"/spatial/scalefactors_json.json"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  csvFile = grep("csv$",filenames,value=T)
  if(csvFile != "tissue_positions_list.csv"){
    file.copy(from = paste0(path,"/spatial/",csvFile),
              to = paste0(path,"/spatial/tissue_positions_list.csv"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  hires_imageFile = grep("hires_image.png$",filenames,value=T)
  if(length(hires_imageFile)==0){

  }else if(hires_imageFile != "tissue_hires_image.png"){
    file.copy(from = paste0(path,"/spatial/",hires_imageFile),
              to = paste0(path,"/spatial/tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  if(length(hires_imageFile)==0){

  }else{
    file.copy(from = paste0(path,"/spatial/",hires_imageFile),
              to = paste0(path,"/",CancerName,".tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  lowres_imageFile = grep("lowres_image.png$",filenames,value=T)
  if(lowres_imageFile != "tissue_lowres_image.png"){
    file.copy(from = paste0(path,"/spatial/",lowres_imageFile),
              to = paste0(path,"/spatial/tissue_lowres_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  file.copy(from = paste0(path,"/spatial/",lowres_imageFile),
            to = paste0(path,"/",CancerName,".tissue_lowres_image.png"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  jpgFile = grep("detected_tissue_image.jpg$",filenames,value=T)
  file.copy(from = paste0(path,"/spatial/",jpgFile),
            to = paste0(path,"/",CancerName,".detected_tissue_image.jpg"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  stRNA = Load10X_Spatial(data.dir = path,
                          filename = h5File,
                          slice = "spatial")
  spatial_location = read.csv(meta.data, header = T, row.names=1)
  spatial_location[is.na(spatial_location)] = 0
  spatial_location[spatial_location< 0 ] = 0
  samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
  countnumber = ncol(stRNA@meta.data)
  if(length(samesample) < length(rownames(stRNA@meta.data))){
    spatial_location[setdiff(rownames(stRNA@meta.data),samesample),] = 0
    samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
    spatial_location = spatial_location[samesample,]
  }else{
    spatial_location = spatial_location[samesample,]
  }
  stRNA@meta.data = cbind(stRNA@meta.data,spatial_location)
  #saveRDS(stRNA,paste0(path,"/",CancerName,".stRNA.RDS"))
  spatial_location_2 = read.csv(meta.data, header = T, row.names=1)
  samesample_2 = intersect(rownames(stRNA@meta.data),rownames(spatial_location_2))
  stRNA2 = stRNA[,colnames(stRNA) %in% samesample_2]
  stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)] = as.numeric(unlist(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  saveRDS(stRNA2,paste0(path,"/",CancerName,".stRNA.RDS"))
  plot = SpatialFeaturePlot(stRNA2, keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = colnames(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  ggsave(paste0(path,"/",CancerName,".SpatialDimPlot.pdf"), plot = plot , width = 16, height = 16)
  stRNA2 = NormalizeData(stRNA2)
  plot = SpatialFeaturePlot(stRNA2,
                            keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = "CENPF",
                            slot = "data")
  ggsave(paste0(path,"/CENPF.SpatialDimPlot.pdf"), plot = plot , width = 6, height = 6)
}
#' @title st_read_10x_h5_hires
#' @description st_read_10x_h5_hires
#' @param CancerName CancerName
#' @param path path
#' @return seurat object
st_read_10x_h5_hires <- function(CancerName,path = "/DL"){
  filenames = list.files(path = path, full.names = FALSE)
  meta.data = grep("csv$",filenames,value=T)
  print(meta.data)
  h5File = grep("h5$",filenames,value=T)
  print(h5File)
  rm(filenames)

  filenames = list.files(path = paste0(path,"/spatial"), full.names = FALSE)
  jsonFile = grep("json$",filenames,value=T)
  print(jsonFile)
  positionsFile = grep("csv$",filenames,value=T)
  print(positionsFile)
  image.nameFile = grep("png$",filenames,value=T)
  print(image.nameFile)

  hires_imageFile = grep("hires_image.png$",filenames,value=T)
  file.copy(from = paste0(path,"/spatial/",hires_imageFile),
            to = paste0(path,"/",CancerName,".tissue_hires_image.png"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  library(Seurat)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(Matrix)
  work.path = path
  setwd(work.path)
  Read10X_Image <- function(image.dir=paste0(path,"/spatial"),
                            image.name = image.nameFile,
                            assay = "Spatial",
                            slice = "slice1",
                            jsonFile = NULL,
                            positions = NULL,
                            filter.matrix = TRUE){
    image <- png::readPNG(source = file.path(image.dir, image.nameFile))
    scale.factors = jsonlite::fromJSON(txt = file.path(image.dir, jsonFile))
    #tissue.positions.path <- Sys.glob(paths = file.path(image.dir, "tissue_positions*"))
    tissue.positions.path = file.path(image.dir, positionsFile)
    tissue.positions = read.csv(file = tissue.positions.path,
                                col.names = c("barcodes", "tissue", "row", "col", "imagerow",
                                              "imagecol"),
                                header = ifelse(test = basename(tissue.positions.path) == positions,
                                                yes = TRUE,
                                                no = FALSE),
                                as.is = TRUE, row.names = 1)
    if (filter.matrix) {
      tissue.positions <- tissue.positions[which(x = tissue.positions$tissue ==
                                                   1), , drop = FALSE]
    }
    unnormalized.radius <- scale.factors$fiducial_diameter_fullres *
      scale.factors$tissue_lowres_scalef
    spot.radius <- unnormalized.radius/max(dim(x = image))
    return(new(Class = "VisiumV1", image = image, scale.factors = scalefactors(spot = scale.factors$spot_diameter_fullres,
                                                                               fiducial = scale.factors$fiducial_diameter_fullres, hires = scale.factors$tissue_hires_scalef,
                                                                               scale.factors$tissue_lowres_scalef), coordinates = tissue.positions,
               spot.radius = spot.radius))
  }
  img = Read10X_Image(paste0(path,"/spatial"),
                      jsonFile = jsonFile,
                      positions = positionsFile,
                      image.name = image.nameFile)

  stRNA = Load10X_Spatial(work.path,
                          filename = h5File,
                          image = img)
  stRNA@images$slice1@scale.factors$lowres = stRNA@images$slice1@scale.factors$hires
  stRNA@images$slice1@spot.radius=0.01248077
  countnumber = ncol(stRNA@meta.data)
  spatial_location = read.csv(meta.data,header = T,row.names=1)
  spatial_location[is.na(spatial_location)] = 0
  samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
  if(length(samesample) < length(rownames(stRNA@meta.data))){
    spatial_location[setdiff(rownames(stRNA@meta.data),samesample),] = 0
    samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
    spatial_location = spatial_location[samesample,]
  }else{
    spatial_location = spatial_location[samesample,]
  }
  stRNA@meta.data = cbind(stRNA@meta.data,spatial_location)
  #saveRDS(stRNA,paste0(path,"/",CancerName,".stRNA.RDS"))
  spatial_location_2 = read.csv(meta.data, header = T, row.names=1)
  samesample_2 = intersect(rownames(stRNA@meta.data),rownames(spatial_location_2))
  stRNA2 = stRNA[,colnames(stRNA) %in% samesample_2]
  saveRDS(stRNA2,paste0(path,"/",CancerName,".stRNA.RDS"))
  plot = SpatialFeaturePlot(stRNA2, keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = colnames(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  ggsave(paste0(path,"/",CancerName,".SpatialDimPlot.pdf"), plot = plot , width = 16, height = 16)
  stRNA2 = NormalizeData(stRNA2)
  plot = SpatialFeaturePlot(stRNA2,
                            keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = "CENPF",
                            slot = "data")
  ggsave(paste0(path,"/CENPF.SpatialDimPlot.pdf"), plot = plot , width = 6, height = 6)
}
#' @title st_read_10x_3file_lowres
#' @description st_read_10x_3file_lowres
#' @param CancerName CancerName
#' @param data_dir_1 data_dir_1
#' @param data_dir_2 data_dir_2
#' @return seurat object
st_read_10x_3file_lowres = function(data_dir_1,data_dir_2,CancerName){
  filenames = list.files(path = paste0(data_dir_2,"/spatial"), full.names = FALSE)

  jsonFile = grep("json$",filenames,value=T)
  if(jsonFile != "scalefactors_json.json"){
    file.copy(from = paste0(data_dir_2,"/spatial/",jsonFile),
              to = paste0(data_dir_2,"/spatial/scalefactors_json.json"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  csvFile = grep("csv$",filenames,value=T)
  if(csvFile != "tissue_positions_list.csv"){
    file.copy(from = paste0(data_dir_2,"/spatial/",csvFile),
              to = paste0(data_dir_2,"/spatial/tissue_positions_list.csv"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  hires_imageFile = grep("hires_image.png$",filenames,value=T)
  if(length(hires_imageFile)==0){

  }else if(hires_imageFile != "tissue_hires_image.png"){
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/spatial/tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  if(length(hires_imageFile)==0){

  }else{
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/",CancerName,".tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  lowres_imageFile = grep("lowres_image.png$",filenames,value=T)
  if(length(lowres_imageFile) == 0){

  }else if(lowres_imageFile != "tissue_lowres_image.png"){
    file.copy(from = paste0(data_dir_2,"/spatial/",lowres_imageFile),
              to = paste0(data_dir_2,"/spatial/tissue_lowres_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  file.copy(from = paste0(data_dir_2,"/spatial/",lowres_imageFile),
            to = paste0(data_dir_2,"/",CancerName,".tissue_lowres_image.png"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  jpgFile = grep("detected_tissue_image.jpg$",filenames,value=T)
  file.copy(from = paste0(data_dir_2,"/spatial/",jpgFile),
            to = paste0(data_dir_2,"/",CancerName,".detected_tissue_image.jpg"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  library(Seurat)
  library(rhdf5)
  library(ggplot2)
  filenames = list.files(path = data_dir_1, full.names = FALSE)

  matrix.mtx.gz = grep("matrix.mtx.gz$",filenames,value=T)
  if(matrix.mtx.gz != "matrix.mtx.gz"){
    file.copy(from = paste0(data_dir_1,"/",matrix.mtx.gz),
              to = paste0(data_dir_1,"/","matrix.mtx.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    matrix.mtx.gz = paste0(data_dir_1,"/",matrix.mtx.gz)
    file.remove(matrix.mtx.gz)
  }

  features.tsv.gz = grep("features.tsv.gz$",filenames,value=T)
  if(features.tsv.gz != "features.tsv.gz"){
    file.copy(from = paste0(data_dir_1,"/",features.tsv.gz),
              to = paste0(data_dir_1,"/","features.tsv.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    features.tsv.gz = paste0(data_dir_1,"/",features.tsv.gz)
    file.remove(features.tsv.gz)
  }

  barcodes.tsv.gz = grep("barcodes.tsv.gz$",filenames,value=T)
  if(barcodes.tsv.gz != "barcodes.tsv.gz"){
    file.copy(from = paste0(data_dir_1,"/",barcodes.tsv.gz),
              to = paste0(data_dir_1,"/","barcodes.tsv.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    barcodes.tsv.gz = paste0(data_dir_1,"/",barcodes.tsv.gz)
    file.remove(barcodes.tsv.gz)
  }

  stRNA = Read10X(data_dir_1)
  image2 = Read10X_Image(image.dir = file.path(data_dir_2,"spatial"),
                         filter.matrix = TRUE)
  stRNA = CreateSeuratObject(counts = stRNA, assay = "Spatial")
  image2 = image2[Cells(x = stRNA)]
  DefaultAssay(stRNA = image2) = "Spatial"
  stRNA[["slice1"]] = image2

  filenames = list.files(path = data_dir_2, full.names = FALSE)
  meta.data = grep("csv$",filenames,value=T)
  print(meta.data)
  spatial_location = read.csv(meta.data, header = T, row.names=1)
  samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
  countnumber = ncol(stRNA@meta.data)
  if(length(samesample) < length(rownames(stRNA@meta.data))){
    spatial_location[setdiff(rownames(stRNA@meta.data),samesample),] = 0
    samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
    spatial_location = spatial_location[samesample,]
  }else{
    spatial_location = spatial_location[samesample,]
  }
  stRNA@meta.data = cbind(stRNA@meta.data,spatial_location)
  #saveRDS(stRNA,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  spatial_location_2 = read.csv(meta.data, header = T, row.names=1)
  samesample_2 = intersect(rownames(stRNA@meta.data),rownames(spatial_location_2))
  stRNA2 = stRNA[,colnames(stRNA) %in% samesample_2]
  saveRDS(stRNA2,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  plot = SpatialFeaturePlot(stRNA2, keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = colnames(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  ggsave(paste0(data_dir_2,"/",CancerName,".SpatialDimPlot.pdf"), plot = plot , width = 16, height = 16)
  stRNA2 = NormalizeData(stRNA2)
  plot = SpatialFeaturePlot(stRNA2,
                            keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = "CENPF",
                            slot = "data")
  ggsave(paste0(data_dir_2,"/CENPF.SpatialDimPlot.pdf"), plot = plot , width = 6, height = 6)
}
#' @title st_read_10x_3file_hires
#' @description st_read_10x_3file_hires
#' @param CancerName CancerName
#' @param data_dir_1 data_dir_1
#' @param data_dir_2 data_dir_2
#' @return seurat object
st_read_10x_3file_hires = function(data_dir_1,data_dir_2,CancerName){
  filenames = list.files(path = paste0(data_dir_2,"/spatial"), full.names = FALSE)

  jsonFile = grep("json$",filenames,value=T)
  if(jsonFile != "scalefactors_json.json"){
    file.copy(from = paste0(data_dir_2,"/spatial/",jsonFile),
              to = paste0(data_dir_2,"/spatial/scalefactors_json.json"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  csvFile = grep("csv$",filenames,value=T)
  if(csvFile != "tissue_positions_list.csv"){
    file.copy(from = paste0(data_dir_2,"/spatial/",csvFile),
              to = paste0(data_dir_2,"/spatial/tissue_positions_list.csv"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  hires_imageFile = grep("hires_image.png$",filenames,value=T)
  if(length(hires_imageFile)==0){

  }else if(hires_imageFile != "tissue_hires_image.png"){
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/spatial/tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  if(length(hires_imageFile)==0){

  }else{
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/",CancerName,".tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  jpgFile = grep("detected_tissue_image.jpg$",filenames,value=T)
  file.copy(from = paste0(data_dir_2,"/spatial/",jpgFile),
            to = paste0(data_dir_2,"/",CancerName,".detected_tissue_image.jpg"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  library(Seurat)
  library(rhdf5)
  library(ggplot2)
  filenames = list.files(path = data_dir_1, full.names = FALSE)

  matrix.mtx.gz = grep("matrix.mtx.gz$",filenames,value=T)
  if(matrix.mtx.gz != "matrix.mtx.gz"){
    file.copy(from = paste0(data_dir_1,"/",matrix.mtx.gz),
              to = paste0(data_dir_1,"/","matrix.mtx.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    matrix.mtx.gz = paste0(data_dir_1,"/",matrix.mtx.gz)
    file.remove(matrix.mtx.gz)
  }

  features.tsv.gz = grep("features.tsv.gz$",filenames,value=T)
  if(features.tsv.gz != "features.tsv.gz"){
    file.copy(from = paste0(data_dir_1,"/",features.tsv.gz),
              to = paste0(data_dir_1,"/","features.tsv.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    features.tsv.gz = paste0(data_dir_1,"/",features.tsv.gz)
    file.remove(features.tsv.gz)
  }

  barcodes.tsv.gz = grep("barcodes.tsv.gz$",filenames,value=T)
  if(barcodes.tsv.gz != "barcodes.tsv.gz"){
    file.copy(from = paste0(data_dir_1,"/",barcodes.tsv.gz),
              to = paste0(data_dir_1,"/","barcodes.tsv.gz"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
    barcodes.tsv.gz = paste0(data_dir_1,"/",barcodes.tsv.gz)
    file.remove(barcodes.tsv.gz)
  }

  stRNA = Read10X(data_dir_1)
  Read10X_Image <- function(image.dir=paste0(data_dir_2,"/spatial"),
                            image.name = NULL,
                            assay = "Spatial",
                            slice = "slice1",
                            jsonFile = NULL,
                            positions = NULL,
                            filter.matrix = TRUE){
    image <- png::readPNG(source = file.path(image.dir, image.name))
    scale.factors = jsonlite::fromJSON(txt = file.path(image.dir, jsonFile))
    #tissue.positions.path <- Sys.glob(paths = file.path(image.dir, "tissue_positions*"))
    tissue.positions.path = file.path(image.dir, positions)
    tissue.positions = read.csv(file = tissue.positions.path,
                                col.names = c("barcodes", "tissue", "row", "col", "imagerow",
                                              "imagecol"),
                                header = ifelse(test = basename(tissue.positions.path) == positions,
                                                yes = TRUE,
                                                no = FALSE),
                                as.is = TRUE, row.names = 1)
    if (filter.matrix) {
      tissue.positions <- tissue.positions[which(x = tissue.positions$tissue ==
                                                   1), , drop = FALSE]
    }
    unnormalized.radius <- scale.factors$fiducial_diameter_fullres *
      scale.factors$tissue_lowres_scalef
    spot.radius <- unnormalized.radius/max(dim(x = image))
    return(new(Class = "VisiumV1", image = image, scale.factors = scalefactors(spot = scale.factors$spot_diameter_fullres,
                                                                               fiducial = scale.factors$fiducial_diameter_fullres, hires = scale.factors$tissue_hires_scalef,
                                                                               scale.factors$tissue_lowres_scalef), coordinates = tissue.positions,
               spot.radius = spot.radius))
  }
  image2 = Read10X_Image(paste0(data_dir_2,"/spatial"),
                         jsonFile = "scalefactors_json.json",
                         positions = "tissue_positions_list.csv",
                         image.name = "tissue_hires_image.png")

  stRNA = CreateSeuratObject(counts = stRNA, assay = "Spatial")
  image2 = image2[Cells(x = stRNA)]
  DefaultAssay(stRNA = image2) = "Spatial"
  stRNA[["slice1"]] = image2
  stRNA@images$slice1@scale.factors$lowres = stRNA@images$slice1@scale.factors$hires
  stRNA@images$slice1@spot.radius=0.01248077
  filenames = list.files(path = data_dir_2, full.names = FALSE)
  meta.data = grep("csv$",filenames,value=T)
  print(meta.data)
  spatial_location = read.csv(meta.data, header = T, row.names=1)
  samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
  countnumber = ncol(stRNA@meta.data)
  if(length(samesample) < length(rownames(stRNA@meta.data))){
    spatial_location[setdiff(rownames(stRNA@meta.data),samesample),] = 0
    samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
    spatial_location = spatial_location[samesample,]
  }else{
    spatial_location = spatial_location[samesample,]
  }
  stRNA@meta.data = cbind(stRNA@meta.data,spatial_location)
  #saveRDS(stRNA,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  spatial_location_2 = read.csv(meta.data, header = T, row.names=1)
  samesample_2 = intersect(rownames(stRNA@meta.data),rownames(spatial_location_2))
  stRNA2 = stRNA[,colnames(stRNA) %in% samesample_2]
  saveRDS(stRNA2,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  plot = SpatialFeaturePlot(stRNA2, keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = colnames(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  ggsave(paste0(data_dir_2,"/",CancerName,".SpatialDimPlot.pdf"), plot = plot , width = 16, height = 16)
  stRNA2 = NormalizeData(stRNA2)
  plot = SpatialFeaturePlot(stRNA2,
                            keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = "CENPF",
                            slot = "data")
  ggsave(paste0(data_dir_2,"/CENPF.SpatialDimPlot.pdf"), plot = plot , width = 6, height = 6)
}
#' @title st_read_10x_countsfile_lowres
#' @description st_read_10x_countsfile_lowres
#' @param CancerName CancerName
#' @param data_dir_1 data_dir_1
#' @param data_dir_2 data_dir_2
#' @return seurat object
st_read_10x_countsfile_lowres  = function(data_dir_1,data_dir_2,CancerName){
  filenames = list.files(path = paste0(data_dir_2,"/spatial"), full.names = FALSE)

  jsonFile = grep("json$",filenames,value=T)
  if(jsonFile != "scalefactors_json.json"){
    file.copy(from = paste0(data_dir_2,"/spatial/",jsonFile),
              to = paste0(data_dir_2,"/spatial/scalefactors_json.json"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  csvFile = grep("csv$",filenames,value=T)
  if(csvFile != "tissue_positions_list.csv"){
    file.copy(from = paste0(data_dir_2,"/spatial/",csvFile),
              to = paste0(data_dir_2,"/spatial/tissue_positions_list.csv"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  hires_imageFile = grep("hires_image.png$",filenames,value=T)
  if(length(hires_imageFile)==0){

  }else if(hires_imageFile != "tissue_hires_image.png"){
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/spatial/tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  if(length(hires_imageFile)==0){

  }else{
    file.copy(from = paste0(data_dir_2,"/spatial/",hires_imageFile),
              to = paste0(data_dir_2,"/",CancerName,".tissue_hires_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }

  lowres_imageFile = grep("lowres_image.png$",filenames,value=T)
  if(lowres_imageFile != "tissue_lowres_image.png"){
    file.copy(from = paste0(data_dir_2,"/spatial/",lowres_imageFile),
              to = paste0(data_dir_2,"/spatial/tissue_lowres_image.png"),
              overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
  }
  file.copy(from = paste0(data_dir_2,"/spatial/",lowres_imageFile),
            to = paste0(data_dir_2,"/",CancerName,".tissue_lowres_image.png"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  jpgFile = grep("detected_tissue_image.jpg$",filenames,value=T)
  file.copy(from = paste0(data_dir_2,"/spatial/",jpgFile),
            to = paste0(data_dir_2,"/",CancerName,".detected_tissue_image.jpg"),
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

  library(Seurat)
  library(rhdf5)
  library(ggplot2)
  filenames = list.files(path = data_dir_1, full.names = FALSE)


  library(data.table)
  library(limma)
  stRNA = fread(paste0(data_dir_1,"/raw_counts.csv"),check.names = F)
  stRNA = as.matrix(stRNA)
  rownames(stRNA) = stRNA[,1]
  stRNA = stRNA[,-1]
  class(stRNA) = "numeric"
  colnames(stRNA) = gsub(".","-",colnames(stRNA),fixed = TRUE)
  image2 = Read10X_Image(image.dir = file.path(data_dir_2,"spatial"),
                         filter.matrix = TRUE)
  stRNA = CreateSeuratObject(counts = stRNA, assay = "Spatial")
  image2 = image2[Cells(x = stRNA)]
  DefaultAssay(stRNA = image2) = "Spatial"
  stRNA[["slice1"]] = image2

  filenames = list.files(path = data_dir_2, full.names = FALSE)
  meta.data = grep("csv$",filenames,value=T)
  print(meta.data)
  spatial_location = read.csv(meta.data, header = T, row.names=1)
  samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
  countnumber = ncol(stRNA@meta.data)
  if(length(samesample) < length(rownames(stRNA@meta.data))){
    spatial_location[setdiff(rownames(stRNA@meta.data),samesample),] = 0
    samesample = intersect(rownames(stRNA@meta.data),rownames(spatial_location))
    spatial_location = spatial_location[samesample,]
  }else{
    spatial_location = spatial_location[samesample,]
  }
  stRNA@meta.data = cbind(stRNA@meta.data,spatial_location)
  #saveRDS(stRNA,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  spatial_location_2 = read.csv(meta.data, header = T, row.names=1)
  samesample_2 = intersect(rownames(stRNA@meta.data),rownames(spatial_location_2))
  stRNA2 = stRNA[,colnames(stRNA) %in% samesample_2]
  saveRDS(stRNA2,paste0(data_dir_2,"/",CancerName,".stRNA.RDS"))
  plot = SpatialFeaturePlot(stRNA2, keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = colnames(stRNA2@meta.data[(countnumber+3):ncol(stRNA2@meta.data)]))
  ggsave(paste0(data_dir_2,"/",CancerName,".SpatialDimPlot.pdf"), plot = plot , width = 16, height = 16)
  stRNA2 = NormalizeData(stRNA2)
  plot = SpatialFeaturePlot(stRNA2,
                            keep.scale = "all",
                            crop = F,
                            pt.size.factor = 1.2,
                            features = "CENPF",
                            slot = "data")
  ggsave(paste0(data_dir_2,"/CENPF.SpatialDimPlot.pdf"), plot = plot , width = 6, height = 6)
}
