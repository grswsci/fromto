#' @title sc_run_cellchat
#' @description Seurat Object
#' @param scRNA Seurat Object
#' @param gene_use gene symbol, default = NULL
#' @param cell_use cell name
#' @param label_use label in colnames(scRNA@meta.data)
#' @param is_v5 is v5 Seurat Object default = FALSE
#' @param height_1 default = 8
#' @param height_2 default = 8
#' @return cellchat Object
sc_run_cellchat = function(scRNA,
                           gene_use = NULL,
                           cell_use,
                           label_use = "CellType_New",
                           is_v5 = FALSE,
                           height_1 = 8,
                           height_2 = 8) {
  library(Seurat)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(Matrix)
  library(limma)
  library(NMF)
  library(ggplot2)
  library(ggalluvial)
  library(svglite)
  library(CellChat)

  if(is_v5 == TRUE){
    scRNA[["RNA"]] = as(scRNA[["RNA"]], "Assay")
  }
  colnames(scRNA@meta.data)[colnames(scRNA@meta.data) == label_use] = "labels"
  cellchat = createCellChat(object = scRNA@assays$RNA@data, meta = scRNA@meta.data, group.by = "labels")

  #V5需要运行下面的两行代码
  #rownames(cellchat@data) = rownames(scRNA@assays$RNA)
  #colnames(cellchat@data) = colnames(scRNA@assays$RNA)

  cellchat = setIdent(cellchat, ident.use="labels")
  groupSize = as.numeric(table(cellchat@idents))
  CellChatDB = CellChatDB.human#CellChatDB.mouse
  CellChatDB.use = subsetDB(CellChatDB, search = "Secreted Signaling")
  cellchat@DB = CellChatDB.use

  cellchat = subsetData(cellchat)
  cellchat = identifyOverExpressedGenes(cellchat)
  cellchat = identifyOverExpressedInteractions(cellchat)
  cellchat = projectData(cellchat, PPI.human)
  cellchat = computeCommunProb(cellchat)
  cellchat = filterCommunication(cellchat, min.cells = 10)

  df.net = subsetCommunication(cellchat)
  write.table(df.net,file="1.CellChat.Comm.network.xls",  sep="\t", row.names=F, quote=F)

  cellchat = computeCommunProbPathway(cellchat)
  cellchat = aggregateNet(cellchat)
  pdf("2.CellChat_cellNetwork_Count.pdf", width = 8, height = 7)
  netVisual_circle(cellchat@net$count, vertex.weight = groupSize, weight.scale = TRUE, label.edge= F, title.name = "Number of interactions")
  dev.off()

  pdf("3.CellChat_cellNetwork_Weight.pdf", width = 8, height = 7)
  netVisual_circle(cellchat@net$weight, vertex.weight = groupSize, weight.scale = TRUE, label.edge= F, title.name = "Interaction strength")
  dev.off()

  pdf("4.CellChat_singleCell_weight.pdf", width = 10, height = 6)
  weight_mat = cellchat@net$weight
  par(mfrow = c(2,4), mgp=c(0,0,0), xpd=TRUE)
  for (cel in unique(cellchat@idents)){
    cir_mat = matrix(0, nrow = nrow(weight_mat),
                      ncol = ncol(weight_mat),
                      dimnames = dimnames(weight_mat)
    )
    cir_mat[cel, ] = weight_mat[cel, ]
    netVisual_circle(cir_mat,
                     vertex.weight= groupSize,
                     weight.scale= T,
                     edge.weight.max = max(weight_mat),
                     vertex.label.cex = 0.8,
                     title.name=cel
    )
  }
  dev.off()

  pdf("5.CellChat_singleCell_count.pdf", width = 10, height = 6)
  weight_mat = cellchat@net$count
  par(mfrow = c(2,4), mgp = c(0,0,0), xpd=TRUE)
  for (cel in unique(cellchat@idents)){
    cir_mat = matrix(0,
                      nrow = nrow(weight_mat),
                      ncol = ncol(weight_mat),
                      dimnames = dimnames(weight_mat)
    )
    cir_mat[cel, ] = weight_mat[cel, ]
    netVisual_circle(cir_mat,
                     vertex.weight= groupSize,
                     weight.scale= T,
                     edge.weight.max = max(weight_mat),
                     vertex.label.cex=0.8,title.name=cel
    )
  }
  dev.off()

  #pdf("6.CellChat.bubble.pdf", width = 12, height = 12)
  #p = netVisual_bubble(cellchat, remove.isolate = FALSE, angle.x = 45)
  #print(p)
  #dev.off()

  cellchat = netAnalysis_computeCentrality(cellchat, slot.name = "netP")
  pdf("7.CellChat_outgoing-incoming-importance.pdf", width=6, height=5)
  gg1 = netAnalysis_signalingRole_scatter(cellchat)
  print(gg1)
  dev.off()

  pdf("8.CellChat.outgoing-incoming-pathway.pdf", width = 10, height = height_1)
  ht1 = netAnalysis_signalingRole_heatmap(cellchat,
                                           font.size = 6,
                                           height = height_1,
                                           color.heatmap = "Reds",
                                           pattern = "outgoing"
  )
  ht2 = netAnalysis_signalingRole_heatmap(cellchat,
                                           font.size = 6,
                                           height = height_1,
                                           color.heatmap = "BuGn",
                                           pattern = "incoming"
  )

  ht = ht1 + ht2
  print(ht)
  dev.off()
  if(!is.null(gene_use)){
  p1 = netVisual_bubble(cellchat,
                        sources.use = levels(cellchat@idents),
                        targets.use = paste0(gene_use,"+ ",cell_use),
                        remove.isolate = FALSE)

  p2 = netVisual_bubble(cellchat,
                        sources.use = levels(cellchat@idents),
                        targets.use = paste0(gene_use,"- ",cell_use), remove.isolate = FALSE)


  p3 = netVisual_bubble(cellchat,
                        sources.use = paste0(gene_use,"+ ",cell_use),
                        targets.use = levels(cellchat@idents),
                        remove.isolate = FALSE)

  p4 = netVisual_bubble(cellchat,
                        sources.use = paste0(gene_use,"- ",cell_use),
                        targets.use = levels(cellchat@idents),
                        remove.isolate = FALSE)

  p5 = p1 + p3
  pdf(paste0("9.CellChat_sources_targets_positive.pdf"),  width = 16, height = height_2)
  print(p5)
  dev.off()

  p6 = p2 + p4
  pdf(paste0("9.CellChat_sources_targets_negative.pdf"),  width = 16, height = height_2)
  print(p6)
  dev.off()
  }else if(is.null(gene_use)){
    p1 = netVisual_bubble(cellchat,
                          sources.use = levels(cellchat@idents),
                          targets.use = cell_use,
                          remove.isolate = FALSE)

    p3 = netVisual_bubble(cellchat,
                          sources.use = cell_use,
                          targets.use = levels(cellchat@idents),
                          remove.isolate = FALSE)

    p5 = p1 + p3
    pdf(paste0("9.CellChat_sources_targets.pdf"),  width = 16, height = height_2)
    print(p5)
    dev.off()
  }

  for(i in cellchat@netP$pathways){
    pathways.show = i
    pdf(paste0("10.CellChat_", pathways.show,"_circle.pdf"), width=8, height=6)
    circle = netVisual_aggregate(cellchat, signaling=pathways.show, layout="circle")
    print(circle)
    dev.off()
    pdf(paste0("10.CellChat_", pathways.show, "_hierarchy.pdf"), width = 12, height = 6)
    hierarchy = netVisual_aggregate(cellchat,
                                  signaling = pathways.show,
                                  layout = "hierarchy",
                                  vertex.receiver=c(1,2,3,4))
    print(hierarchy)
    dev.off()

    pdf(paste0("10.CellChat_", pathways.show, "_heatmap.pdf"), width=8, height=6)
    heatmap = netVisual_heatmap(cellchat, signaling=pathways.show, color.heatmap = "Reds", measure= 'weight')
    print(heatmap)
    dev.off()

    pdf(paste0("10.CellChat_", pathways.show,"_netAnalysis.pdf"), width=6, height=5)
    cellchat = netAnalysis_computeCentrality(cellchat, slot.name = "netP")
    netAnalysis=netAnalysis_signalingRole_network(cellchat, signaling = pathways.show, width = 8, height = 5, color.heatmap = "Reds", font.size = 12)
    print(netAnalysis)
    dev.off()

    #pdf(paste0("10.CellChat.", pathways.show,".contribution.pdf"), width=8, height=2)
    #contribution = netAnalysis_contribution(cellchat, signaling= pathways.show)
    #print(contribution)
    #dev.off()
    #pdf(paste0("10.CellChat.", pathways.show, ".geneExp.pdf"), width=8, height=6)
    #geneExp = plotGeneExpression(cellchat, signaling=pathways.show)
    #print(geneExp)
    #dev.off()
    pairLR = extractEnrichedLR(cellchat, signaling=pathways.show, geneLR.return=FALSE)
    #pdf(paste0("10.CellChat.", pathways.show, ".pairLR.pdf"), width=9, height=8)
    #pairCircos=netVisual_individual(cellchat, signaling=pathways.show, pairLR.use=pairLR[1] , layout="circle" )
    #print(pairCircos)
    #dev.off()

    for(i in 1:nrow(pairLR)){
      pdf(paste0("10.CellChat_", pathways.show,"_", pairLR[i,], "_pairLR.pdf"), width = 8, height = 6)
      pairChord = netVisual_individual(cellchat, signaling = pathways.show, pairLR.use = pairLR[i,], layout = "chord")
      print(pairChord)
      dev.off()
    }
  }

  return(cellchat)
}
