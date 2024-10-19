#' @title cplot1
#' @description expression correlation analysis
#' @param GeneName1 GeneName1
#' @param GeneName2 GeneName2
#' @param DatasetName DatasetName
#' @param method pearson or spearman
#' @return pdf plot
cplot1 = function(data,
                  GeneName1,
                  GeneName2,
                  DatasetName,
                  method = 'pearson'){
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(ggpubr))
  suppressPackageStartupMessages(library(ggExtra))
  suppressPackageStartupMessages(library(limma))
  data$shape = ifelse(data[,GeneName1] > median(data[,GeneName1]) & data[,GeneName2]>median(data[,GeneName2]),"Mixed",
                      ifelse(data[,GeneName1] > median(data[,GeneName1]) & data[,GeneName2]<= median(data[,GeneName2]),GeneName1,
                             ifelse(data[,GeneName1] <= median(data[,GeneName1]) & data[,GeneName2]>median(data[,GeneName2]),GeneName2,
                                    "Quiescent")
                      )
  )
  write.csv(data,paste0(GeneName1,"_",GeneName2,"_",DatasetName,"_plotdata_Cor2genes.csv"),row.names=T)
  data$shape = factor(data$shape, levels=levels(factor(data$shape)))
  shape = levels(factor(data$shape))
  length = length(levels(factor(data$shape)))
  bioCol = c("#BC3C29FF","#0072B5FF","#7876B1FF","#6F99ADFF")
  p1 = ggplot(data, aes(data[,GeneName1], data[,GeneName2])) +
    xlab(GeneName1) +
    ylab(GeneName2) +
    geom_point(aes(colour=shape,shape=shape))+
    scale_color_manual(values=bioCol[1:length])+
    scale_shape_manual(values=c(2,8,12,18))+
    geom_smooth(method="lm",formula = y ~ x) +
    theme_bw()+
    stat_cor(method = method, aes(x =data[,GeneName1], y =data[,GeneName2]))+
    theme(legend.position = "none")#bottom
  p2 = ggMarginal(p1, type="densigram", xparams=list(fill = alpha("#E18727FF",0.5)), yparams=list(fill = alpha("#20854EFF",0.5)))
  print(p2)
  p2
  ggsave(paste0(GeneName1,"_",GeneName2,"_",DatasetName,"_Scatter_diagram.pdf"), p2,width = 5.5, height = 5.5)
}
#' @title cplot2
#' @description expression correlation analysis
#' @param GeneName1 GeneName1
#' @param GeneName2 GeneName2
#' @param DatasetName DatasetName
#' @param method pearson or spearman
#' @return pdf plot
cplot2 = function(data,
                  GeneName1,
                  GeneName2,
                  DatasetName,
                  method = 'pearson'){
  r.cor = cor(data[,GeneName1], data[,GeneName2],method = method)
  r.cor
  p.cor = cor.test(data[,GeneName1], data[,GeneName2],method = method)$p.value
  p.cor
  data$GeneName1_classify = factor(cut(data[,GeneName1], quantile(data[,GeneName1]),
                                          labels = c("Negative", "Weak", "Moderate", "Positive")),
                                      levels = c("Positive", "Moderate", "Weak", "Negative"))
  data$GeneName2_classify = factor(cut(data[,GeneName2], quantile(data[,GeneName2]),
                                          labels = c("Negative", "Weak", "Moderate", "Positive")),
                                      levels = c("Negative", "Weak", "Moderate", "Positive"))
  tab_classify = as.data.frame.array(table(data$GeneName1_classify,
                                           data$GeneName2_classify)
                                     )

  p.fisher = fisher.test(tab_classify, workspace = 3e9,simulate.p.value=TRUE)$p.value

  blue   = "#204F8D"
  lblue  = "#498EB9"
  dwhite = "#B6D1E8"
  white  = "#E6EAF7"

  pdf(paste0(GeneName1,"_",GeneName2,"_",DatasetName,"_Fisher’s_exact_test.pdf"),width = 6,height = 6)
  par(bty="n", mgp = c(2,0.5,0), mar = c(5.1,6.1,4.1,2.1),tcl=-.25, font.main=3)
  par(xpd=NA)

  plot(c(0,ncol(tab_classify)),
       c(0,nrow(tab_classify)),
       col = "white",
       xlab = "",xaxt = "n",
       ylab = "",yaxt = "n")

  title(paste("Correlation between ", GeneName1, "and ", GeneName2,
              "\nrho = ", round(r.cor,2),
              "; ",
              "P.cor = ", format(p.cor, digits = 3, scientific = T),
              "\nP.fisher = ", format(p.fisher, digits = 3, scientific = T)),
        adj = 0,
        line = 0)
  axis(2, at = 0.5:(ncol(tab_classify)-0.5), labels = FALSE)
  text(y = 0.5:(ncol(tab_classify)-0.5),
       par("usr")[1],
       labels = rownames(tab_classify)[nrow(tab_classify):1],
       srt = 0, pos = 2, xpd = TRUE)
  mtext(GeneName2, side=2, line = 4.5)
  axis(1, at = 0.5:(ncol(tab_classify)-0.5), labels = FALSE)
  text(x = 0.5:(ncol(tab_classify)-0.5),
       par("usr")[1] - 0.2,
       labels = colnames(tab_classify),
       srt = 45, pos = 1, xpd = TRUE)
  mtext(GeneName1, side=1, line = 3.5)

  input_matrix = as.matrix(tab_classify)
  mat.max = max(input_matrix)
  unq.value = unique(sort(as.vector(input_matrix)))
  rbPal = colorRampPalette(c(white,dwhite,lblue,blue))
  col.vec = rbPal(max(unq.value) + 1)
  col.mat = matrix(NA,byrow = T,ncol = ncol(input_matrix),nrow = nrow(input_matrix))

  for (i in 1:nrow(col.mat)) {
    for (j in 1:ncol(col.mat)) {
      col.mat[i,j] <- col.vec[input_matrix[i,j] + 1]
    }
  }

  x_size = ncol(input_matrix)
  y_size = nrow(input_matrix)
  my_xleft = rep(c(0:(x_size-1)),each = x_size)
  my_xright = my_xleft + 1
  my_ybottom = rep(c((y_size-1):0),y_size)
  my_ytop = my_ybottom + 1
  rect(xleft = my_xleft,
       ybottom = my_ybottom,
       xright = my_xright,
       ytop = my_ytop,
       col=col.mat,
       border = F)
  text(my_xleft + 0.5,my_ybottom + 0.5,input_matrix, cex = 1.3)
  invisible(dev.off())
}

