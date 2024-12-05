library(fromto)
gpl = read_fromto("/fromto/GPL13607-20416.txt",row_names = FALSE)
gpl = gpl[,c(1,6)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
saveRDS(gpl,"/fromto/data/GPL13607.RDS")
