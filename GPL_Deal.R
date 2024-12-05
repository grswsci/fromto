library(fromto)
gpl = read_fromto("/fromto/refGene.txt",row_names = FALSE)
gpl = gpl[,c(1,4)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
gpl = gpl[which(gpl$`Gene Symbol` != "-"),]
saveRDS(gpl,"/fromto/data/GPL25759.RDS")
