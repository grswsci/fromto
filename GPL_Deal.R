library(fromto)
gpl = read_fromto("/fromto/GPL20844-88004.txt",row_names = FALSE)
gpl = gpl[,c(1,10)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
gpl = gpl[which(gpl$`Gene Symbol` != "-"),]
saveRDS(gpl,"/fromto/data/GPL20844.RDS")
