library(fromto)
gpl = read_fromto("/fromto/GPL22120-25936.txt",row_names = FALSE)
gpl = gpl[,c(1,10)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
gpl = gpl[which(gpl$`Gene Symbol` != "-"),]
saveRDS(gpl,"/fromto/data/GPL22120.RDS")
