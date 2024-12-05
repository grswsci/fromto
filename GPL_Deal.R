library(fromto)
gpl = read_fromto("/fromto/GPL7191.txt",row_names = FALSE)
gpl = gpl[,c(1,19)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
saveRDS(gpl,"/fromto/data/GPL7191.RDS")
