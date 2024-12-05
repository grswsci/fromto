library(fromto)
gpl = read_fromto("/fromto/GPL15207-17536.txt",row_names = FALSE)
gpl = gpl[,c(1,17)]
colnames(gpl) = c("ID","Gene Symbol")
gpl = gpl[which(gpl$`Gene Symbol` != ""),]
saveRDS(gpl,"/fromto/data/GPL15207.RDS")
