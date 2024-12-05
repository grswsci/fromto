library(fromto)
gpl = read_fromto("/fromto/data/GPL13607-20416.txt")
gpl = gpl[,c(1,5)]
colnames(gpl) = c("ID","Gene Symbol")
saveRDS(gpl,"/fromto/data/GPL13607.RDS")
