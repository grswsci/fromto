library(fromto)
gpl = read_fromto("/fromto/GPL10381.txt",row_names = FALSE)
#colnames(gpl) = gpl[1,]
#gpl = gpl[-1,]
gpl = gpl[,c(1,3)]
colnames(gpl) = c("ID","Gene Symbol")
#gpl = GPL6254
gpl = gpl[which(gpl$`Gene Symbol` != "LOC110806262"),]
gpl = gpl[which(gpl$`Gene Symbol` != "-"),]
gpl = gpl[which(gpl$`Gene Symbol` != "1a"),]
gpl = gpl[which(gpl$`Gene Symbol` != "1B"),]
gpl = gpl[which(gpl$`Gene Symbol` != "2a"),]
gpl = gpl[which(gpl$`Gene Symbol` != "5865/sin/000009"),]
gpl = gpl[which(gpl$`Gene Symbol` != "5b"),]
gpl = gpl[which(gpl$`Gene Symbol` != "6b"),]
gpl = gpl[which(gpl$`Gene Symbol` != "6B"),]
gpl = gpl[which(gpl$`Gene Symbol` != "5-HT3c2"),]

gpl = gpl[which(gpl$`Gene Symbol` != ""),]
gpl = gpl[which(gpl$`Gene Symbol` != "---"),]
gpl = gpl[which(nchar(gpl$`Gene Symbol`) < 20 ),]
gpl = gpl[which(nchar(gpl$`Gene Symbol`) >= 2 ),]
gpl = gpl[which(is.na(as.numeric(gpl$`Gene Symbol`))),]
gpl = gpl[-c(find_index("_",gpl$`Gene Symbol`)),]
gpl$`Gene Symbol2` = strsplit_fromto(gpl$`Gene Symbol`,"|",1)
unique(gpl$`Gene Symbol`)
saveRDS(gpl,"/fromto/data/GPL10381.RDS")

