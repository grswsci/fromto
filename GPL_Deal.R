library(fromto)
gpl = read_fromto("/fromto/GPL9551-1207.txt",row_names = FALSE)
#colnames(gpl) = gpl[1,]
#gpl = gpl[-1,]
gpl = gpl[,c(1,18)]
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
gpl = gpl[which(nchar(gpl$`Gene Symbol`) < 20 ),]
gpl = gpl[which(nchar(gpl$`Gene Symbol`) >= 2 ),]
gpl = gpl[which(is.na(as.numeric(gpl$`Gene Symbol`))),]
gpl = gpl[-c(find_index("_",gpl$`Gene Symbol`)),]
saveRDS(gpl,"/fromto/data/GPL9551.RDS")



21192
ASHGV40030400
2-Sep
27123
ASHGV40040767
3-Mar
23247
ASHGV40033790
3-Sep
19875
ASHGV40028037
4-Mar
14977
ASHGV40020633
4-Sep
5228
ASHGV40005995
5-Mar
255
ASHGV40000252
5-Sep
27542
ASHGV40041468
6-Mar
34679
ASHGV40054360
6-Sep
20768
ASHGV40029696
7-Mar
36805
ASHGV40057311
7-Sep
33934
ASHGV40053096
8-Mar
27148
ASHGV40040807
8-Sep
8851
ASHGV40011566
9-Mar
16086
ASHGV40022273
9-Sep
