setwd("C:\\fromto\\data")
library(fromto)

files = list.files()
files = find_name("GPL" ,files)
files = gsub(".RDS","",files);files
#for (variable in files) {
  variable = "GPL97"
  gpl = readRDS(paste0("/fromto/data/",variable,".RDS"))
  gpl = gpl[which(gpl$`Gene Symbol` != "LOC110806262"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "-"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "3.8-1"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "1a"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "1B"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "2a"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "Repeat_FLAM"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "DC_BSDPD_REVCOMP"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "5865/sin/000009"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "5b"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "NR_001453"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "NR_001454"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "6b"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "6B"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "5-HT3c2"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "3xSLv1"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "DarkCorner"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "61E3.4"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "5-HT3C2"),]
  gpl = gpl[which(gpl$`Gene Symbol` != ""),]
  gpl = gpl[which(gpl$`Gene Symbol` != "---"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "15E1.2"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "A18395"),]
  gpl = gpl[which(gpl$`Gene Symbol` != "1-Nov"),]
  gpl = gpl[which(nchar(gpl$`Gene Symbol`) < 18 ),]
  gpl = gpl[which(nchar(gpl$`Gene Symbol`) >= 2 ),]
  gpl = gpl[which(is.na(as.numeric(gpl$`Gene Symbol`))),]
  gpl = gpl[which(is.na(as.numeric(gsub("AA","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("AB","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("THC","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("EST_AA","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("EST_AI","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("Random_","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("XM_","",gpl$`Gene Symbol`)))),]
  gpl = gpl[which(is.na(as.numeric(gsub("NM_","",gpl$`Gene Symbol`)))),]
  #gpl = gpl[-c(find_index("_",gpl$`Gene Symbol`)),]
  saveRDS(gpl,paste0("/fromto/data/",variable,".RDS"))
  paste0("/fromto/data/",variable,".RDS")
  length(unique(gpl$`Gene Symbol`))
#}




