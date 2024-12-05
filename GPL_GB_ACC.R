library(fromto)
GB_ACC = readRDS("/fromto/data/UCSC_GB_ACC_refGene.txt.gz.RDS")
GB_ACC = GB_ACC[,c(2,13)]
colnames(GB_ACC) = c("GB_ACC","Gene Symbol")
GB_ACC = GB_ACC[!duplicated(GB_ACC$GB_ACC),]
library(fromto)
gpl = read_fromto("/fromto/GPL2700.txt",row_names = FALSE)
gpl = gpl[,c(1,3)]
gpl = gpl[which(gpl[,2] != ""),]
gpl$GB_ACC_use = strsplit_fromto(gpl[,2],".",1)

gpl_new = merge_col_add(data1 = gpl,
                        data2 = GB_ACC,
                        data1_var = "GB_ACC_use",
                        data2_var_same_data1 = "GB_ACC",
                        data2_var_add_data1 = "Gene Symbol")
gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "-"),]
saveRDS(gpl_new,"/fromto/data/GPL2700.RDS")
