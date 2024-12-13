library(fromto)
GB_ACC = readRDS("/fromto/data/UCSC_GB_ACC_refGene.txt.gz.RDS")
GB_ACC = GB_ACC[,c(2,13)]
colnames(GB_ACC) = c("GB_ACC","Gene Symbol")
GB_ACC = GB_ACC[!duplicated(GB_ACC$GB_ACC),]
gpl = read_fromto("/fromto/GPL5981.txt",row_names = FALSE)
gpl = gpl[,c(1,3,6)]
gpl = gpl[which(gpl[,2] != ""),]
#gpl_new = gpl
#gpl$GB_ACC_use = strsplit_fromto(gpl[,2],".",1)
colnames(gpl)
gpl_new = merge_col_add(data1 = gpl,
                        data2 = GB_ACC,
                        data1_var = colnames(gpl)[ncol(gpl)],
                        data2_var_same_data1 = "GB_ACC",
                        data2_var_add_data1 = "Gene Symbol")
#gpl_new$add_col2 = extract_parentheses(gpl_new$Descr)
colnames(gpl_new)
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new[,2],gpl_new$add_col)
#gpl_new = as.data.frame(gpl_new)
#gpl_new = gpl_new[nchar(gpl_new$add_col)<20,]

#gene_with_parentheses = regmatches(str, match_position) # 获取匹配的内容
#gene_name = gsub("\$|\$", "", gene_with_parentheses)
gpl_new$add_col[gpl_new$add_col == "8-Mar"] = "MARCHF8"
gpl_new = gpl_new[,c(1,ncol(gpl_new))]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "13-Sep"),]
gpl_new = gpl_new[which(is.na(as.numeric(gpl_new$`Gene Symbol`))),]
gpl_new$`Gene Symbol` = strsplit_fromto(gpl_new$`Gene Symbol`,"|",1)
saveRDS(gpl_new,"/fromto/inst/GPL/GPL5981.RDS")
