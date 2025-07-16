library(fromto)
setwd("D:\\fromto")
gpl = read_fromto("GPL6246-18741.txt",row_names = FALSE)
#colnames(gpl) = gpl[1,]
#gpl = gpl[-1,]
gpl = gpl[,c(1,2,3)]
colnames(gpl)
Symbol = fromto(genes = gpl[,ncol(gpl)], from = "NCBI_GeneID", to = "Symbol")
gpl_new = merge_col_add(data1 = gpl,
                        data2 = Symbol,
                        data1_var = colnames(gpl)[ncol(gpl)],
                        data2_var_same_data1 = "NCBI_GeneID",
                        data2_var_add_data1 = "Symbol")
colnames(gpl_new)
#gpl_new = GPL17930
#gpl_new = gpl_new[!is.na(gpl_new$`ENTREZ_GENE_ID`),]
#gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new[,2],gpl_new$add_col)
gpl_new = gpl_new[,c(1,ncol(gpl_new))]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
gpl_new = gpl_new[which(nchar(gpl_new$`Gene Symbol`) < 20 ),]
gpl_new$`Gene Symbol` = strsplit_fromto(gpl_new$`Gene Symbol`,"///",1)
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/inst/GPL/GPL22166.RDS")
