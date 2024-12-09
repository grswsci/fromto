library(fromto)
gpl = read_fromto("/fromto/GPL21970.txt",row_names = FALSE)
gpl = gpl[,c(1,2,4)]
colnames(gpl)
Symbol = fromto(genes = gpl[,3], from = "NCBI_GeneID", to = "Symbol")
gpl_new = merge_col_add(data1 = gpl,
                        data2 = Symbol,
                        data1_var = colnames(gpl)[3],
                        data2_var_same_data1 = "NCBI_GeneID",
                        data2_var_add_data1 = "Symbol")
colnames(gpl_new)
#gpl_new = gpl_new[!is.na(gpl_new$`ENTREZ_GENE_ID`),]
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new$`GeneSymbol`,gpl_new$add_col)
gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
gpl_new = gpl_new[which(nchar(gpl_new$`Gene Symbol`) < 20 ),]
gpl_new$`Gene Symbol2` = strsplit_fromto(gpl_new$`Gene Symbol`,"///",1)
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL21970.RDS")
