library(fromto)
gpl = read_fromto("/fromto/GPL6102-11574.txt",row_names = FALSE)
gpl = gpl[,c(1,13,10)]
colnames(gpl)
Symbol = fromto(genes = gpl$`Entrez_Gene_ID`, from = "NCBI_GeneID", to = "Symbol")
gpl_new = merge_col_add(data1 = gpl,
                        data2 = Symbol,
                        data1_var = "Entrez_Gene_ID",
                        data2_var_same_data1 = "NCBI_GeneID",
                        data2_var_add_data1 = "Symbol")
colnames(gpl_new)
gpl_new = gpl_new[!is.na(gpl_new$`Entrez_Gene_ID`),]
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new$`Symbol`,gpl_new$add_col)
gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL6102.RDS")
