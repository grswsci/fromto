#devtools::install_github("grswsci/fromto")
library(fromto)
gpl = read_fromto("/fromto/GPL6254.txt",row_names = FALSE)
gpl = gpl[,c(1,6,3)]
gpl$GeneName = strsplit_fromto(gpl$gene_assignment," // ",2)
gpl_new = gpl[,c(1,3)]
#gpl$hgnc_id = stringr::str_extract(gpl$SPOT_ID, "(?<=HGNC:)[0-9]+")
colnames(gpl)
Symbol = fromto(genes = gpl$ENSEMBL_GENE_ID, from = "Ensembl_GeneID", to = "Symbol")

gpl_new = merge_col_add(data1 = gpl,
                    data2 = Symbol,
                    data1_var = "ENSEMBL_GENE_ID",
                    data2_var_same_data1 = "Ensembl_GeneID",
                    data2_var_add_data1 = "Symbol")
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new$GENE_SYMBOL,gpl_new$add_col)
gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "-"),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL6254.RDS")
