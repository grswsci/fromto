#devtools::install_github("grswsci/fromto")
library(fromto)
gpl = read_fromto("/fromto/GPL23126.txt",row_names = FALSE)
gpl = gpl[,c(1,9)]
gpl$GeneName = strsplit_fromto(gpl[,2]," // ",2)
#gpl$hgnc_id = stringr::str_extract(gpl[,2], "(?<=HGNC:)[0-9]+")
colnames(gpl)

gpl_new = gpl[,c(1,3)]

Symbol = fromto(genes = gpl[,4], from = "HGNC_GeneID", to = "Symbol")

gpl_new = merge_col_add(data1 = gpl,
                    data2 = Symbol,
                    data1_var = "hgnc_id",
                    data2_var_same_data1 = "HGNC_GeneID",
                    data2_var_add_data1 = "Symbol")
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new$GENE_SYMBOL,gpl_new$add_col)
gpl_new = gpl_new[,c(1,ncol(gpl_new))]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "-"),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "UCSC Genes"),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL23126.RDS")
