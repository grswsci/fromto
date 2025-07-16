#devtools::install_github("grswsci/fromto")
data = readRDS("D:\\fromto\\inst\\GPL/GPL22166.RDS")
library(fromto)
gpl = read_fromto("GPL6246-18741.txt",row_names = FALSE)
gpl = gpl[,c(1,10)]
gpl$GeneName = strsplit_fromto(gpl[,2]," // ",2)
gpl = gpl[which(!is.na(gpl$GeneName)),]
#gpl$hgnc_id = stringr::str_extract(gpl[,2], "(?<=HGNC:)[0-9]+")
gpl$Ensembl_GeneID = stringr::str_extract(gpl[,2], "(?<=ENSG)[0-9]+")
gpl$Ensembl_GeneID = stringr::str_extract(gpl[,2], "(?<=ENSMU)[0-9]+")
gpl$Ensembl_GeneID = paste0("ENSG",gpl$Ensembl_GeneID)
colnames(gpl)

gpl_new = gpl[,c(1,3)]#gpl = gpl_new[,c(1,3)]

Symbol = fromto(genes = gpl[,4], from = "Ensembl_GeneID", to = "Symbol")

gpl_new = merge_col_add(data1 = gpl,
                    data2 = Symbol,
                    data1_var = "Ensembl_GeneID",
                    data2_var_same_data1 = "Ensembl_GeneID",
                    data2_var_add_data1 = "Symbol")
gpl_new$add_col = ifelse(is.na(gpl_new$add_col),gpl_new$GENE_SYMBOL,gpl_new$add_col)
gpl_new = gpl_new[,c(1,ncol(gpl_new))]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "-"),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "---"),]
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != "UCSC Genes"),]
gpl_new$`Gene Symbol` = strsplit_fromto(gpl_new$`Gene Symbol`,"///",1)
rownames(gpl_new) = NULL
saveRDS(gpl_new,"D:\\fromto\\inst\\GPL/GPL6246.RDS")
