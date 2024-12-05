#devtools::install_github("grswsci/fromto")
library(fromto)
gpl = read_fromto("/fromto/GPL23159-184565.txt",row_names = FALSE)
gpl = gpl[,c(1,10)]
gpl$hgnc_id = stringr::str_extract(gpl$SPOT_ID, "(?<=HGNC:)[0-9]+")
Symbol = fromto(genes = gpl$hgnc_id, from = "HGNC_GeneID", to = "Symbol")

gpl_new = merge_col_add(data1 = gpl,
                    data2 = Symbol,
                    data1_var = "hgnc_id",
                    data2_var_same_data1 = "HGNC_GeneID",
                    data2_var_add_data1 = "Symbol")

gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL23159.RDS")
