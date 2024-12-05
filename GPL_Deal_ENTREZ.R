library(fromto)
gpl = read_fromto("/fromto/GPL25864.txt",row_names = FALSE)
rownames(gpl) = gpl[,2]
Symbol = fromto(genes = gpl$ENTREZ_GENE_ID, from = "NCBI_GeneID", to = "Symbol")
rownames(Symbol) = Symbol[,1]
gpl_new = merge_row(gpl,Symbol)
gpl_new = gpl_new[,c(1,5)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL25864.RDS")
