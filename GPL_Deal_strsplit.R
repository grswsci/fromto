library(fromto)
gpl = read_fromto("/fromto/GPL23159-184565.txt",row_names = FALSE)
gpl = gpl[,c(1,10)]
gpl$hgnc_id = stringr::str_extract(gpl$SPOT_ID, "(?<=HGNC:)[0-9]+")
Symbol = fromto(genes = gpl$hgnc_id, from = "HGNC_GeneID", to = "Symbol")
rownames(Symbol) = Symbol[,1]
add_line = c()
for (variable in gpl$hgnc_id) {
  if(variable %in% Symbol$HGNC_GeneID){
    Symbol_subset = Symbol[which(Symbol$HGNC_GeneID == variable),]
    add_line = c(add_line,Symbol_subset$Symbol)
  }else{
    add_line = c(add_line,NA)
  }
}
gpl_new = cbind(gpl,add_line)
gpl_new = gpl_new[,c(1,4)]
colnames(gpl_new) = c("ID","Gene Symbol")
gpl_new = gpl_new[which(gpl_new$`Gene Symbol` != ""),]
rownames(gpl_new) = NULL
saveRDS(gpl_new,"/fromto/data/GPL23159.RDS")
