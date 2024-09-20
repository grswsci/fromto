.onLoad <- function(libname, pkgname){
  msg <- paste0(pkgname, " v", "0.1.0", "\n\n","For help: https://grswsci.top/","\n\n")
  citation <- paste0("If you use ", pkgname, " in published research, please thank:\n",
                      "Yuyao Liu, Bioinformatics R&D Department, Hefei GuangRe Biotechnology Co., Ltd, Hefei, China","\n\n",
                      "You can freely convert the following GeneIDs!","\n",
                      "Symbol","\n","NCBI_GeneID""\n","HGNC_GeneID","\n","SwissProt_GeneID","\n",
                      "Ensembl_GeneID","\n","OMIM_GeneID","\n",
                      "Proteins_GeneID","\n","Description_GeneID","\n",
                      "Transcripts_GeneID","\n","Gene_Group_Identifier_GeneID""\n",
                      "Synonyms_GeneID")
  packageStartupMessage(paste0(msg, citation))
}

