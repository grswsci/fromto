devtools::install_github("grswsci/fromto")

library(fromto)

#Convert Gene Name

Symbol = fromto2(rownames(RNAseq),from = "NCBI_GeneID",to = "Symbol")

#Download the GEO dataset - Mirror version - Fast

geoget(GEO_IDs = GEO)

#Download the GEO dataset - NCBI version - Slow

geoget2(GEO_IDs = GEO)

