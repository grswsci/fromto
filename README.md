devtools::install_github("grswsci/fromto")

library(fromto)

library(tidyverse)

library(R.utils)



#Convert Gene Name

Symbol = fromto2(rownames(RNAseq),from = "NCBI_GeneID",to = "Symbol")<br>



#Remove the old compressed package under the current folder

list.files() %>% grepl("\\.txt\\.gz$", ., fixed = FALSE) %>% file.remove()<br>



#Download the GEO dataset - Mirror version - Fast

geoget(GEO_IDs = "GSE103668")<br>




#Download the GEO dataset - NCBI version - Slow

geoget2(GEO_IDs = "GSE103668")<br>



#Decompress the gz compression package

list.files() %>% grepl("\\.txt\\.gz$", ., fixed = FALSE) %>%  which() %>%  list.files()[.] %>% gunzip(., remove = FALSE, overwrite = TRUE)<br>



#The probe name was annotated as gene name

data = geoann(GPL_ID = "GPL570",GEO_ID = "GSE103668")<br>



#Update the old gene name to the new gene name.

data_new = fromtoupdate(data)<br>



#Organize clinical data

clinical = geocli(GEO)<br>



