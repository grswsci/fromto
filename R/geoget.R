geoget <- function(GEO_IDs){
  options(timeout=1000000)
  for (GEO_ID in GEO_IDs){
    if(nchar(GEO_ID) == 8){
      url = paste0("http://218.108.182.182:9000/pubmed/DownFile/GEO?fileName=/geo/series/",substr(GEO_ID,1,5),"nnn/",GEO_ID,"/matrix/",GEO_ID,"_series_matrix.txt.gz")
      destfile <- paste0(GEO_ID,"_series_matrix.txt.gz")
      download.file(url, destfile, mode = "wb")
    }else if(nchar(GEO_ID) == 9){
      url = paste0("http://218.108.182.182:9000/pubmed/DownFile/GEO?fileName=/geo/series/",substr(GEO_ID,1,6),"nnn/",GEO_ID,"/matrix/",GEO_ID,"_series_matrix.txt.gz")
      destfile <- paste0(GEO_ID,"_series_matrix.txt.gz")
      download.file(url, destfile, mode = "wb")
    }else if(nchar(GEO_ID) == 7){
      url = paste0("http://218.108.182.182:9000/pubmed/DownFile/GEO?fileName=/geo/series/",substr(GEO_ID,1,4),"nnn/",GEO_ID,"/matrix/",GEO_ID,"_series_matrix.txt.gz")
      destfile <- paste0(GEO_ID,"_series_matrix.txt.gz")
      download.file(url, destfile, mode = "wb")
    }
  }
}
