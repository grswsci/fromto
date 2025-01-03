#' @title geoget
#' @description Download the GEO dataset from a Mirror
#' @param GEO_ID GEO ID
#' @return txt.gz
#' @examples
#' # examples
#' geoget(GEO_IDs = "GSE103668")

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


#' List files from a URL
#'
#' Extracts and lists files from a specified URL that match a certain pattern.
#'
#' @param url Character. The URL to fetch files from.
#'
#' @return A character vector containing file names matching the pattern.
#'
#' @importFrom xml2 read_html xml_text xml_find_all
#'
#' @examples
#' listURL(gse2url('GSE129516', series = 'matrix'))
#'
#' @keywords internal
listURL = function(url) {

  contents = xml2::read_html(url)
  files = grep("^G", xml2::xml_text(xml2::xml_find_all(contents, "//a/@href")), value = TRUE)

  return(files)

}

#' Generate GEO Series URL
#'
#' Constructs the URL for accessing GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to access ('matrix' or 'suppl').
#'
#' @return A character string containing the constructed URL.
#'
#' @examples
#' gse2url('GSE129516', series = 'matrix')
#'
#' @keywords internal
gse2url = function(gse_id, series = 'matrix') {

  gse_uid = gsub("\\d{1,3}$", "nnn", gse_id, perl = TRUE)

  if (series == 'matrix') {
    url_base = 'https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/'
    url_spec = sprintf(url_base, gse_uid, gse_id)
  } else {
    url_base = 'https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/suppl/'
    url_spec = sprintf(url_base, gse_uid, gse_id)
  }

  return(url_spec)

}

#' List GEO Files
#'
#' Lists GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to access ('matrix', 'suppl', or 'supplementary').
#' @param full Logic. Whether to return full URLs.
#'
#' @return A character vector of file names or full URLs.
#'
#' @examples
#' listGEO('GSE129516', series = 'matrix', full = TRUE)
#'
#' @export
listGEO = function(gse_id, series = 'matrix', full = T) {

  series = tolower(series)
  match.arg(series, c('matrix', 'suppl', 'supplementary'))
  if (series == 'supplementary') series = 'suppl'

  url_gse = gse2url(gse_id, series)
  files = listURL(url_gse)

  if (full) files = paste0(url_gse, files)

  return(files)

}

#' Download GEO Files
#'
#' Downloads GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to download ('matrix' or 'suppl').
#'
#' @return No return value. Files are downloaded to the current working directory.
#'
#' @examples
#' downloadGSE('GSE129516', series = 'matrix')
#'
#' @export
downloadGSE = function(gse_id, series = 'matrix') {

  files = listGEO(gse_id, series)

  for (file in files) {

    status = tryCatch({
      each = download.file(
        file,
        destfile = basename(file),
        mode = 'wb',
        method = 'auto'
      )
      each == 0},
      error = function(e) return(F),
      warning = function(w) return(F)
    )

    if (!status) stop(sprintf('Failed to download %s!', file))

  }

  invisible()

}

#' @title geoget2
#' @description Download the GEO dataset from a NCBI
#' @param GEO_ID GEO ID
#' @return txt.gz
#' @examples
#' # examples
#' geoget(GEO_IDs = "GSE103668")
geoget2 = function(GEO_IDs) {

  options(timeout=1000000)
  for (GEO_ID in GEO_IDs) downloadGSE(GEO_ID)

}
