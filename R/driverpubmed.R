#' @title driverpubmed
#' @description publications find
#' @param term you want to search what
#' @param term_AND_1 you want to search what else and
#' @param year_start you know bro
#' @param year_end you know bro
#' @return html and text
#' @examples
#' # examples
#' driverpubmed(term = "Melanoma",
#'              term_AND_1 = "TCGA",
#'              term_AND_2 = "Signature",
#'              term_AND_3 = NULL,
#'              term_AND_4 = NULL,
#'              year_start = "2024",
#'              year_end = "2024")
driverpubmed <- function(term = "Melanoma",
                         term_AND_1 = "TCGA",
                         term_AND_2 = "Signature",
                         term_AND_3 = NULL,
                         term_AND_4 = NULL,
                         term_AND_5 = NULL,
                         term_AND_6 = NULL,
                         term_AND_7 = NULL,
                         term_AND_8 = NULL,
                         term_AND_9 = NULL,
                         year_start = "2021",
                         year_end = "2024"){
createLink <- function(base,val) {
    sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val)
}
createLink_doi <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',paste0("https://",base),val)
}
year_start = as.numeric(year_start)
year_end = as.numeric(year_end)
year = year_start:year_end
year = sapply(year,function(x) paste0(x,"[PDAT]"))
year = paste(year, collapse = " OR ")
year = paste0(" AND (",year,")")
term = paste0("(",term,")")
term = paste0("(",term, year,")")

if(!is.null(term_AND_1)){
  term_AND_1 = paste0("(",term_AND_1,")")
  term = paste0(term ," AND ", term_AND_1)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_2)){
  term_AND_2 = paste0("(",term_AND_2,")")
  term = paste0(term ," AND ", term_AND_2)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_3)){
  term_AND_3 = paste0("(",term_AND_3,")")
  term = paste0(term ," AND ", term_AND_3)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_4)){
  term_AND_4 = paste0("(",term_AND_4,")")
  term = paste0(term ," AND ", term_AND_4)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_5)){
  term_AND_5 = paste0("(",term_AND_5,")")
  term = paste0(term ," AND ", term_AND_5)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_6)){
  term_AND_6 = paste0("(",term_AND_6,")")
  term = paste0(term ," AND ", term_AND_6)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_7)){
  term_AND_7 = paste0("(",term_AND_7,")")
  term = paste0(term ," AND ", term_AND_7)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_8)){
  term_AND_8 = paste0("(",term_AND_8,")")
  term = paste0(term ," AND ", term_AND_8)
  term = paste0("(",term,")")
}

if(!is.null(term_AND_9)){
  term_AND_9 = paste0("(",term_AND_9,")")
  term = paste0(term ," AND ", term_AND_9)
  term = paste0("(",term,")")
}
suppressPackageStartupMessages(library(rentrez))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(htmlwidgets))

Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE)
data_common_words_new= system.file("data", "common_words_new.RDS", package = "fromto")
common_words_new = readRDS(data_common_words_new)
data_HGNCdata= system.file("data", "HGNCdata.RDS", package = "fromto")
HGNCdata = readRDS(data_common_words_new)

pre_result = entrez_search(db = "pubmed", term = term, retmode = "xml", retmax = 0)
result = entrez_search(db="pubmed", term = term, retmode = "xml", retmax = pre_result$count)
n = 100
res = c()
for (i in seq(1,length(result$ids),n)) {
  i = 1
  multi_summ = entrez_summary(db = "pubmed",
                              id = result$ids[i:(i+n-1)]
                              )
  date_and_cite = extract_from_esummary(multi_summ,
                                        c("uid",
                                          "pubdate",
                                          "sortfirstauthor",
                                          "lastauthor",
                                          "title",
                                          "fulljournalname",
                                          "elocationid"
                                          )
                                        )
  pre_res = data.frame(t(date_and_cite))
  pre_res = data.frame(lapply(pre_res, as.character), stringsAsFactors=FALSE)
  res = rbind(res,pre_res)
}


uid = res$uid
url = paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=",uid)
pubdate = res$pubdate
sortfirstauthor = res$sortfirstauthor
lastauthor =res$lastauthor
title = res$title
journal = res$fulljournalname
elocationid = res$elocationid
elocationid = gsub("doi: ","",elocationid)

abstract_text_all = c()
for (variable in url) {
  message(paste0("we are getting ",variable," abstact..."))
  webpage = read_html(variable)
  abstract_div = webpage %>%
    html_nodes("#abstract")
  abstract_text = abstract_div %>%
    html_text(trim = TRUE)
  abstract_text_all = c(abstract_text_all,abstract_text)
}

res = data.frame(uid = createLink(paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=",uid),uid),
                 pubdate = pubdate,
                 firstauthor = sortfirstauthor,
                 lastauthor = lastauthor,
                 title = createLink(paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=",uid),title),
                 abstract = abstract_text_all,
                 journal = journal,
                 DataBase = createLink(paste0("https://grswsci.top/"),"光热生物数据库\n微信：bioinformaticsboy"),
                 stringsAsFactors = F)
res = na.omit(res)
y = DT::datatable(res,escape = F,rownames=F)
DT::saveWidget(y,paste0("output_paper.html"),selfcontained = TRUE)
write.table(res[,c("pubdate","title","abstract")],paste0("output_paper.txt"))
}

