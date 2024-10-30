#' @title driverpubmed
#' @description publications find
#' @param ... search terms
#' @param year_start you know bro
#' @param year_end you know bro
#' @return html and text
#' @examples
#' # examples
#' driverpubmed('Melanoma', 'TCGA', 'Signature')
#' driverpubmed_example() # or
driverpubmed <- function(...,
                         year_start = "2021",
                         year_end = "2024"){
createLink <- function(base,val) {
    sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',base,val)
}
createLink_doi <- function(base,val) {
  sprintf('<a href="%s" class="btn btn-link" target="_blank" >%s</a>',paste0("https://",base),val)
}

is_string = \(vec) is.character(vec) & length(vec) == 1

year_start = as.numeric(year_start)
year_end = as.numeric(year_end)
year = year_start:year_end
year = sapply(year,function(x) paste0(x,"[PDAT]"))
year = paste(year, collapse = " OR ")
year = paste0(" AND (",year,")")

# 使用不定参数传递检索词
# 检查检索词类型后拼接检索词
# 如果检索词类型不为单长度字符串, 报错检索词长度信息
# 确保检索词格式与原先一致
terms = list(...)
if (all(unlist(lapply(terms, is_string)))) {
  terms = sapply(terms, \(str) paste0("(", str, ")"))

  term = terms[[1]]
  ands = terms[-1]
  term = paste0("(", term, year, ")")
  if (length(ands) != 0) {
    term = Reduce(\(term_, and_) paste0('(', term_, ' AND ', and_, ')'), ands, init = term)
  }
} else {
  stop('You need to provide search terms in "...", which are character vectors of length one.')
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

# 检查检索结果
if (length(result$ids) == 0) stop('No relevant results found, search terms are: \n', term)
# 更合适的步长
n = 100
while (length(result$ids) < n) n = n/10

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
for (var_url in url) {
  message(paste0("we are getting ", var_url, " abstract..."))

  # 尝试读取网页并提取摘要
  webpage <- tryCatch({
    read_html(var_url)
  }, error = function(e) {
    message(paste0("Error reading webpage: ", e$message))
    return(NULL)
  })

  if (is.null(webpage)) {
    abstract_text <- ""
  } else {
    abstract_div <- tryCatch({
      webpage %>% html_nodes("#abstract")
    }, error = function(e) {
      message(paste0("Error finding abstract div: ", e$message))
      return(html_nodes(webpage, "div"))
    })

    if (abstract_div %>% length() == 0) {
      abstract_text <- ""
    } else {
      abstract_text <- tryCatch({
        abstract_div %>% html_text(trim = TRUE)
      }, error = function(e) {
        message(paste0("Error extracting abstract text: ", e$message))
        return("")
      })
    }
  }

  abstract_text_all <- c(abstract_text_all, abstract_text)
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

driverpubmed_example = function() {

  driverpubmed('Melanoma', 'TCGA', 'Signature')

}
