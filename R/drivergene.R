#' @title drivergene
#' @description Gene Function find
#' @param binary firefox.exe is where
#' @param myport port
#' @param neverAsk.saveToDisk just for fun
#' @return html
#' @examples
#'if(.Platform$OS.type == "windows"){
#'username <- Sys.getenv("USERNAME")
#'}else{
#'username <- system("whoami", intern = TRUE)
#'username <- trimws(username)
#'}
#'
#'file.copy(from = paste0("C:\\Users\\",username,"\\Desktop\\fromto_java\\binman.7z"),
#'to = paste0("C:\\Users\\",username,"\\AppData\\Local\\binman.7z"))
#'
#'if(!require("devtools",quietly = TRUE)){
#'install.packages("devtools")
#'}
#'
#'if(!require("BiocManager",quietly = TRUE)){
#'install.packages("BiocManager")
#'}
#'
#' BiocManager::install("limma")
#' BiocManager::install("magick")
#' devtools::install_github("grswsci/fromto")

#' @examples
#' # examples
#' drivergene(GeneName = "CDC20",
#'           binary = "C:\\Program Files\\Mozilla Firefox\\firefox.exe",
#'           neverAsk.saveToDisk = "application\\octet-stream",
#'           myport = sample(1:10000, 1, replace = TRUE)

drivergene <- function(GeneName,
                       binary = "C:\\Program Files\\Mozilla Firefox\\firefox.exe",
                       neverAsk.saveToDisk = "application\\octet-stream",
                       myport = sample(1:10000, 1, replace = TRUE)
                       ){
  data_file = system.file("data", "trans2gene.RDS", package = "fromto")
  GeneID_data = readRDS(data_file)
  GeneID_data = GeneID_data[which(GeneID_data$Symbol == GeneName),
  ]
  GeneID_NCBI = GeneID_data$NCBI_GeneID[1]

  add_strings = function(strings) {
    result = character()
    for (string in strings) {
      if (grepl("See all PubMed", string)) {
        result = c(result, string, "Not find")
      } else {
        result = c(result, string)
      }
    }
    return(result)
  }

  #if(!require(wdman,quietly = TRUE)){
  #  install.packages(system.file("extdata/binman_seleniumserver/", "wdman_0.2.6.tar.gz", package = "fromto"), repos = NULL, type = "source")
  #}

  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(RSelenium))
  suppressPackageStartupMessages(library(wdman))
  suppressPackageStartupMessages(library(DT))
  suppressPackageStartupMessages(library(htmlwidgets))
  options(warn = -1)
  options(timeout = 100000000000000000)

  rD = rsDriver(browser = "firefox",
                version = "4.0.0-alpha-2",
                geckover = "0.33.0",
                phantomver = "2.1.1",
                chromever = NULL,
                port = myport,
                extraCapabilities = list(`moz:firefoxOptions` = list(binary = binary,
                                         args = list("--headless")),
                                         browser.startup.homepage = "about:blank",
                                         browser.privatebrowsing.autostart = TRUE, browser.download.manager.showWhenStarting = FALSE,
                                         browser.helperApps.neverAsk.saveToDisk = neverAsk.saveToDisk ),
                check = F, verbose = F)

  driver = rD$client
  driver$open()
  url = paste0("https://www.ncbi.nlm.nih.gov/gene?db=gene&report=generif&term=",
               GeneID_NCBI)
  driver$navigate(url)
  Sys.sleep(5)
  driver$screenshot(display = TRUE)
  elements_page = driver$findElements(using = "xpath", "//a[@data-page]")
  data_pages = sapply(elements_page, function(element) {
    element$getElementAttribute("data-page")[[1]]
  })
  driver$screenshot(display = TRUE)
  if (is.null(data_pages[2][[1]])) {
    element = driver$findElement(using = "xpath", value = "//tbody")
    tdElements = element$findElements(using = "tag name",
                                      value = "td")
    tdText = tdElements[[1]]$getElementText()[[1]]
    tdText_split =unlist(str_split(tdText, "\n"))
    tdText_split = tdText_split[tdText_split!=""]
    tdText_split = add_strings(tdText_split)
    seq_1_to_n = seq(from = 1, to = length(tdText_split), by = 4)
    a_elements = driver$findElements(using = "xpath", value = "//tr/td/p/a")

    hrefs = sapply(a_elements, function(element) {
      element$getElementAttribute("href")[[1]]
    })
    hrefs = str_subset(hrefs,"https://www.ncbi.nlm.nih.gov/pubmed/")
    df_all = data.frame()
    for(variable in seq_1_to_n) {
      trText_split1 = tdText_split[variable]
      trText_split2 = tdText_split[variable+1]
      trText_split3 = tdText_split[variable+2]
      trText_split4 = tdText_split[variable+3]
      df_pre = data.frame(trText_split1, trText_split2,
                          trText_split3, trText_split4)
      df_all = rbind(df_all, df_pre)
    }
    df_all = df_all[df_all$trText_split3!= "Not find",]
    df_all = data.frame(df_all, hrefs)

  }else{
    df_all = data.frame()
    number_of_elements = as.numeric(data_pages[2])
    for (pages in (1:number_of_elements)) {
      if (pages == (1:number_of_elements)[1]) {
        print(pages)
        element = driver$findElement(using = "xpath", value = "//tbody")
        tdElements = element$findElements(using = "tag name",
                                          value = "td")
        tdText = tdElements[[1]]$getElementText()[[1]]
        tdText_split =unlist(str_split(tdText, "\n"))
        tdText_split = tdText_split[tdText_split!=""]
        tdText_split = add_strings(tdText_split)
        seq_1_to_n = seq(from = 1, to = length(tdText_split), by = 4)
        a_elements = driver$findElements(using = "xpath", value = "//tr/td/p/a")
        hrefs = sapply(a_elements, function(element) {
          element$getElementAttribute("href")[[1]]
        })
        hrefs = str_subset(hrefs,"https://www.ncbi.nlm.nih.gov/pubmed/")
        df = data.frame()
        for(variable in seq_1_to_n) {
          trText_split1 = tdText_split[variable]
          trText_split2 = tdText_split[variable+1]
          trText_split3 = tdText_split[variable+2]
          trText_split4 = tdText_split[variable+3]
          df_pre = data.frame(trText_split1, trText_split2,
                              trText_split3, trText_split4)
          df = rbind(df, df_pre)
        }
        df = df[df$trText_split3!= "Not find",]
        df = data.frame(df, hrefs)
        df_all = rbind(df_all, df)
      }else{
        print(pages)
        driver$navigate(paste0("https://www.ncbi.nlm.nih.gov/gene/?db=gene&term=",
                               GeneID_NCBI, "&report=generif&page=", pages))
        Sys.sleep(5)
        element = driver$findElement(using = "xpath", value = "//tbody")
        tdElements = element$findElements(using = "tag name",
                                          value = "td")
        tdText = tdElements[[1]]$getElementText()[[1]]
        tdText_split =unlist(str_split(tdText, "\n"))
        tdText_split = tdText_split[tdText_split != ""]
        tdText_split = add_strings(tdText_split)
        seq_1_to_n = seq(from = 1, to = length(tdText_split), by = 4)
        a_elements = driver$findElements(using = "xpath", value = "//tr/td/p/a")
        hrefs = sapply(a_elements, function(element) {
          element$getElementAttribute("href")[[1]]
        })
        hrefs = str_subset(hrefs,"https://www.ncbi.nlm.nih.gov/pubmed/")
        df = data.frame()
        for(variable in seq_1_to_n) {
          trText_split1 = tdText_split[variable]
          trText_split2 = tdText_split[variable+1]
          trText_split3 = tdText_split[variable+2]
          trText_split4 = tdText_split[variable+3]
          df_pre = data.frame(trText_split1, trText_split2,
                              trText_split3, trText_split4)
          df = rbind(df, df_pre)
        }
        df = df[df$trText_split3!= "Not find",]
        df = data.frame(df, hrefs)
        df_all = rbind(df_all, df)
      }
    }
  }

  createLink = function(base, val) {
    sprintf("<a href=\"%s\" class=\"btn btn-link\" target=\"_blank\" >%s</a>",
            base, val)
  }
  res = data.frame(title = createLink(df_all$hrefs, df_all$trText_split2),
                   Function = df_all$trText_split1, pubdate = df_all$trText_split4,
                   authors = gsub(", Free PMC Article", "", df_all$trText_split3),
                   DataBase = createLink(paste0("https://grswsci.top/"),
                                         "光热生物数据库"), Superiority = "半步博导",
                   stringsAsFactors = F)
  res = na.omit(res)
  y = DT::datatable(res, escape = F, rownames = F)
  DT::saveWidget(y, paste0(GeneName, "_output_paper.html"),
                 selfcontained = T)
  cite = 1:nrow(df_all)
  text_long_string = paste(paste0(df_all$trText_split2, "[",
                                  cite, "]"), collapse = " ")
  cite_link = paste0("[", cite, "] ", df_all$hrefs)
  write.table(text_long_string, paste0(GeneName, "_text_long_string.txt"),
              quote = F, row.names = F)
  write.table(cite_link, paste0(GeneName, "_text_long_cite.txt"),
              quote = F, row.names = F)
  driver$closeWindow()
  rD = rD$server$stop()
}
