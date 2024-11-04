
call_for_papers = function(press = "mdpi",
                           journal = "International Journal of Molecular Sciences",
                           url = "https://www.mdpi.com/topics?journal=ijms"){
  library(data.table)
  library(httr)
  library(tidyverse)
  library(rvest)
  library(RSelenium)
  library(DT)
  library(htmlwidgets)
  library(fromto)

  if(press == "mdpi"){
    webpage = read_html(url)

    href_page_number = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "&page_no=", ys = .)
    href_page_s = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "&page_no=", ys = .)
    if(length(href_page_number) > 0){
      href_page_number = strsplit_fromto(href_page_number,str = "=",idx = 3) %>% as.numeric(.)
      href_page_number = 2:max(href_page_number)
      href_page_s = strsplit_fromto(href_page_s,str = "2",idx = 1)[1]
      href_page = paste0("https://www.mdpi.com",href_page_s,href_page_number)
    }

    href_topic = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "/topics/",ys = .) %>%
      .[6:length(.)] %>% paste0("https://www.mdpi.com",.)

    href_topic_add_all = href_topic
    if(length(href_page_number) > 0){
      for (url_variable in href_page) {
        url_webpage = read_html(url_variable)
        href_topic_add = url_webpage %>%
          html_nodes(xpath = "//a") %>%
          html_attr("href") %>%
          find_name(x = "/topics/",ys = .) %>%
          .[6:length(.)] %>% paste0("https://www.mdpi.com",.)
        href_topic_add_all = c(href_topic_add_all,href_topic_add)
      }
    }

    topic_data_frame = data.frame()
    for (url_topic in href_topic_add_all) {
      message(url_topic)
      #url_topic = "https://www.mdpi.com/topics/GBX2UOVIC5"
      topic_webpage = read_html(url_topic)

      topic_title = topic_webpage %>%
        html_nodes("title") %>%
        html_text()

      topic_Information = topic_webpage %>%
        html_nodes("#information + p, #information ~ p") %>%
        html_text() %>% paste(., collapse = "\n")

      topic_keywords = topic_webpage %>%
        html_nodes("h2#keywords + div > ul > li") %>%
        html_text(trim = TRUE) %>% paste(., collapse = "\n")

      topic_papers = topic_webpage %>%
        html_nodes(".generic-item.article-item") %>%
        length(.)

      topic_data_frame = rbind(topic_data_frame,data.frame(
                               Titles = topic_title,
                               Informations = topic_Information,
                               Keywords = topic_keywords,
                               Links = url_topic,
                               Papers = topic_papers)
      )
    }
    createLink = function(base, val) {
      sprintf("<a href=\"%s\" class=\"btn btn-link\" target=\"_blank\" >%s</a>",
              base, val)
    }
    res = data.frame(Titles = createLink(topic_data_frame$Links,
                                         topic_data_frame$Titles),
                     Informations = topic_data_frame$Informations,
                     Keywords = topic_data_frame$Keywords,
                     Papers = topic_data_frame$Papers,
                     Source = createLink("https://grswsci.top",
                                         "光热生物数据库"),
                     stringsAsFactors = F)
    res = na.omit(res)
    y = DT::datatable(res, escape = F, rownames = F)
    DT::saveWidget(y, paste0(journal,"_call_for_papers.html"),selfcontained = TRUE)
  }
}

call_for_papers_SI = function(press = "mdpi",
                           journal = "International Journal of Molecular Sciences",
                           url = "https://www.mdpi.com/journal/ijms/special_issues"){
  library(data.table)
  library(httr)
  library(tidyverse)
  library(rvest)
  library(RSelenium)
  library(DT)
  library(htmlwidgets)
  library(fromto)

  if(press == "mdpi"){
    webpage = read_html(url)

    href_page_number = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "?page_no=", ys = .)
    href_page_s = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "?page_no=", ys = .)
    if(length(href_page_number) > 0){
      href_page_number = strsplit_fromto(href_page_number,str = "=",idx = 2) %>% as.numeric(.)
      href_page_number = 2:max(href_page_number)
      href_page_s = strsplit_fromto(href_page_s,str = "2",idx = 1)[1]
      href_page = paste0("https://www.mdpi.com",href_page_s,href_page_number)
    }

    href_topic = webpage %>%
      html_nodes(xpath = "//a") %>%
      html_attr("href") %>%
      find_name(x = "/special_issues/",ys = .) %>%
      paste0("https://www.mdpi.com",.)

    href_topic_add_all = href_topic
    if(length(href_page_number) > 0){
      for (url_variable in href_page) {
        url_webpage = read_html(url_variable)
        href_topic_add = url_webpage %>%
          html_nodes(xpath = "//a") %>%
          html_attr("href") %>%
          find_name(x = "/special_issues/",ys = .) %>%
          .[6:length(.)] %>% paste0("https://www.mdpi.com",.)
        href_topic_add_all = c(href_topic_add_all,href_topic_add)
      }
    }

    topic_data_frame = data.frame()
    for (url_topic in href_topic_add_all) {
      message(url_topic)
      #url_topic = "https://www.mdpi.com/journal/ijms/special_issues/KF069TG4AQ"
      topic_webpage = read_html(url_topic)

      topic_title = topic_webpage %>%
        html_nodes("title") %>%
        html_text()

      topic_Information = topic_webpage %>%
        html_nodes("h2") %>%
        html_nodes("a[name='info']") %>%
        html_node(xpath = "./parent::*") %>%
        html_node(xpath = "following-sibling::div[1]") %>%
        html_nodes("p") %>%
        html_text() %>%
        paste(., collapse = "\n")

      topic_papers = topic_webpage %>%
        html_nodes(".generic-item.article-item") %>%
        length(.)

      topic_data_frame = rbind(topic_data_frame,data.frame(
        Titles = topic_title,
        Informations = topic_Information,
        Links = url_topic,
        Papers = topic_papers)
      )
    }
    createLink = function(base, val) {
      sprintf("<a href=\"%s\" class=\"btn btn-link\" target=\"_blank\" >%s</a>",
              base, val)
    }
    res = data.frame(Titles = createLink(topic_data_frame$Links,
                                         topic_data_frame$Titles),
                     Informations = topic_data_frame$Informations,
                     Papers = topic_data_frame$Papers,
                     Source = createLink("https://grswsci.top",
                                         "光热生物数据库"),
                     stringsAsFactors = F)
    res = na.omit(res)
    y = DT::datatable(res, escape = F, rownames = F)
    DT::saveWidget(y, paste0(journal,"_call_for_papers.html"),selfcontained = TRUE)
  }
}
