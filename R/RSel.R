#' @title istart
#' @description istart
#' @return envir
#' @export
istart = function(){
  library(httr);library(tidyverse);library(rvest);library(RSelenium);library(wdman) ;library(XML);library(fromto);library(ellmer)
  extraCapabilities_list = list("moz:firefoxOptions" = list(binary = '/Program Files/Mozilla Firefox/firefox.exe',args = list("--headless","--window-size=1920,1080", "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")),"browser.startup.homepage" = "about:blank","browser.privatebrowsing.autostart" = TRUE,"browser.download.manager.showWhenStarting" = FALSE,"browser.helperApps.neverAsk.saveToDisk" = "application/octet-stream")
  return(extraCapabilities_list)
}
#' @title igo
#' @description igo
#' @return igo
#' @export
igo = function(driver,url_use){
  driver$navigate(url_use)

}
#' @title ichat_deepseek
#' @description ichat_deepseek
#' @param system_prompt_use system_prompt_use
#' @param model_use model_use
#' @param api_key api_key
#' @param ask_use ask_use
#' @param base_url_use base_url_use
#' @param api_key_use api_key_use
#' @param split_use split_use = FALSE
#' @param top_p_use top_p_use
#' @param frequency_penalty_use frequency_penalty_use
#' @param presence_penalty_use presence_penalty_use
#' @param max_tokens_use max_tokens_use
#' @param temperature_use temperature_use
#' @param timeout_use timeout_use
#' @return ai_reply
#' @export
ichat_deepseek = function(system_prompt_use = "你是一个R语言、Python与生物信息学专家",
                          model_use = "deepseek-chat",
                          ask_use,
                          api_key = "sk-81e0a52d66d249d4bbe69b6d4afff6e0",
                          base_url_use ="https://api.deepseek.com/v1/chat/completions",
                          api_key_use = "sk-81e0a52d66d249d4bbe69b6d4afff6e0",
                          split_use = FALSE,
                          top_p_use = 0.9,
                          frequency_penalty_use = 0.5,
                          presence_penalty_use = 0.3,
                          max_tokens_use = 8192,
                          temperature_use = 0,
                          timeout_use = 180
){
  headers = add_headers(
    `Authorization` = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )

  body = list(
    model = model_use,
    messages = list(
      list(role = "system", content = system_prompt_use),
      list(role = "user", content = ask_use)
    ),
    top_p = top_p_use,                # 更可控的随机性
    frequency_penalty = frequency_penalty_use,    # 稍微减少重复
    presence_penalty = presence_penalty_use,      # 稍微鼓励新话题
    max_tokens = max_tokens_use,  # 设置最大 tokens（默认可能是 2048 或 4096，取决于模型）
    temperature = temperature_use    # 设置温度（0~2，值越小越确定，越大越随机）
  )

  response = POST(
    url = base_url_use,
    headers,
    body = body,
    encode = "json",
    timeout(timeout_use)
  )

  response_content = content(response, "parsed")  # 自动解析 JSON
  #print(response_content)

  # 或者手动解析 JSON
  #response_text = content(response, "text")
  #response_json = fromJSON(response_text)
  #print(response_json)

  ai_reply = response_content$choices[[1]]$message$content
  cat(ai_reply)
  return(ai_reply)
}
#' @title isleep
#' @description isleep
#' @param runif_use runif_use = 1
#' @param min_use min_use = 4
#' @param max_use max_use = 8
#' @return isleep
#' @export
isleep = function(runif_use = 1,min_use = 4,max_use = 8){
  Sys.sleep(runif(runif_use, min = min_use, max = max_use))
}
#' @title iclick
#' @description iclick
#' @param driver driver
#' @param js_use js_use
#' @return iclick
#' @export
iclick = function(driver,js_use){
  driver$findElement(using = "css selector", js_use)$clickElement()

}
#' @title iclick2
#' @description iclick2
#' @param driver driver
#' @param js_use js_use
#' @return iclick2
iclick2 = function(driver,js_use){
  driver$executeScript("arguments[0].click();", list(driver$findElement(using = "css selector", js_use)))

}
#' @title iinput
#' @description iinput
#' @param driver driver
#' @param js_use js_use
#' @param input_use input_use
#' @return iinput
#' @export
iinput = function(driver,js_use,input_use){
  driver$findElement(using = "css selector", js_use)$sendKeysToElement(list(input_use))

}
#' @title iinput2
#' @description iinput2
#' @param driver driver
#' @param js_use js_use
#' @param input_use input_use
#' @return iinput2
#' @export
iinput2 = function(driver,js_use,input_use){
  driver$executeScript(paste0("document.querySelector('",js_use,"').innerHTML += arguments[0] + '<br>';"),list(input_use))

}
#' @title iview
#' @description iview
#' @param driver driver
#' @return iview
#' @export
iview = function(driver){
  driver$maxWindowSize();driver$screenshot(display = TRUE)

}
#' @title iview_down
#' @description iview_down
#' @param driver driver
#' @param seq_min seq_min
#' @param seq_max seq_max
#' @param seq_use seq_use
#' @return iview_down
#' @export
iview_down = function(driver, seq_min = 0, seq_max =10000, seq_use = 100){
  for (variable in seq(seq_min, seq_max, seq_use)) {
    driver$executeScript(paste("window.scrollTo(0, ", variable, ");"))
    isleep(1,1,3)
  }
  driver$screenshot(display = TRUE)

}
#' @title iswitch
#' @description iswitch
#' @param driver driver
#' @param window_use window_use = NULL
#' @return iswitch
#' @export
iswitch = function(driver,window_use = NULL){
  if(is.null(window_use)){
    window_use = length(driver$getWindowHandles())
  }else{
    window_use = as.numeric(window_use)
  }
  driver$switchToWindow(driver$getWindowHandles()[[window_use]])

}
#' @title iback
#' @description iback
#' @param driver driver
#' @param js_use js_use
#' @return iback
#' @export
iback = function(driver,js_use){
  driver$findElement(using = "css selector", js_use)$sendKeysToElement(list(key = "backspace"))

}
#' @title idelete
#' @description idelete
#' @param driver driver
#' @param js_use js_use
#' @return idelete
#' @export
idelete = function(driver,js_use){
  driver$executeScript("arguments[0].value = '';", list(driver$findElement(using = "css selector", js_use)))

}
#' @title idelete2
#' @description idelete2
#' @param driver driver
#' @param js_use js_use
#' @return idelete2
#' @export
idelete2 = function(driver,js_use){
  driver$executeScript(paste0("document.querySelector('",js_use,"').innerHTML = '';"))

}
#' @title ipage
#' @description ipage
#' @param driver driver
#' @param page_use page_use
#' @return ipage
#' @export
ipage = function(driver, page_use = NULL){
  if(is.null(page_use)){
    page_use = length(driver$getWindowHandles())
  }else{
    page_use = as.numeric(page_use)
  }
  page = driver$getPageSource()[[page_use]]
  webpage = read_html(page)
  return(webpage)
}
#' @title ipage_links
#' @description ipage_links
#' @param driver driver
#' @param js_use js_use
#' @param page_use page_use
#' @return ipage_links
#' @export
ipage_links = function(driver,js_use, page_use = NULL){
  if(is.null(page_use)){
    page_use = length(driver$getWindowHandles())
  }else{
    page_use = as.numeric(page_use)
  }
  links_get = driver$getPageSource()[[page_use]] %>% read_html() %>% html_nodes(js_use) %>% html_attr('href')
  return(links_get)
}
#' @title ipage_texts
#' @description ipage_texts
#' @param driver driver
#' @param js_use js_use
#' @param page_use page_use
#' @return ipage_texts
#' @export
ipage_texts = function(driver,js_use, page_use = NULL){
  if(is.null(page_use)){
    page_use = length(driver$getWindowHandles())
  }else{
    page_use = as.numeric(page_use)
  }
  texts_get = driver$getPageSource()[[page_use]] %>% read_html()  %>% html_nodes(js_use) %>% html_text()
  return(texts_get)
}

ipage_texts2 = function(driver,js_use){
  content = driver$executeScript(paste0("return document.querySelector('",js_use,"').innerText;"))
  return(content)
}

#' @title ipage_nodes
#' @description ipage_nodes
#' @param driver driver
#' @param js_use js_use
#' @param page_use page_use
#' @return ipage_nodes
#' @export
ipage_nodes = function(driver,js_use, page_use = NULL){
  if(is.null(page_use)){
    page_use = length(driver$getWindowHandles())
  }else{
    page_use = as.numeric(page_use)
  }
  nodes_get = driver$getPageSource()[[page_use]] %>% read_html() %>% html_nodes(js_use)
  return(nodes_get)
}
#' @title ipage_nodes_change
#' @description ipage_nodes_change
#' @param driver driver
#' @param js_use js_use
#' @param change_pre change_pre
#' @param change_post change_post
#' @param page_use page_use
#' @return ipage_nodes_change
#' @export
ipage_nodes_change = function(driver,js_use,change_pre = "<br>|<p>|<code>",change_post = "\n", page_use = NULL){
  if(is.null(page_use)){
    page_use = length(driver$getWindowHandles())
  }else{
    page_use = as.numeric(page_use)
  }
  page_content = read_html(driver$getPageSource()[[page_use]]) %>%
    gsub(change_pre, change_post, .) %>%
    read_html() %>%
    html_nodes(js_use) %>%
    html_text2(preserve_nbsp = TRUE)
  return(page_content)
}
