restart_web_safe <- function() {

  # 第一步：安全地关闭现有连接
  tryCatch({
    if (exists("driver", envir = .GlobalEnv)) {
      if (!is.null(driver) && "remoteDriver" %in% class(driver)) {
        tryCatch(driver$close(), error = function(e) cat("Driver already closed\n"))
      }
    }
  }, error = function(e) cat("Error closing driver:", e$message, "\n"))

  tryCatch({
    if (exists("rD", envir = .GlobalEnv)) {
      if (!is.null(rD) && "rsDriver" %in% class(rD)) {
        tryCatch(rD$server$stop(), error = function(e) cat("Server already stopped\n"))
      }
    }
  }, error = function(e) cat("Error stopping server:", e$message, "\n"))

  # 第二步：清理环境变量
  if (exists("driver", envir = .GlobalEnv)) {
    rm(driver, envir = .GlobalEnv)
  }
  if (exists("rD", envir = .GlobalEnv)) {
    rm(rD, envir = .GlobalEnv)
  }

  # 第三步：强制垃圾回收
  gc()

  # 第四步：等待一下让系统清理
  Sys.sleep(2)

  # 第五步：重新启动
  tryCatch({
    library(RSelenium)
    library(fromto)

    # 生成随机端口
    port <- sample(4000:9999, 1)
    cat("Using port:", port, "\n")

    # 获取extraCapabilities
    extraCapabilities_list <- istart()

    # 启动新的driver
    rD <<- rsDriver(
      browser = "firefox",
      port = port,
      chromever = NULL,
      geckover = "0.33.0",
      extraCapabilities = extraCapabilities_list,
      check = FALSE,
      verbose = FALSE
    )

    # 等待服务器启动
    Sys.sleep(3)

    # 创建客户端连接
    driver <<- rD$client
    driver$open()

    cat("Selenium restarted successfully!\n")
    return(TRUE)

  }, error = function(e) {
    cat("Error restarting Selenium:", e$message, "\n")
    return(FALSE)
  })
}
gbd_click_year = function(
    year = "1991",#"Select all"
    initial = FALSE,
    year_clear = "1990"
){
  if(initial){
    Year_xpath = "//span[text()='Year']/ancestor::div[contains(@class, 'J9RmkHiFTJ')]//div[contains(@class, 'ant-select')]"
    Year_xpath_1 = "//span[contains(@class, 'ant-tag') and .//span[contains(text(), '2021')]]//span[contains(@class, 'ant-tag-close-icon')]"
    Year_xpath_2 = paste0("//div[@aria-label='",year,"' and @label='",year,"']")
    driver$findElement(using = "xpath", value = Year_xpath)$clickElement()
    tryCatch({
      driver$findElement(using = "xpath", value = Year_xpath_1)$clickElement()
    }, error = function(e) {message("Error: ", conditionMessage(e))})
    driver$findElement(using = "xpath", value = Year_xpath_2)$clickElement()
    driver$findElement(using = "xpath", value = "//body")$clickElement()
    iview(driver)
  }else{
    Year_xpath = "//span[text()='Year']/ancestor::div[contains(@class, 'J9RmkHiFTJ')]//div[contains(@class, 'ant-select')]"
    Year_xpath_1 = paste0("//span[contains(@class, 'ant-tag') and .//span[contains(text(), '",year_clear,"')]]//span[contains(@class, 'ant-tag-close-icon')]")
    Year_xpath_2 = paste0("//div[@aria-label='",year,"' and @label='",year,"']")
    driver$findElement(using = "xpath", value = Year_xpath)$clickElement()
    tryCatch({
      driver$findElement(using = "xpath", value = Year_xpath_1)$clickElement()
    }, error = function(e) {message("Error: ", conditionMessage(e))})
    driver$findElement(using = "xpath", value = Year_xpath_2)$clickElement()
    driver$findElement(using = "xpath", value = "//body")$clickElement()
    iview(driver)
  }
}

gbd_click_metric = function(metric = "Number"){
  metric_xpath = "//span[contains(text(),'Metric')]/ancestor::div[contains(@class, 'J9RmkHiFTJ')]//div[contains(@class, 'ant-select')]"
  metric_xpath_1 = "//span[contains(@class, 'ant-tag') and .//span[contains(text(), 'Number')]]//span[contains(@class, 'ant-tag-close-icon')]"
  metric_xpath_2 = "//span[contains(@class, 'ant-tag') and .//span[contains(text(), 'Percent')]]//span[contains(@class, 'ant-tag-close-icon')]"
  metric_xpath_3 = "//span[contains(@class, 'ant-tag') and .//span[contains(text(), 'Rate')]]//span[contains(@class, 'ant-tag-close-icon')]"
  if(metric == "Number"){
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_2)$clickElement())
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_3)$clickElement())
  }else if(metric == "Percent"){
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_1)$clickElement())
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_3)$clickElement())
  }else if(metric == "Rate"){
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_1)$clickElement())
    try_fromto(driver$findElement(using = "xpath", value = metric_xpath_2)$clickElement())
  }
  driver$findElement(using = "xpath", value = "//body")$clickElement();iview(driver)
  iview(driver)
}

gbd_click_share = function(){
  driver$findElement(using = "xpath", value = "//button[contains(@class,'ant-btn') and .//span[text()='Share']]")$clickElement();isleep();iview(driver)
  input_xpath = "//input[@class='ant-input ant-input-lg' and @type='text']"
  input_element = wait_for_element(driver, input_xpath)
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  permalink = input_element$getElementAttribute("value")[[1]]
  return(permalink)
}
gbd_click_download = function(){
  #Download
  driver$findElement(using = "xpath", value = "//button[contains(@class,'ant-btn') and .//span[text()='Download']]")$clickElement()
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  iview(driver)
  #Submit
  driver$findElement(using = "xpath", value = "//button[contains(@class,'ant-btn') and .//span[text()='Submit']]")$clickElement()
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  iview(driver)
  #pages
  page_html = driver$getPageSource()[[1]]
  url_pattern = '/gbd-results/result/[a-f0-9]{32}'
  extracted_url = regmatches(page_html, regexpr(url_pattern, page_html))
  return(extracted_url)
}
gbd_download = function(
    var_dir,
    title_modify
){
  html_content = get_page_content()
  iview(driver)
  driver$close()
  download_ihme_files(html_content,
                      download_dir = var_dir,
                      title_modify = title_modify)
}
reinit_browser <- function() {
  tryCatch({
    if (exists("driver")) driver$close()
    if (exists("rD")) rD$client$close()
    rm(driver, rD, envir = .GlobalEnv)
  }, error = function(e) {
    message("清理资源时出现警告: ", conditionMessage(e))
  })

  library(fromto)
  extraCapabilities_list <<- istart()
  rD <<- rsDriver(browser = "firefox", port = sample(4000:9000, 1), chromever = NULL, geckover = "0.33.0", extraCapabilities = extraCapabilities_list, check = F, verbose = F)
  driver <<- rD$client
  driver$open()
  igo(driver, "https://vizhub.healthdata.org/gbd-results/")
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  iview(driver)
  iclick(driver = driver, js_use = "body > div > div > div.ant-modal-wrap > div > div.ant-modal-content > div.ant-modal-body > div > div:nth-child(3) > button")
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  iview(driver)
  iinput(driver = driver, js_use = "#signInName", input_use = "215672062@qq.com")
  iinput(driver = driver, js_use = "#password", input_use = "5380856zhX")
  iclick(driver = driver, js_use = "#localAccountForm > div.entry > div.buttons > button.fakeSubmitBtn")
  isleep(runif_use = 1, min_use = 20, max_use = 25)
  iview(driver)

  # 改进的横幅关闭
  tryCatch({
    driver$executeScript("try { document.querySelector('#vizhub-template').shadowRoot.querySelector('div.vizhub-wrapper.single-page > div.banner.banner--info.banner--closeable > button').click(); } catch(e) { console.log('Banner not found'); }")
  }, error = function(e) {
    message("关闭横幅失败，但继续执行")
  })
  isleep()
  iview(driver)
}
click_by_index <- function(index) {
  if (index > 0 && index <= length(visible_options)) {
    tryCatch({
      visible_options[[index]]$clickElement()
      print(paste("已点击选项:", options_text[index]))
    }, error = function(e) {
      print(paste("点击失败:", e$message))
    })
  } else {
    print("索引超出范围")
  }
}
try_fromto = function(
    manus,
    try_max = 10,
    min_sleep = 1,
    max_sleep = 2){
  for (try in 1:try_max) {
    isleep(runif_use = 1, min_use = min_sleep, max_use = max_sleep)
    success = TRUE
    tryCatch({
      manus
    }, error = function(e) {
      message("Error: ", conditionMessage(e))
      success <<- FALSE
    })
    if (success) {
      message("操作成功，停止循环。")
      break
    }
  }
}


download_ihme_files <- function(html_file = NULL,
                                html_content = NULL,
                                download_dir = "ihme_data",
                                title_modify) {
  # 加载必要的库
  if (!requireNamespace("rvest", quietly = TRUE)) {
    install.packages("rvest")
  }
  library(rvest)

  # 创建下载目录
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
    print(paste("创建下载目录:", download_dir))
  }

  # 解析HTML
  if (!is.null(html_file)) {
    html_doc <- read_html(html_file)
  } else if (!is.null(html_content)) {
    html_doc <- read_html(html_content)
  } else {
    stop("必须提供html_file或html_content参数")
  }

  # 提取所有下载链接
  download_links <- html_doc %>%
    html_nodes(".download-button a") %>%
    html_attr("href")

  # 提取所有文件标题
  file_titles <- html_doc %>%
    html_nodes(".ant-list-item-meta-title") %>%
    html_text()

  # 显示找到的链接数量
  total_links <- length(download_links)
  print(paste("找到", total_links, "个下载链接"))

  # 下载所有文件
  for (i in 1:total_links) {
    link <- download_links[i]
    title <- file_titles[i]
    filename <- paste0(download_dir, "/", title, ".zip")

    # 替换文件名中的特殊字符
    filename <- gsub("[\\/:*?\"<>|]", "_", filename)

    print(paste("正在下载", i, "/", total_links, ":", title))
    tryCatch({
      download.file(link,
                    destfile = paste0(download_dir, "/",title_modify,"_",filename),
                    mode = "wb",
                    quiet = FALSE)
      print(paste("  下载完成:", filename))
    }, error = function(e) {
      print(paste("  下载失败:", filename))
      print(paste("  错误信息:", e$message))
    })
  }

  print(paste("所有下载完成。文件保存在:", download_dir))
  return(invisible(data.frame(title = file_titles, url = download_links)))
}
wait_for_element <- function(driver, xpath, timeout = 10) {
  for (i in 1:timeout) {
    elem <- try(driver$findElement("xpath", xpath), silent = TRUE)
    if (!inherits(elem, "try-error")) return(elem)
    Sys.sleep(1)
  }
  stop("Element not found")
}
wait_for_react <- function(manus, timeout = 10000) {
  for (attempt in 1:timeout) {
    elem <- try(manus, silent = TRUE)
    if (!inherits(elem, "try-error")) {
      return(elem)
    }
    Sys.sleep(1)
  }
  stop("Element not found within timeout period")
}
get_page_content = function() {
  html_content = driver$getPageSource()[[1]]
  return(html_content)
}
get_page_links = function() {
  html_content = driver$getPageSource()[[1]]

  page = read_html(html_content)

  links <- page %>%
    html_nodes("a") %>%
    html_attr("href")

  link_text <- page %>%
    html_nodes("a") %>%
    html_text()

  return(data.frame(
    text = link_text,
    url = links,
    stringsAsFactors = FALSE
  ))
}
#' @title istart
#' @description istart
#' @return envir
#' @export
istart = function(){
  library(httr);library(tidyverse);library(rvest);library(RSelenium);library(wdman) ;library(XML);library(fromto);library(ellmer)
  extraCapabilities_list = list("moz:firefoxOptions" = list(binary = 'C:\\Program Files\\Mozilla Firefox\\firefox.exe',args = list("--headless","--window-size=1920,1080", "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")),"browser.startup.homepage" = "about:blank","browser.privatebrowsing.autostart" = TRUE,"browser.download.manager.showWhenStarting" = FALSE,"browser.helperApps.neverAsk.saveToDisk" = "application/octet-stream")
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
