si_clean = function(
    read_txt = "Elsevier.txt",
    save_txt = "Elsevier_clean"
){
  library(fromto)
  library(httr)
  library(rvest)
  library(data.table)
  data = fread(read_txt)
  ichat_all = c()
  for (variable in 1:nrow(data)) {
    data_subset = data[variable, ]
    data_subset = paste(data_subset, collapse = " ")
    ichat = ichat_deepseek(
      system_prompt_use = "你是一个生物学家、医学家、研究者",
      model_use = "deepseek-chat", ask_use = paste0(
        "我将给你一段文字，我希望你以中文总结出6个信息(对于专刊的标题和期刊的名字必须用英文)，并以|隔开(|前后不可以有空格),整个本文中不可以有换行符、不可以有序号、不可以有额外冗余：,\n  ",
        data_subset, "
1) 专刊的名字是什么，\n
2) 专刊所发布的期刊是什么(期刊名字不可以包括广告，例如ScienceDirect.com by Elsevier)，\n
3) 以简短而精准的话告诉我，这个专刊收稿的方向，\n
4) 专刊的截止日期是多少 (日期必须是这个格式：2025/7/1，没有截止时间定义为：9999/12/31)，\n
5) 专刊的链接是什么\n
6) 根据姓名判断专刊编辑中有没有中国人\n\n
示例：皮肤黑色素瘤免疫治疗|Frontiers in Immunology|专刊旨在聚焦皮肤黑色素瘤这一高度恶性、易转移的肿瘤，围绕近年来快速发展的免疫检查点抑制剂（如抗PD-1、抗CTLA-4抗体）等免疫治疗手段，总结国内外最新研究成果与临床经验，探讨疗效预测标志物、耐药机制、联合治疗策略及肿瘤微环境与免疫应答的关系，分析生物标志物在疗效预测中的作用，梳理免疫治疗相关不良反应管理及真实世界研究与长期随访数据，目标是提升临床医生实践能力，促进多学科协作和个体化治疗，服务于皮肤科、肿瘤科、免疫学研究人员及相关专业研究生，搭建推动我国黑色素瘤免疫治疗进步的学术交流平台|2025/7/1|https://www.sciencedirect.com/special-issue/323990/genegenome-editing-in-cardiovascular-biology-and-disease|有中国编辑"))
    ichat_all = c(ichat_all, ichat)
    fromto::save_txt(ichat_all, save_txt)
  }
}

si_SpringNature_links = function(dir_path){
  #https://link.springer.com/search?new-search=true&query=&content-type=Call+For+Papers&featureFlags.book-accessibility-search=true&sortBy=relevance
  files = list.files(path = dir_path,
                     pattern = ".csv")
  links = data.frame()
  for (variable in files) {
    links_pre = data.table::fread(paste0(dir_path,"/",variable))
    links_pre = as.data.frame(links_pre)
    links = rbind(links,links_pre)
  }
  links = links[which(!duplicated(links$URL)),]
  links = links$URL
  fromto::save_txt(links,"SpringNature_links",col.names_use = TRUE)
  return(links)
}

si_SpringNature <- function(driver) {
  # 获取页面源代码
  page_source <- driver$getPageSource()[[1]]

  # 解析HTML
  html_doc <- read_html(page_source)


  # 提取标题 - 从collection-title元素中提取
  title <- html_doc %>%
    html_node("[data-test='collection-title']") %>%
    html_text(trim = TRUE)

  # 如果没有找到data-test属性，尝试其他选择器
  if (is.na(title) || title == "") {
    title <- html_doc %>%
      html_node("h1") %>%
      html_text(trim = TRUE)
  }

  # 提取截止日期 - 从submission-deadline元素中提取
  deadline_element <- html_doc %>%
    html_node("[data-test='submission-deadline']")

  deadline <- ""
  if (!is.null(deadline_element)) {
    # 查找date标签或提取日期文本
    date_element <- deadline_element %>% html_node("date")
    if (!is.null(date_element)) {
      deadline <- html_text(date_element, trim = TRUE)
    } else {
      deadline_text <- html_text(deadline_element, trim = TRUE)
      # 提取日期部分
      extracted_date <- str_extract(deadline_text, "\\d{1,2}\\s+\\w+\\s+\\d{4}")
      if (!is.na(extracted_date)) {
        deadline <- extracted_date
      } else {
        deadline <- deadline_text
      }
    }
  }

  # 提取详细信息 - 从collection-description区域提取
  description_element <- html_doc %>%
    html_node("[data-test='collection-description']")

  details_parts <- c()

  # 1. 提取专刊描述
  if (!is.null(description_element)) {
    detail_paragraphs <- description_element %>%
      html_nodes("p") %>%
      html_text(trim = TRUE)

    # 过滤掉空段落
    detail_paragraphs <- detail_paragraphs[detail_paragraphs != ""]
    if (length(detail_paragraphs) > 0) {
      description_text <- paste(detail_paragraphs, collapse = " ")
      details_parts <- c(details_parts, description_text)
    }
  }

  # 2. 提取参与期刊信息
  participating_journal <- html_doc %>%
    html_node("[data-test='participating-journal']")

  if (!is.null(participating_journal)) {
    # 期刊名称
    journal_name <- participating_journal %>%
      html_node(".eds-c-card-composable__title a") %>%
      html_text(trim = TRUE)

    # 期刊描述
    journal_description <- participating_journal %>%
      html_node(".eds-c-card-composable__journal-description") %>%
      html_text(trim = TRUE)

    # 期刊元数据
    journal_metadata <- participating_journal %>%
      html_nodes(".eds-c-card-composable__metadata-list dd") %>%
      html_text(trim = TRUE)

    journal_metadata_titles <- participating_journal %>%
      html_nodes(".eds-c-card-composable__metadata-list dt") %>%
      html_text(trim = TRUE)

    # 组合期刊信息
    journal_info <- c()
    if (!is.na(journal_name) && journal_name != "") {
      journal_info <- c(journal_info, paste("Participating journal:", journal_name))
    }
    if (!is.na(journal_description) && journal_description != "") {
      journal_info <- c(journal_info, journal_description)
    }

    # 添加元数据
    if (length(journal_metadata) > 0 && length(journal_metadata_titles) > 0) {
      metadata_pairs <- paste(journal_metadata_titles, journal_metadata, sep = ": ")
      journal_info <- c(journal_info, paste("Journal details:", paste(metadata_pairs, collapse = ", ")))
    }

    if (length(journal_info) > 0) {
      details_parts <- c(details_parts, paste(journal_info, collapse = " "))
    }
  }

  # 3. 提取编辑信息
  editors_section <- html_doc %>%
    html_node("[data-test='collection-editors-section']")

  if (!is.null(editors_section)) {
    # 获取所有编辑
    editors <- editors_section %>%
      html_nodes(".app-collection-editor")

    if (length(editors) > 0) {
      editor_info <- c()
      for (i in 1:length(editors)) {
        editor <- editors[[i]]

        # 编辑姓名
        editor_name <- editor %>%
          html_node(".app-collection-editor__heading") %>%
          html_text(trim = TRUE)

        # 编辑描述
        editor_description <- editor %>%
          html_node(".app-collection-editor__description") %>%
          html_text(trim = TRUE)

        if (!is.na(editor_name) && editor_name != "") {
          editor_text <- paste("Editor:", editor_name)
          if (!is.na(editor_description) && editor_description != "") {
            editor_text <- paste(editor_text, editor_description, sep = " - ")
          }
          editor_info <- c(editor_info, editor_text)
        }
      }

      if (length(editor_info) > 0) {
        details_parts <- c(details_parts, paste(editor_info, collapse = " "))
      }
    }
  }

  # 合并所有信息
  details <- ""
  if (length(details_parts) > 0) {
    combined_text <- paste(details_parts, collapse = " ")
    # 将换行符替换为空格，清理多余空格
    combined_text <- str_replace_all(combined_text, "\\n", " ")
    combined_text <- str_replace_all(combined_text, "\\s+", " ")
    details <- str_trim(combined_text)
  }

  # 提取提交链接 - 查找"Submit to this journal"按钮的链接
  submission_link <- html_doc %>%
    html_node("[data-test='submit-to-this-journal']") %>%
    html_attr("href")

  # 如果没有找到，查找其他可能的提交链接
  if (is.na(submission_link)) {
    # 查找包含submission的链接
    all_links <- html_doc %>%
      html_nodes("a") %>%
      html_attr("href")

    # 过滤掉NA值
    all_links <- all_links[!is.na(all_links)]

    if (length(all_links) > 0) {
      # 查找包含关键词的链接
      submit_links <- all_links[str_detect(all_links, "submission|submit|manuscript", ignore.case = TRUE)]
      if (length(submit_links) > 0) {
        submission_link <- submit_links[1]
      } else {
        submission_link <- ""
      }
    } else {
      submission_link <- ""
    }
  }

  # 创建结果数据框
  result_df <- data.frame(
    title = ifelse(is.na(title) || title == "", "", title),
    submission_deadline = deadline,
    details = details,
    submission_link = ifelse(is.na(submission_link), "", submission_link),
    stringsAsFactors = FALSE
  )

  return(result_df)
}

si_SpringNature_easy = function(driver,links){
  infor_final = data.frame()
  for (link in links) {
    message(link)
    suppressMessages(
      tryCatch({
        igo(driver,link)
        isleep(runif_use = 1, min_use = 4, max_use = 6)
        iview(driver)
        infor = si_SpringNature(driver)
        infor$submission_link = link
        infor_final = rbind(infor_final,infor)
        fromto::save_txt(infor_final,"SpringNature",col.names_use = TRUE)
      }, error = function(e) {
        NULL
      })
    )
  }
  return(infor_final)
}

si_BMC_easy = function(driver,links){
  infor_all = data.frame()
  for (link in links) {
    message(link)
    suppressMessages(
      tryCatch({
        igo(driver,link)
        isleep(runif_use = 1, min_use = 4, max_use = 6)
        iview(driver)
        infor = si_BMC(driver)
        infor$submission_link = link
        infor_all = rbind(infor_all,infor)
        fromto::save_txt(infor_all,"BMC",col.names_use = TRUE)
      }, error = function(e) {
        NULL
      })
    )
  }
  return(infor_all)
}
si_BMC_links = function(driver){
  igo(driver,paste0("https://www.biomedcentral.com/collections?subject=Biomedicine"))
  iview(driver)
  link = driver$getPageSource()[[1]] %>% read_html() %>% html_nodes("a") %>% html_attr('href')
  link = link[fromto::is_in("/collections/",link)]
  link_Biomedicine = link[!duplicated(link)]

  igo(driver,paste0("https://www.biomedcentral.com/collections?subject=Medicine+%26+Public+Health"))
  iview(driver)
  link = driver$getPageSource()[[1]] %>% read_html() %>% html_nodes("a") %>% html_attr('href')
  link = link[fromto::is_in("/collections/",link)]
  link_PublicHealth = link[!duplicated(link)]

  igo(driver,paste0("https://www.biomedcentral.com/collections?subject=Life+Sciences"))
  iview(driver)
  link = driver$getPageSource()[[1]] %>% read_html() %>% html_nodes("a") %>% html_attr('href')
  link = link[fromto::is_in("/collections/",link)]
  link_LifeSciences = link[!duplicated(link)]

  link_BMC = c(link_Biomedicine,link_PublicHealth,link_LifeSciences)
  link_BMC = link_BMC[!duplicated(link_BMC)]
  link_BMC_SI = paste0("https://www.biomedcentral.com",link_BMC)

  fromto::save_txt(link_BMC_SI,"BMC_SI")
  return(link_BMC_SI)
}
si_BMC = function(driver) {
  library(RSelenium)
  library(rvest)
  library(dplyr)
  library(stringr)
  # 获取页面源代码
  page_source <- driver$getPageSource()[[1]]

  # 使用rvest解析HTML
  html_doc <- read_html(page_source)

  # 提取各个字段
  extracted_data <- list()

  # 1. 提取专刊标题 - 尝试多种选择器
  title <- ""

  # 方法1: h1标签
  title <- html_doc %>%
    html_node("h1") %>%
    html_text() %>%
    str_trim()

  # 方法2: 如果是collection页面的标题
  if(is.na(title) || title == "") {
    title <- html_doc %>%
      html_node("h1[data-test='collection-title']") %>%
      html_text() %>%
      str_trim()
  }

  # 方法3: 如果是特刊页面的b标签
  if(is.na(title) || title == "") {
    title <- html_doc %>%
      html_node("h3 b") %>%
      html_text() %>%
      str_trim()
  }

  extracted_data$title <- ifelse(is.na(title) || title == "", "", title)

  # 2. 提取截止时间 - 尝试多种模式
  deadline <- ""

  # 方法1: 新页面格式 "Submission Deadline: 30 April 2026"
  deadline_text <- html_doc %>%
    html_nodes("h3, p, div") %>%
    html_text() %>%
    str_subset("Submission Deadline:") %>%
    str_extract("\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}") %>%
    .[!is.na(.)]

  if(length(deadline_text) > 0) {
    deadline <- deadline_text[1]
  }

  # 方法2: 原格式
  if(deadline == "") {
    deadline <- html_doc %>%
      html_node(".text-xs.u-padding-xs-top strong") %>%
      html_text() %>%
      str_trim()

    if(is.na(deadline)) deadline <- ""
  }

  # 方法3: collection页面格式
  if(deadline == "") {
    deadline <- html_doc %>%
      html_node("date.app-collection-page-sidebar__date") %>%
      html_text() %>%
      str_trim()

    if(is.na(deadline)) deadline <- ""
  }

  extracted_data$deadline <- deadline

  # 3. 提取专刊信息与作者信息
  # 提取编辑信息
  editor_info <- ""

  # 方法1: 新页面格式 "Edited by:"
  edited_by_text <- html_doc %>%
    html_nodes("h3, p") %>%
    html_text() %>%
    str_subset("Edited by:")

  if(length(edited_by_text) > 0) {
    # 找到"Edited by:"后面的段落
    all_text <- html_doc %>%
      html_nodes("h3, p") %>%
      html_text() %>%
      str_trim()

    edited_idx <- which(str_detect(all_text, "Edited by:"))
    if(length(edited_idx) > 0) {
      # 取下一个段落作为编辑信息
      if(edited_idx[1] < length(all_text)) {
        editor_info <- all_text[edited_idx[1] + 1]
      }
    }
  }

  # 方法2: collection页面的editors section
  if(editor_info == "") {
    editor_names <- html_doc %>%
      html_nodes("h3.app-collection-editor__heading") %>%
      html_text() %>%
      str_trim()

    if(length(editor_names) > 0) {
      editor_info <- paste(editor_names, collapse = "; ")
    }
  }

  # 方法3: 原来的Guest editors格式
  if(editor_info == "") {
    all_paragraphs <- html_doc %>%
      html_nodes("p") %>%
      html_text() %>%
      str_trim()

    guest_editors_idx <- which(str_detect(all_paragraphs, "[Gg]uest [Ee]ditors?:"))

    if(length(guest_editors_idx) > 0) {
      start_idx <- guest_editors_idx[1] + 1
      end_idx <- min(start_idx + 3, length(all_paragraphs))
      potential_editors <- all_paragraphs[start_idx:end_idx]

      editor_paragraphs <- potential_editors[str_detect(potential_editors,
                                                        "[A-Z][a-zA-Z]+\\s+[A-Z][a-zA-Z]+")]

      if(length(editor_paragraphs) > 0) {
        editor_info <- paste(editor_paragraphs, collapse = "; ")
      }
    }
  }

  # 提取专刊描述信息
  collection_info <- ""

  # 方法1: collection description div
  collection_info <- html_doc %>%
    html_node("div[data-test='collection-description']") %>%
    html_text() %>%
    str_trim()

  # 方法2: About the Collection section
  if(is.na(collection_info) || collection_info == "") {
    about_section <- html_doc %>%
      html_node("#about\\+the\\+collection .cms-article__body") %>%
      html_nodes("p") %>%
      html_text() %>%
      str_trim()

    if(length(about_section) > 0) {
      # 取前两个段落作为描述
      collection_info <- paste(about_section[1:min(2, length(about_section))], collapse = " ")
    }
  }

  # 方法3: 通过关键词匹配寻找描述段落
  if(is.na(collection_info) || collection_info == "") {
    all_paragraphs <- html_doc %>%
      html_nodes("p") %>%
      html_text() %>%
      str_trim()

    # 寻找包含特定关键词的段落
    desc_patterns <- c("calling for submissions", "special issue", "collection",
                       "this issue", "manuscripts", "research", "welcome")

    for(pattern in desc_patterns) {
      matched_paras <- all_paragraphs[str_detect(all_paragraphs, regex(pattern, ignore_case = TRUE))]
      if(length(matched_paras) > 0) {
        collection_info <- matched_paras[which.max(nchar(matched_paras))]
        break
      }
    }
  }

  # 合并编辑信息和专刊信息
  combined_info <- ""
  if(editor_info != "" && collection_info != "") {
    combined_info <- paste("Editors:", editor_info, "\n\nCollection Info:", collection_info)
  } else if(editor_info != "") {
    combined_info <- paste("Editors:", editor_info)
  } else if(collection_info != "") {
    combined_info <- collection_info
  }

  extracted_data$info_and_editors <- combined_info

  # 4. 提取提交链接
  submission_link <- ""

  # 方法1: 新页面的Submit to Collection按钮
  submission_link <- html_doc %>%
    html_node("a[data-test='teaser-button-link']") %>%
    html_attr("href")

  # 方法2: collection页面的submit按钮
  if(is.na(submission_link)) {
    submission_link <- html_doc %>%
      html_node("a[data-test='submit-to-this-journal']") %>%
      html_attr("href")
  }

  # 方法3: 包含submission的链接
  if(is.na(submission_link)) {
    submission_link <- html_doc %>%
      html_node("a[href*='submission']") %>%
      html_attr("href")
  }

  # 方法4: editorialmanager链接
  if(is.na(submission_link)) {
    submission_link <- html_doc %>%
      html_node("a[href*='editorialmanager.com']") %>%
      html_attr("href")
  }

  extracted_data$submission_link <- ifelse(is.na(submission_link), "", submission_link)

  # 转换为data.frame
  result_df <- data.frame(
    title = extracted_data$title,
    deadline = extracted_data$deadline,
    details = extracted_data$info_and_editors,
    submission_link = extracted_data$submission_link,
    stringsAsFactors = FALSE
  )
  result_df$details = gsub("\n"," ",result_df$details)
  return(result_df)
}

si_Elsevier_easy = function(driver,links){
  infor_final = data.frame()
  for (link in links) {
    message(link)
    suppressMessages(
      tryCatch({
        igo(driver,link)
        isleep(runif_use = 1, min_use = 4, max_use = 6)
        iview(driver)
        infor = si_Elsevier(driver)
        infor$submission_link = link
        infor_final = rbind(infor_final,infor)
        fromto::save_txt(infor_final,"Elsevier",col.names_use = TRUE)
      }, error = function(e) {
        NULL
      })
    )
  }
  return(infor_final)
}
si_Elsevier_links = function(driver){
  igo(driver,"https://www.sciencedirect.com/browse/calls-for-papers?subject=health-sciences")
  iview(driver)
  links = driver$getPageSource()[[1]] %>% read_html() %>% html_nodes("a") %>% html_attr('href')
  links = links[fromto::is_in("/special-issue/",links)]
  links = paste0("https://www.sciencedirect.com",links)
  links = links[!duplicated(links)]
  return(links)
}
si_Elsevier <- function(driver) {
  library(RSelenium)
  library(rvest)
  library(dplyr)
  library(stringr)
  # 获取页面源代码
  page_source <- driver$getPageSource()[[1]]

  # 解析HTML
  html_doc <- read_html(page_source)
  title_journal <- driver$getTitle()
  # 提取标题 - 从h3标签中的b标签
  title <- html_doc %>%
    html_node("h3 b") %>%
    html_text(trim = TRUE)

  # 如果没有找到b标签，直接从h3提取
  if (is.na(title) || title == "") {
    title <- html_doc %>%
      html_node("h3") %>%
      html_text(trim = TRUE)
  }

  # 提取截止日期 - 从包含"Submission deadline"的div中提取
  deadline_element <- html_doc %>%
    html_node("div.text-xs.u-padding-xs-top")

  deadline <- ""
  if (!is.null(deadline_element)) {
    deadline_text <- html_text(deadline_element, trim = TRUE)
    # 提取日期部分 (例如: "25 July 2026")
    extracted_date <- str_extract(deadline_text, "\\d{1,2}\\s+\\w+\\s+\\d{4}")
    if (!is.na(extracted_date)) {
      deadline <- extracted_date
    } else {
      # 如果没有匹配到标准格式，返回原始文本
      deadline <- deadline_text
    }
  }

  # 提取详细信息 - 获取主要内容区域的所有p标签
  detail_paragraphs <- html_doc %>%
    html_nodes("div.inner p")

  # 提取所有段落的文本内容
  details_list <- c()
  for (p in detail_paragraphs) {
    p_text <- html_text(p, trim = TRUE)
    if (!is.na(p_text) && p_text != "") {
      details_list <- c(details_list, p_text)
    }
  }

  # 合并所有段落文本
  details <- ""
  if (length(details_list) > 0) {
    combined_text <- paste(details_list, collapse = " ")
    # 将换行符替换为空格，清理多余空格
    combined_text <- str_replace_all(combined_text, "\\n", " ")
    combined_text <- str_replace_all(combined_text, "\\s+", " ")
    details <- str_trim(combined_text)
  }

  # 提取提交链接 - 查找editorialmanager链接
  submission_link <- html_doc %>%
    html_node("a[href*='editorialmanager.com']") %>%
    html_attr("href")

  # 如果没有找到editorialmanager链接，查找其他提交相关链接
  if (is.na(submission_link)) {
    all_links <- html_doc %>%
      html_nodes("a") %>%
      html_attr("href")

    # 过滤掉NA值
    all_links <- all_links[!is.na(all_links)]

    if (length(all_links) > 0) {
      # 查找包含提交相关关键词的链接
      submit_links <- all_links[str_detect(all_links, "submit|manuscript|upload", ignore.case = TRUE)]
      if (length(submit_links) > 0) {
        submission_link <- submit_links[1]
      } else {
        # 如果都没有找到，取第一个http链接
        http_links <- all_links[str_detect(all_links, "^https?://")]
        if (length(http_links) > 0) {
          submission_link <- http_links[1]
        } else {
          submission_link <- ""
        }
      }
    } else {
      submission_link <- ""
    }
  }

  # 创建结果数据框
  result_df <- data.frame(
    title = ifelse(is.na(title) || title == "", "", title),
    submission_deadline = deadline,
    details = paste0(title_journal," ",details),
    submission_link = ifelse(is.na(submission_link), "", submission_link),
    stringsAsFactors = FALSE
  )

  return(result_df)
}
