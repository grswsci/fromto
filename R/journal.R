#' @title journal_abstract
#' @description journal abstract
#' @param docx journal_abstract.docx
#' @return txt html
journal_abstract = function(docx = "journal_abstract.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_split(doc_data$text, "\n"))
extracted_text = ifelse(nchar(extracted_text) <= 40,
                        paste0("<h1 class='articlegroup' id='",extracted_text,"'>",extracted_text,"</h1> <img  class='clickfig' onclick = openPopupFig('/static/logo.png'); src='/static/logo.png' alt='Graphic abstract' title='&copy; The Author. Life Conflux'>"),
                        paste0("<p><b>",extracted_text,"</b></p><p style='clear:both;'></p>")
                        )

write.table(extracted_text,"abstract.txt",quote = F,row.names = F)
write.table(extracted_text,"abstract.html",quote = F,row.names = F)
}
#' @title journal_text
#' @description journal text
#' @param docx journal_text.docx
#' @return txt html
journal_text = function(docx = "journal_text.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_extract_all(doc_data$text, "\\[(.*?)\\]"))
extracted_text = gsub(" ","",extracted_text)
extracted_text = gsub("[","",extracted_text,fixed = TRUE)
extracted_text = gsub("]","",extracted_text,fixed = TRUE)
process_string = function(str) {
  if (grepl(",", str)) {
    parts <- strsplit(str, ",", fixed = TRUE)[[1]][1:2]
    str1 <- parts[1]
    str2 <- parts[2]
    paste0("[<a href='#B",str1,"'",">",str1,"</a>,",
           "<a href='#B",str2,"'",">",str2,"</a>]")
  } else if(grepl("-", str)){
    parts <- strsplit(str, "-", fixed = TRUE)[[1]][1:2]
    str1 <- parts[1]
    str2 <- parts[2]
    paste0("[<a href='#B",str1,"'",">",str1,"</a>-",
           "<a href='#B",str2,"'",">",str2,"</a>]")
  }else {
    paste0("[<a href='#B",str,"'",">",str,"</a>]")
  }
}
processed_text = sapply(extracted_text, process_string)
extracted_text2 = unlist(str_split(doc_data$text, "\\[(.*?)\\]"))
extracted_text2 = paste0(extracted_text2,processed_text)
extracted_text2 = paste(extracted_text2, collapse = " ")
extracted_text2 = unlist(str_split(extracted_text2, "\n"))

extracted_text2 = ifelse(nchar(extracted_text2) <= 40,
                         paste0("<h1 class='articlegroup' id='",gsub(" ","",extracted_text2),"'>",extracted_text2,"</h1>"),
                         ifelse(nchar(extracted_text2) > 40 & nchar(extracted_text2)<= 100,
                                paste0("<h2>",extracted_text2,"</h2>"),paste0("<p>",extracted_text2,"</p>")))

write.table(extracted_text2,"正文.txt",quote = F,row.names = F)
write.table(extracted_text2,"正文.html",quote = F,row.names = F)
}
#' @title journal_reference_numbers_no
#' @description journal reference
#' @param docx journal_reference_numbers_no.docx
#' @return txt html
journal_reference_numbers_no = function(docx = "journal_reference_numbers_no.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_split(doc_data$text, "\n"))
extracted_text_new = c()
i = 0
for (variable in extracted_text) {
  i = i+1
  extracted_text_new_add = paste0("<p class='reference' id='B",i,"'>",i,". ",
                                  variable,"</p>")
  extracted_text_new = c(extracted_text_new, extracted_text_new_add)
}
write.table(extracted_text_new,"参考文献.txt",quote = F,row.names = F)
write.table(extracted_text_new,"参考文献.html",quote = F,row.names = F)
}
#' @title journal_reference_numbers_yes
#' @description journal reference
#' @param docx journal_reference_numbers_yes.docx
#' @return txt html
journal_reference_numbers_yes = function(docx = "journal_reference_numbers_yes.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_split(doc_data$text, "\n"))
extracted_text_new = c()
i = 0
for (variable in extracted_text) {
  i = i + 1
  extracted_text_new_add = paste0("<p class='reference' id='B",i,"'>",
                                  variable,"</p>")
  extracted_text_new = c(extracted_text_new, extracted_text_new_add)
}
write.table(extracted_text_new,"参考文献.txt",quote = F,row.names = F)
write.table(extracted_text_new,"参考文献.html",quote = F,row.names = F)
}
#' @title journal_figure_legends_jpg
#' @description journal figure legends jpg
#' @param docx journal_figure_legends_jpg.docx
#' @return txt html
journal_figure_legends_jpg = function(divname, docx = "journal_figure_legends_jpg.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_split(doc_data$text, "\n"))
extracted_text_new = c()
i = 0
fa_add_new = c()
for (variable in extracted_text) {
  i = i+1
  fa_add = paste0("<a href='#F",i,"'>Figure ",i,"</a>")
  fa_add_new = c(fa_add_new, fa_add)
  extracted_text_new_add = paste0("<div class='ivytablediv' id='F",i,"'><b>&nbsp;Figure ",i,
                                  "</b>&nbsp;<p>",
                                  variable,"</p>",
                                  "<img  class='dispnewfig' src='/static/published-articles-figure/",
                                  divname,
                                  "/Figure",i,".jpg' onclick = openPopupFig('/static/published-articles-figure/",
                                  divname, "/Figure",
                                  i,".jpg'); alt='Life Conflux Image'> </div>")
  extracted_text_new = c(extracted_text_new, extracted_text_new_add)
}
write.table(extracted_text_new,"Figure_Legends_jpg1.txt",quote = F,row.names = F)
write.table(fa_add_new,"Figure_Legends_jpg2.txt",quote = F,row.names = F)
}
#' @title journal_figure_legends_png
#' @description journal figure legends png
#' @param docx journal_figure_legends_png.docx
#' @return txt html
journal_figure_legends_png = function(divname, docx = "journal_figure_legends_png.docx"){
  library(readtext)
  library(stringr)
  doc_data = readtext(docx)
  extracted_text = unlist(str_split(doc_data$text, "\n"))
  extracted_text_new = c()
  i = 0
  fa_add_new = c()
  for (variable in extracted_text) {
    i = i+1
    fa_add = paste0("<a href='#F",i,"'>Figure. ",i,"</a>")
    fa_add_new = c(fa_add_new, fa_add)
    extracted_text_new_add = paste0("<div class='ivytablediv' id='F",i,"'><b>&nbsp;Figure ",i,
                                    "</b>&nbsp;<p>",
                                    variable,"</p>",
                                    "<img  class='dispnewfig' src='/static/published-articles-figure/",
                                    divname,
                                    "/Figure",i,".png' onclick = openPopupFig('/static/published-articles-figure/",
                                    divname, "/Figure",
                                    i,".png'); alt='Life Conflux Image'> </div>")
    extracted_text_new = c(extracted_text_new, extracted_text_new_add)
  }
  write.table(extracted_text_new,"Figure_Legends_png1.txt",quote = F,row.names = F)
  write.table(fa_add_new,"Figure_Legends_png2.txt",quote = F,row.names = F)
}
#' @title journal_table_jpg
#' @description journal_table_jpg
#' @param docx journal_table_jpg.docx
#' @return txt html
journal_table_jpg = function(divname, docx = "journal_table_jpg.docx"){
library(readtext)
library(stringr)
doc_data = readtext(docx)
extracted_text = unlist(str_split(doc_data$text, "\n"))
extracted_text_new = c()
i = 0
fa_add_new = c()
for (variable in extracted_text) {
  i = i+1
  fa_add = paste0("<a href='#T",i,"'>Table. ",i,"</a>")
  fa_add_new = c(fa_add_new, fa_add)
  extracted_text_new_add = paste0("<div class='ivytablediv' id='T",i,"'><b>&nbsp;Table ",i,
                                  "</b>&nbsp;<p>",
                                  variable,"</p>",
                                  "<img  class='dispnewfig' src='/static/published-articles-figure/",
                                  divname,
                                  "/Table",i,".jpg' onclick = openPopupFig('/static/published-articles-figure/",
                                  divname, "/Table",
                                  i,".jpg'); alt='Life Conflux Image'> </div>")
  extracted_text_new = c(extracted_text_new, extracted_text_new_add)
}
write.table(extracted_text_new,"journal_table_jpg1.txt",quote = F,row.names = F)
write.table(fa_add_new,"journal_table_jpg2.txt",quote = F,row.names = F)
}
