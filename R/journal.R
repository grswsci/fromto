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
#' @title journal_editor
#' @description journal_editor
#' @param editor Editor-
#' @param section Cell Death Research
#' @param email email
#' @param institution institution
#' @param focus focus
#' @param chief TRUE/FALSE
#' @return txt html
journal_editor = function(editor,section,email,institution,focus,chief = TRUE){
  data = unlist(strsplit(editor,"\n"))
  for (variable in data) {
    variable = gsub(" ","-",variable)
    editorweb = paste0("{% extends 'myjournal/base.html' %}
{% block main-content %}
<div class='p-3' style='background-color: #f9f9f9; padding: 20px; border-radius: 8px; font-family: Arial, sans-serif; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);'>
    <div style='font-size: 1.5rem; font-weight: bold; color: #333; margin-bottom: 15px; border-bottom: 2px solid #ddd; padding-bottom: 5px;'>",ifelse(chief,"Editor-in-Chief","Editor")," of the ",section," Section</div>

    <div style='display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;'>
        <div>
            <h5>",strsplit_fromto(data,"-",2),"</h5>
            <ul style='list-style-type: disc; margin-left: 20px; color: #555;'>
                <li>M.D./Ph.D.</li>
                <li> </li>
                <li><a href='mailto:",email,"' style='color: #0a58ca; text-decoration: none; font-size: 1rem;'>",email,"</a></li>
                <li>",institution,"</li>
            </ul>
        </div>
        <img src='/static/Editors_id_photo/",editor,".jpg' alt='",strsplit_fromto(data,"-",2),"' style='width: auto; height: 120px; border-radius: 50%;'>
    </div>

    <div style='margin-bottom: 20px;'>
        <h5>Research Field</h5>
        <ul style='list-style-type: disc; margin-left: 20px; color: #555;'>
            <li></li>
            <li></li>
            <li></li>
        </ul>
    </div>

    <div style='margin-bottom: 20px;'>
        <h5>Biography</h5>
        <ul style='list-style-type: disc; margin-left: 20px; color: #555;'>
            <p>

            </p>
        </ul>
    </div>

    <div style='border-top: 2px solid #ddd; padding-top: 20px;'>
        <h5>Selected Publications</h5>
        <ol style='list-style-type: decimal; margin-left: 20px; color: #555; line-height: 1.6;'>
            <li>Wei Wang, et al. Machine Learning-Driven Identification of Critical Gene Programs and Key Transcription Factors in Migraine, <em>Journal of Headache and Pain.</em> 2024.</li>
        </ol>
    </div>
</div>
{% endblock %}")
    write.table(editorweb,paste0(variable,".html"))
  }

  data2 = paste0("def ",gsub("-","_",data),"(request): \n    ","return render(request, 'myjournal/Editors/",gsub(" ","-",data),".html')")
  #write.table(data2,paste0("views.py.txt"),quote = FALSE,row.names = FALSE)

  data3 = paste0("    path('",gsub(" ","-",data),"/', views.",gsub("-","_",data),",name='",gsub(" ","-",data),"'),")
  #write.table(data3,paste0("urls.py.txt"),quote = FALSE,row.names = FALSE)
  ##############主编##############
  if(chief){
    data4 = paste0("<hr><!--",section,"--><table class='tableframe'><tr><td><div class='img-shadow'><img src='https://www.lifeconflux.com/static/Editors_id_photo/",data,".jpg' alt='",strsplit_fromto(data,"-",2),"'  width=100></div></td><td></td><td></td><td><strong><a href='https://www.lifeconflux.com/",editor,"/' style='color: #336699; text-decoration: none; font-weight: bold; transition: color 0.3s, border-bottom 0.3s;' target='_blank' rel='noopener noreferrer'>",strsplit_fromto(data,"-",2),"</a></strong><br>Degree: M.D./Ph.D.<br>Institution: ",institution,"<br>Email: ",email,"<br>Focus: ",focus,"<br><strong>Section: <a href='https://www.lifeconflux.com/",gsub(" ","-",section),"/' style='color: #336699; text-decoration: none; font-weight: bold; transition: color 0.3s, border-bottom 0.3s;' target='_blank' rel='noopener noreferrer'>",section,"</a></strong><br><button onclick=",'"',"toggleVisibility('",gsub(" ","-",section),"ers')",'"'," style='display: inline-block; background-color: #4CAF50; color: white; border: none; padding: 4px 8px; /* 减小了填充 */ font-size: 0.75em; /* 减小了字体大小 */ font-family: Arial, sans-serif; border-radius: 20px; cursor: pointer; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); font-weight: 500; letter-spacing: 0.5px; outline: none; line-height: 1.2;' onmouseover=",'"',"this.style.backgroundColor='#45a049'; this.style.boxShadow='0 3px 6px rgba(0, 0, 0, 0.2)'; this.style.transform='translateY(-1px)';",'"'," onmouseout=",'"',"this.style.backgroundColor='#4CAF50'; this.style.boxShadow='0 2px 4px rgba(0, 0, 0, 0.1)'; this.style.transform='translateY(0px)';",'"'," onfocus=",'"',"this.style.outline='2px solid #45a049'; this.style.outlineOffset='4px';",'"'," onblur=",'"',"this.style.outline='none';",'"'," onmousedown=",'"',"this.style.backgroundColor='#3e8e41'; this.style.boxShadow='0 1px 3px rgba(0, 0, 0, 0.2)'; this.style.transform='translateY(1px)';",'"'," onmouseup=",'"',"this.style.backgroundColor='#45a049'; this.style.boxShadow='0 3px 6px rgba(0, 0, 0, 0.2)'; this.style.transform='translateY(-1px)';",'"',">Members</button><br></td></tr></table>
                    <div id='",gsub(" ","-",section),"ers' class='collapsible-content'>
                    </div>")

    #write.table(data4,paste0("Editor.Chief.txt"),quote = FALSE,row.names = FALSE)
  }else{
    data4 = paste0("<hr><table class='tableframe' ><tr><td><div class='img-shadow'><img src='https://www.lifeconflux.com/static/Editors_id_photo/",data,".jpg' alt='",strsplit_fromto(data,"-",2),"'  width=100></div></td><td></td><td></td><td><strong><a href='https://www.lifeconflux.com/",editor,"/' style='color: #336699; text-decoration: none; font-weight: bold; transition: color 0.3s, border-bottom 0.3s;' target='_blank' rel='noopener noreferrer'>",strsplit_fromto(data,"-",2),"</a></strong><br>Degree: M.D./Ph.D.<br>Institution: ",institution,"<br>Email: ",email,"<br>Focus: ",focus,"<br><strong>Section: <a href='https://www.lifeconflux.com/",gsub(" ","-",section),"/' style='color: #336699; text-decoration: none; font-weight: bold; transition: color 0.3s, border-bottom 0.3s;' target='_blank' rel='noopener noreferrer'>",section,"</a></strong><br></td></tr></table>")
  }

  data5 = paste0("<a href='https://www.lifeconflux.com/",data,"/' style='text-decoration: none; color: inherit; flex: 1 1 calc(33.333% - 40px); max-width: calc(33.333% - 40px);'><div style='height: 320px; background-color: #fff; padding: 20px; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'><img src='/static/Editors_id_photo/",data,".jpg' alt='",strsplit_fromto(data,"-",2),"' style='width: auto; height: 120px; border-radius: 50%; margin-bottom: 15px; display: block; margin-left: auto; margin-right: auto;'><h6 style='margin: 0 0 10px; text-align: center;'>",strsplit_fromto(data,"-",2),"</h6><p style='margin: 0 0 10px; text-align: center;'>",ifelse(chief,"Editor-in-Chief","Editor"),"</p><p style='margin: 0 0 10px; text-align: center;'>",institution,"</p></div></a>")

  data_all = paste0(data,"\n","\n",
                    data2,"\n","\n",
                    data3,"\n","\n",
                    data4,"\n","\n",
                    data5)
  if(chief){
    write.table(data_all,paste0("Editor.Chief.txt"),quote = FALSE,row.names = FALSE)
  }else{
    write.table(data_all,paste0("Editor.txt"),quote = FALSE,row.names = FALSE)
  }
}

