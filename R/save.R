save_txt = function(df,
                    name_use = "name_use",
                    quote_use = FALSE,
                    row.names_use = FALSE,
                    col.names_use = FALSE){
  write.table(df,
              paste0(name_use,".txt"),
              sep = "\t",
              quote = quote_use,
              row.names = row.names_use,
              col.names = col.names_use)
}


save_qs = function(df,name_use = "name_use"){
  qs::qsave(df,paste0(name_use,".qs"))
}
