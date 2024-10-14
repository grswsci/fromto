list2df <- function(original_list){
  element_data_list = lapply(names(original_list), function(pathway) {
    data.frame(
      term = rep(pathway, length(original_list[[pathway]])),
      element = original_list[[pathway]],
      stringsAsFactors = FALSE
    )
  })
  transformed_df = do.call(rbind, element_data_list)
  return(transformed_df)
}

list2df_rbind <- function(original_list){
  transformed_df = do.call(rbind, original_list)
  return(transformed_df)
}

list2df_cbind <- function(original_list){
  transformed_df = do.call(cbind, original_list)
  return(transformed_df)
}
