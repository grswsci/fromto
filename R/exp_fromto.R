exp_fromto <- function(df){
  suppressPackageStartupMessages(library(tidyverse))
  df = df %>% mutate(across(everything(), ~ (exp(.))))
  return(df)
}

