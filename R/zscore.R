zscore_row <- function(x) {
  rowmean = apply(x, 1, mean)
  rowsd = apply(x, 1, sd)
  rv = sweep(x, 1, rowmean,"-")
  rv = sweep(rv, 1, rowsd, "/")
  return(rv)
}

zscore_col <- function(x){
  colmean = apply(x, 2, mean)
  colsd = apply(x, 2, sd)
  cv = sweep(x, 2, colmean,"-")
  cv = sweep(cv, 2, colsd,"/")
  return(cv)
}

