
col.order <- function(d, n){
  d <- d[, c(n, names(d)[!(names(d) %in% n)])]
  return(d)
}

mp <- function(x) paste0(sort(unlist(strsplit(as.character(x), "_"))), collapse = "_")
