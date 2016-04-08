
col.order <- function(d, n){
  d <- d[, c(n, names(d)[!(names(d) %in% n)])]
  return(d)
}


# Internal Movement path function (unique for each feeder path)
mp <- function(x) {
  x$move_path <- paste0(sort(unlist(strsplit(as.character(x$move_dir), "_"))), collapse = "_")
  return(x)
}
