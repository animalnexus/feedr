
col.order <- function(d, n){
  d <- d[,c(n, names(r)[!(names(r) %in% n)])]
  return(d)
}


#' Internal Movement path function (unique for each feeder path)
mp <- function(x) {
  x$move_path <- paste(as.character(sort(unlist(x[,c("feeder_left","feeder_arrived")]))),collapse = "_")
  return(x)
}
