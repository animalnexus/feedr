# A scaling function used by mapping functions
scale_area <- function(r, radius = FALSE,
                       min = 10, max = 105,
                       val_max = NULL, val_min = NULL){

  if(is.null(val_max)) val_max = max(r, na.rm = TRUE)
  if(is.null(val_min)) val_min = min(r, na.rm = TRUE)

  if(val_max == val_min) {
    val_min <- val_min * 0.5
    val_max <- val_max * 1.5
  }

  if(radius) {
    min <- pi * (min)^2
    max <- pi * (max)^2
  }
  r <- as.numeric(r) - val_min
  r <- ((r / (val_max - val_min)) * (max - min)) + min

  if(radius) r <- sqrt(r/pi)

  return(r)
}

controls <- function(map, group) {
  c <- list(character(0), group)
  c_old <- grep("addLayersControl", map$x$calls)
  if(length(c_old > 0)){

    c[[1]] <- map$x$calls[[c_old[length(c_old)]]]$args[[1]]
    c[[2]] <- unique(c(map$x$calls[[c_old[length(c_old)]]]$args[[2]], c[[2]]))
  }
  map <- removeLayersControl(map) %>%
    addLayersControl(baseGroups = c[[1]], overlayGroups = c[[2]],
                     options = layersControlOptions(collapsed = TRUE))
  return(map)
}

rename_locs <- function(d) {
  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")
  names(d)[names(d) %in% lat] <- "lat"
  names(d)[names(d) %in% lon] <- "lon"
  return(d)
}

get_locs <- function(d) {
  lat <- "lat"
  lon <- "lon"

  if(any(lat %in% names(d)) &
     any(lon %in% names(d)) &
     "logger_id" %in% names(d)) {

    if(sum(lat %in% names(d)) > 1 | sum(lon %in% names(d)) > 1) {
      stop(paste0("Muliple latitude or longitudes in data possible. ",
                  "Looking for latitude (", paste0(lat, collapse = ", "),
                  ") or longitude (", paste0(lon, collapse = ", "),")"))
    }

    locs <- dplyr::ungroup(d) %>%
      dplyr::select(logger_id, lat, lon) %>%
      dplyr::distinct()
    return(locs)
  } else return(data.frame())
}

add_locs <- function(d, locs) {

 if(all(c("lat", "lon") %in% names(d))) {
   by <- c("logger_id", "lat", "lon")
 }
  else by <- "logger_id"

 d <- dplyr::left_join(d, locs, by = by)

 l <- which(is.na(d$lat) | is.na(d$lon))

 if("move_path" %in% names(d)) {
   d_final <- dplyr::filter(d, !(.data$move_path %in% unique(d$move_path[l])))
 } else {
   d_final <- dplyr::filter(d, !(.data$logger_id %in% unique(d$logger_id[l])))
 }
 l <- unique(d$logger_id[l])

 if(nrow(d) > nrow(d_final)) {
   message("Removed logger",
           ifelse(length(l) > 1, "s ", " "),
           paste0(sort(l), collapse = ", "),
           " due to missing lat or lon.")
 }

 d_final
}

