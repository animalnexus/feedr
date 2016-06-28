# A scaling function used by mapping functions
scale_area <- function(r, radius = FALSE,
                       min = 5, max = 105,
                       val_max = NULL, val_min = NULL){

  if(is.null(val_max)) val_max = max(r, na.rm = TRUE)
  if(is.null(val_min)) val_min = min(r, na.rm = TRUE)

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
                     options = layersControlOptions(collapsed = FALSE))
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
  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")
  if(any(lat %in% names(d)) & any(lon %in% names(d)) & "feeder_id" %in% names(d)) {
    if(sum(lat %in% names(d)) > 1 | sum(lon %in% names(d)) > 1) stop(paste0("Muliple latitude or longitudes in data possible. Looking for latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "),")"))
    d <- rename_locs(d)
    locs <- d[ , c("feeder_id", "lat", "lon")]
    return(locs)
  } else return(data.frame())
}
