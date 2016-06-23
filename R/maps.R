# A scaling function used by mapping functions
scale.area <- function(r, radius = FALSE,
                       min = 5, max = 105,
                       val.max = NULL, val.min = NULL){

  if(is.null(val.max)) val.max = max(r, na.rm = TRUE)
  if(is.null(val.min)) val.min = min(r, na.rm = TRUE)

   if(radius) {
     min <- pi * (min)^2
     max <- pi * (max)^2
  }
  r <- as.numeric(r) - val.min
  r <- ((r / (val.max - val.min)) * (max - min)) + min

  if(radius) r <- sqrt(r/pi)

  return(r)
}

controls <- function(map, group) {
  c <- list(character(0), group)
  c.old <- grep("addLayersControl", map$x$calls)
  if(length(c.old > 0)){

    c[[1]] <- map$x$calls[[c.old[length(c.old)]]]$args[[1]]
    c[[2]] <- unique(c(map$x$calls[[c.old[length(c.old)]]]$args[[2]], c[[2]]))
  }
  map <- removeLayersControl(map) %>%
    addLayersControl(baseGroups = c[[1]], overlayGroups = c[[2]],
                     options = layersControlOptions(collapsed = FALSE))
  return(map)
}

rename.locs <- function(d) {
  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")
  names(d)[names(d) %in% lat] <- "lat"
  names(d)[names(d) %in% lon] <- "lon"
  return(d)
}

get.locs <- function(d) {
  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")
  if(any(lat %in% names(d)) & any(lon %in% names(d)) & "feeder_id" %in% names(d)) {
    if(sum(lat %in% names(d)) > 1 | sum(lon %in% names(d)) > 1) stop(paste0("Muliple latitude or longitudes in data possible. Looking for latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "),")"))
    d <- rename.locs(d)
    locs <- d[ , c("feeder_id", "lat", "lon")]
    return(locs)
  } else return(data.frame())
}


# Prep data for mapping
# A data prep function used by mapping functions
map.prep <- function(u = NULL, p = NULL, locs = NULL) {

  # Check data
  if(!is.null(u)){
    if(!all(c("feeder_id","amount") %in% names(u))) stop("The use dataframe (u) requires two columns: 'feeder_id' and 'amount'.")
  }
  if(!is.null(p)){
    if(!all(c("move_path","path_use") %in% names(p))) stop("The path dataframe (p) requires two columns: 'move_path' and 'path_use'.")
    if(any(stringr::str_count(p$move_path, "_") > 1)) stop("Using '_' in feeder_id names conflicts with the mapping functions. You should remove any '_'s.")
  }

  # Check and format location data
  # Lat / Lon needs to be in EITHER locs, u, OR p

  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")

  locs <- unique(rbind(get.locs(u), get.locs(p), get.locs(locs)))
  if(nrow(locs) == 0) stop(paste0("Locations (locs) dataframe is missing either latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "), "), or both."))

  # Get locations and alert if any missing
  if(!is.null(u)){
    u <- rename.locs(u)
    n <- names(u)[names(u) %in% c("feeder_id", "lat", "lon")]
    u <- merge(u, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(u[is.na(u$lat) | is.na(u$lon),]) > 0) message(paste0("Removed ", nrow(u[is.na(u$lat) | is.na(u$lon),]), " feeders due to missing lat or lon."))
    u <- u[!(is.na(u$lat) | is.na(u$lon)),]
  }

  if(!is.null(p)){
    p <- rename.locs(p)
    n <- names(p)[names(p) %in% c("feeder_id", "lat", "lon")]
    p <- merge(p, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(p[is.na(p$lat) | is.na(p$lon),]) > 0) message(paste0("Removed ", nrow(p[is.na(p$lat) | is.na(p$lon),]), " paths due to at least one missing lat or lon."))
    p <- dplyr::group_by(p, move_path) %>%
      dplyr::do(if(any(is.na(.[,c('lat','lon')]))) return(data.frame()) else return(.)) %>%
      dplyr::ungroup()
  }

  if(!is.null(u)) if(nrow(u) == 0) stop("Missing use lat/lon data, did you supply location data in either u, p, or locs?")
  if(!is.null(p)) if(nrow(p) == 0) stop("Missing path lat/lon data, did you supply location data in either u, p, or locs?")

  return(list('u' = u, 'p' = p, 'locs' = locs))
}

#' Base map for leaflet
#'
#' Designed for advanced use (see map.leaflet() for general mapping)
#' @export
map.leaflet.base <- function(locs, marker = "feeder_id", name = "Readers", controls = TRUE) {
  leaflet(data = locs) %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addMarkers(~lon, ~lat,
                     popup  = ~ htmltools::htmlEscape(marker),
                     group = name) %>%
    addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                     overlayGroups = name,
                     options = layersControlOptions(collapsed = FALSE))
}

#' Add movement layer to leaflet map
#'
#' Designed for advanced use (see map.leaflet() for general mapping)
#'
#' @import leaflet
#' @export
path.layer <- function(map, p,
                       p.scale = 1, p.title = "Path use",
                       p.pal = c("yellow","red"), controls = TRUE) {

  if(!(all(c("lat", "lon") %in% names(p)))) stop("Missing path lat/lon data, did you supply location data?")

  p <- dplyr::arrange(p, desc(path_use))
  p$use <- round(p$path_use, digits = if(max(p$path_use) < 10) 1 else 0)

  # Define palette
  p.pal <- colorNumeric(palette = colorRampPalette(p.pal)(15), domain = p$path_use)

  # Add movement path lines to map
  for(path in unique(p$move_path)) {
    map <- addPolylines(map,
                        data = p[p$move_path == path, ],
                        ~lon, ~lat,
                        weight = ~scale.area(path_use, max = 50 * p.scale,
                                             val.max = max(p$path_use),
                                             val.min = min(p$path_use)),
                        opacity = 0.5,
                        color = ~p.pal(path_use),
                        group = p.title,
                        popup = ~htmltools::htmlEscape(as.character(use)))
  }

  map <- map %>% addLegend(title = p.title,
                           position = 'topright',
                           pal = p.pal,
                           opacity = 1,
                           values = p$path_use) %>%
    hideGroup("Reader") %>%
    showGroup("Reader")

  if(controls) map <- controls(map, p.title)

  return(map)
}

#' Add feeding layer to leaflet map
#'
#' Designed for advanced use (see map.leaflet() for general mapping)
#'
#' @import leaflet
#' @export
use.layer <- function(map, u, u.scale = 1, u.title = "Time", u.pal = c("yellow","red"), controls = TRUE) {
  if(!(all(c("lat", "lon") %in% names(u)))) stop("Missing use lat/lon data, did you supply location data?")
  u$amount <- as.numeric(u$amount)
  u <- u[order(u$amount, decreasing = TRUE),]

  # Define palette
  u.pal <- colorNumeric(palette = colorRampPalette(u.pal)(15), domain = u$amount)

  # Add feeder use data to map
  map <- addCircleMarkers(map,
                           data = u,
                           ~lon, ~lat,
                           weight = 1,
                           opacity = 1,
                           fillOpacity = 0.5,
                           radius = ~ scale.area(amount, max = 50 * u.scale, radius = TRUE),
                           color = "black",
                           fillColor = ~u.pal(amount),
                           popup = ~htmltools::htmlEscape(as.character(round(amount))),
                           group = u.title) %>%
    addLegend(title = u.title,
              position = 'topright',
              pal = u.pal,
              values = u$amount,
              bins = 5,
              opacity = 1) %>%
    hideGroup("Readers") %>%
    showGroup("Readers")

  if(controls) map <- controls(map, u.title)
  return(map)
}

# ----------------------------------
# map.leaflet()
# ----------------------------------
#' Map data using leaflet
#'
#' Visualize feeder use and path data using leaflet for R. This produces
#' an interactive html map. The user must summarize feeder use and path data in
#' the manner that they wish to visualize it. This function can take invidiual
#' bird data as well as grand summarise (see Details and Examples).
#'
#' @param u Dataframe. Summarized 'use' data with columns \code{feeder_id} and
#'   \code{amount}. It can also contain \code{bird_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param p Dataframe. Summarized 'path' data with columns \code{feeder_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{bird_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and long for each feeder_id, required if lat and
#'   lon not in either u or p.
#' @param u.scale,p.scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of use (u) and path (p) data.
#' @param u.title,p.title Character. Titles for the legends of use (u) and
#'   path (p) data.
#' @param u.pal,p.pal Character vectors. Colours used to construct gradients for
#'   use (u) and path (p) data.
#' @param controls Logical. Add controls to map (allows showing/hiding of
#'   different layers)
#'
#' @return An interactive leaflet map with layers for use, paths and feeder positions.
#'
#' @examples
#' \dontrun{
#'
#' # Get feeding and movement data
#'
#' library(plyr)
#'
#' v <- visits(finches)
#' f <- ddply(v, .(bird_id), feeding, bw = 15)
#' m <- ddply(v, .(bird_id), move)
#'
#' # Get feeder locations from data
#' l <- unique(f[, c("feeder_id", "lat", "lon")])
#'
#' # OR get feeder locations from file
#' l <- read.csv("feeder index.csv")
#'
#' # Summarise data for visualization (use totals):
#' f.all <- ddply(f, .(feeder_id), summarise,
#'            amount = sum(feed_length) / bird_n[1])
#'
#' m.all <- ddply(m, .(feeder_id, move_path), summarise,
#'            path_use = length(move_path) / bird_n[1])
#'
#' # Look at total summary maps
#' map.leaflet(u = f.all, p = m.all, locs = l)
#' map.ggmap(u = f.all, p = m.all, locs = l)
#'
#' # Summarise data for visualization (use individuals):
#' f.indiv <- ddply(f, .(bird_id, feeder_id), summarise,
#'            amount = sum(feed_length))
#'
#' m.indiv <- ddply(m, .(bird_id, feeder_id, move_path), summarise,
#'            path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#'   one on top of the other)
#' map.leaflet(u = f.indiv, p = m.indiv, locs = l)
#' map.ggmap(u = f.indiv, p = m.indiv, locs = l)
#' }
#' @export
#' @import leaflet
map.leaflet <- function(u = NULL, p = NULL, locs = NULL,
                 u.scale = 1, p.scale = 1,
                 u.title = "Time", p.title = "Path use",
                 u.pal = c("yellow","red"),
                 p.pal = c("yellow","red"),
                 controls = TRUE) {

  data <- map.prep(u = u, p = p, locs = locs)
  u <- data[['u']]
  p <- data[['p']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(u) == "bird_id", names(p) == "bird_id")) bird_id <- unique(unlist(list(u$bird_id, p$bird_id))) else bird_id = NULL

  # BASE MAP
  map <- map.leaflet.base(locs = locs)

  # Layers
  if(!is.null(p)) map <- path.layer(map, p = p, p.scale = p.scale, p.pal = p.pal, p.title = p.title)
  if(!is.null(u)) map <- use.layer(map, u = u, u.scale = u.scale, u.pal = u.pal, u.title = u.title)

  return(map)
}

# ----------------------------------
# map.ggmap()
# ----------------------------------
#' Map data using ggmap
#'
#' Visualize feeder use and path data using ggmap in R. This produces a
#' static map. The user must summarize feeder use and path data in the manner
#' that they wish to visualize it. This function can take invidiual bird data as
#' well as grand summarise (see Details and Examples).
#'
#' @param u Dataframe. Summarized feeding data with columns \code{feeder_id} and
#'   \code{amount}. It can also contain \code{bird_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param p Dataframe. Summarized movement data with columns \code{feeder_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{bird_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and long for each feeder_id, required if lat and
#'   lon not in either u or p.
#' @param u.scale,p.scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of use (u) and path (p) data.
#' @param u.title,p.title Character. Titles for the legends of use (u) and
#'   path (p) data.
#' @param u.pal,p.pal Character vectors. Colours used to construct gradients for
#'   use (u) and movement (p) data.
#' @param maptype Character. The type of map to download. See \code{maptype}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param mapsource Character. The source of the map to download. See
#'   \code{source} under \code{\link[ggmap]{get_map}} for more options.
#' @param zoom Numeric. The zoom level of the map to download. See \code{zoom}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param which Character vector. A vector of bird ids specifying which to show.
#'   Only applies when using individual data.
#'
#' @return An interactive leaflet map with layers for feeding time, movement
#'   paths and feeder positions.
#'
#' @examples
#' \dontrun{
#'
#' v <- visits(finches)
#' f <- ddply(v, .(bird_id), feeding, bw = 15)
#' m <- ddply(v, .(bird_id), move)
#'
#' # Summarise data for visualization (use totals):
#' f.all <- ddply(f, .(feeder_id, lat, lon), summarise,
#'            amount = sum(feed_length) / bird_n)
#'
#' m.all <- ddply(m, .(move_path), summarise,
#'            path_use = length(move_path) / bird_n)
#'
#' # Look at total summary maps
#' map.leaflet(u = f.all, p = m.all)
#' map.ggmap(u = f.all, p = m.all)
#'
#' # Summarise data for visualization (use individuals):
#' f.indiv <- ddply(f, .(bird_id, feeder_id, lat, lon), summarise,
#'            amount = sum(feed_length))
#'
#' m.indiv <- ddply(m, .(bird_id, move_path), summarise,
#'            path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#' one on top of the other)
#' map.leaflet(u = f.indiv, p = m.indiv)
#' map.ggmap(u = f.indiv, p = m.indiv)
#' }
#' @export
map.ggmap <- function(u = NULL, p = NULL, locs = NULL,
                 u.scale = 1, p.scale = 1,
                 u.title = "Time", p.title = "Path use",
                 u.pal = c("yellow","red"),
                 p.pal = c("yellow","red"),
                 maptype = "satellite",
                 mapsource = "google",
                 zoom = 17,
                 which = NULL) {

  # Prep and Check Data
  data <- map.prep(u = u, p = p, locs = locs)
  u <- data[['u']]
  p <- data[['p']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(u) == "bird_id", names(p) == "bird_id")) {
    if(is.null(which)) which <- as.character(unique(c(as.character(u$bird_id), as.character(p$bird_id))))
    bird_id <- unique(unlist(list(u$bird_id, p$bird_id)))
    if(length(bird_id) > 10 & length(which) > 10) {
      stop("You have chosen to run this function on more than 10 birds. This may overload your system. We recommend trying again using the 'which' argument to specify a subset of birds.")
    }
    u <- droplevels(u[u$bird_id %in% which,])
    p <- droplevels(p[p$bird_id %in% which,])

   # temp.u <- plyr::ddply(u, "bird_id", summarize, sum = sum(amount))
    temp.u <- dplyr::group_by(u, bird_id) %>% summarize(sum = sum(amount))
    #temp.p <- plyr::ddply(p, "bird_id", summarize, sum = length(path_use))
    temp.p <- dplyr::group_by(p, bird_id) %>% summarize(sum = length(path_use))

    keep.id <- intersect(temp.u$bird_id[temp.u$sum > 0], temp.p$bird_id[temp.p$sum > 0])
    if(length(setdiff(which, keep.id)) > 0) {
      message(paste0("Some bird_ids removed due to lack of data: ", paste(setdiff(which, keep.id), collapse = ", ")))
      u <- droplevels(u[u$bird_id %in% keep.id, ])
      p <- droplevels(p[p$bird_id %in% keep.id, ])
    }
  } else bird_id = NULL

  # Final Data Prep
  if(!is.null(u)){
    # Sort and Scale
    u$amount <- as.numeric(u$amount)
  #  u <- u[order(u$amount, decreasing = TRUE),]
    u$amount2 <- scale.area(u$amount, max = 150 * u.scale)
  }

  if(!is.null(p)){
  #  # Sort and Scale
    p <- p[order(p$path_use, decreasing = TRUE), ]
    p$path_use2 <- scale.area(p$path_use, max = 1 * p.scale)
  }

  # Basic Map (reverse order to make sure feeders are on top)
  visual <- ggmap::get_map(location = c(median(locs$lon), median(locs$lat)),
                           zoom = zoom,
                           source = mapsource,
                           maptype = maptype)
  map <- ggmap::ggmap(visual,
                       legend = "topleft",
                       ylab = "Latitude",
                       xlab = "Longitude") +
          ggplot2::labs(x = "Longitude", y = "Latitude")

  # If feeding data specified
  if(!is.null(u)) {
    map <- map +
      ggplot2::geom_point(data = u, ggplot2::aes(x = lon, y = lat, fill = amount, size = amount2), shape = 21, alpha = 0.75) +
      ggplot2::scale_fill_gradientn(name = u.title, colours = u.pal) +
      ggplot2::scale_size_area(guide = FALSE, max_size = 40)
  }

  # If movement paths specified
  if(!is.null(p)){
    map <- map +
      ggplot2::geom_path(data = p, ggplot2::aes(x = lon,
                                                y = lat,
                                                group = move_path,
                                                colour = path_use,
                                                size = path_use2),
                         lineend = "round", alpha = 0.75) +
      ggplot2::scale_colour_gradientn(name = p.title, colours = p.pal)
  }

  # Add feeder points
  map <- map + ggplot2::geom_point(data = locs, ggplot2::aes(x = lon, y = lat), colour = "black")

  # If Multiple bird ids included
  if(!is.null(bird_id)) {
    map <- map + ggplot2::facet_wrap(~ bird_id)
    message("You have specified multiple birds and static maps, this means that an individual map will be drawn for each bird. This may take some time to display in the plots window.")
  }
  return(map)
}
