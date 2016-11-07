
# Prep data for mapping
# A data prep function used by mapping functions
map_prep <- function(u = NULL, p = NULL, locs = NULL) {

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

  locs <- unique(dplyr::bind_rows(get_locs(u), get_locs(p), get_locs(locs)))
  if(nrow(locs) == 0) stop(paste0("Locations (locs) dataframe is missing either latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "), "), or both."))

  # Get locations and alert if any missing
  if(!is.null(u)){
    u <- rename_locs(u)
    n <- names(u)[names(u) %in% c("feeder_id", "lat", "lon")]
    u <- merge(u, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(u[is.na(u$lat) | is.na(u$lon),]) > 0) message(paste0("Removed ", nrow(u[is.na(u$lat) | is.na(u$lon),]), " feeders due to missing lat or lon."))
    u <- u[!(is.na(u$lat) | is.na(u$lon)),]
  }

  if(!is.null(p)){
    p <- rename_locs(p)
    n <- names(p)[names(p) %in% c("feeder_id", "lat", "lon")]
    p <- merge(p, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(p[is.na(p$lat) | is.na(p$lon),]) > 0) message(paste0("Removed ", nrow(p[is.na(p$lat) | is.na(p$lon),]), " paths due to at least one missing lat or lon."))
    p <- dplyr::group_by(p, move_path) %>%
      dplyr::do(if(any(is.na(.[,c('lat','lon')]))) return(data.frame()) else return(.)) %>%
      dplyr::ungroup()
  }

  #if(!is.null(u)) if(nrow(u) == 0) stop("Missing use lat/lon data, did you supply location data in either u, p, or locs?")
  #if(!is.null(p)) if(nrow(p) == 0) stop("Missing path lat/lon data, did you supply location data in either u, p, or locs?")

  return(list('u' = u, 'p' = p, 'locs' = locs))
}

#' Base map for leaflet
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
#' @export
map_leaflet_base <- function(locs, marker = "feeder_id", name = "Loggers", controls = TRUE) {
  l <- leaflet(data = locs) %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addCircleMarkers(~lon, ~lat,
                     popup  = htmltools::htmlEscape(as.character(unlist(locs[, marker]))),
                     group = name,
                     weight = 1,
                     opacity = 1,
                     fillOpacity = 1,
                     fillColor = "black",
                     color = "black",
                     radius = 5)
  if(controls) {
    l <- l %>%
      addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                       overlayGroups = name,
                       options = layersControlOptions(collapsed = TRUE))
  }
  return(l)
}

map.leaflet.base <- function(locs, marker = "feeder_id", name = "Loggers", controls = TRUE) {
  .Deprecated("map_leaflet_base")
}

#' Add movement layer to leaflet map
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
#'
#' @import leaflet
#' @export
path_layer <- function(map, p,
                       p_scale = 1, p_title = "Path use",
                       p_pal = c("yellow", "red"), controls = TRUE,
                       p.scale, p.title, p.pal) {

  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- p.scale
  }

  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- p.title
  }

  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- p.pal
  }

  if(!(all(c("lat", "lon") %in% names(p)))) stop("Missing path lat/lon data, did you supply location data?")

  p <- dplyr::arrange(p, desc(path_use))
  p$use <- round(p$path_use, digits = if(max(p$path_use) < 10) 1 else 0)

  # Define palette
  p_pal <- colorNumeric(palette = colorRampPalette(p_pal)(15), domain = p$path_use)

  # Add movement path lines to map
  for(path in unique(p$move_path)) {
    map <- addPolylines(map,
                        data = p[p$move_path == path, ],
                        ~lon, ~lat,
                        weight = ~scale_area(path_use, max = 50 * p_scale,
                                             val_max = max(p$path_use),
                                             val_min = min(p$path_use)),
                        opacity = 0.5,
                        color = ~p_pal(path_use),
                        group = p_title,
                        popup = ~htmltools::htmlEscape(as.character(use)))
  }

  map <- map %>% addLegend(title = p_title,
                           position = 'topright',
                           pal = p_pal,
                           opacity = 1,
                           values = p$path_use) %>%
    hideGroup("Loggers") %>%
    showGroup("Loggers")

  if(controls) map <- controls(map, p_title)

  return(map)
}

path.layer <- function(map, p,
                       p_scale = 1, p_title = "Path use",
                       p_pal = c("yellow","red"), p.scale, p.title, p.pal, controls = TRUE) {
  .Deprecated("path_layer")
}

#' Add feeding layer to leaflet map
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
#'
#' @import leaflet
#' @export
use_layer <- function(map, u,
                      u_scale = 1, u_title = "Time", u_pal = c("yellow","red"),
                      controls = TRUE,
                      u.scale, u.title, u.pal) {
  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use u_scale instead.",
            call. = FALSE)
    u_scale <- u.scale
  }

  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use u_title instead.",
            call. = FALSE)
    u_title <- u.title
  }

  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use u_pal instead.",
            call. = FALSE)
    u_pal <- u.pal
  }


  if(!(all(c("lat", "lon") %in% names(u)))) stop("Missing use lat/lon data, did you supply location data?")
  u$amount <- as.numeric(u$amount)
  u <- u[order(u$amount, decreasing = TRUE),]

  # Define palette
  u_pal <- colorNumeric(palette = colorRampPalette(u_pal)(15), domain = u$amount)

  # Add feeder use data to map
  map <- addCircleMarkers(map,
                           data = u,
                           ~lon, ~lat,
                           weight = 1,
                           opacity = 1,
                           fillOpacity = 0.5,
                           radius = ~ scale_area(amount, max = 50 * u_scale, radius = TRUE),
                           color = "black",
                           fillColor = ~u_pal(amount),
                           popup = ~htmltools::htmlEscape(as.character(round(amount))),
                           group = u_title) %>%
    addLegend(title = u_title,
              position = 'topright',
              pal = u_pal,
              values = u$amount,
              bins = 5,
              opacity = 1) %>%
    hideGroup("Loggers") %>%
    showGroup("Loggers")

  if(controls) map <- controls(map, u_title)
  return(map)
}

use.layer <- function(map, u, u_scale = 1, u_title = "Time", u_pal = c("yellow","red"), controls = TRUE,
                      u.scale, u.title, u.pal) {
  .Deprecated("use_layer")
}

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
#' @param locs Dataframe. Lat and lon for each feeder_id, required if lat and
#'   lon not in either u or p.
#' @param u_scale,p_scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of use (u) and path (p) data.
#' @param u_title,p_title Character. Titles for the legends of use (u) and
#'   path (p) data.
#' @param u_pal,p_pal Character vectors. Colours used to construct gradients for
#'   use (u) and path (p) data.
#' @param controls Logical. Add controls to map (allows showing/hiding of
#'   different layers)
#' @param u.scale,p.scale,u.title,p.title,u.pal,p.pal Depreciated.
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
#' map_leaflet(u = f.all, p = m.all, locs = l)
#' map_ggmap(u = f.all, p = m.all, locs = l)
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
#' map_leaflet(u = f.indiv, p = m.indiv, locs = l)
#' map_ggmap(u = f.indiv, p = m.indiv, locs = l)
#' }
#' @export
#' @import leaflet
map_leaflet <- function(u = NULL, p = NULL, locs = NULL,
                 u_scale = 1, p_scale = 1,
                 u_title = "Time", p_title = "Path use",
                 u_pal = c("yellow","red"),
                 p_pal = c("yellow","red"),
                 controls = TRUE,
                 u.scale, p.scale, u.title, p.title, u.pal, p.pal) {

  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use u_scale instead.",
            call. = FALSE)
    u_scale <- u.scale
  }
  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use u_title instead.",
            call. = FALSE)
    u_title <- u.title
  }
  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use u_pal instead.",
            call. = FALSE)
    u_pal <- u.pal
  }
  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- p.scale
  }
  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- p.title
  }
  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- p.pal
  }

  data <- map_prep(u = u, p = p, locs = locs)
  u <- data[['u']]
  p <- data[['p']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(u) == "bird_id", names(p) == "bird_id")) bird_id <- unique(unlist(list(u$bird_id, p$bird_id))) else bird_id = NULL

  # BASE MAP
  map <- map_leaflet_base(locs = locs, controls = controls)

  # Layers
  if(!is.null(u) && nrow(u) != 0) map <- use_layer(map, u = u, u_scale = u_scale, u_pal = u_pal, u_title = u_title, controls = controls)
  if(!is.null(p) && nrow(p) != 0) map <- path_layer(map, p = p, p_scale = p_scale, p_pal = p_pal, p_title = p_title, controls = controls)

  return(map)
}

map.leaflet <- function(u = NULL, p = NULL, locs = NULL,
                        u_scale = 1, p_scale = 1,
                        u_title = "Time", p_title = "Path use",
                        u_pal = c("yellow","red"),
                        p_pal = c("yellow","red"),
                        controls = TRUE,
                        u.scale, p.scale, u.title, p.title, u.pal, p.pal) {
  .Deprecated("map_leaflet")
}

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
#' @param locs Dataframe. Lat and lon for each feeder_id, required if lat and
#'   lon not in either u or p.
#' @param u_scale,p_scale Numerical. Relative scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of use (u) and path (p) data.
#' @param u_title,p_title Character. Titles for the legends of use (u) and
#'   path (p) data.
#' @param u_pal,p_pal Character vectors. Colours used to construct gradients for
#'   use (u) and movement (p) data.
#' @param maptype Character. The type of map to download. See \code{maptype}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param mapsource Character. The source of the map to download. See
#'   \code{source} under \code{\link[ggmap]{get_map}} for more options.
#' @param zoom Numeric. The zoom level of the map to download. See \code{zoom}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param which Character vector. A vector of bird ids specifying which to show.
#'   Only applies when using individual data.
#' @param u.scale,p.scale,u.title,p.title,u.pal,p.pal Depreciated.
#'
#' @return An interactive leaflet map with layers for feeding time, movement
#'   paths and feeder positions.
#'
#' @examples
#'
#' v <- visits(finches)
#' f <- feeding(v, bw = 15)
#' m <- move(v)
#'
#' # Summarise data for visualization (use totals):
#' library(dplyr)
#' f_all <- group_by(f, feeder_id, lat, lon) %>%
#'            summarise(amount = sum(feed_length) / bird_n[1])
#'
#' m_all <- group_by(m, move_path, feeder_id, lat, lon) %>%
#'            summarise(path_use = length(move_path) / bird_n[1])
#'
#' # Look at total summary maps
#' map_leaflet(u = f_all, p = m_all)
#' map_ggmap(u = f_all, p = m_all)
#'
#' # Summarise data for visualization (use individuals):
#' f_indiv <- group_by(f, bird_id, feeder_id, lat, lon) %>%
#'              summarise(amount = sum(feed_length))
#'
#' m_indiv <- group_by(m, move_path, feeder_id, lat, lon) %>%
#'              summarise(path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#' one on top of the other)
#' map_leaflet(u = f_indiv, p = m_indiv)
#' map_ggmap(u = f_indiv, p = m_indiv)
#'
#' @export
map_ggmap <- function(u = NULL, p = NULL, locs = NULL,
                      u_scale = 1, p_scale = 1,
                      u_title = "Time", p_title = "Path use",
                      u_pal = c("yellow","red"),
                      p_pal = c("yellow","red"),
                      maptype = "satellite",
                      mapsource = "google",
                      zoom = 17,
                      which = NULL,
                      u.scale, p.scale, u.title, p.title, u.pal, p.pal) {

  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use u_scale instead.",
            call. = FALSE)
    u_scale <- u.scale
  }
  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use u_title instead.",
            call. = FALSE)
    u_title <- u.title
  }
  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use u_pal instead.",
            call. = FALSE)
    u_pal <- u.pal
  }
  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- p.scale
  }
  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- p.title
  }
  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- p.pal
  }


  # Prep and Check Data
  data <- map_prep(u = u, p = p, locs = locs)
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

    temp_u <- dplyr::group_by(u, bird_id) %>% dplyr::summarize(sum = sum(amount))
    temp_p <- dplyr::group_by(p, bird_id) %>% dplyr::summarize(sum = length(path_use))

    keep_id <- intersect(temp_u$bird_id[temp_u$sum > 0], temp_p$bird_id[temp_p$sum > 0])
    if(length(setdiff(which, keep_id)) > 0) {
      message(paste0("Some bird_ids removed due to lack of data: ", paste(setdiff(which, keep_id), collapse = ", ")))
      u <- droplevels(u[u$bird_id %in% keep_id, ])
      p <- droplevels(p[p$bird_id %in% keep_id, ])
    }
  } else bird_id = NULL

  # Final Data Prep
  if(!is.null(u)){
    # Sort and Scale
    u$amount <- as.numeric(u$amount)
  #  u <- u[order(u$amount, decreasing = TRUE),]
    u$amount2 <- scale_area(u$amount, max = 30 * u_scale, min = 0.1 * u_scale)
  }

  if(!is.null(p)){
  #  # Sort and Scale
    p <- p[order(p$path_use, decreasing = TRUE), ]
    p$path_use2 <- scale_area(p$path_use, max = 3 * p_scale, min = 0.1 * p_scale)
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
      ggplot2::geom_point(data = u, ggplot2::aes(x = lon, y = lat, fill = amount, size = amount2), shape = 21, alpha = 0.5) +
      ggplot2::scale_fill_gradientn(name = u_title, colours = u_pal) +
      ggplot2::scale_size_area(guide = FALSE, max_size = 35 * u_scale)
  }

  # If movement paths specified
  if(!is.null(p)){
    p <- p %>%
      dplyr::ungroup() %>%
      dplyr::mutate(n = rep(1:2, nrow(p)/2)) %>%
      tidyr::gather(type, value, lat, lon) %>%
      dplyr::select(-feeder_id) %>%
      tidyr::unite(combo, type, n) %>%
      tidyr::spread(combo, value) %>%
      dplyr::arrange(desc(path_use))

    map <- map +
      ggplot2::geom_segment(data = p, ggplot2::aes(x = lon_1,
                                                 y = lat_1,
                                                 xend = lon_2,
                                                 yend = lat_2,
                                                 colour = path_use,
                                                 size = path_use2),
                          lineend = "round", alpha = 0.75) +
      ggplot2::scale_colour_gradientn(name = p_title, colours = p_pal)
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

map.ggmap <- function(u = NULL, p = NULL, locs = NULL,
                      u_scale = 1, p_scale = 1,
                      u_title = "Time", p_title = "Path use",
                      u_pal = c("yellow","red"),
                      p_pal = c("yellow","red"),
                      maptype = "satellite",
                      mapsource = "google",
                      zoom = 17,
                      which = NULL,
                      u.scale, p.scale, u.title, p.title, u.pal, p.pal) {
 .Deprecated("map_ggmap")
}
