
# Prep data for mapping
# A data prep function used by mapping functions
map_prep <- function(p = NULL, m = NULL, locs = NULL) {

  # Check data
  if(!is.null(p)){
    if(!all(c("logger_id","amount") %in% names(p))) stop("The presence dataframe (p) requires two columns: 'logger_id' and 'amount'.")
  }
  if(!is.null(m)){
    if(!all(c("move_path","path_use", "logger_id") %in% names(m))) stop("The path dataframe (m) requires three columns: 'logger_id', 'move_path', 'path_use'.")
    if(any(stringr::str_count(m$move_path, "_") > 1)) stop("Using '_' in logger_id names conflicts with the mapping functions. You should remove any '_'s from logger_ids.")
  }

  # Check and format location data
  # Lat / Lon needs to be in EITHER locs, p, OR p

  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")

  locs <- unique(dplyr::bind_rows(get_locs(p), get_locs(m), get_locs(locs)))
  if(nrow(locs) == 0) stop(paste0("Locations (locs) dataframe is missing either latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "), "), or both."))

  # Get locations and alert if any missing
  if(!is.null(p)){
    p <- rename_locs(p)
    n <- names(p)[names(p) %in% c("logger_id", "lat", "lon")]
    p <- merge(p, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(p[is.na(p$lat) | is.na(p$lon),]) > 0) message(paste0("Removed ", nrow(p[is.na(p$lat) | is.na(p$lon),]), " loggers due to missing lat or lon."))
    p <- p[!(is.na(p$lat) | is.na(p$lon)),]
  }

  if(!is.null(m)){
    m <- rename_locs(m)
    n <- names(m)[names(m) %in% c("logger_id", "lat", "lon")]
    m <- merge(m, locs, by = n, all.x = TRUE, all.y = FALSE)
    if(nrow(m[is.na(m$lat) | is.na(m$lon),]) > 0) message(paste0("Removed ", nrow(m[is.na(m$lat) | is.na(m$lon),]), " paths due to at least one missing lat or lon."))
    m <- dplyr::group_by(m, move_path) %>%
      dplyr::do(if(any(is.na(.[,c('lat','lon')]))) return(data.frame()) else return(.)) %>%
      dplyr::ungroup()
  }

  #if(!is.null(p)) if(nrow(p) == 0) stop("Missing presence lat/lon data, did you supply location data in either p, m, or locs?")
  #if(!is.null(m)) if(nrow(m) == 0) stop("Missing path lat/lon data, did you supply location data in either p, m, or locs?")

  return(list('p' = p, 'm' = m, 'locs' = locs))
}

#' Base map for leaflet
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
map_leaflet_base <- function(locs, marker = "logger_id", controls = TRUE, minZoom = NULL, maxZoom = 18) {
  l <- leaflet::leaflet(data = locs,
                        options = leaflet::leafletOptions(minZoom = minZoom, maxZoom = maxZoom)) %>%
    leaflet::addTiles(group = "Open Street Map") %>%
    leaflet::addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    leaflet::addCircleMarkers(~lon, ~lat,
                              popup  = htmltools::htmlEscape(paste("Logger:", as.character(unlist(locs[, marker])))),
                              group = "Loggers",
                              weight = 2,
                              opacity = 1,
                              fillOpacity = 1,
                              fillColor = "black",
                              color = "white",
                              radius = 5)
  if(controls) {
    l <- l %>%
      leaflet::addLayersControl(baseGroups = c("Satellite", "Terrain", "Open Street Map", "Black and White"),
                                overlayGroups = "Loggers",
                                options = leaflet::layersControlOptions(collapsed = TRUE))
  }
  return(l)
}

map.leaflet.base <- function(locs, marker = "logger_id", name = "Loggers", controls = TRUE) {
  .Deprecated("map_leaflet_base")
}

path_lines <- function(map, data, m_scale, m_pal, m_title, val_min, val_max, layerId = NULL){
  color <- m_pal(data$path_use)
  leaflet::addPolylines(map = map,
                        data = data,
                        ~lon, ~lat,
                        color = ~m_pal(path_use[1]),
                        opacity = 0.8,
                        weight = ~scale_area(path_use[1],
                                             max = 50 * m_scale,
                                             val_max = val_max,
                                             val_min = val_min),
                        group = m_title,
                        popup = ~as.character(round(path_use[1], digits = if(max(path_use[1]) < 10) 1 else 0)),
                        layerId = layerId)
}

#' Add movement layer to leaflet map
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
#'
#' @import magrittr
path_layer <- function(map, m,
                       m_scale = 1, m_title = "Path use",
                       m_pal = c("yellow", "orange", "red"), controls = TRUE,
                       p.scale, p.title, p.pal) {

  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use m_scale instead.",
            call. = FALSE)
    m_scale <- p.scale
  }

  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use m_title instead.",
            call. = FALSE)
    m_title <- p.title
  }

  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use m_pal instead.",
            call. = FALSE)
    m_pal <- p.pal
  }

  if(!(all(c("lat", "lon") %in% names(m)))) stop("Missing path lat/lon data, did you supply location data?")

  m <- dplyr::arrange(m, path_use)

  # Define palette
  m_pal <- leaflet::colorNumeric(palette = grDevices::colorRampPalette(m_pal)(15), domain = m$path_use)

  # Add movement path lines to map
  for(path in unique(m$move_path)) {
    map <- path_lines(map, data = m[m$move_path == path, ],
                      m_scale = m_scale, m_title = m_title, m_pal = m_pal,
                      val_max = max(m$path_use), val_min = min(m$path_use),
                      layerId = path)
  }

  map <- map %>% leaflet::addLegend(title = m_title,
                                    position = 'topright',
                                    pal = m_pal,
                                    opacity = 1,
                                    values = m$path_use) %>%
    leaflet::hideGroup("Loggers") %>%
    leaflet::showGroup("Loggers")

  if(controls) map <- controls(map, m_title)

  return(map)
}

path.layer <- function(map, p,
                       p_scale = 1, p_title = "Path use",
                       p_pal = c("yellow","red"), p.scale, p.title, p.pal, controls = TRUE) {
  .Deprecated("path_layer")
}


presence_markers <- function(map, data, p_scale, p_pal, p_title, val_min = NULL, val_max = NULL, layerId = "presence") {
  if(length(layerId) != nrow(data)) layerId <- paste0(layerId, "-", 1:nrow(data))
  addCircleMarkers(map,
                   data = data,
                   ~lon, ~lat,
                   weight = 1,
                   opacity = 1,
                   fillOpacity = 0.8,
                   radius = ~ scale_area(amount, max = 50 * p_scale, radius = TRUE, val_min = val_min, val_max = val_max),
                   color = "black",
                   fillColor = ~p_pal(amount),
                   popup = ~htmltools::htmlEscape(as.character(round(amount, digits = 0))),
                   group = p_title,
                   layerId = layerId)
}

#' Add presence layer to leaflet map
#'
#' Designed for advanced use (see map_leaflet() for general mapping)
#'
#' @import leaflet
presence_layer <- function(map, p,
                      p_scale = 1, p_title = "Presence", p_pal = c("yellow","red"),
                      controls = TRUE,
                      u.scale, u.title, u.pal) {
  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- u.scale
  }

  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- u.title
  }

  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- u.pal
  }


  if(!(all(c("lat", "lon") %in% names(p)))) stop("Missing presence lat/lon data, did you supply location data?")
  p$amount <- as.numeric(p$amount)
  p <- p[order(p$amount, decreasing = TRUE),]

  # Define palette
  p_pal <- colorNumeric(palette = grDevices::colorRampPalette(p_pal)(15), domain = p$amount)

  # Add presence at loggers data to map
  map <- presence_markers(map, p, p_scale, p_pal, p_title) %>%
    addLegend(title = p_title,
              position = 'topright',
              pal = p_pal,
              values = p$amount,
              bins = 5,
              opacity = 1) %>%
    hideGroup("Loggers") %>%
    showGroup("Loggers")

  if(controls) map <- controls(map, p_title)
  return(map)
}

use.layer <- function(map, p, p_scale = 1, p_title = "Time", p_pal = c("yellow","red"), controls = TRUE,
                      u.scale, u.title, u.pal) {
  .Deprecated("presence_layer")
}

#' Map data using leaflet
#'
#' Visualize presence at and movements between loggers using leaflet for R. This produces
#' an interactive html map. The user must summarize presence and movements in
#' the manner that they wish to visualize it. This function can take invidiual
#' data as well as grand summaries (see Details and Examples).
#'
#' @param p Dataframe. Summarized 'presence' data with columns \code{logger_id} and
#'   \code{amount}. It can also contain \code{animal_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param m Dataframe. Summarized 'movement' data with columns \code{logger_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{animal_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and lon for each logger_id, required if lat and
#'   lon not in either p or m.
#' @param p_scale,m_scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of presence (p) and movements (m) data.
#' @param p_title,m_title Character. Titles for the legends of presence (p) and
#'   movements (m) data.
#' @param p_pal,m_pal Character vectors. Colours used to construct gradients for
#'   presence (p) and path (m) data.
#' @param controls Logical. Add controls to map (allows showing/hiding of
#'   different layers)
#' @param u.scale,p.scale,u.title,p.title,u.pal,p.pal Depreciated.
#'
#' @return An interactive leaflet map with layers for presence, paths and logger positions.
#'
#' @examples
#' v <- visits(finches)
#' p <- presence(v, bw = 15)
#' m <- move(v)
#'
#' # Summarise data for visualization (use totals):
#' library(dplyr)
#' p_all <- p %>%
#'   group_by(logger_id, lat, lon) %>%
#'   summarise(amount = sum(length) / animal_n[1])
#'
#' m_all <- m %>%
#'   group_by(logger_id, move_path, lat, lon) %>%
#'   summarise(path_use = length(move_path) / animal_n[1])
#'
#' # Look at total summary maps
#' map_leaflet(p = p_all, m = m_all)
#'
#' # Summarise data for visualization (use individuals):
#' p_indiv <- p %>%
#'   group_by(animal_id, logger_id, lat, lon) %>%
#'   summarise(amount = sum(length))
#'
#' m_indiv <- m %>%
#'   group_by(animal_id, logger_id, move_path, lat, lon) %>%
#'   summarise(path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#' # one on top of the other)
#' map_leaflet(p = p_indiv, m = m_indiv)
#'
#' @export
#' @import leaflet
map_leaflet <- function(p = NULL, m = NULL, locs = NULL,
                 p_scale = 1, m_scale = 1,
                 p_title = "Time", m_title = "Path use",
                 p_pal = c("yellow","red"),
                 m_pal = c("yellow","red"),
                 controls = TRUE,
                 u.scale, p.scale, u.title, p.title, u.pal, p.pal) {

  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- u.scale
  }
  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- u.title
  }
  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- u.pal
  }
  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use m_scale instead.",
            call. = FALSE)
    m_scale <- p.scale
  }
  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use m_title instead.",
            call. = FALSE)
    m_title <- p.title
  }
  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use m_pal instead.",
            call. = FALSE)
    m_pal <- p.pal
  }

  data <- map_prep(p = p, m = m, locs = locs)
  p <- data[['p']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual animals?
  if(any(names(p) == "animal_id", names(m) == "animal_id")) animal_id <- unique(unlist(list(p$animal_id, m$animal_id))) else animal_id = NULL

  # BASE MAP
  map <- map_leaflet_base(locs = locs, controls = controls)

  # Layers
  if(!is.null(p) && nrow(p) != 0) map <- presence_layer(map, p = p, p_scale = p_scale, p_pal = p_pal, p_title = p_title, controls = controls)
  if(!is.null(m) && nrow(m) != 0) map <- path_layer(map, m = m, m_scale = m_scale, m_pal = m_pal, m_title = m_title, controls = controls)

  return(map)
}

map.leaflet <- function(p = NULL, m = NULL, locs = NULL,
                        p_scale = 1, m_scale = 1,
                        p_title = "Time", m_title = "Path use",
                        p_pal = c("yellow","red"),
                        m_pal = c("yellow","red"),
                        controls = TRUE,
                        u.scale, p.scale, u.title, p.title, u.pal, p.pal) {
  .Deprecated("map_leaflet")
}

#' Map data using ggmap
#'
#' Visualize presence at and movements between loggers using ggmap in R. This produces a
#' static map. The user must summarize presence and movements in the manner
#' that they wish to visualize it. This function can take invidiual animal data as
#' well as grand summarise (see Details and Examples).
#'
#' @param p Dataframe. Summarized presence data with columns \code{logger_id} and
#'   \code{amount}. It can also contain \code{animal_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param m Dataframe. Summarized movement data with columns \code{logger_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{animal_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and lon for each logger_id, required if lat and
#'   lon not in either p or m.
#' @param p_scale,m_scale Numerical. Relative scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of presence (p) and path (m) data.
#' @param p_title,m_title Character. Titles for the legends of presence (p) and
#'   path (m) data.
#' @param p_pal,m_pal Character vectors. Colours used to construct gradients for
#'   presence (p) and movement (m) data.
#' @param maptype Character. The type of map to download. See \code{maptype}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param mapsource Character. The source of the map to download. See
#'   \code{source} under \code{\link[ggmap]{get_map}} for more options.
#' @param zoom Numeric. The zoom level of the map to download. See \code{zoom}
#'   under \code{\link[ggmap]{get_map}} for more options.
#' @param which Character vector. A vector of animal ids specifying which to show.
#'   Only applies when using individual data.
#' @param u.scale,p.scale,u.title,p.title,u.pal,p.pal Depreciated.
#'
#' @return An interactive leaflet map with layers for presence, movements and
#'   logger positions.
#'
#' @examples
#'
#' v <- visits(finches)
#' p <- presence(v, bw = 15)
#' m <- move(v)
#'
#' # Summarise data for visualization (use totals):
#' library(dplyr)
#' p_all <- p %>%
#'   group_by(logger_id, lat, lon) %>%
#'   summarise(amount = sum(length) / animal_n[1])
#'
#' m_all <- m %>%
#'   group_by(logger_id, move_path, lat, lon) %>%
#'   summarise(path_use = length(move_path) / animal_n[1])
#'
#' # Look at total summary maps
#' map_ggmap(p = p_all, m = m_all)
#'
#' # Summarise data for visualization (use individuals):
#' p_indiv <- p %>%
#'   group_by(animal_id, logger_id, lat, lon) %>%
#'   summarise(amount = sum(length))
#'
#' m_indiv <- m %>%
#'   group_by(animal_id, logger_id, move_path, lat, lon) %>%
#'   summarise(path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#' # one on top of the other)
#' map_ggmap(p = p_indiv, m = m_indiv)
#'
#' @export
map_ggmap <- function(p = NULL, m = NULL, locs = NULL,
                      p_scale = 1, m_scale = 1,
                      p_title = "Time", m_title = "Path use",
                      p_pal = c("yellow","red"),
                      m_pal = c("yellow","red"),
                      maptype = "satellite",
                      mapsource = "google",
                      zoom = 17,
                      which = NULL,
                      u.scale, p.scale, u.title, p.title, u.pal, p.pal) {

  if (!missing(u.scale)) {
    warning("Argument u.scale is deprecated; please use p_scale instead.",
            call. = FALSE)
    p_scale <- u.scale
  }
  if (!missing(u.title)) {
    warning("Argument u.title is deprecated; please use p_title instead.",
            call. = FALSE)
    p_title <- u.title
  }
  if (!missing(u.pal)) {
    warning("Argument u.pal is deprecated; please use p_pal instead.",
            call. = FALSE)
    p_pal <- u.pal
  }
  if (!missing(p.scale)) {
    warning("Argument p.scale is deprecated; please use m_scale instead.",
            call. = FALSE)
    m_scale <- p.scale
  }
  if (!missing(p.title)) {
    warning("Argument p.title is deprecated; please use m_title instead.",
            call. = FALSE)
    m_title <- p.title
  }
  if (!missing(p.pal)) {
    warning("Argument p.pal is deprecated; please use m_pal instead.",
            call. = FALSE)
    m_pal <- p.pal
  }


  # Prep and Check Data
  data <- map_prep(p = p, m = m, locs = locs)
  p <- data[['p']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual animals?
  if(any(names(p) == "animal_id", names(m) == "animal_id")) {
    if(is.null(which)) which <- as.character(unique(c(as.character(p$animal_id), as.character(m$animal_id))))
    animal_id <- unique(unlist(list(p$animal_id, m$animal_id)))
    if(length(animal_id) > 10 & length(which) > 10) {
      stop("You have chosen to run this function on more than 10 animals. This may overload your system. We recommend trying again using the 'which' argument to specify a subset of animals.")
    }
    p <- droplevels(p[p$animal_id %in% which,])
    m <- droplevels(m[m$animal_id %in% which,])

    temp_p <- dplyr::group_by(p, animal_id) %>% dplyr::summarize(sum = sum(amount))
    temp_m <- dplyr::group_by(m, animal_id) %>% dplyr::summarize(sum = length(path_use))

    keep_id <- intersect(temp_p$animal_id[temp_p$sum > 0], temp_m$animal_id[temp_m$sum > 0])
    if(length(setdiff(which, keep_id)) > 0) {
      message(paste0("Some animal_ids removed due to lack of data: ", paste(setdiff(which, keep_id), collapse = ", ")))
      p <- droplevels(p[p$animal_id %in% keep_id, ])
      m <- droplevels(m[m$animal_id %in% keep_id, ])
    }
  } else animal_id = NULL

  # Final Data Prep
  if(!is.null(p)){
    # Sort and Scale
    p$amount <- as.numeric(p$amount)
  #  p <- p[order(p$amount, decreasing = TRUE),]
    p$amount2 <- scale_area(p$amount, max = 30 * p_scale, min = 0.1 * p_scale)
  }

  if(!is.null(m)){
  #  # Sort and Scale
    m <- m[order(m$path_use, decreasing = TRUE), ]
    m$path_use2 <- scale_area(m$path_use, max = 3 * m_scale, min = 0.1 * m_scale)
  }

  # Basic Map (reverse order to make sure loggers are on top)
  visual <- ggmap::get_map(location = c(median(locs$lon), median(locs$lat)),
                           zoom = zoom,
                           source = mapsource,
                           maptype = maptype)
  map <- ggmap::ggmap(visual,
                      legend = "topleft",
                      ylab = "Latitude",
                      xlab = "Longitude") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  # If presence data specified
  if(!is.null(p)) {
    map <- map +
      ggplot2::geom_point(data = p, ggplot2::aes(x = lon, y = lat, fill = amount, size = amount2), shape = 21, alpha = 0.5) +
      ggplot2::scale_fill_gradientn(guide = guide_colourbar(title = p_title,
                                                            reverse = TRUE),
                                    colours = p_pal) +
      ggplot2::scale_size_area(guide = FALSE, max_size = 35 * p_scale)
  }

  # If movement paths specified
  if(!is.null(m)){
    sort_by <- c("animal_id", "move_path", "logger_id")
    sort_by <- sort_by[sort_by %in% names(m)]
    m <- m %>%
      dplyr::ungroup() %>%
      dplyr::arrange_(.dots = sort_by) %>%
      dplyr::mutate(n = rep(1:2, nrow(m)/2)) %>%
      tidyr::gather(type, value, lat, lon) %>%
      dplyr::select(-logger_id) %>%
      tidyr::unite(combo, type, n) %>%
      tidyr::spread(combo, value) %>%
      dplyr::arrange(desc(path_use))

    map <- map +
      ggplot2::geom_segment(data = m, ggplot2::aes(x = lon_1,
                                                 y = lat_1,
                                                 xend = lon_2,
                                                 yend = lat_2,
                                                 colour = path_use,
                                                 size = path_use2),
                          lineend = "round", alpha = 0.75) +
      ggplot2::scale_colour_gradientn(guide = guide_colourbar(title = m_title,
                                                              reverse = TRUE),
                                      colours = m_pal)
  }

  # Add logger points
  map <- map + ggplot2::geom_point(data = locs, ggplot2::aes(x = lon, y = lat), colour = "black")

  # If Multiple animal ids included
  if(!is.null(animal_id)) {
    map <- map + ggplot2::facet_wrap(~ animal_id)
    message("You have specified multiple animals and static maps, this means that an individual map will be drawn for each animal. This may take some time to display in the plots window.")
  }
  return(map)
}

map.ggmap <- function(u = NULL, p = NULL, locs = NULL,
                      u_scale = 1, p_scale = 1,
                      u_title = "Presence", p_title = "Movements",
                      u_pal = c("yellow","red"),
                      p_pal = c("yellow","red"),
                      maptype = "satellite",
                      mapsource = "google",
                      zoom = 17,
                      which = NULL,
                      u.scale, p.scale, u.title, p.title, u.pal, p.pal) {
 .Deprecated("map_ggmap")
}
