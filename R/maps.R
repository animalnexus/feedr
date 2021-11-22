
# Prep data for mapping
# A data prep function used by mapping functions
map_prep <- function(p = NULL, m = NULL, locs = NULL, summary = "none") {

  # Check data
  msg <- character()
  if(!is.null(p) && nrow(p) > 0) {
    p <- dplyr::ungroup(p)

    if(!("logger_id" %in% names(p))) {
      msg <- c(msg, "The presence dataframe (p) requires the column: 'logger_id'")
    }
  }

  if(!is.null(m) && nrow(m) > 0) {
    m <- dplyr::ungroup(m)

    if(!all(c("move_path", "logger_id") %in% names(m))) {
      msg <- c(msg, "The movement dataframe (m) requires two columns: ",
               "'logger_id' and 'move_path'")
    }

    if("move_path" %in% names(m) && any(stringr::str_count(m$move_path, "_") > 1)) {
      msg <- c(msg, "Using '_' in logger_id names conflicts with the ",
               "mapping functions. You should remove any '_'s from logger_ids.")
    }
  }

  if(length(msg) > 0) stop(paste0(msg, collapse = "\n  "))

  # Check and format location data
  # Lat / Lon needs to be in EITHER locs, p, OR m

  if(!is.null(locs) && nrow(locs) > 0) locs <- dplyr::ungroup(locs)

  locs <- dplyr::bind_rows(get_locs(p), get_locs(m), get_locs(locs)) %>%
    dplyr::distinct()

  if(nrow(locs) == 0) stop("Data is missing latitude (lat) ",
                           "or longitude (lon) or both.")

  locs <- dplyr::arrange(locs, .data$logger_id)

  # Add locations and alert if any missing
  if(!is.null(p) && nrow(p) > 0) p <- add_locs(p, locs)
  if(!is.null(m) && nrow(m) > 0) m <- add_locs(m, locs)

  # Summarize data if not already summarized
  if(summary != "none") {
    if(!is.null(p) && nrow(p) > 0) p <- summaries(p, summary = summary)
    if(!is.null(m) && nrow(m) > 0) m <- summaries(m, summary = summary)
  } else {
    if(!("amount" %in% names(p)) | !("path_use" %in% names(m))) {
      stop("If not supplying a summary type (i.e. summary = 'none') ",
           "data must already be summarized. Presence data (p) ",
           "should contain a summary column 'amount', Movement data (m) ",
           "should contain a summary column 'path_use' (see examples).")
    }

      # Check data format
    msg <- character()

    if(xor("animal_id" %in% names(p), "animal_id" %in% names(m))) {
      msg <- c(msg, "Movements (m) and presence (p) data do not both have an ",
               "'animal_id' column. Either summarize both data sets to ",
               "animal_id or neither data set.")
    } else {
      if("animal_id" %in% names(p)){
        if(nrow(unique(p[, c("logger_id", "animal_id")])) != nrow(p)) {
          msg <- c(msg, "Presence data should be summarized to have at most ",
                   "one row per logger, or one row per individual per logger.")
        }

        if(nrow(unique(m[, c("move_path", "animal_id")])) != nrow(m)/2) {
          msg <- c(msg, "Movement data should be summarized to have two rows ",
                   "per movement path (one row per logger), or two rows per ",
                   "individual per movement path.")
        }
      } else {
        if(length(unique(p$logger_id)) != nrow(p)) {
          msg <- c(msg, "Presence data should be summarized to have one row ",
                   "per logger, or one row per individual per logger.")
        }

        if(length(unique(m$move_path)) != nrow(m)/2) {
          msg <- c(msg, "Movement data should be summarized to have two rows ",
                   "per movement path (one row for each logger), or two rows ",
                   "per individual per movement path.")
        }

        if(nrow(unique(m[, c("move_path", "path_use")])) !=
           length(unique(m$move_path))) {
          msg <- c(msg, "Movement data should have identical path_use values ",
                   "for each move_path category")
        }
      }
    }

    if(length(msg) > 0) stop(paste0(msg, collapse = "\n  "))
  }

  list('p' = p, 'm' = m, 'locs' = locs)
}

# Base map for leaflet
map_leaflet_base <- function(locs, marker = "logger_id",
                             controls = TRUE, minZoom = NULL, maxZoom = 18) {

  l <- leaflet::leaflet(
    data = locs,
    options = leaflet::leafletOptions(minZoom = minZoom,
                                      maxZoom = maxZoom)) %>%
    leaflet::addTiles(group = "Open Street Map") %>%
    leaflet::addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    leaflet::addCircleMarkers(~lon, ~lat,
                              popup  = htmltools::htmlEscape(
                                paste("Logger:",
                                      as.character(unlist(locs[, marker])))),
                              group = "Loggers",
                              weight = 2,
                              opacity = 1,
                              fillOpacity = 1,
                              fillColor = "black",
                              color = "white",
                              radius = 5)
  if(controls) {
    l <- l %>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite", "Terrain",
                       "Open Street Map", "Black and White"),
        overlayGroups = "Loggers",
        options = leaflet::layersControlOptions(collapsed = TRUE))
  }
  l
}

map.leaflet.base <- function(locs, marker = "logger_id",
                             name = "Loggers", controls = TRUE) {

  .Deprecated("map_leaflet_base")
}

path_lines <- function(map, data, m_scale, m_pal, m_title,
                       val_min, val_max, layerId = NULL){
  color <- m_pal(data$path_use)
  leaflet::addPolylines(
    map = map,
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

# Add movement layer to leaflet map
#
# Designed for advanced use (see map_leaflet() for general mapping)
#

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

  if(!(all(c("lat", "lon") %in% names(m)))) {
    stop("Missing path lat/lon data, did you supply location data?")
  }

  m <- dplyr::arrange(m, path_use)

  # Define palette
  m_pal <- leaflet::colorNumeric(
    palette = grDevices::colorRampPalette(m_pal)(15), domain = m$path_use)

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
                       p_pal = c("yellow","red"),
                       p.scale, p.title, p.pal, controls = TRUE) {
  .Deprecated("path_layer")
}


presence_markers <- function(map, data, p_scale, p_pal, p_title,
                             val_min = NULL, val_max = NULL,
                             layerId = "presence") {
  if(length(layerId) != nrow(data)) layerId <- paste0(layerId, "-", 1:nrow(data))
  addCircleMarkers(
    map,
    data = data,
    ~lon, ~lat,
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    radius = ~ scale_area(amount, max = 50 * p_scale,
                          radius = TRUE, val_min = val_min,
                          val_max = val_max),
    color = "black",
    fillColor = ~p_pal(amount),
    popup = ~htmltools::htmlEscape(as.character(round(amount, digits = 0))),
    group = p_title,
    layerId = layerId)
}

# Add presence layer to leaflet map
#
# Designed for advanced use (see map_leaflet() for general mapping)
#
#' @import leaflet
presence_layer <- function(map, p,
                      p_scale = 1, p_title = "Presence",
                      p_pal = c("yellow","red"),
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


  if(!(all(c("lat", "lon") %in% names(p)))) {
    stop("Missing presence lat/lon data, did you supply location data?")
  }
  p$amount <- as.numeric(p$amount)
  p <- p[order(p$amount, decreasing = TRUE),]

  # Define palette
  p_pal <- colorNumeric(palette = grDevices::colorRampPalette(p_pal)(15),
                        domain = p$amount)

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

use.layer <- function(map, p, p_scale = 1, p_title = "Time",
                      p_pal = c("yellow","red"), controls = TRUE,
                      u.scale, u.title, u.pal) {
  .Deprecated("presence_layer")
}

#' Map data using leaflet
#'
#' A visual summary of presence at and movements between loggers using leaflet
#' for R. This produces an interactive html map. This function can take
#' invidiual data as well as grand summaries (see Details and Examples).
#'
#' @details The type of summary visualized is defined by the `summary`
#'   argument: `summary = "sum"` calculates the total amount of time spent
#'   at each logger (presence) or total number of movements between loggers
#'   (movements). `summary = "sum_indiv"` averages these totals by the
#'   number of individuals in the data set, resulting in an average total amount
#'   of time per individual and an average amount of movements per individual
#'   for, or between, each logger. `summary = "indiv"` calculates
#'   individual totals. If the data is already summarized, use `summary =
#'   "none"`.
#'
#' @param p Dataframe. Regular or summarized presence data with columns
#'   `logger_id` and, if already summarized, `amount`. It can also
#'   contain `animal_id` for individual-based data, and lat/lon columns.
#' @param m Dataframe. Regular or summarized movement data with columns
#'   `logger_id`, `move_path`, and, if already summarized,
#'   `path_use`. It can also contain `animal_id` for individual-based
#'   data, and lat/lon columns.
#' @param locs Dataframe. Lat and lon for each logger_id, required if lat and
#'   lon not provided in either p or m.
#' @param summary Character. How the data should be summarized. If providing
#'   summarized data, use "none" (default). "sum" calculates total movements and
#'   total amount of time spent at a logger, "sum_indiv", averages the totals by
#'   the number of individuals in the data, "indiv" calculates totals for each
#'   individual.
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
#' @return An interactive leaflet map with layers for presence, movement paths
#'   and logger positions.
#'
#' @examples
#' v <- visits(finches)
#' p <- presence(v, bw = 15)
#' m <- move(v)
#'
#' # Built in summaries
#' map_leaflet(p = p, m = m, summary = "sum")
#' map_leaflet(p = p, m = m, summary = "sum_indiv")
#' map_leaflet(p = p, m = m, summary = "indiv")
#'
#' # Custom summaries
#' # Here, we average by the number of loggers (using dplyr)
#'
#' library(dplyr)
#'
#' p2 <- p %>%
#'   group_by(logger_id, lat, lon) %>%
#'   summarize(amount = sum(length) / logger_n[1])
#'
#' m2 <- m %>%
#'   group_by(logger_id, move_path, lat, lon) %>%
#'   summarize(path_use = length(move_path) / logger_n[1])
#'
#' map_leaflet(p = p2, m = m2)
#'
#'
#' @export
#' @import leaflet
map_leaflet <- function(p = NULL, m = NULL, locs = NULL,
                        summary = "none",
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

  data <- map_prep(p = p, m = m, locs = locs, summary = summary)
  p <- data[['p']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual animals?
  if(any(names(p) == "animal_id", names(m) == "animal_id")) {
    animal_id <- unique(unlist(list(p$animal_id, m$animal_id)))
  } else animal_id = NULL

  # BASE MAP
  map <- map_leaflet_base(locs = locs, controls = controls)

  # Layers
  if(!is.null(p) && nrow(p) > 0) {
    map <- presence_layer(map, p = p, p_scale = p_scale, p_pal = p_pal,
                          p_title = p_title, controls = controls)
  }

  if(!is.null(m) && nrow(m) > 0) {
    map <- path_layer(map, m = m, m_scale = m_scale, m_pal = m_pal,
                      m_title = m_title, controls = controls)
  }

  map
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
#'  Deprecated
#'
#'
#' @export
map_ggmap <- function() {
  .Defunct()
}

map.ggmap <- function() {
 .Defunct()
}
