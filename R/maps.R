# Scaling feeding and movement data for maps
# A scaling function used by mapping functions
smart.scale <- function(x, m) {
  x <- as.numeric(x)
  x <- x - min(x) + 0.01
  x <- (x / max(x)) * (25 * m)
  return(x)
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
    if(sum(lat %in% names(d)) > 1 | sum(lon %in% names(d)) > 1) stop(paste0("Muliple latitude or longitudes in feeding data possible. Looking for latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "),")"))
    d <- rename.locs(d)
    locs <- d[ , c("feeder_id", "lat", "lon")]
    return(locs)
  } else return(data.frame())
}


# Prep data for mapping
# A data prep function used by mapping functions
map.prep <- function(f = NULL, m = NULL, locs = NULL) {

  # Check data
  if(!is.null(f)){
    if(!all(c("feeder_id","feed_length") %in% names(f))) stop("The feeding dataframe (f) requires two columns: 'feeder_id' and 'feed_length'.")
  }
  if(!is.null(m)){
    if(!all(c("move_path","path_use") %in% names(m))) stop("The movement dataframe (m) requires two columns: 'move_path' and 'path_use'.")
    if(any(stringr::str_count(m$move_path, "_") > 1)) stop("Using '_' in feeder_id names conflicts with the mapping functions. You should remove any '_'s.")
  }

  # Check and format location data
  # Lat / Lon needs to be in EITHER locs, f, OR m

  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")

  locs <- unique(rbind(get.locs(f), get.locs(m), get.locs(locs)))
  if(nrow(locs) == 0) stop(paste0("Locations (locs) data.frame is missing either latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "), "), or both."))

  # Get locations and alert if any missing
  if(!is.null(f)){
    f <- rename.locs(f)
    n <- names(f)[names(f) %in% c("feeder_id", "lat", "lon")]
    f <- merge(f, locs, by = n, all.x = T, all.y = F)
    if(nrow(f[is.na(f$lat) | is.na(f$lon),]) > 0) message(paste0("Removed ", nrow(f[is.na(f$lat) | is.na(f$lon),]), " feeders due to missing lat or lon."))
    f <- f[!(is.na(f$lat) | is.na(f$lon)),]
  }

  if(!is.null(m)){
    m <- rename.locs(m)
    n <- names(m)[names(m) %in% c("feeder_id", "lat", "lon")]
    m <- merge(m, locs, by = n, all.x = T, all.y = F)
    if(nrow(m[is.na(m$lat) | is.na(m$lon),]) > 0) message(paste0("Removed ", nrow(m[is.na(m$lat) | is.na(m$lon),]), " movement paths due to at least one missing lat or lon."))
    m <- plyr::ddply(m, c("move_path"), .fun = function(x) if(any(is.na(x[,c('lat','lon')]))) return(data.frame()) else return(x))
  }

  if(!is.null(f)) if(nrow(f) == 0) stop("Missing 'f' lat/lon data, did you supply location data in either f, m, or locs?")
  if(!is.null(m)) if(nrow(m) == 0) stop("Missing 'm' lat/lon data, did you supply location data in either f, m, or locs?")

  return(list('f' = f, 'm' = m, 'locs' = locs))
}

#' Base map for leaflet
#'
#' Designed for advanced use (see map.leaflet() for general mapping)
#' @export
map.leaflet.base <- function(locs, marker = "feeder_id", name = "Feeders", controls = TRUE) {
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
#' @export
move.layer <- function(map, m, m.scale = 1, m.title = "Path use", m.pal = c("yellow","red"), controls = TRUE) {

  if(!(all(c("lat", "lon") %in% names(m)))) stop("Missing 'm' lat/lon data, did you supply location data?")

  m <- m[order(m$path_use, decreasing = TRUE), ]

  # Define palette
  m.pal <- colorNumeric(palette = colorRampPalette(m.pal)(10), domain = m$path_use)

  # Add movement path lines to map
  for(path in unique(m$move_path)) {
    map <- addPolylines(map,
                        data = m[m$move_path == path, ],
                        ~lon, ~lat,
                        weight = ~smart.scale(path_use, m.scale),
                        opacity = 0.75,
                        color = ~m.pal(path_use),
                        group = m.title,
                        popup = ~htmltools::htmlEscape(as.character(round(path_use))))
  }
  map <- map %>% addLegend(title = m.title,
                           position = 'topright',
                           pal = m.pal,
                           opacity = 1,
                           values = m$path_use) %>%
    hideGroup("Feeders") %>%
    showGroup("Feeders")

  if(controls) map <- controls(map, m.title)

  return(map)
}

#' Add feeding layer to leaflet map
#'
#' Designed for advanced use (see map.leaflet() for general mapping)
#' @export
feeding.layer <- function(map, f, f.scale = 1, f.title = "Feeding time", f.pal = c("yellow","red"), controls = TRUE) {
  if(!(all(c("lat", "lon") %in% names(f)))) stop("Missing 'f' lat/lon data, did you supply location data?")
  f$feed_length <- as.numeric(f$feed_length)
  f <- f[order(f$feed_length, decreasing = TRUE),]

  # Define palette
  f.pal <- colorNumeric(palette = colorRampPalette(f.pal)(10), domain = f$feed_length)

  # Add feeder use data to map
  map <- addCircleMarkers(map,
                           data = f,
                           ~lon, ~lat,
                           weight = 1,
                           opacity = 1,
                           fillOpacity = 0.75,
                           radius = ~  sqrt(smart.scale(feed_length, f.scale)*100/pi),
                           color = "black",
                           fillColor = ~f.pal(feed_length),
                           popup = ~htmltools::htmlEscape(as.character(round(feed_length))),
                           group = f.title) %>%
    addLegend(title = f.title,
              position = 'topright',
              pal = f.pal,
              values = f$feed_length,
              bins = 5,
              opacity = 1) %>%
    hideGroup("Feeders") %>%
    showGroup("Feeders")

  if(controls) map <- controls(map, f.title)
  return(map)
}

# ----------------------------------
# map.leaflet()
# ----------------------------------
#' Map data using leaflet
#'
#' Visualize feeding bout and movement data using leaflet for R. This produces
#' an interactive html map. The user must summarize feeding and movement data in
#' the manner that they wish to visualize it. This function can take invidiual
#' bird data as well as grand summarise (see Details and Examples).
#'
#' @param f Dataframe. Summarized feeding data with columns \code{feeder_id} and
#'   \code{feed_length}. It can also contain \code{bird_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param m Dataframe. Summarized movement data with columns \code{feeder_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{bird_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and long for each feeder_id, required if lat and
#'   lon not in either f or m.
#' @param f.scale,m.scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of feeding (f) and movement (m) data.
#' @param f.title,m.title Character. Titles for the legends of feeding (f) and
#'   movement (m) data.
#' @param f.pal,m.pal Character vectors. Colours used to construct gradients for
#'   feeding (f) and movement (m) data.
#' @param controls Logical. Add controls to map (allows showing/hiding of different layers)
#'
#' @return An interactive leaflet map with layers for feeding time, movement
#'   paths and feeder positions.
#'
#' @examples
#' \dontrun{
#'
#' # Get feeding and movement data
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
#'            feed_length = sum(feed_length) / bird_n[1])
#'
#' m.all <- ddply(m, .(move_path), summarise,
#'            path_use = length(move_path) / bird_n[1])
#'
#' # Look at total summary maps
#' map.leaflet(f = f.all, m = m.all, locs = l)
#' map.ggmap(f = f.all, m = m.all, locs = l)
#'
#' # Summarise data for visualization (use individuals):
#' f.indiv <- ddply(f, .(bird_id, feeder_id), summarise,
#'            feed_length = sum(feed_length))
#'
#' m.indiv <- ddply(m, .(bird_id, move_path), summarise,
#'            path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#'   one on top of the other)
#' map.leaflet(f = f.indiv, m = m.all, locs = l)
#' map.ggmap(f = f.indiv, m = m.all, locs = l)
#' }
#' @export
#' @import leaflet
map.leaflet <- function(f = NULL, m = NULL, locs = NULL,
                 f.scale = 1, m.scale = 1,
                 f.title = "Feeding time", m.title = "Path use",
                 f.pal = c("yellow","red"),
                 m.pal = c("yellow","red"),
                 controls = TRUE) {

  data <- map.prep(f = f, m = m, locs = locs)
  f <- data[['f']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(f) == "bird_id", names(m) == "bird_id")) bird_id <- unique(unlist(list(f$bird_id, m$bird_id))) else bird_id = NULL

  # BASE MAP
  map <- map.leaflet.base(locs = locs)

  # Layers
  map <- map %>%
    move.layer(m = m, m.scale = m.scale, m.pal = m.pal, m.title = m.title) %>%
    feeding.layer(f = f, f.scale = f.scale, f.pal = f.pal, f.title = f.title)

  return(map)
}

# ----------------------------------
# map.ggmap()
# ----------------------------------
#' Map data using ggmap
#'
#' Visualize feeding bout and movement data using ggmap in R. This produces a
#' static map. The user must summarize feeding and movement data in the manner
#' that they wish to visualize it. This function can take invidiual bird data as
#' well as grand summarise (see Details and Examples).
#'
#' @param f Dataframe. Summarized feeding data with columns \code{feeder_id} and
#'   \code{feed_length}. It can also contain \code{bird_id} for individual-based
#'   data, and lat/lon instead of a locs argument.
#' @param m Dataframe. Summarized movement data with columns \code{feeder_id},
#'   \code{move_path}, and \code{path_use}. It can also contain \code{bird_id}
#'   for individual-based data, and lat/lon instead of a locs argument.
#' @param locs Dataframe. Lat and long for each feeder_id, required if lat and
#'   lon not in either f or m.
#' @param f.scale,m.scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of feeding (f) and movement (m) data.
#' @param f.title,m.title Character. Titles for the legends of feeding (f) and
#'   movement (m) data.
#' @param f.pal,m.pal Character vectors. Colours used to construct gradients for
#'   feeding (f) and movement (m) data.
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
#' # Get feeding and movement data
#' r <- loadWeb("downloaded_data.csv")
#'
#' v <- visits(r)
#' f <- ddply(v, .(bird_id), feeding, bw = 15)
#' m <- ddply(v, .(bird_id), move)
#'
#' # Get feeder locations
#' l <- read.csv("feeder index.csv")
#'
#' # If from web, lat/lon may be in the same column, extract it:
#' l$lon <- as.numeric(gsub("\\(([-0-9.]+),[-0-9.]+\\)", "\\1", l$loc))
#' l$lat <- as.numeric(gsub("\\([-0-9.]+,([-0-9.]+)\\)", "\\1", l$loc))
#'
#' # Summarise data for visualization (use totals):
#' f.all <- ddply(f, .(feeder_id), summarise,
#'            feed_length = sum(feed_length) / bird_n)
#'
#' m.all <- ddply(m, .(move_path), summarise,
#'            path_use = length(move_path) / bird_n)
#'
#' # Look at total summary maps
#' map.leaflet(f = f.all, m = m.all, locs = l)
#' map.ggmap(f = f.all, m = m.all, locs = l)
#'
#' # Summarise data for visualization (use individuals):
#' f.indiv <- ddply(f, .(bird_id, feeder_id), summarise,
#'            feed_length = sum(feed_length))
#'
#' m.indiv <- ddply(m, .(bird_id, move_path), summarise,
#'            path_use = length(move_path))
#'
#' # Look at individual summary maps (note that Leaflet just stacks individuals
#' one on top of the other)
#' map.leaflet(f = f.indiv, m = m.indiv, locs = l)
#' map.ggmap(f = f.indiv, m = m.indiv, locs = l, f.scale = 0.7, m.scale = 0.05)
#' }
#' @export
map.ggmap <- function(f = NULL, m = NULL, locs = NULL,
                 f.scale = 1, m.scale = 1,
                 f.title = "Feeding time", m.title = "Path use",
                 f.pal = c("yellow","red"),
                 m.pal = c("yellow","red"),
                 maptype = "satellite",
                 mapsource = "google",
                 zoom = 17,
                 which = NULL) {

  # Prep and Check Data
  data <- map.prep(f = f, m = m, locs = locs)
  f <- data[['f']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(f) == "bird_id", names(m) == "bird_id")) {
    if(is.null(which)) which <- as.character(unique(c(as.character(f$bird_id), as.character(m$bird_id))))
    bird_id <- unique(unlist(list(f$bird_id, m$bird_id)))
    if(length(bird_id) > 10 & length(which) > 10) {
      stop("You have chosen to run this function on more than 10 birds. This may overload your system. I would recommend trying again using the 'which' argument to specify a subset of birds.")
    }
    f <- droplevels(f[f$bird_id %in% which,])
    m <- droplevels(m[m$bird_id %in% which,])

    temp.f <- plyr::ddply(f, "bird_id", summarize, sum = sum(feed_length))
    temp.m <- plyr::ddply(m, "bird_id", summarize, sum = length(path_use))
    keep.id <- intersect(temp.f$bird_id[temp.f$sum > 0], temp.m$bird_id[temp.m$sum > 0])
    if(length(setdiff(which, keep.id)) > 0) {
      message(paste0("Some bird_ids removed due to lack of data: ", paste(setdiff(which, keep.id), collapse = ", ")))
      f <- droplevels(f[f$bird_id %in% keep.id, ])
      m <- droplevels(m[m$bird_id %in% keep.id, ])
    }
  } else bird_id = NULL

  # Final Data Prep
  if(!is.null(f)){
    # Sort and Scale
    f$feed_length <- as.numeric(f$feed_length)
    f <- f[order(f$feed_length, decreasing = TRUE),]
    f$feed_length2 <- smart.scale(f$feed_length, f.scale * 0.7)
  }

  if(!is.null(m)){
    # Sort and Scale
    m <- m[order(m$path_use, decreasing = TRUE), ]
    m$path_use2 <- smart.scale(m$path_use, m.scale * 1.75)
  }

  # Basic Map (reverse order to make sure feeders are on top)
  visual <- ggmap::get_map(location = c(median(locs$lon),median(locs$lat)),
                           zoom = zoom,
                           source = mapsource,
                           maptype = maptype)
  map <- ggmap::ggmap(visual,
                       legend = "topleft",
                       ylab = "Latitude",
                       xlab = "Longitude") +
          ggplot2::labs(x = "Longitude", y = "Latitude")

  # If feeding data specified
  if(!is.null(f)) {
    map <- map +
      ggplot2::geom_point(data = f, ggplot2::aes(x = lon, y = lat, fill = feed_length, size = feed_length), shape = 21, alpha = 0.75) +
      ggplot2::scale_fill_gradientn(name = f.title, colours = f.pal) +
      ggplot2::scale_size(guide = FALSE, range = c(min(f$feed_length2), max(f$feed_length2))) #specify radius, as the scale has already been back calculated to represent area in the end (based on requirements for leaflet)
  }

  # If movement paths specified
  if(!is.null(m)){
    map <- map +
      ggplot2::geom_path(data = m, ggplot2::aes(x = lon, y = lat, group = move_path, colour = path_use, size = path_use2), lineend = "round", alpha = 0.75) +
      ggplot2::scale_colour_gradientn(name = m.title, colours = m.pal)
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
