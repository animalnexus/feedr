#' Scaling feeding and movement data for maps
#'
#' A scaling function used by mapping functions
#'
smart.scale <- function(x, m) {
  x <- as.numeric(x)
  x <- x - min(x) + 0.01
  x <- (x / max(x)) * (25 * m)
  return(x)
}


#' Prep data for mapping
#'
#' A data prep function used by mapping functions
#'
map.prep <- function(locs, f = NULL, m = NULL) {

  # Check data
  if(!is.null(f)){
    if(!all(c("feeder_id","feed_length") %in% names(f))) stop("The feeding dataframe (f) requires two columns: 'feeder_id' and 'feed_length'.")
  }
  if(!is.null(m)){
    if(!all(c("move_path","path_use") %in% names(m))) stop("The movement dataframe (m) requires two columns: 'move_path' and 'path_use'.")
    if(any(stringr::str_count(m$move_path, "_") > 1)) stop("Using '_' in feeder_id names conflicts with the mapping functions. You should remove any '_'s.")
  }

  # Check and format location data
  lat <- c("lat", "latitude")
  lon <- c("lon", "long", "longitude")

  if(!(any(lat %in% names(locs)) & any(lon %in% names(locs)))) stop(paste0("Locations (locs) data.frame is missing either latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "), ")"))

  if(sum(lat %in% names(locs)) > 1 | sum(lon %in% names(locs)) > 1) stop(paste0("Muliple latitude or longitudes possible. Looking for latitude (", paste0(lat, collapse = ", "), ") or longitude (", paste0(lon, collapse = ", "),")"))

  message(paste0("Using columns '", names(locs)[names(locs) %in% lat], "' and '", names(locs)[names(locs) %in% lon], "' as latitude and longitude respectively"))
  names(locs)[names(locs) %in% lat] <- "lat"
  names(locs)[names(locs) %in% lon] <- "lon"
  locs <- locs[ , c("feeder_id","lat","lon")]

  # Get locations and alert if any missing
  if(!is.null(f)){
    f <- merge(f, locs, by = c("feeder_id"), all.x = T, all.y = F)
    if(nrow(f[is.na(f$lat) | is.na(f$lon),]) > 0) message(paste0("Removed ", nrow(f[is.na(f$lat) | is.na(f$lon),]), " feeders due to missing lat or lon."))
    f <- f[!(is.na(f$lat) | is.na(f$lon)),]
  }

  if(!is.null(m)){
    m <- cbind(m, stringr::str_split_fixed(m$move_path, "_", 2))
    m <- suppressWarnings(reshape2::melt(m, measure.vars = c("1","2"), value.name = "feeder_id", variable.name = "feeder"))
    m <- merge(m, locs, by = "feeder_id", all.x = T, all.y = F)
    if(nrow(m[is.na(m$lat) | is.na(m$lon),]) > 0) message(paste0("Removed ", nrow(m[is.na(m$lat) | is.na(m$lon),]), " movement paths due to at least one missing lat or lon."))
    m <- plyr::ddply(m, c("move_path"), .fun = function(x) if(any(is.na(x[,c('lat','lon')]))) return(data.frame()) else return(x))
  }


  if(!is.null(f)) if(nrow(f) == 0) stop("Missing 'f' lat/lon data, did you supply a correct 'locs' file?")
  if(!is.null(m)) if(nrow(m) == 0) stop("Missing 'm' lat/lon data, did you supply a correct 'locs' file?")

  return(list('f' = f, 'm' = m, 'locs' = locs))
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
#' @param f Dataframe. Contains raw reads from an RFID reader with colums
#'   \code{bird_id}, \code{feeder_id}, \code{time}.
#' @param m Dataframe.
#' @param locs Dataframe.
#' @param f.scale m.scale Numerical. Scaling constants to increase (> 1) or
#'   decrease (< 1) the relative size of feeding (f) and movement (m) data.
#' @param f.title m.title Character. Titles for the legends of feeding (f) and
#'   movement (m) data.
#' @param f.pal m.pal Character vectors. Colours used to construct gradients for
#'   feeding (f) and movement (m) data.
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
#' map.leaflet(f = f.indiv, m = m.all, locs = l)
#' map.ggmap(f = f.indiv, m = m.all, locs = l)
#' }
#' @export
#' @import leaflet
map.leaflet <- function(f, m, locs,
                 f.scale = 1, m.scale = 1,
                 f.title = "Feeding time", m.title = "Path use",
                 f.pal = c("yellow","red"),
                 m.pal = c("yellow","red")) {

  data <- map.prep(f = f, m = m, locs = locs)
  f <- data[['f']]
  m <- data[['m']]
  locs <- data[['locs']]

  # Summaries or individual birds?
  if(any(names(f) == "bird_id", names(m) == "bird_id")) bird_id <- unique(unlist(list(f$bird_id, m$bird_id))) else bird_id = NULL

  # Final Data Prep
  if(!is.null(f)){
    # Sort and Scale
    u <- units(f$feed_length)
    f$feed_length <- as.numeric(f$feed_length)
    f <- f[order(f$feed_length, decreasing = TRUE),]
    f$feed_length2 <- smart.scale(f$feed_length, f.scale)
  }

  if(!is.null(m)){
    # Sort and Scale
    m <- m[order(m$path_use, decreasing = TRUE), ]
    m$path_use2 <- smart.scale(m$path_use, m.scale)
  }

  # Get groups to display as layers
  groups <- c("Feeders", "Time", "Movements")[c(TRUE, !is.null(f), !is.null(m))]
  #if(!is.null(bird_id)) groups <- c(groups, as.character(bird_id))

  # Basic Map
  maps <- leaflet(data = locs) %>%
    addTiles(group = "Default (Open Street Map)") %>%
    addProviderTiles("Stamen.Toner", group = "Black and White") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
    addCircleMarkers(~lon, ~lat,
                     radius = 3,
                     opacity = 1,
                     fillColor = "black",
                     fillOpacity = 1,
                     color = "black",
                     popup  = ~ as.character(feeder_id),
                     group = "Feeders") %>%
    addLayersControl(overlayGroups = groups,
                     baseGroups = c("Default (Open Street Map)", "Black and White", "Satelite","Terrain"),
                     options = layersControlOptions(collapsed = FALSE))

  # If a feeding data frame is specified:
  if(!is.null(f)){

    # Define palette to match range of scaled values
    f.pal <- colorNumeric(palette = colorRampPalette(f.pal)(10), domain = f$feed_length)

    # Add feeder use data to map
    maps <- addCircleMarkers(maps,
                             data = f,
                             ~lon, ~lat,
                             weight = 1,
                             opacity = 1,
                             fillOpacity = 0.75,
                             radius = ~  sqrt(feed_length2*100/pi),
                             color = "black",
                             fillColor = ~f.pal(feed_length),
                             popup = ~htmltools::htmlEscape(as.character(round(feed_length))),
                             group = "Time") %>%
      addLegend(title = f.title,
                position = 'topright',
                pal = f.pal,
                values = f$feed_length,
                bins = 5,
                opacity = 1,
                labFormat = labelFormat(suffix = paste0(" ",u)))
  }

  # If a movements data frame is specified
  if(!is.null(m)){

    # Define palette to match range of scaled values
    m.pal <- colorNumeric(palette = colorRampPalette(m.pal)(10), domain = m$path_use)

    # Add movement path lines to map
    for(path in unique(m$move_path)) {
      maps <- addPolylines(maps,
                           data = m[m$move_path == path, ],
                           ~lon, ~lat,
                           weight = ~path_use2,
                           opacity = 0.75,
                           color = ~m.pal(path_use),
                           group = "Movements",
                           popup = ~htmltools::htmlEscape(as.character(round(path_use))))
    }
    maps <- addLegend(maps,
                      title = m.title,
                      position = 'topright',
                      pal = m.pal,
                      opacity = 1,
                      values = m$path_use)
  }

  maps <- hideGroup(maps, c("Feeders","Time","Movements"))
  maps <- showGroup(maps, c("Movements","Time","Feeders"))

  return(maps)
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
#' @param f Dataframe. Contains raw reads from an RFID reader with colums
#'   \code{bird_id}, \code{feeder_id}, \code{time}.
#' @param m Dataframe.
#' @param locs Dataframe.
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
#' map.leaflet(f = f.indiv, m = m.all, locs = l)
#' map.ggmap(f = f.indiv, m = m.all, locs = l)
#' }
#' @export
map.ggmap <- function(f, m, locs,
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
    bird_id <- unique(unlist(list(f$bird_id, m$bird_id)))
    if(length(bird_id) > 10 & (is.null(which) | length(which) > 10)) {
      stop("You have chosen to run this funciton on more than 10 birds. This may overload your system. I would recommend trying again using the 'which' argument to specify a subset of birds.")
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
    u <- units(f$feed_length)
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
  maps <- ggmap::ggmap(visual,
                       legend = "topleft",
                       ylab = "Latitude",
                       xlab = "Longitude") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  # If movement paths specified
  if(!is.null(m)){
    maps <- maps +
      ggplot2::geom_line(data = m, ggplot2::aes(x = lon, y = lat, group = move_path, colour = path_use, size = path_use2, lineend = "round"), alpha = 0.75) +
      ggplot2::scale_colour_gradientn(name = m.title, colours = m.pal)
  }

  # If feeding data specified
  if(!is.null(f)) {
    maps <- maps +
      ggplot2::geom_point(data = f, ggplot2::aes(x = lon, y = lat, fill = feed_length, size = feed_length), shape = 21, alpha = 0.75) +
      ggplot2::scale_fill_gradientn(name = f.title, colours = f.pal) +
      ggplot2::scale_size(guide = FALSE, range = c(min(f$feed_length2), max(f$feed_length2))) #specify radius, as the scale has already been back calculated to represent area in the end (based on requirements for leaflet)
  }

  # Add feeder points
  maps <- maps + ggplot2::geom_point(data = locs, ggplot2::aes(x = lon, y = lat), colour = "black")

  # If Multiple bird ids included
  if(!is.null(bird_id)) {
    maps <- maps + ggplot2::facet_wrap(~ bird_id)
    message("You have specified multiple birds and static maps, this means that an individual map will be drawn for each bird. This may take some time to display in the plots window.")
  }
  return(maps)
}
