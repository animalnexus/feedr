#' Read data on Finches from BirdMoves website
#'
#' A dataset containing raw reads of finches visiting feeders in Kamloops, BC,
#' Canada
#'
#' @format A data frame with 412 rows and 5 variables:
#' \describe{
#'   \item{bird_id}{Unique RFID tag attached to a finch}
#'   \item{time}{Time of read}
#'   \item{feeder_id}{Unique id of feeder where read was made}
#'   \item{loc}{Lat/lon as downloaded from website}
#'   \item{species}{Individual species}}
"finches"

#' Read data on Chickadees from gap-crossing study
#'
#' A dataset containing raw reads of black-capped chickadees visiting feeders in
#' Prince George, BC, Canada
#'
#' @format A data frame with 22095 rows and 6 variables:
#' \describe{
#'   \item{experiment}{Which experiment (site) the data was collected at}
#'   \item{feeder_id}{Unique id of feeder where read was made}
#'   \item{bird_id}{Unique RFID tag attached to a chickadee}
#'   \item{time}{Time of read}
#'   \item{lat}{Latitude of feeder location}
#'   \item{lon}{Longitude of feeder location}}
"chickadees"