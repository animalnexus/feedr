#' Finche visits to RFID-enabled feeders
#'
#' A small dataset containing raw reads of finches visiting feeders (RFID
#' loggers) on Thompson Rivers University campus in Kamloops, BC, Canada
#'
#' @format A data frame with 412 rows and 7 variables:
#' \describe{
#'   \item{animal_id}{Unique RFID tag attached to a finch}
#'   \item{time}{Time of read}
#'   \item{logger_id}{Unique id of logger where read was made}
#'   \item{species}{Individual species}
#'   \item{sex}{Individual sex, if known}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}}
"finches"

#' Large dataset of finche visits to RFID-enabled feeders
#'
#' A large dataset containing raw reads of finches visiting feeders (RFID
#' loggers) on Thompson Rivers University campus in Kamloops, BC, Canada
#'
#' @format A data frame with 42708 rows and 7 variables:
#' \describe{
#'   \item{animal_id}{Unique RFID tag attached to a finch}
#'   \item{time}{Time of read}
#'   \item{logger_id}{Unique id of logger where read was made}
#'   \item{species}{Individual species}
#'   \item{sex}{Individual sex, if known}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}}
"finches_lg"

#' Read data on Chickadees from gap-crossing study
#'
#' A dataset containing raw reads of black-capped chickadees visiting
#' RFID-enabled feeders (loggers) in Prince George, BC, Canada
#'
#' @format A data frame with 26718 rows and 7 variables:
#' \describe{
#'   \item{animal_id}{Unique RFID tag attached to a chickadee}
#'   \item{time}{Time of read}
#'   \item{logger_id}{Unique id of logger where read was made}
#'   \item{experiment}{Which experiment (site) the data was collected at}
#'   \item{date}{Date of read}
#'   \item{lat}{Latitude of logger location}
#'   \item{lon}{Longitude of logger location}}
"chickadees"
