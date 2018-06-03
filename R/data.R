#' Receiver locations from a black sea bass array
#'
#' Projected and scaled receiver coordinates. Scaling is recommended to reduce computation time and prevent convergence issues.
#'
#' @format A data frame with three variables: \code{Station} Receiver ID, \code{east} East-West coordinate,
#'   \code{north} North-South coordinate.
#'   
"rlocs"

#' Location of a stationary test transmitter placed in the black sea bass array
#'
#' Projected and scaled as for the receiver coordinates. 
#'
#' @format A data frame with two variables: \code{east} East-West coordinate, \code{north} North-South coordinate.
#'   
"testloc"

#' Stationary test transmitter data
#'
#' Detection data from a stationary, known-location test tag aggregated to the hour.
#'
#' @format A data frame with five variables: \code{Station} Receiver ID, \code{Transmitter} Transmitter ID, 
#'    \code{east} East-West coordinate, \code{north} North-South coordinate, \code{hour} Hour of monitoring.
#'   
"testdat"

#' Black sea bass detection data
#'
#' Detection data from a tagged black sea bass aggregated to the hour.
#'
#' @format A data frame with five variables: \code{Station} Receiver ID, \code{Transmitter} Transmitter ID, 
#'    \code{east} East-West coordinate, \code{north} North-South coordinate, \code{hour} Hour of monitoring.
#'   
"fishdat"
