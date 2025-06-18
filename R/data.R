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

#' Example model parameters
#'
#' Example model parameters for `COA_standard()`
#'
#' @format A data frame with four variables and one row:
#' `nind` which is the number of individuals, `nrec` is the number of receivers
#' in the array, `tsteps` is the number of time steps used in the example,
#' and `ntrans` is the number of expected transmissions within a time step.
#'
"model_param_ex"

#' Example array extent
#'
#' Example array extent used in each model
#'
#' @format A data frame with two variables and two row: `ylim` is the minimum
#' and maximum extent on the y-axis (i.e., latitudinal) for the array and
#' `xlim` is the minimum and maximum extent on the x-axis (i.e., longitudinal)
#' for the array.
#'
"example_extent"

#' Counts of detection per time steps
#'
#' Array of counts of detection per time step per receivers
#'
#' @format An array with demensions of 1 by 10 (number of time steps) by 30 number
#' of receivers.
#'
"Y"
