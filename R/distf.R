# Save this file as `R/Housekeeping.R`

#' Calculate Euclidean distance between receivers and activity centers
#'
#' @export
#' @param x Data frame or matrix containing 2-dimensional coordinates
#' @param y Data frame or matrix containing 2-dimensional coordinates
#' @return 'distf' returns a matrix containing the Euclidean distance between each location in dataframe x with that in dataframe y 
#'
distf=function (x, y) 
{
  i = sort(rep(1:nrow(y), nrow(x)))
  dvec = sqrt((x[, 1] - y[i, 1])^2 + (x[, 2] - y[i, 2])^2)
  matrix(dvec, nrow = nrow(x), ncol = nrow(y), byrow = F)
}
