# Save this file as `R/COA_TimeVarying.R`

#' Fits a test-tag integrated Bayesian Spatial Point Process model to estimate individual centers of activity from acoustic telemetry data using Stan
#'

#' @param nind   Number of tagged individuals
#' @param nrec   Number of receivers
#' @param ntime  Number of time steps
#' @param ntrans Number of expected transmissions per tag per time interval
#' @param y      Array of detection data, where row = individual, column = receiver, and matrix = time step
#' @param recX   Receiver coordinates in the east-west direction (should be projected and scaled for computational efficiency)
#' @param recY   Receiver coordinates in the north-south direction (should be projected and scaled for computational efficiency)
#' @param xlim   East-west boundaries of spatial extent (receiver array + buffer)
#' @param ylim   North-south boundaries of spatial extent (receiver array + buffer)
#' @param ... Additional arguments passed to `sampling` from `rstan`.
#' This can include setting `chains`, `iter`, `warmup`, and `control`. Please see
#' `rstan::sampling` for more info.
#'
#' @return COA_TimeVarying returns an object of class `stanfit` returned by `rstan::sampling`. See the 'rstan' package documentation for details.
#' @return This function returns a list containing the following components: 1) a summary of the detection function parameters; 2) the time required for model fitting; 3) time-varying detection probabilites for each receiver; 4) the estimated COAs for each individual in each time step and 95 percent credible interval; and 5) a dataframe containing values for each parameter and latent parameter from chain iterations. These can be used to plot posterior distributions and the credible interval around each estimated COA.
#'
#' @export
#'
COA_TimeVarying <- function(
  nind,
  nrec,
  ntime,
  ntrans,
  y,
  recX,
  recY,
  xlim,
  ylim,
  ...
) {
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  standata <- list(
    nind = nind,
    nrec = nrec,
    ntime = ntime,
    ntrans = ntrans,
    y = y,
    recX = recX,
    recY = recY,
    xlim = xlim,
    ylim = ylim
  )

  fit_model <- rstan::sampling(stanmodels$COA_TimeVarying, data = standata, ...)

  # Save chains after discarding warmup
  fit_estimates <- as.data.frame(fit_model) # Note this returns parameters and latent states/derived values

  # Summary statistics and convergence diagnostics
  fit_summary <- rstan::summary(fit_model, pars = c("p0", "sigma"))$summary
  #fit_summary <- fit_sum$summary

  # How much time did fitting take (in minutes)?
  fit_time <- sum(print(rstan::get_elapsed_time(fit_model))) / 60

  # Extract COA estimates
  coas <- array(NA, dim = c(ntime, 7, nind))
  dimnames(coas)[[2]] <- c(
    'time',
    'x',
    'y',
    'x_lower',
    'x_upper',
    'y_lower',
    'y_upper'
  )
  ew <- NULL
  ns <- NULL

  for (i in 1:nind) {
    coas[, 1, i] <- seq(1, ntime, 1)
    ew <- dplyr::select(
      fit_estimates,
      dplyr::starts_with(paste("sx[", i, ",", sep = ''))
    )
    ns <- dplyr::select(
      fit_estimates,
      dplyr::starts_with(paste("sy[", i, ",", sep = ''))
    )
    coas[, 2, i] <- apply(ew, 2, stats::median)
    coas[, 3, i] <- apply(ns, 2, stats::median)
    coas[, 4, i] <- apply(ew, 2, stats::quantile, probs = 0.025)
    coas[, 5, i] <- apply(ew, 2, stats::quantile, probs = 0.975)
    coas[, 6, i] <- apply(ns, 2, stats::quantile, probs = 0.025)
    coas[, 7, i] <- apply(ns, 2, stats::quantile, probs = 0.975)
  }
  coas <- as.data.frame(coas[,, 1])
  # Extract time-varying detection probability estimates
  d_probs <- array(NA, dim = c(nrec, ntime))
  p0est <- NULL

  for (i in 1:ntime) {
    p0est <- dplyr::select(
      fit_estimates,
      dplyr::starts_with(paste("p0[", i, ",", sep = ''))
    )
    for (j in 1:nrec) {
      d_probs[j, i] <- stats::median(p0est[, j])
    }
  }

  # Report results
  model_results <- list(
    fit_model,
    fit_summary,
    fit_time,
    coas,
    d_probs,
    fit_estimates
  )
  names(model_results) <- c(
    'model',
    'summary',
    'time',
    'coas',
    'detection_probs',
    'all_estimates'
  )
  return(model_results)
}
