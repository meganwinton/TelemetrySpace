# Save this file as `R/coa`

#' Fits a Bayesian Spatial Point Process model to estimate individual centers of activity from acoustic telemetry data using Stan
#'

#' @param n_ind   Number of tagged individuals
#' @param n_rec   Number of receivers
#' @param n_time  Number of time steps
#' @param n_trans Number of expected transmissions per tag per time interval
#' @param det      Array of detection data, where row = individual, column = receiver, and matrix = time step
#' @param rec_x   Receiver coordinates in the east-west direction (should be projected and scaled for computational efficiency)
#' @param rec_y   Receiver coordinates in the north-south direction (should be projected and scaled for computational efficiency)
#' @param lim_x   East-west boundaries of spatial extent (receiver array + buffer)
#' @param lim_y   North-south boundaries of spatial extent (receiver array + buffer)
#' @param ... Additional arguments passed to `sampling` from `rstan`.
#' This can include setting `chains`, `iter`, `warmup`, and `control`. Please see
#' `rstan::sampling` for more info.
#'
#' @return COA_Standard returns an object of class `stanfit` returned by `rstan::sampling`. See the `rstan` package documentation for details.
#' @return This function returns a list containing the following components: 1) a summary of the detection function parameters; 2) the time required for model fitting; 3) the estimated COAs for each individual in each time step and 95 percent credible interval; and 4) a dataframe containing values for each parameter and latent parameter from chain iterations. These can be used to plot posterior distributions and the credible interval around each estimated COA.
#' @seealso [rstan::sampling()]
#'
#' @export
coa <- function(
    n_ind,
    n_rec,
    n_time,
    n_trans,
    det,
    rec_x,
    rec_y,
    lim_x,
    lim_y,
    ...
) {
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  standata <- list(
    n_ind = n_ind,
    n_rec = n_rec,
    n_time = n_time,
    n_trans = n_trans,
    det = det,
    rec_x = rec_x,
    rec_y = rec_y,
    lim_x = lim_x,
    lim_y = lim_y
  )

  fit_model <- rstan::sampling(stanmodels$coa, data = standata, ...)

  # Save chains after discarding warmup
  fit_estimates <- as.data.frame(fit_model)
  # Note this returns parameters and latent states/derived values

  # Summary statistics and convergence diagnostics
  fit_summary <- rstan::summary(fit_model, pars = c("p0", "sigma"))$summary
  #fit_summary <- fit_sum$summary

  # How much time did fitting take?
  fit_time <- sum(print(rstan::get_elapsed_time(fit_model))) / 60

  # Extract COA estimates
  coas <- array(NA, dim = c(n_time, 7, n_ind))
  dimnames(coas)[[2]] <- c(
    'time',
    'x',
    'y',
    'x_lower',
    'x_upper',
    'y_lower',
    'y_upper'
  )
  e_w <- NULL
  n_s <- NULL

  for (i in 1:n_ind) {
    coas[, 1, i] <- seq(1, n_time, 1)
    e_w <- dplyr::select(
      fit_estimates,
      dplyr::starts_with(paste("x[", i, ",", sep = ''))
    )
    n_s <- dplyr::select(
      fit_estimates,
      dplyr::starts_with(paste("y[", i, ",", sep = ''))
    )
    coas[, 2, i] <- apply(e_w, 2, stats::median)
    coas[, 3, i] <- apply(n_s, 2, stats::median)
    coas[, 4, i] <- apply(e_w, 2, stats::quantile, probs = 0.025)
    coas[, 5, i] <- apply(e_w, 2, stats::quantile, probs = 0.975)
    coas[, 6, i] <- apply(n_s, 2, stats::quantile, probs = 0.025)
    coas[, 7, i] <- apply(n_s, 2, stats::quantile, probs = 0.975)
  }

  coas <- as.data.frame(coas[ , , 1])
  # Report results
  model_results <- list(fit_model, fit_summary, fit_time, coas, fit_estimates)
  names(model_results) <- c('model', 'summary', 'time', 'coas', 'all_estimates')
  return(model_results)
}
