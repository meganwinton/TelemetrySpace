# Save this file as `R/COA_standard.R`

#' Fits a Bayesian Spatial Point Process model to estimate individual centers of activity from acoustic telemetry data using Stan
#'
#' @export
#' @param nind   Number of tagged individuals
#' @param nrec   Number of receivers
#' @param ntime  Number of time steps
#' @param ntrans Number of expected transmissions per tag per time interval
#' @param y      Array of detection data, where row = individual, column = receiver, and matrix = time step
#' @param recX   Receiver coordinates in the east-west direction (should be projected and scaled for computational efficiency)
#' @param recY   Receiver coordinates in the north-south direction (should be projected and scaled for computational efficiency)
#' @param xlim   East-west boundaries of spatial extent (receiver array + buffer)
#' @param ylim   North-south boundaries of spatial extent (receiver array + buffer)
#' @return COA_Standard_Bayes returns an object of class `stanfit` returned by `rstan::sampling`. See the 'rstan' package documentation for details.
#' @return This function returns a list containing the following components: 1) a summary of the detection function parameters; 2) the time required for model fitting; 3) the estimated COAs for each individual in each time step and 95 percent credible interval; and 4) a dataframe containing values for each parameter and latent parameter from chain iterations. These can be used to plot posterior distributions and the credible interval around each estimated COA.
#'
COA_Standard <- function(nind, nrec, ntime, ntrans, y, recX, recY, xlim, ylim) {
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  standata <- list(nind = nind, nrec = nrec, ntime = ntime, ntrans = ntrans, y = y, recX = recX, recY = recY, xlim = xlim, ylim = ylim)
  fit_model <- rstan::sampling(stanmodels$COA_Standard, data = standata,
                               iter=10000, control = list(adapt_delta = 0.95))

  # Save chains after discarding warmup
  fit_estimates <- as.data.frame(fit_model) # Note this returns parameters and latent states/derived values

  # Summary statistics and convergence diagnostics
  fit_summary <- rstan::summary(fit_model, pars = c("p0","sigma") )$summary
  #fit_summary <- fit_sum$summary

  # How much time did fitting take?
  fit_time <- sum(print(rstan::get_elapsed_time(fit_model)))/60

  # Extract COA estimates
  coas <- array(NA, dim=c(ntime, 7, nind))
  dimnames(coas)[[2]] <- c('time','x','y','x.lower','x.upper','y.lower','y.upper')
  ew <- NULL
  ns <- NULL

  for (i in 1:nind){
    coas[,1,i] <- seq(1, ntime, 1)
    ew <- dplyr::select(fit_estimates, dplyr::starts_with( paste("sx[",i,",", sep='') ) )
    ns <- dplyr::select(fit_estimates, dplyr::starts_with( paste("sy[",i,",", sep='') ) )
    coas[,2,i] <- apply(ew, 2, stats::median)
    coas[,3,i] <- apply(ns, 2, stats::median)
    coas[,4,i] <- apply(ew,2,stats::quantile,probs=0.025)
    coas[,5,i] <- apply(ew,2,stats::quantile,probs=0.975)
    coas[,6,i] <- apply(ns,2,stats::quantile,probs=0.025)
    coas[,7,i] <- apply(ns,2,stats::quantile,probs=0.975)
  }

  # Report results
  ModelResults <- list(fit_model,fit_summary, fit_time, coas, fit_estimates)
  names(ModelResults) <- c('Model','Summary','Time','COAs','All_estimates')
  return(ModelResults)
}

