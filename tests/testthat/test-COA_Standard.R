


### Format data for model fitting
fit <- COA_Standard(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = as.vector(rlocs$east), # E-W receiver coordinates
  recY = as.vector(rlocs$north), # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim,
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
) # N-S boundary of spatial extent (receiver array + buffer)


# rstan::traceplot(fit$Model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_standard model results to make sure its consisitent", {
  mean_p0 <- fit$summary[1]
  expected_mean_p0 <- 0.2818
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.005)
})
