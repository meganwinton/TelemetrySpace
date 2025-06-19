
# ---- test each argument if it errors apprproatly -----
test_that("test nind of model errors", {

  expect_error(
    COA_Standard(
      nind = rbind(model_param_ex$nind, model_param_ex$nind),  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nind' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_Standard(
      nind = "1",  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nind' must be a numeric vector that has a length of 1."
  )
}
)
# ---- check nrec -----
test_that("test nrec of model errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = rbind(model_param_ex$nrec, model_param_ex$nrec), # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nrec' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = "1", # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'nrec' must be a numeric vector that has a length of 1."
  )
}
)
















# ----- run model and chekc of it works ----
fit <- COA_Standard(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)


# rstan::traceplot(fit$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_standard model results to make sure its consisitent", {
  mean_p0 <- fit$summary[1]
  expected_mean_p0 <- 0.2818
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})





