
# ---- test each argument if it errors appropriately -----
# ---- Check if nind errors -----
test_that("test nind if it errors", {

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
test_that("test nrec to see if it errors", {

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
# ---- check ntime -----
test_that("test ntime to see if it errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, model_param_ex$nrec, # number of receivers
      ntime = rbind(model_param_ex$tsteps, model_param_ex$tsteps), # number of time steps
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
    regexp = "'ntime' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = "1", # number of time steps
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
    regexp = "'ntime' must be a numeric vector that has a length of 1."
  )
}
)

# ---- check ntrans -----
test_that("test ntime to see if it errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = rbind(model_param_ex$ntrans, model_param_ex$ntrans),
      # number of expected transmissions per tag per time interval
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
    regexp = "'ntrans' must be a numeric vector that has a length of 1."
  )

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = "1", # number of expected transmissions per tag per time interval
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
    regexp = "'ntrans' must be a numeric vector that has a length of 1."
  )
}
)
# ---- check y -----
test_that("test y to see if errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = c(1, 2, 3), # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'y' must be a 3-dimensional numeric array."
  )

}
)
# ---- check recX -----
test_that("test recX to see if errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = "6", # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'recX' must be a numeric vector."
  )

}
)
# ---- check recY-----
test_that("test recY to see if errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = "4", # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'recY' must be a numeric vector."
  )

}

)
# ---- check xlim-----
test_that("test xlim to see if errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = c(1), # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'xlim' must be a numeric vector that has a length of 2."
  )
  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = "1", # E-W boundary of spatial extent (receiver array + buffer)
      ylim = example_extent$ylim, # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'xlim' must be a numeric vector that has a length of 2."
  )

}

)

# ---- check ylim-----
test_that("test ylim to see if errors", {

  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = c(1), # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ylim' must be a numeric vector that has a length of 2."
  )
  expect_error(
    COA_Standard(
      nind = model_param_ex$nind,  # number of individuals
      nrec = model_param_ex$nrec, # number of receivers
      ntime = model_param_ex$tsteps, # number of time steps
      ntrans = model_param_ex$ntrans,
      # number of expected transmissions per tag per time interval
      y = Y, # array of detections
      recX = rlocs$east, # E-W receiver coordinates
      recY = rlocs$north, # N-S receiver coordinates
      xlim = example_extent$ylim, # E-W boundary of spatial extent (receiver array + buffer)
      ylim = "1", # N-S boundary of spatial extent (receiver array + buffer)
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95)
    ),
    regexp = "'ylim' must be a numeric vector that has a length of 2."
  )

}
)

# ---- run model and check of it works ----
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





