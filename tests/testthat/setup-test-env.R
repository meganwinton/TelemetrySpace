nsentinal <- 1
# ----- standata 1 ------
standata <- list(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim
)
standata_1 <- list(
  nind = model_param_ex$nind, # number of individuals
  nrec = model_param_ex$nrec, # number of receivers
  ntime = model_param_ex$tsteps, # number of time steps
  ntrans = model_param_ex$ntrans, # number of expected transmissions per tag per time interval
  y = Y, # array of detections
  recX = rlocs$east, # E-W receiver coordinates
  recY = rlocs$north, # N-S receiver coordinates
  xlim = example_extent$xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = example_extent$ylim,
  ntest = nsentinal,
  test = testY,
  testX = array(testloc$east, dim = c(nsentinal)),
  testY = array(testloc$north, dim = c(nsentinal)) # N-S b
)

# ----- run each model ------
# ----- standard coa ------
model_coa_standard <- do.call(
  COA_Standard,
  c(
    standata,
    list(
      chains = 2,
      warmup = 1000,
      iter = 2000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11
    )
  )
)
# ----- time integrated -----
model_coa_time_vary <- do.call(
  COA_TimeVarying,
  c(
    standata,
    list(
      chains = 2,
      warmup = 3000,
      iter = 7000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11
    )
  )
)

# ----- tag integraged -----
model_coa_tag_int <- do.call(
  COA_TagInt,
  c(
    standata_1,
    list(
      chains = 2,
      warmup = 4000,
      iter = 8000,
      control = list(adapt_delta = 0.95),
      seed = 4,
      ndraws = 11
    )
  )
)
