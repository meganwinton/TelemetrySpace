# ---- this is slopy and I'll refine this is just for me right now ----
# to confirm if the model consistently works as it should
#
library(TelemetrySpace)
library(testthat)
rlocs # Receiver locations
testloc # Test tag location


# Define state-space of point process - 'buffer' adds a fixed buffer to the outer extent of the recs
buffer <- 1
xlim <- c(min(rlocs$east - buffer), max(rlocs$east + buffer))
ylim <- c(min(rlocs$north - buffer), max(rlocs$north + buffer))
# Set up a blank vector for storage
D <- NULL
# Loop over each hour
for (i in 1:nrow(testdat)) {
  D[i] <- distf(testloc[, c('east', 'north')], testdat[i, c('east', 'north')])
}

testdat$D <- unlist(D) # Assign to testdat data frame

# Plot to examine variation in detection rate over time
m1 <- dplyr::count(testdat, Station, hour, D, east, north)
#Round distance for plotting
m1$dist <- round(m1$D * 1000)
#Create label by merging station name and distance
m1$label <- paste(m1$dist, "m", "(", m1$Station, ")")


fishdat <- fishdat[fishdat$hour < 11, ]
testdat <- testdat[testdat$hour < 11, ]

# Create a copy of the receiver locations for tallying
rs <- rlocs
# Add column for each time interval to indicate whether receiver was operational or not
rs[, c(4:(max(fishdat$hour) + 3))] <- 1
rs <- dplyr::rename(
  rs,
  `1` = "...4",
  `2` = "...5",
  `3` = "...6",
  `4` = "...7",
  `5` = "...8",
  `6` = "...9",
  `7` = "...10",
  `8` = "...11",
  `9` = "...12",
  `10` = "...13"
)

# Create vector of the number of sampling occasions for each receiver
tsteps <- apply(rs[, 4:ncol(rs)], 1, sum)


## Starting with the test tag data
testdat$rec <- as.numeric(substr(testdat$Station, 3, 4))
testdat$count <- 1 # Add a column that indicates each record corresponds to 1 detection

# Aggregate the number of detections for each individual at each receiver in each time step
test.agg <- stats::aggregate(
  count ~ Transmitter + rec + east + north + hour,
  testdat,
  sum
)
# Create a numeric identifier for each transmitter
test.agg$tag <- as.numeric(as.factor(test.agg$Transmitter))
# Rename hour to time for consistency when plotting below
test.agg$time <- test.agg$hour

# Specify quantities for indexing
ntest <- length(unique(test.agg$Transmitter)) # number of individual tags (here just the one test tag)
nrec <- nrow(rlocs) # number of receivers
tsteps <- max(test.agg$hour)

# This chunk saves the total number of encounters of each individual (rows) in each trap (cols) at each sampling occasion (array elements)
testY <- array(NA, dim = c(ntest, nrec, tsteps))
for (t in 1:max(tsteps)) {
  for (i in 1:nrow(testY)) {
    h1 <- test.agg[test.agg$tag == i, ]
    for (j in 1:nrow(rlocs)) {
      # If there are no detections at that receiver in that time period, set to 0; otherwise set to the number of detections
      testY[i, j, t] = ifelse(
        identical(h1[h1$hour == t & h1$rec == j, 6], numeric(0)),
        0,
        h1[h1$hour == t & h1$rec == j, 6]
      )
    }
  }
}

## Now do the same for each tagged fish
fishdat$rec <- as.numeric(substr(fishdat$Station, 3, 4))
fishdat$count <- 1 # Add a column that indicates each record corresponds to 1 detection

# Aggregate the number of detections for each individual at each receiver in each time step
fish.agg <- stats::aggregate(
  count ~ Transmitter + rec + east + north + hour,
  fishdat,
  sum
)
# Create a numeric identifier for each transmitter
fish.agg$tag <- as.numeric(as.factor(fish.agg$Transmitter))
# Rename hour to time for consistency when plotting below
fish.agg$time <- fish.agg$hour

# Specify quantities for indexing
nind <- length(unique(fish.agg$Transmitter)) # number of individual tags (here just the one test tag)

# This chunk saves the total number of encounters of each individual (rows) in each trap (cols) at each sampling occasion (array elements)
Y = array(NA, dim = c(nind, nrec, tsteps))
for (t in 1:max(tsteps)) {
  for (i in 1:nrow(Y)) {
    h1 <- fish.agg[fish.agg$tag == i, ]
    for (j in 1:nrow(rlocs)) {
      # If there are no detections at that receiver in that time period, set to 0; otherwise set to the number of detections
      Y[i, j, t] = ifelse(
        identical(h1[h1$hour == t & h1$rec == j, 6], numeric(0)),
        0,
        h1[h1$hour == t & h1$rec == j, 6]
      )
    }
  }
}

### Format data for model fitting
## Specify the number of sentinal tags (this step is necessary because of issues that arise with Stan indexing if you have only 1 test tag)
nsentinal <- 1

fit_vary <- COA_TimeVarying(
  nind = nind, # number of individuals
  nrec = nrec, # number of receivers
  ntime = tsteps, # number of time steps
  ntrans = 30, # number of expected transmissions per tag per time interval
  y = Y, # array of detections from tagged fish
  recX = as.vector(rlocs$east), # E-W receiver coordinates
  recY = as.vector(rlocs$north), # N-S receiver coordinates
  xlim = xlim, # E-W boundary of spatial extent (receiver array + buffer)
  ylim = ylim,
  iter = 3000,
  warmup = 2000,
  chain = 2,
  control = list(adapt_delta = 0.95)
)


rstan::traceplot(fit_vary$model, pars = c("alpha1", "sigma", "lp__"))
fit_vary$coas
test_that("test COA_TagInt model results to make sure its consisitent", {
  mean_p0 <- fit_vary$summary[1]
  expected_mean_p0 <- 0.498
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})
