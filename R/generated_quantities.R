#' Generate Quantities
#'
#' Used internally to calculate generated quantities for each draw
#'
#' @param model Stan model object
#' @param standata Data fed to Stan model
#' @param ndraws  is the number of draws to take. Default to 10.
#'
#' @return generated quantities from the model
#' @keywords internal
#' @name generated_quantities

generated_quantities <- function(model, standata, ndraws = NULL) {
  # check stan object
  check_stan_object(model)
  # check to see if ntest is in standata this will allow for gq's to be made
  # for test tag
  check_test_tag <- "ntest" %in% names(standata)
  # Set default number of draws
  if (is.null(ndraws)) {
    ndraws <- 10
  }

  check_num_vec_len(ndraws, vec_length = 1)
  # extract posteriors
  post <- rstan::extract(model)

  # pull everything out from post and put into function environment
  list2env(post, envir = environment())

  # pull everything from standata
  list2env(standata, envir = environment())

  # create vector to loop over
  ndraw <- seq_len(ndraws)
  # list to dump stuf into
  yrep_list <- vector("list", length = ndraws)

  if (check_test_tag) {
    yrep_test_list <- vector("list", length = ndraws)
  }

  for (k in seq_along(ndraw)) {
    # grab extracted values for ndarws
    draw <- ndraw[k]
    a1 <- alpha1[k]

    if (length(dim(alpha0)) %in% 3) {
      # p0 has shape [ndraws, ntime, nrec]
      p0 <- stats::plogis(alpha0[draw, , ])
    } else {
      p0 <- stats::plogis(alpha0[draw])
    }
    # create blank array with the name of eveyrhting

    yrep <- array(
      NA,
      c(nind, nrec, ntime),
      dimnames = list(
        tag = seq_len(nind),
        rec = seq_len(nrec),
        time = seq_len(ntime)
      )
    )

    if (check_test_tag) {
      yrep_test <- array(
        NA,
        c(ntest, nrec, ntime),
        dimnames = list(
          tag = seq_len(ntest),
          rec = seq_len(nrec),
          time = seq_len(ntime)
        )
      )
    }
    # ----- generate quantities ------
    # First for number of detections for each tagged individual
    for (t in 1:ntime) {
      for (i in 1:nind) {
        for (j in 1:nrec) {
          # create distances
          d <- sqrt(
            (sx[draw, i, t] - recX[j])^2 +
              (sy[draw, i, t] - recY[j])^2
          )
          # make this work for when p0 is dimensions
          if (is.matrix(p0)) {
            base <- p0[t, j]
          } else {
            base <- p0
          }
          p <- base * exp(-a1 * d^2)
          # make sure the pobablity is above 0
          p <- min(max(p, 1e-9), 1 - 1e-9)
          # then run int using a the iteration of transmission by probability
          # to get the number of detections
          yrep[i, j, t] <- stats::rbinom(1, ntrans, p)
        }
      }
    }
    yrep_list[[k]] <- yrep

    # ----- run generated quantities for ntest ------
    if (check_test_tag) {
      for (l in 1:ntime) {
        for (m in 1:nrec) {
          for (s in 1:ntest) {
            # Euclidean distance between test tag s and receiver m
            td <- sqrt((testX[s] - recX[m])^2 + (testY[s] - recY[m])^2)
            # Probability
            ptest <- p0[l, m] * exp(-a1 * td^2)
            ptest <- min(max(ptest, 1e-9), 1 - 1e-9)
            # Simulate detection
            yrep_test[s, m, l] <- stats::rbinom(1, ntrans, ptest)
          }
        }
      }
      yrep_test_list[[k]] <- yrep_test
    }
  }
  if (check_test_tag) {
    return(list(yrep = yrep_list, testrep = yrep_test_list))
  } else {
    return(list(yrep = yrep_list))
  }
}
