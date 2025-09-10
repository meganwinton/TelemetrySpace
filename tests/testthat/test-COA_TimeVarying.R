# ----- Model checked from setup-test-env is object model_coa_time_vary -----

# ---- test each argument if it errors appropriately -----
# ---- Check if nind errors -----

# Base arguments for COA_Standard
coa_args <- list(
  nind = model_param_ex$nind,
  nrec = model_param_ex$nrec,
  ntime = model_param_ex$tsteps,
  ntrans = model_param_ex$ntrans,
  y = Y,
  recX = rlocs$east,
  recY = rlocs$north,
  xlim = example_extent$xlim,
  ylim = example_extent$ylim,
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)

# Helper to run COA_Standard with overridden args
call_coa_timevarying <- function(overrides) {
  do.call(COA_TimeVarying, modifyList(coa_args, overrides))
}


params_table <- list(
  list(
    param = "nind",
    bad = list("a", NA, c(1, 2)),
    regex = "`nind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "nrec",
    bad = list("a", NA, c(1, 2)),
    regex = "`nrec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntime",
    bad = list("a", NA, c(1, 2)),
    regex = "`ntime` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntrans",
    bad = list(c(model_param_ex$ntrans, model_param_ex$ntrans), "1"),
    regex = "`ntrans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "y",
    bad = list(c(1, 2, 3), "a"),
    regex = "`y` must be a 3-dimensional numeric array."
  ),
  list(
    param = "recX",
    bad = list("a", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad = list("a", NA),
    regex = "`recY` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "xlim",
    bad = list("a", c(1, 2, 3)),
    regex = "`xlim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "ylim",
    bad = list("a", c(1, 2, 3)),
    regex = "`ylim` must be a numeric vector that has a length of 2."
  )
)

# ----- Check Params -----

test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_coa_timevarying(setNames(list(bad_val), pt$param)),
            regexp = pt$regex,
            label = sprintf("param=%s, bad_val=%s", pt$param, deparse(bad_val))
          )
        },
        error = function(e) {
          cat(
            "\n Error for param:",
            pt$param,
            " bad_val:",
            deparse(bad_val),
            "\n"
          )
          stop(e)
        }
      )
    }
  }
})


# ---- run model and check of it works ----
# summary(model_coa_time_vary)

# bayesplot::ppc_dens_overlay(y = as.vector(Y), yrep = model_coa_time_vary$generated_quantities)
# rstan::traceplot(model_coa_time_vary$model, pars = c(
#   # "alpha0",
#   "alpha1",
#   "sigma", "lp__"))
# model_coa_time_vary$coas
test_that("test COA_TimeVarying model results to make sure its consisitent", {
  mean_p0 <- model_coa_time_vary$summary[1]
  expected_mean_p0 <- 0.498
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})


test_that("check to see if model_coa_time_vary classes", {
  expect_type(model_coa_time_vary, "list")
  expect_s4_class(model_coa_time_vary$model, "stanfit")
  expect_s3_class(model_coa_time_vary$coas, "data.frame")
  expect_s3_class(model_coa_time_vary$all_estimates, "data.frame")
  expect_type(model_coa_time_vary$summary, "double")
  expect_true(is.matrix(model_coa_time_vary$summary))
  expect_true(is.matrix(model_coa_time_vary$generated_quantities$yrep))
  expect_type(model_coa_time_vary$generated_quantities, "list")
  expect_true(is.numeric(model_coa_time_vary$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(model_coa_time_vary))
  expect_equal(nrow(model_coa_time_vary$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(model_coa_time_vary$coas),
    c(
      "time",
      "x",
      "y",
      "x_lower",
      "x_upper",
      "y_lower",
      "y_upper"
    )
  )

  for (col in colnames(model_coa_time_vary$coas)) {
    expect_type(model_coa_time_vary$coas[[col]], "double")
    expect_true(all(is.finite(model_coa_time_vary$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- model_coa_time_vary$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(model_coa_time_vary$generated_quantities$yrep) %in% expected)
})
