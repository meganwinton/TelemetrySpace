# ----- Model checked from setup-test-env is object model_coa_tag_int-----

# ---- test each argument if it errors appropriately -----
coa_args <- list(
  nind = model_param_ex$nind,
  nrec = model_param_ex$nrec,
  ntime = model_param_ex$tsteps,
  ntrans = model_param_ex$ntrans,
  ntest = nsentinal,
  y = Y,
  test = testY,
  recX = rlocs$east,
  recY = rlocs$north,
  xlim = example_extent$xlim,
  ylim = example_extent$ylim,
  testX = array(testloc$east, dim = c(nsentinal)),
  testY = array(testloc$north, dim = c(nsentinal)),
  chains = 2,
  warmup = 1000,
  iter = 2000,
  control = list(adapt_delta = 0.95)
)
# ----- call coa_tag
call_coa_tagint <- function(overrides) {
  do.call(COA_TagInt, modifyList(coa_args, overrides))
}
# ----- create param tables
params_table <- list(
  list(
    param = "nind",
    bad = list("bc", NA, c(1, 2)),
    regex = "`nind` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "nrec",
    bad = list("bc", NA, c(1, 2)),
    regex = "`nrec` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntime",
    bad = list("bc", NA, c(1, 2)),
    regex = "`ntime` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntrans",
    bad = list(c(model_param_ex$ntrans, model_param_ex$ntrans), "1"),
    regex = "`ntrans` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "ntest",
    bad = list(c(3, 6, 3), "1"),
    regex = "`ntest` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "y",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`y` must be a 3-dimensional numeric array."
  ),
  list(
    param = "test",
    bad = list(c(1, 2, 3), "bc"),
    regex = "`test` must be a 3-dimensional numeric array."
  ),
  list(
    param = "recX",
    bad = list("bc", NA),
    regex = "`recX` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "recY",
    bad = list("bc", NA),
    regex = "`recY` must be a numeric vector that has a length of 1."
  ),
  list(
    param = "xlim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`xlim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "ylim",
    bad = list("bc", c(1, 2, 3)),
    regex = "`ylim` must be a numeric vector that has a length of 2."
  ),
  list(
    param = "testX",
    bad = list("bc", NA),
    regex = "`testX` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  ),
  list(
    param = "testY",
    bad = list("bc", NA),
    regex = "`testY` must be a numeric array with length equal to 1 \\(the number of test tags\\)\\."
  )
)

params_table
# ----- Check Params -----

test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_coa_tagint(setNames(list(bad_val), pt$param)),
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

# rstan::traceplot(model_coa_tag_int$model, pars = c("alpha0", "alpha1",
#                                      "sigma", "lp__"))

test_that("test COA_TagInt model results to make sure its consisitent", {
  mean_p0 <- model_coa_tag_int$summary[1]

  expected_mean_p0 <- 0.486
  expect_equal(mean_p0, expected_mean_p0, tolerance = 0.05)
})


test_that("check to see if model_coa_tag_int classes", {
  expect_type(model_coa_tag_int, "list")
  expect_s4_class(model_coa_tag_int$model, "stanfit")
  expect_s3_class(model_coa_tag_int$coas, "data.frame")
  expect_s3_class(model_coa_tag_int$all_estimates, "data.frame")
  expect_type(model_coa_tag_int$summary, "double")
  expect_true(is.matrix(model_coa_tag_int$summary))
  expect_type(model_coa_tag_int$generated_quantities, "list")
  expect_true(is.matrix(model_coa_tag_int$generated_quantities$yrep))
  expect_true(is.matrix(model_coa_tag_int$generated_quantities$testrep))
  expect_true(is.numeric(model_coa_tag_int$time))
})


test_that("check to see if coa returns proper info", {
  expect_true("coas" %in% names(model_coa_tag_int))
  expect_equal(nrow(model_coa_tag_int$coas), model_param_ex$tsteps)
  expect_equal(
    colnames(model_coa_tag_int$coas),
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

  for (col in colnames(model_coa_tag_int$coas)) {
    expect_type(model_coa_tag_int$coas[[col]], "double")
    expect_true(all(is.finite(model_coa_tag_int$coas[[col]])))
  }
})

test_that("check to see model converged and has a good rhat", {
  rhat <- model_coa_tag_int$summary[, "Rhat"]
  expect_true(all(rhat > 0.95 & rhat < 1.05))
})

# ----- check if gq retruns the correct length ------

test_that("check to see if gq is the correct length", {
  expected <- 11
  expect_true(nrow(model_coa_tag_int$generated_quantities$yrep) %in% expected)
  expect_true(
    nrow(model_coa_tag_int$generated_quantities$testrep) %in% expected
  )
})
