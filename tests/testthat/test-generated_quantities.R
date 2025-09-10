# ----- make all models into a list -----
all_models <- list(
  model_coa_standard,
  model_coa_time_vary,
  model_coa_tag_int
)
# ---- do the same for the data -----
all_data <- list(
  standata,
  standata,
  standata_1
)
# set the number of draws to test
ndraws_test <- 5


# ----- create function to loop over for errors -----
call_generated_quantities <- function(overrides) {
  do.call(generated_quantities, modifyList(gq_args, overrides))
}
# ----- gq_arguments to check ------
gq_args <- list(
  model = model_coa_standard$model,
  standata = standata,
  ndraws = ndraws_test
)

# ----- check if params error properly ------
params_table <- list(
  list(
    param = "model",
    bad = list("j", NA, c(1, 2)),
    regex = "`model` must be a Stan object \\(from rstan or cmdstanr\\)\\."
  ),
  list(
    param = "ndraws",
    bad = list("a", NA, c(1, 2)),
    regex = "`ndraws` must be a numeric vector that has a length of 1."
  )
)


# ---- see if it errors properly -----
test_that("parameter validation works", {
  for (pt in params_table) {
    for (bad_val in pt$bad) {
      tryCatch(
        {
          expect_error(
            call_generated_quantities(setNames(list(bad_val), pt$param)),
            regexp = pt$regex,
            label = sprintf("param=%s, bad_val=%s", pt$param, deparse1(bad_val))
          )
        },
        error = function(e) {
          cat(
            "\n Error for param:",
            pt$param,
            " bad_val:",
            deparse1(bad_val),
            "\n"
          )
          stop(e)
        }
      )
    }
  }
})


# create empty list to dump all gc to check

yreps <- list()
# ----- loop over generated quantities -----
for (i in seq_along(all_models)) {
  # Call your function
  yreps[[i]] <- generated_quantities(
    model = all_models[[i]]$model,
    standata = all_data[[i]],
    ndraws = ndraws_test
  )
}

# length of  each object returned bs = basic_structure
bs_returned <- c(1, 1, 2)

# length of yrep give the 5 draws
length_yrep <- ndraws_test

# -----  checks the structure of the structure of generated_quantites -------
test_that("generated_quantities returns correct structure", {
  for (s in seq_along(yreps)) {
    bs <- yreps[[s]]

    expect_type(bs, "list")
    expect_length(bs, bs_returned[s])

    for (n in seq_along(bs)) {
      post_draws <- bs[[n]]

      expect_type(post_draws, "list")
      expect_length(post_draws, length_yrep)

      for (h in seq_along(post_draws)) {
        one_draw <- post_draws[[h]]

        expect_true(is.array(one_draw))
        expect_true(any(dim(one_draw) %in% c(1, 10, 30)))

        dn <- dimnames(one_draw)
        expect_named(dn, c("tag", "rec", "time"))
        expect_equal(dn$tag, "1")
        expect_equal(dn$rec, as.character(1:30))
        expect_equal(dn$time, as.character(1:10))
      }
    }
  }
})


# do not test actual values as these will change
test_that("generated_quantities returns correct integer ", {
  for (s in seq_along(yreps)) {
    bs <- yreps[[s]]

    for (n in seq_along(bs)) {
      post_draws <- bs[[n]]
      one_draw <- post_draws[[1]]
      expect_type(one_draw, "integer")
    }
  }
})
