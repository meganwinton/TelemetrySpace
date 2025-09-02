# ----- create example gq to transform -------
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
yreps <- list()


# ----- loop over generated quantities -----
for (i in seq_along(all_models)) {
  # Call your function
  yreps[[i]] <- generated_quantities(model = all_models[[i]]$model,
                                     standata = all_data[[i]],
                                     ndraws = ndraws_test)
}


# test if the returned object matches the correct format

test_that("check transformation of gq to matrix", {

  for (i in seq_along(yreps)) {
    tran_gq <- transform_gq(yreps[[i]])
    expect_type(tran_gq, "integer")
    expect_true(is.matrix(tran_gq))
  }
}
)

test_that("check row and column names of gq in matrix", {

  for (i in seq_along(yreps)) {
    tran_gq <- transform_gq(yreps[[i]])
    # check row names
    expect_true(all(grepl("^(yrep|testrep)_[0-9]+$", rownames(tran_gq))))

    # check column names
    expect_true(all(grepl(
      "^tag_[0-9]+_rec_[0-9]+_time_[0-9]+$",
      colnames(tran_gq)
    )))

    # also check correct counts
    expect_length(rownames(tran_gq), nrow(tran_gq))
    expect_length(colnames(tran_gq), ncol(tran_gq))
  }
})



#   for (i in 1:n_draws) {
#     y_rep_mat[i, ] <- as.vector(draws$yrep[i, , , ])
#   }
#   # make sure there's no NA and make sure obs vfallls within a range
#   for (i in 1:n_draws) {
#     expect_false(unique(is.na( y_rep_mat[i, ])))
#     expect_true(all(y_rep_mat[i, ] >= 0 &  y_rep_mat[i, ] <= 25))
#   }
# }

