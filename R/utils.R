#' Error functions
#'
#' @param x is a vector to pass to check.
#' @param vec_length is the length of the vector to check.
#' @param arg_name the name of the argument to check.
#'
#'
#' @keywords internal
#' @name error_functions

check_num_vec_len <- function(x, vec_length = NULL, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.numeric(x) || !is.vector(x) || length(x) != vec_length) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric vector that has a length of {vec_length}."
    )
  }
}

#' @param x is a vector to pass to check.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_array <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.array(x) || !is.numeric(x) || length(dim(x)) != 3) {
    cli::cli_abort("`{arg_name}` must be a 3-dimensional numeric array.")
  }
}


#' @param x is a vector to pass to check.
#' @param len is the length to make the array. This needs to be the
#' same length as `ntest` or the number of tags.
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions
#'
check_array_tag <- function(x, len, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  if (!is.array(x) || !is.numeric(x) || length(x) != len) {
    cli::cli_abort(
      "`{arg_name}` must be a numeric array with length equal to {.val {len}} (the number of test tags)."
    )
  }
}

#' @param x is a `Stan` object
#' @param arg_name the name of the argument to check.
#'
#' @keywords internal
#' @name error_functions

check_stan_object <- function(x, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- rlang::as_label(rlang::enexpr(x))
  }

  # valid classes from rstan and cmdstanr
  valid_classes <- c(
    "stanfit",
    "stanmodel",
    "CmdStanMCMC",
    "CmdStanMLE",
    "CmdStanVB",
    "CmdStanModel"
  )

  if (!inherits(x, valid_classes)) {
    cli::cli_abort(
      "`{arg_name}` must be a Stan object (from {.pkg rstan} or {.pkg cmdstanr})."
    )
  }
}

#' Expected lengths of variables in `standata`
#'
#' @param recX is the receiver or station x coordinates (e.g, lon).
#' @param recY is the receiver or station y coordinates (e.g., lat).
#' @param ntest_len is the number of reference tags which is used as length
#' by `testX` and `testY`.
#'
#'
#' @keywords internal
#' @name expected_lengths

expected_lengths <- function(recX = NULL, recY = NULL, ntest_len = NULL) {
  if (!is.null(ntest_len)) {
    check_num_vec_len(ntest_len, vec_length = 1, arg_name = "ntest")
  }

  lengths <- list(
    nind = 1,
    nrec = 1,
    ntime = 1,
    ntrans = 1,
    ntest = 1,
    recX = length(recX),
    recY = length(recY),
    xlim = 2,
    ylim = 2,
    testX = ntest_len,
    testY = ntest_len
  )
  return(lengths)
}

#' Validate `standata`
#'
#' @param standata is a list of data that will be supplied to the model.
#' @param lengths is the length of each object.
#'
#'
#' @keywords internal
#' @name vaidate_standata

validate_standata <- function(standata, lengths) {
  array_vars <- intersect(c("y", "test", "testX", "testY"), names(standata))

  for (var in array_vars) {
    # check station locations
    if (var %in% c("testX", "testY")) {
      check_array_tag(standata[[var]], len = lengths[[var]], arg_name = var)
    } else {
      # Check 3d array used for counts
      check_array(standata[[var]], arg_name = var)
    }
  }

  # check vectors
  mapply(
    FUN = function(len, name) {
      if (
        !(name %in% array_vars) && !is.null(len) && !is.null(standata[[name]])
      ) {
        check_num_vec_len(standata[[name]], vec_length = len, arg_name = name)
      }
    },
    lengths,
    names(lengths)
  )
}

#' Transform classes and structure of the output of different data objects
#'
#' Transforms output of `generated_quantities()`,
#' @param input list of three dimensional array
#'
#' @return a `matrices` of generated quantities.
#'
#' @keywords internal
#' @name transform_objects

transform_gq <- function(input) {
  # first grab the names of the input
  post_type <- names(input)

  # loop over each object in input and grab the names as
  # well as the actual input.
  output <- lapply(seq_along(input), function(i) {
    group_name <- post_type[i]
    open_input <- input[[i]]

    # check arrays to ensure that they are 3 demisions
    lapply(open_input, check_array)

    # move into matrix
    rep_mat <- do.call(rbind, lapply(open_input, as.vector))

    # rownames with group name + index
    rownames(rep_mat) <- paste0(group_name, "_", seq_along(open_input))

    # start grabbing col names
    dim_x <- dim(open_input[[1]])

    grid <- expand.grid(
      tag = seq_len(dim_x[1]),
      rec = seq_len(dim_x[2]),
      time = seq_len(dim_x[3])
    )

    # add in col names
    colnames(rep_mat) <- apply(grid, 1, function(idx) {
      paste0("tag_", idx[1], "_rec_", idx[2], "_time_", idx[3])
    })
    return(rep_mat)
  })
  names(output) <- post_type
  output
}
