.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) {
    loadModule(m, what = TRUE)
  }
}

# ---- global variables ------
# add variables going forward in alphabetical order
utils::globalVariables(
  c(
    "alpha0",
    "alpha1",
    "nind",
    "nrec",
    "ntest",
    "ntime",
    "ntrans",
    "recX",
    "recY",
    "sx",
    "sy",
    "testX",
    "testY"
  )
)
