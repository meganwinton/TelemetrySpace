# TelemetrySpace: Fit spatial point process and geostatistical mixed effects models to electronic tagging data
## Installation
The `TelemetrySpace` package uses Stan [http://mc-stan.org/] for model fitting.  

To install from GitHub, you will need to install the `rstan` package and C++ toolchain by following the instructions at [https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started]. Once rstan is successfully installed, install `TelemetrySpace` from GitHub by executing the following in R:
```
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("meganwinton/TelemetrySpace", build_vignettes = FALSE)
```
If you change `build_vignettes` to `TRUE`, installation will take longer. Vignettes are already available in the `vignettes` folder on this page.
Note that installation will take a few minutes because all Stan models are compiled when the package is built.
