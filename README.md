# TelemetrySpace: Fit spatial point process and geostatistical mixed effects models to electronic tagging data

The `TelemetrySpace` package implements the model described in [Winton et al. 2018](https://doi.org/10.1111/2041-210X.13080), *A spatial point process model to estimate individual centres of activity from passive acoustic telemetry data*.

[![R-CMD-check](https://github.com/meganwinton/TelemetrySpace/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/meganwinton/TelemetrySpace/actions/workflows/R-CMD-check.yaml)
[![TelemetrySpace status badge](https://ocean-tracking-network.r-universe.dev/TelemetrySpace/badges/version)](https://ocean-tracking-network.r-universe.dev/TelemetrySpace)

## Note

As of June 2025, `TelemetrySpace` is undergoing rapid development to bring the package up to date with current tooling. You should be able to install and use `TelemetrySpace` to recreate analyses in Winton et al. 2018 by following instructions below, but the package should not be considered stable at this time.


## Installation

We suggest installing via the Ocean Tracking Network's R-universe:

```
install.packages('TelemetrySpace', repos = c('https://ocean-tracking-network.r-universe.dev', getOption("repos"))
```

The `TelemetrySpace` package uses [Stan](http://mc-stan.org/) for model fitting. If you would like to use more-up-to-date versions of Stan or [RStan](https://mc-stan.org/rstan/) dependencies, please follow the instructions at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.
