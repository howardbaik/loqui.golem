
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{loqui.golem}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{loqui.golem}` like so:

``` r
remotes::install_github("howardbaik/loqui.golem")
```

## Run

You can launch the application by running:

``` r
loqui.golem::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-02-09 17:01:58 EST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ───────────────────────────── loqui.golem 0.0.0.9000 ────
#> Duration: 1m 8.5s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> loqui.golem Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
