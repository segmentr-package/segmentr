
<!-- README.md is generated from README.Rmd. Please edit that file -->

# segmentr

[![Travis-CI Build
Status](https://travis-ci.org/thalesmello/segmentr.svg?branch=master)](https://travis-ci.org/thalesmello/segmentr)

Given a likelihood provided by the user, this package applies it to a
given matrix dataset in order to find change points in the data that
maximize the sum of the likelihoods of all the segments.

This package provides a handful of algorithms with different time
complexities and assumption compromises so the user is able to choose
the best one for the problem at hand.

## Installation

You can install segmentr from GitHub using:

``` r
# install.packages("devtools")
devtools::install_github("thalesmello/segmentr", build_vignettes = TRUE)
```

## Example

Sample code using the package to identify change points in the segments’
means:

``` r
require(segmentr)
#> Loading required package: segmentr

make_segment <- function(n, p) matrix(rbinom(100 * n, 1, p), nrow = 100)
data <- cbind(make_segment(5, 0.1), make_segment(10, 0.9), make_segment(2, 0.1))
mean_lik <- function(X) abs(mean(X) - 0.5) * ncol(X)^2
segment(data, likelihood = mean_lik, algorithm = "hieralg")
#> Segments (total of 3):
#> 
#> 1:5
#> 6:15
#> 16:17
```

For an in depth step-by-step, please check `vignette("segmentr")`.
