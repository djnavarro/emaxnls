
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emaxnls

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/emaxnls/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djnavarro/emaxnls/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/emaxnls/graph/badge.svg)](https://app.codecov.io/gh/djnavarro/emaxnls)
<!-- badges: end -->

The **emaxnls** package provides tools for nonlinear least squares
estimation for Emax regression models.

## Installation

You can install the development version of emaxnls from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("djnavarro/emaxnls")
```

## Example

``` r
library(emaxnls)
emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Structural model:
#> 
#>   Exposure:  exposure_1 
#>   Response:  response_1 
#>   Emax type: hyperbolic 
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#> 1 E0_Intercept         6.89     0.225         30.6 1.99e-106    6.45     7.33 
#> 2 E0_cnt_a             0.487    0.0343        14.2 2.61e- 37    0.420    0.555
#> 3 Emax_Intercept       5        0.233         21.5 2.19e- 68    4.54     5.46 
#> 4 logEC50_Intercept    6.65     0.381         17.5 4.95e- 51    5.90     7.40 
#> 
#> Variance-covariance matrix:
#> 
#>                   E0_Intercept E0_cnt_a Emax_Intercept logEC50_Intercept
#> E0_Intercept            0.0507 -0.00569       -0.02357           0.00186
#> E0_cnt_a               -0.0057  0.00118        0.00015           0.00085
#> Emax_Intercept         -0.0236  0.00015        0.05422           0.05088
#> logEC50_Intercept       0.0019  0.00085        0.05088           0.14519
```
