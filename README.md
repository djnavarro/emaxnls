
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

## Minimal example

``` r
library(tibble)
library(emaxnls)
set.seed(123)

emax_df
#> # A tibble: 400 × 10
#>     dose exposure_1 exposure_2 response_1 response_2 cnt_a cnt_b cnt_c bin_d
#>    <dbl>      <dbl>      <dbl>      <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     0          0          0       6.49          0  3.06  6.50 5.93      0
#>  2     0          0          0       7.81          0  5.72  3.84 5.60      0
#>  3     0          0          0       7.26          0  4.31  3.68 8.16      0
#>  4     0          0          0       7.45          0  4.03  2.86 9.38      1
#>  5     0          0          0       6.33          0  2.46  3.36 8.29      0
#>  6     0          0          0       7.13          0  4.87  8.90 7.06      0
#>  7     0          0          0       6.07          0  2.87  4.85 0.989     0
#>  8     0          0          0       5.47          1  1.07  5.34 3.34      1
#>  9     0          0          0       7.12          0  3.94  5.68 4.01      1
#> 10     0          0          0       8.21          1  7.46  8.16 6.92      1
#> # ℹ 390 more rows
#> # ℹ 1 more variable: bin_e <dbl>

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
#> 1 E0_Intercept         4.99     0.0740        67.5 3.21e-219    4.85     5.14 
#> 2 E0_cnt_a             0.498    0.0113        44.2 4.30e-155    0.476    0.520
#> 3 Emax_Intercept      10.0      0.104         96.3 7.23e-277    9.80    10.2  
#> 4 logEC50_Intercept    8.27     0.0366       226.  0            8.19     8.34 
#> 
#> Variance-covariance matrix:
#> 
#>                   E0_Intercept E0_cnt_a Emax_Intercept logEC50_Intercept
#> E0_Intercept           0.00548 -6.2e-04       -2.2e-03           4.3e-04
#> E0_cnt_a              -0.00062  1.3e-04        4.2e-05           2.5e-05
#> Emax_Intercept        -0.00224  4.2e-05        1.1e-02           2.6e-03
#> logEC50_Intercept      0.00043  2.5e-05        2.6e-03           1.3e-03
```

## Stepwise covariate modelling

``` r
base_model <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

covariate_list <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)

final_mod <- base_model |> 
  emax_forward(candidates = covariate_list, threshold = .01) |> 
  emax_backward(candidates = covariate_list, threshold = .001) 
#> try add: E0 ~ cnt_c
#> try add: Emax ~ bin_e
#> try add: E0 ~ cnt_b
#> try add: Emax ~ cnt_c
#> try add: Emax ~ cnt_a
#> try add: Emax ~ bin_d
#> try add: E0 ~ cnt_a
#> try add: Emax ~ cnt_b
#> try add: E0 ~ bin_e
#> try add: E0 ~ bin_d
#> addition: E0 ~ cnt_a p: <0.001
#> try add: Emax ~ bin_e
#> try add: E0 ~ bin_e
#> try add: E0 ~ cnt_c
#> try add: Emax ~ cnt_c
#> try add: E0 ~ bin_d
#> try add: Emax ~ cnt_a
#> try add: Emax ~ bin_d
#> try add: Emax ~ cnt_b
#> try add: E0 ~ cnt_b
#> addition: Emax ~ bin_e p: 0.008
#> try add: Emax ~ cnt_c
#> try add: Emax ~ cnt_b
#> try add: E0 ~ cnt_b
#> try add: Emax ~ cnt_a
#> try add: E0 ~ cnt_c
#> try add: E0 ~ bin_d
#> try add: Emax ~ bin_d
#> try add: E0 ~ bin_e
#> no improvements found
#> try remove: Emax ~ bin_e p: 0.008
#> try remove: E0 ~ cnt_a p: <0.001
#> removal: Emax ~ bin_e p: 0.008
#> try remove: E0 ~ cnt_a p: <0.001
#> no improvements found

final_mod
#> Structural model:
#> 
#>   Exposure:  exposure_1 
#>   Response:  response_1 
#>   Emax type: hyperbolic 
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#> 1 E0_Intercept         4.99     0.0740        67.5 3.21e-219    4.85     5.14 
#> 2 E0_cnt_a             0.498    0.0113        44.2 4.30e-155    0.476    0.520
#> 3 Emax_Intercept      10.0      0.104         96.3 7.23e-277    9.80    10.2  
#> 4 logEC50_Intercept    8.27     0.0366       226.  0            8.19     8.34 
#> 
#> Variance-covariance matrix:
#> 
#>                   E0_Intercept E0_cnt_a Emax_Intercept logEC50_Intercept
#> E0_Intercept           0.00548 -6.2e-04       -2.2e-03           4.3e-04
#> E0_cnt_a              -0.00062  1.3e-04        4.2e-05           2.5e-05
#> Emax_Intercept        -0.00224  4.2e-05        1.1e-02           2.6e-03
#> logEC50_Intercept      0.00043  2.5e-05        2.6e-03           1.3e-03
```
