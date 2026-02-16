# Emax model with arbitrary covariates (does not support interactions)

Emax model with arbitrary covariates (does not support interactions)

## Usage

``` r
emax_nls(structural_model, covariate_model, data, quiet = FALSE)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each of specifying a covariate model for
  a structural parameter

- data:

  A data frame

- quiet:

  When quiet=TRUE messages and warnings are suppressed

## Value

An object of class `emaxnls`

## Examples

``` r
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
