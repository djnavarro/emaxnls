# Add or remove a covariate term from an Emax regression

Add or remove a covariate term from an Emax regression

## Usage

``` r
emax_add_term(object, formula, quiet = FALSE)

emax_remove_term(object, formula, quiet = FALSE)
```

## Arguments

- object:

  An `emaxnls` object

- formula:

  A formula such as E0 ~ AGE

- quiet:

  When quiet=TRUE messages and warnings are suppressed

## Value

An object of class `emaxnls`

## Examples

``` r
mod_0 <- emax_nls(response_1 ~ exposure_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
mod_1 <- emax_nls(response_1 ~ exposure_1, list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), emax_df)

emax_add_term(mod_0, E0 ~ cnt_a)
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
emax_remove_term(mod_1, E0 ~ cnt_a)
#> Structural model:
#> 
#>   Exposure:  exposure_1 
#>   Response:  response_1 
#>   Emax type: hyperbolic 
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#> 1 E0_Intercept          7.41    0.121         61.0 7.36e-204     7.17     7.65
#> 2 Emax_Intercept        9.84    0.244         40.3 1.80e-142     9.37    10.3 
#> 3 logEC50_Intercept     8.17    0.0904        90.3 8.34e-267     7.98     8.34
#> 
#> Variance-covariance matrix:
#> 
#>                   E0_Intercept Emax_Intercept logEC50_Intercept
#> E0_Intercept            0.0147         -0.012            0.0032
#> Emax_Intercept         -0.0124          0.060            0.0146
#> logEC50_Intercept       0.0032          0.015            0.0082
```
