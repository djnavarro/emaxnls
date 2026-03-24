# Add or remove a covariate term from an Emax regression

Add or remove a covariate term from an Emax regression

## Usage

``` r
emax_add_term(mod, formula)

emax_remove_term(mod, formula)
```

## Arguments

- mod:

  An `emaxnls` object

- formula:

  A formula such as E0 ~ AGE

## Value

An object of class `emaxnls`

## Examples

``` r
mod_0 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
mod_1 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), emax_df)

emax_add_term(mod_0, E0 ~ cnt_a)
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
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
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 
emax_remove_term(mod_1, E0 ~ cnt_a)
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
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
#> 1 E0_Intercept          7.42    0.119         62.4 2.52e-207     7.19     7.66
#> 2 Emax_Intercept        9.86    0.251         39.3 7.31e-139     9.37    10.4 
#> 3 logEC50_Intercept     8.16    0.0931        87.6 7.61e-262     7.97     8.35
```
