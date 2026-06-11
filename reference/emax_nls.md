# Emax model with arbitrary covariates (does not support interactions)

Emax model with arbitrary covariates (does not support interactions)

## Usage

``` r
emax_nls(structural_model, covariate_model, data, init = NULL, opts = NULL)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each of specifying a covariate model for
  a structural parameter

- data:

  A data frame

- init:

  Initial values and bounds for parameters. See
  [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

- opts:

  Model fitting and optimization options. See
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)

## Value

An object of class `emaxnls`

## Examples

``` r
emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
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
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 
 
```
