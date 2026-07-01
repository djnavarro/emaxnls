# Print an Emax regression model object

The `print()` method for `emaxnls` and `emaxlogistic` regression models
reports the structural and covariate model along with a table of
coefficients, test statistics, and confidence intervals

## Usage

``` r
# S3 method for class 'emaxlogistic'
print(x, ...)

# S3 method for class 'emaxnls'
print(x, ...)
```

## Arguments

- x:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

Invisibly returns the original object

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
print(mod_c)
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

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
print(mod_b)
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_2 
#>   Emax type:      hyperbolic 
#>   Response type: binary (logit link)
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error z_statistic  p_value ci_lower ci_upper
#> 1 E0_cnt_a             0.659    0.0800        8.24 1.79e-16    0.501    0.816
#> 2 E0_Intercept        -5.00     0.578        -8.64 5.43e-18   -6.14    -3.87 
#> 3 Emax_Intercept       8.12     2.27          3.58 3.45e- 4    5.08    17.6  
#> 4 logEC50_Intercept    9.78     0.518        18.9  1.20e-79    8.89    11.0  
#> 
#> Deviance: 331.4698 
#> AIC:      339.4698 
```
