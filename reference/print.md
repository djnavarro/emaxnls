# Print an Emax regression model object

The `print()` method for `emaxnls` and `emaxlogistic` objects provides a
concise model overview: the structural and covariate formulas, key fit
statistics, and a coefficient table showing estimates and confidence
intervals. Hypothesis tests are deliberately omitted from the printed
output; use
[`summary()`](https://emaxnls.djnavarro.net/reference/summary.md) for
inferential results.

## Usage

``` r
# S3 method for class 'emaxlogistic'
print(x, conf_level = 0.95, ...)

# S3 method for class 'emaxnls'
print(x, conf_level = 0.95, ...)
```

## Arguments

- x:

  An `emaxnls` or `emaxlogistic` object

- conf_level:

  Confidence level for the coefficient intervals shown in the printed
  table. Defaults to 0.95.

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
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6431 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.

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
#> Model fit:
#> 
#>   Observations:  400 
#>   Residual df:   396 
#>   Deviance:      331.4698 
#>   AIC:           339.4698 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error  lower  upper
#> 1 E0_cnt_a             0.659    0.0800  0.501  0.816
#> 2 E0_Intercept        -5.00     0.578  -6.14  -3.87 
#> 3 Emax_Intercept       8.12     2.27    5.08  17.6  
#> 4 logEC50_Intercept    9.78     0.518   8.89  11.0  
#> 
#> Use summary() for hypothesis tests.
```
