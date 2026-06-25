# Summary of an Emax regression model

Summary of an Emax regression model

## Usage

``` r
# S3 method for class 'emaxnls'
summary(object, conf_level = 0.95, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` object

- conf_level:

  Confidence level for interval estimates

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A data frame or tibble containing a table of parameter estimates and
other statistical summaries. Please note that the
[`summary()`](https://rdrr.io/r/base/summary.html) method is
experimental (moreso than other methods), and the return value may be
modified in future releases as the package matures.

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# standard summary
summary(mod)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 

# summary with adjusted confidence level
summary(mod, conf_level = 0.99)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.456    0.516
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.86     5.25 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.68    10.3  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.17     8.37 

# summary with log-scale parameters transformed to original scale
summary(mod, back_transform = TRUE)
#> # A tibble: 4 × 7
#>   label          estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>             <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a          0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept      5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept    9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 EC50_Intercept 3900.      NA            210.  0         3608.    4211.   
```
