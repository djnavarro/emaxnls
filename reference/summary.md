# Summary of an Emax regression model

Returns a tidy coefficient table for a fitted `emaxnls` model, combining
parameter estimates, standard errors, test statistics, p-values, and
confidence intervals.

## Usage

``` r
# S3 method for class 'emaxlogistic'
summary(object, conf_level = 0.95, back_transform = FALSE, ...)

# S3 method for class 'emaxnls'
summary(object, conf_level = 0.95, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- conf_level:

  Confidence level for interval estimates

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A data frame containing one row per model parameter with columns for the
estimate, standard error, test statistic, p-value, and confidence
interval bounds. The return format is experimental and may change in
future releases.

## Details

The `back_transform` argument applies the same log-scale transformation
as in [`coef()`](https://rdrr.io/r/stats/coef.html) and
[`confint()`](https://rdrr.io/r/stats/confint.html), exponentiating
logEC50 and logHill and renaming them. For continuous Emax regressions,
the test statistic is a t-statistic, whereas binary Emax models report
z-statistics.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# standard summary
summary(mod_c)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 

# summary with adjusted confidence level
summary(mod_c, conf_level = 0.99)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.456    0.516
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.86     5.25 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.68    10.3  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.17     8.37 

# summary with log-scale parameters transformed to original scale
summary(mod_c, back_transform = TRUE)
#> # A tibble: 4 × 7
#>   label          estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>             <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a          0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept      5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept    9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 EC50_Intercept 3900.      NA            210.  0         3608.    4211.   

# logistic emax equivalent
mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
summary(mod_b)
#> # A tibble: 4 × 7
#>   label             estimate std_error z_statistic  p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>    <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.659    0.0800        8.24 1.79e-16    0.501    0.816
#> 2 E0_Intercept        -5.00     0.578        -8.64 5.43e-18   -6.14    -3.87 
#> 3 Emax_Intercept       8.12     2.27          3.58 3.45e- 4    5.08    17.6  
#> 4 logEC50_Intercept    9.78     0.518        18.9  1.20e-79    8.89    11.0  
```
