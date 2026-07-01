# Summary of an Emax regression model

Returns a tidy coefficient table for a fitted `emaxnls` model, combining
parameter estimates, standard errors, t-statistics, p-values, and
confidence intervals.

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

A data frame containing one row per model parameter with columns for the
estimate, standard error, test statistic, p-value, and confidence
interval bounds. The return format is experimental and may change in
future releases.

## Details

The `back_transform` argument applies the same log-scale transformation
as in [`coef()`](https://rdrr.io/r/stats/coef.html) and
[`confint()`](https://rdrr.io/r/stats/confint.html), exponentiating
logEC50 and logHill and renaming them. See
[`summary.emaxlogistic()`](https://emaxnls.djnavarro.net/reference/summary.emaxlogistic.md)
for the analogous method for binary response models, which reports
z-statistics rather than t-statistics.

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
