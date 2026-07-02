# Summary of an Emax regression model

Returns a tidy coefficient table for a fitted `emaxnls` or
`emaxlogistic` model, combining parameter estimates, standard errors,
test statistics, p-values, and confidence intervals.

## Usage

``` r
# S3 method for class 'emaxlogistic'
summary(
  object,
  conf_level = 0.95,
  back_transform = FALSE,
  p_adjust = "none",
  simultaneous = FALSE,
  suppress_nonsensical = TRUE,
  ...
)

# S3 method for class 'emaxnls'
summary(
  object,
  conf_level = 0.95,
  back_transform = FALSE,
  p_adjust = "none",
  simultaneous = FALSE,
  suppress_nonsensical = TRUE,
  ...
)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- conf_level:

  Confidence level for interval estimates. Defaults to 0.95.

- back_transform:

  Should logEC50 and logHill parameters be back-transformed to the
  concentration scale? If `TRUE`, these parameters are exponentiated and
  renamed to `EC50` and `Hill` respectively, and their confidence
  intervals are transformed accordingly. Standard errors and test
  statistics on the back-transformed scale are not available and are set
  to `NA`.

- p_adjust:

  Method for adjusting p-values for multiple comparisons, passed to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Defaults
  to `"none"`. Parameters with suppressed p-values (see
  `suppress_nonsensical`) are excluded from the adjustment set.

- simultaneous:

  If `TRUE`, compute simultaneous (joint) confidence intervals using the
  multivariate normal distribution via
  [`mvtnorm::qmvnorm()`](https://rdrr.io/pkg/mvtnorm/man/qmvnorm.html).
  This gives wider intervals that provide joint coverage at `conf_level`
  across all parameters simultaneously. Defaults to `FALSE`.

- suppress_nonsensical:

  If `TRUE` (the default), suppress the test statistic and p-value for
  `logEC50_Intercept`. The logEC50 intercept is estimated on the
  log-concentration scale, and testing `H0: logEC50 = 0` is equivalent
  to testing whether EC50 equals 1 on the concentration scale — a
  threshold with no general pharmacometric meaning. The confidence
  interval for logEC50 is always reported regardless of this setting.
  Pass `suppress_nonsensical = FALSE` to restore the raw test results.

- ...:

  Ignored

## Value

A tibble with one row per model parameter and columns for the estimate,
standard error, test statistic, p-value, and confidence interval bounds.
The column for the test statistic is named `t_statistic` for `emaxnls`
models and `z_statistic` for `emaxlogistic` models. The return format is
experimental and may change in future releases.

## Details

### Which tests are reported by default

Most parameters have a meaningful point null at zero:

- `Emax_Intercept`: tests whether any exposure-response relationship
  exists.

- `logHill_Intercept`: tests whether logHill = 0, i.e., whether the Hill
  parameter equals 1 on the concentration scale, which would mean the
  sigmoidal model reduces to a hyperbolic one.

- `E0_Intercept`: tests whether the baseline response is zero. This is
  informative when the outcome is expressed as change from baseline, a
  common convention in pharmacometrics.

- Covariate beta terms: test whether a given covariate has any effect on
  the corresponding structural parameter.

The one exception is `logEC50_Intercept`. The model is parameterized in
terms of logEC50 (on the log-concentration scale), not EC50 directly, so
the null `H0: logEC50 = 0` corresponds to testing EC50 = 1 on the
concentration scale — a value with no intrinsic pharmacometric meaning.
By default, the test statistic and p-value for `logEC50_Intercept` are
suppressed (set to `NA`), while the confidence interval for logEC50 is
retained. To work with the EC50 on the concentration scale, use
`back_transform = TRUE`.

### Simultaneous intervals

When `simultaneous = TRUE`, a single critical value is derived from the
joint multivariate normal distribution of the standardized parameter
estimates, using the covariance matrix returned by
[`vcov()`](https://rdrr.io/r/stats/vcov.html). The resulting intervals
have simultaneous coverage at `conf_level` and will be wider than the
individual (pointwise) intervals.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)

# standard summary (logEC50_Intercept p-value suppressed by default)
summary(mod_c)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic    p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>      <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1  3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6  4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3  2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394        NA   NA            8.19     8.35 

# show all tests, including the logEC50 intercept
summary(mod_c, suppress_nonsensical = FALSE)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 

# Bonferroni-adjusted p-values
summary(mod_c, p_adjust = "bonferroni")
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic    p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>      <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1  1.09e-147    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6  1.25e-216    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3  6.32e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394        NA   NA            8.19     8.35 

# simultaneous confidence intervals
summary(mod_c, simultaneous = TRUE)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic    p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>      <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1  3.63e-148    0.458    0.514
#> 2 E0_Intercept         5.05     0.0759        66.6  4.16e-217    4.87     5.24 
#> 3 Emax_Intercept       9.97     0.112         89.3  2.11e-264    9.70    10.2  
#> 4 logEC50_Intercept    8.27     0.0394        NA   NA            8.17     8.36 

# adjusted confidence level
summary(mod_c, conf_level = 0.99)
#> # A tibble: 4 × 7
#>   label             estimate std_error t_statistic    p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>      <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.486    0.0116        42.1  3.63e-148    0.456    0.516
#> 2 E0_Intercept         5.05     0.0759        66.6  4.16e-217    4.86     5.25 
#> 3 Emax_Intercept       9.97     0.112         89.3  2.11e-264    9.68    10.3  
#> 4 logEC50_Intercept    8.27     0.0394        NA   NA            8.17     8.37 

# back-transform logEC50 and logHill to concentration scale
summary(mod_c, back_transform = TRUE)
#> # A tibble: 4 × 7
#>   label          estimate std_error t_statistic    p_value ci_lower ci_upper
#>   <chr>             <dbl>     <dbl>       <dbl>      <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a          0.486    0.0116        42.1  3.63e-148    0.463    0.509
#> 2 E0_Intercept      5.05     0.0759        66.6  4.16e-217    4.91     5.20 
#> 3 Emax_Intercept    9.97     0.112         89.3  2.11e-264    9.75    10.2  
#> 4 EC50_Intercept 3900.      NA             NA   NA         3608.    4211.   

# logistic emax equivalent
mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
summary(mod_b)
#> # A tibble: 4 × 7
#>   label             estimate std_error z_statistic   p_value ci_lower ci_upper
#>   <chr>                <dbl>     <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
#> 1 E0_cnt_a             0.659    0.0800        8.24  1.79e-16    0.501    0.816
#> 2 E0_Intercept        -5.00     0.578        -8.64  5.43e-18   -6.14    -3.87 
#> 3 Emax_Intercept       8.12     2.27          3.58  3.45e- 4    5.08    17.6  
#> 4 logEC50_Intercept    9.78     0.518        NA    NA           8.89    11.0  
```
