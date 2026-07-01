# Simulate responses from an Emax regression model

Generates simulated response datasets from a fitted Emax model,
propagating uncertainty in the parameter estimates. This is useful for
constructing simulation-based confidence bands, for predictive checks,
or for bootstrapping downstream analyses.

## Usage

``` r
# S3 method for class 'emaxlogistic'
simulate(object, nsim = 1, seed = NULL, ...)

# S3 method for class 'emaxnls'
simulate(object, nsim = 1, seed = NULL, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- nsim:

  Number of replicates

- seed:

  Used to set RNG seed

- ...:

  Ignored

## Value

A data frame with `nsim` columns named `sim_1`, `sim_2`, etc.

## Details

The `simulate()` method samples new parameter values from the
multivariate normal distribution implied by the estimated covariance
matrix, then simulates responses at those parameter values using
[`mvtnorm::rmvnorm()`](https://rdrr.io/pkg/mvtnorm/man/Mvnorm.html). For
`emaxlogistic` objects, predicted probabilities are computed from each
parameter draw and binary outcomes are drawn from `Bernoulli(p)` for
each observation.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
if (requireNamespace("mvtnorm", quietly = TRUE)) simulate(mod_c)
#> # A tibble: 400 × 11
#>    dat_id sim_id    mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int> <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 14.5  14.8     0.490         5.02           10.0
#>  2      2      1 15.6  15.9     0.490         5.02           10.0
#>  3      3      1  5.62  6.12    0.490         5.02           10.0
#>  4      4      1 13.4  14.0     0.490         5.02           10.0
#>  5      5      1 13.6  14.2     0.490         5.02           10.0
#>  6      6      1 16.9  16.9     0.490         5.02           10.0
#>  7      7      1 17.2  17.6     0.490         5.02           10.0
#>  8      8      1 14.8  15.0     0.490         5.02           10.0
#>  9      9      1  7.39  7.48    0.490         5.02           10.0
#> 10     10      1 13.0  13.7     0.490         5.02           10.0
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_1 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
if (requireNamespace("mvtnorm", quietly = TRUE)) simulate(mod_b)
#> # A tibble: 400 × 11
#>    dat_id sim_id     mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int>  <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 0.573      1    0.636        -4.82           11.4
#>  2      2      1 0.853      1    0.636        -4.82           11.4
#>  3      3      1 0.0172     0    0.636        -4.82           11.4
#>  4      4      1 0.276      0    0.636        -4.82           11.4
#>  5      5      1 0.400      0    0.636        -4.82           11.4
#>  6      6      1 0.981      1    0.636        -4.82           11.4
#>  7      7      1 0.981      1    0.636        -4.82           11.4
#>  8      8      1 0.659      0    0.636        -4.82           11.4
#>  9      9      1 0.148      0    0.636        -4.82           11.4
#> 10     10      1 0.340      0    0.636        -4.82           11.4
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_2 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>
```
