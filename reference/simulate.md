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
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)
simulate(mod_c)
#> # A tibble: 400 × 11
#>    dat_id sim_id    mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int> <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 14.5  14.7     0.480         5.20           10.0
#>  2      2      1 15.6  15.7     0.480         5.20           10.0
#>  3      3      1  5.79  5.80    0.480         5.20           10.0
#>  4      4      1 13.4  13.7     0.480         5.20           10.0
#>  5      5      1 13.5  13.4     0.480         5.20           10.0
#>  6      6      1 16.9  16.7     0.480         5.20           10.0
#>  7      7      1 17.2  17.3     0.480         5.20           10.0
#>  8      8      1 14.8  14.3     0.480         5.20           10.0
#>  9      9      1  7.52  7.96    0.480         5.20           10.0
#> 10     10      1 12.9  13.0     0.480         5.20           10.0
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_1 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)
simulate(mod_b)
#> # A tibble: 400 × 11
#>    dat_id sim_id     mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int>  <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 0.694      1    0.634        -4.68           6.64
#>  2      2      1 0.886      1    0.634        -4.68           6.64
#>  3      3      1 0.0197     0    0.634        -4.68           6.64
#>  4      4      1 0.410      1    0.634        -4.68           6.64
#>  5      5      1 0.553      1    0.634        -4.68           6.64
#>  6      6      1 0.975      1    0.634        -4.68           6.64
#>  7      7      1 0.983      1    0.634        -4.68           6.64
#>  8      8      1 0.745      1    0.634        -4.68           6.64
#>  9      9      1 0.167      0    0.634        -4.68           6.64
#> 10     10      1 0.483      0    0.634        -4.68           6.64
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_2 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>
```
