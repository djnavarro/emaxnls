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
simulate(mod_c)
#> # A tibble: 400 × 11
#>    dat_id sim_id    mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int> <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 14.5  14.1     0.485         5.03           9.87
#>  2      2      1 15.6  14.7     0.485         5.03           9.87
#>  3      3      1  5.62  5.27    0.485         5.03           9.87
#>  4      4      1 13.4  13.1     0.485         5.03           9.87
#>  5      5      1 13.6  13.3     0.485         5.03           9.87
#>  6      6      1 16.8  16.9     0.485         5.03           9.87
#>  7      7      1 17.1  17.6     0.485         5.03           9.87
#>  8      8      1 14.8  14.7     0.485         5.03           9.87
#>  9      9      1  7.38  6.66    0.485         5.03           9.87
#> 10     10      1 13.0  13.1     0.485         5.03           9.87
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_1 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
simulate(mod_b)
#> # A tibble: 400 × 11
#>    dat_id sim_id      mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int>   <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 0.672       0    0.753        -5.60           8.41
#>  2      2      1 0.903       1    0.753        -5.60           8.41
#>  3      3      1 0.00918     0    0.753        -5.60           8.41
#>  4      4      1 0.332       0    0.753        -5.60           8.41
#>  5      5      1 0.498       0    0.753        -5.60           8.41
#>  6      6      1 0.986       1    0.753        -5.60           8.41
#>  7      7      1 0.990       1    0.753        -5.60           8.41
#>  8      8      1 0.739       1    0.753        -5.60           8.41
#>  9      9      1 0.124       0    0.753        -5.60           8.41
#> 10     10      1 0.419       0    0.753        -5.60           8.41
#> # ℹ 390 more rows
#> # ℹ 4 more variables: logEC50_Intercept <dbl>, rsp_2 <dbl>, exp_1 <dbl>,
#> #   cnt_a <dbl>
```
