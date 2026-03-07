# Simulate responses from Emax regression model

Simulate responses from Emax regression model

## Usage

``` r
# S3 method for class 'emaxnls'
simulate(object, nsim = 1, seed = NULL, ...)
```

## Arguments

- object:

  An `emaxnls` object

- nsim:

  Number of replicates

- seed:

  Used to set RNG seed

- ...:

  Ignored

## Value

A data frame or tibble

## Examples

``` r
mod <- emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
simulate(mod)
#> # A tibble: 400 × 8
#>    dat_id sim_id    mu   val E0_Intercept E0_cnt_a Emax_Intercept
#>     <int>  <int> <dbl> <dbl>        <dbl>    <dbl>          <dbl>
#>  1      1      1  6.45  6.48         4.87    0.516           10.0
#>  2      2      1  7.82  7.50         4.87    0.516           10.0
#>  3      3      1  7.09  7.07         4.87    0.516           10.0
#>  4      4      1  6.95  6.82         4.87    0.516           10.0
#>  5      5      1  6.14  6.36         4.87    0.516           10.0
#>  6      6      1  7.39  8.76         4.87    0.516           10.0
#>  7      7      1  6.35  6.37         4.87    0.516           10.0
#>  8      8      1  5.42  5.71         4.87    0.516           10.0
#>  9      9      1  6.90  6.96         4.87    0.516           10.0
#> 10     10      1  8.72  7.76         4.87    0.516           10.0
#> # ℹ 390 more rows
#> # ℹ 1 more variable: logEC50_Intercept <dbl>
```
