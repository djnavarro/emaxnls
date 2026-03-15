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
#>  1      1      1  6.52  6.55         4.87    0.516           10.0
#>  2      2      1  7.84  7.52         4.87    0.516           10.0
#>  3      3      1  7.14  7.11         4.87    0.516           10.0
#>  4      4      1  7.00  6.87         4.87    0.516           10.0
#>  5      5      1  6.22  6.44         4.87    0.516           10.0
#>  6      6      1  7.42  8.80         4.87    0.516           10.0
#>  7      7      1  6.42  6.45         4.87    0.516           10.0
#>  8      8      1  5.53  5.82         4.87    0.516           10.0
#>  9      9      1  6.95  7.01         4.87    0.516           10.0
#> 10     10      1  8.71  7.75         4.87    0.516           10.0
#> # ℹ 390 more rows
#> # ℹ 1 more variable: logEC50_Intercept <dbl>
```
