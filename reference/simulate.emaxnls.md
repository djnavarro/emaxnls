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
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
simulate(mod)
#> # A tibble: 400 × 8
#>    dat_id sim_id    mu   val E0_cnt_a E0_Intercept Emax_Intercept
#>     <int>  <int> <dbl> <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1      1 14.5  14.1     0.468         5.09           10.2
#>  2      2      1 15.6  15.6     0.468         5.09           10.2
#>  3      3      1  5.67  5.24    0.468         5.09           10.2
#>  4      4      1 13.4  12.7     0.468         5.09           10.2
#>  5      5      1 13.6  14.0     0.468         5.09           10.2
#>  6      6      1 16.9  17.0     0.468         5.09           10.2
#>  7      7      1 17.2  17.3     0.468         5.09           10.2
#>  8      8      1 14.9  15.7     0.468         5.09           10.2
#>  9      9      1  7.36  7.42    0.468         5.09           10.2
#> 10     10      1 12.9  12.9     0.468         5.09           10.2
#> # ℹ 390 more rows
#> # ℹ 1 more variable: logEC50_Intercept <dbl>
```
