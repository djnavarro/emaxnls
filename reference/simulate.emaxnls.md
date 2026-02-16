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
#>  1      1      1  6.52  6.49         4.96    0.506          10.00
#>  2      2      1  7.84  7.72         4.96    0.506          10.00
#>  3      3      1  7.14  7.36         4.96    0.506          10.00
#>  4      4      1  7.00  8.38         4.96    0.506          10.00
#>  5      5      1  6.22  6.24         4.96    0.506          10.00
#>  6      6      1  7.42  7.71         4.96    0.506          10.00
#>  7      7      1  6.42  6.48         4.96    0.506          10.00
#>  8      8      1  5.53  4.57         4.96    0.506          10.00
#>  9      9      1  6.95  7.39         4.96    0.506          10.00
#> 10     10      1  8.71  8.59         4.96    0.506          10.00
#> # ℹ 390 more rows
#> # ℹ 1 more variable: logEC50_Intercept <dbl>
```
