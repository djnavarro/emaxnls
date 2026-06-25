# Residual standard deviation for Emax regression models

Residual standard deviation for Emax regression models

## Usage

``` r
# S3 method for class 'emaxnls'
sigma(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Ignored

## Value

Numeric

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
sigma(mod)
#> [1] 0.510758
```
