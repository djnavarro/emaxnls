# Number of observations for an Emax regression model

Number of observations for an Emax regression model

## Usage

``` r
# S3 method for class 'emaxnls'
nobs(object, ...)
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
nobs(mod)
#> [1] 400
```
