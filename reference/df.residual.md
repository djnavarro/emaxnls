# Residual degrees of freedom for an Emax regression model

Returns the residual degrees of freedom, equal to the number of
observations minus the number of estimated parameters.

## Usage

``` r
# S3 method for class 'emaxlogistic'
df.residual(object, ...)

# S3 method for class 'emaxnls'
df.residual(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

A numeric scalar

## Details

For `emaxnls` objects, the value is obtained directly from the
underlying `nls` fit. For `emaxlogistic` objects, it is computed as
`nobs(object) - length(coef(object))`.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
df.residual(mod_c)
#> [1] 396

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
df.residual(mod_b)
#> [1] 396
```
