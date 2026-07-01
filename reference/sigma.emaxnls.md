# Residual standard deviation for an Emax regression model

Returns the estimated residual standard deviation from the NLS fit. This
method is only available for `emaxnls` objects; there is no analogous
quantity for `emaxlogistic` models.

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

A numeric scalar

## Details

Under the assumption of normally distributed errors, `sigma` estimates
the standard deviation of the error term in the Emax model. It is
computed as the square root of the residual sum of squares divided by
the residual degrees of freedom.

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
