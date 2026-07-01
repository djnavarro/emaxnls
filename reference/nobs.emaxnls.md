# Number of observations for an Emax regression model

Returns the number of observations used when fitting the model.

## Usage

``` r
# S3 method for class 'emaxnls'
nobs(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

A numeric scalar

## Details

This reflects the actual number of rows passed to the fitting algorithm
after any missing-value handling specified via the `na.action` option in
[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
or
[`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md).
The value is used internally by
[`BIC()`](https://rdrr.io/r/stats/AIC.html) and
[`df.residual()`](https://emaxnls.djnavarro.net/reference/df.residual.md).

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
nobs(mod_c)
#> [1] 400

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
nobs(mod_b)
#> [1] 400
```
