# Residuals for an Emax regression model

For `emaxnls` objects, returns raw residuals on the response scale. For
`emaxlogistic` objects, Pearson or deviance residuals are available via
the `type` argument.

## Usage

``` r
# S3 method for class 'emaxlogistic'
residuals(object, type = c("pearson", "deviance"), ...)

# S3 method for class 'emaxnls'
residuals(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- type:

  For `emaxlogistic` objects: the type of residuals to return.
  `"pearson"` (default) returns Pearson residuals; `"deviance"` returns
  deviance residuals. Ignored for `emaxnls` objects.

- ...:

  Ignored

## Value

A numeric vector of residuals

## Details

Pearson residuals are the raw residuals divided by
`sqrt(mu * (1 - mu))`, the estimated standard deviation of a Bernoulli
observation, giving a standardized measure of discrepancy. Deviance
residuals are the signed square root of each observation's contribution
to the total binomial deviance; their sum of squares equals the model
deviance returned by
[`deviance()`](https://emaxnls.djnavarro.net/reference/deviance.md).

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
residuals(mod_c)[1:20]
#>  [1]  1.169353898 -0.331348862  0.002093523 -0.942470064 -0.311530566
#>  [6] -0.051552000  0.249590372  1.074812535 -0.107757380 -0.155508320
#> [11] -0.142699949  0.566790807 -0.919796990 -0.562819131  0.645000441
#> [16]  0.567107657 -0.444984478  0.479534720  0.076365401 -0.194421634

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
residuals(mod_b)[1:20]
#>  [1]  0.6489920  0.3226195 -0.1226589 -0.8087642  0.9370711  0.1263583
#>  [7]  0.1105628 -1.8130010 -0.4041427  1.1013699  0.0471736  0.2834388
#> [13] -0.2695163 -0.1977513  0.1588366  0.1829438 -0.1167459  0.2205969
#> [19]  0.2721279  0.4045338
residuals(mod_b, type = "deviance")[1:20]
#>  [1]  0.83844501  0.44500653 -0.17281864 -1.00325150  1.12273043  0.17799039
#>  [7]  0.15588470 -1.70621267 -0.55006853  1.26044129  0.06667647  0.39312482
#> [13] -0.37449161 -0.27698491  0.22323109  0.25659526 -0.16454519  0.30827265
#> [19]  0.37799446  0.55056257
```
