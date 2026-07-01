# Fitted values for an Emax regression model

Returns the model predictions at the original data points. For
`emaxlogistic` objects, the `type` argument controls whether fitted
probabilities or the linear predictor on the logit scale are returned.

## Usage

``` r
# S3 method for class 'emaxlogistic'
fitted(object, type = c("response", "link"), ...)

# S3 method for class 'emaxnls'
fitted(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- type:

  For `emaxlogistic` objects: `"response"` (default) returns fitted
  probabilities; `"link"` returns the linear predictor on the logit
  scale. Ignored for `emaxnls` objects.

- ...:

  Ignored

## Value

A numeric vector of fitted values, with length equal to the number of
observations in the original data

## Details

For `emaxnls` objects, these are the predicted values from the Emax
curve evaluated at each observation's exposure and covariate values.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
fitted(mod_c)[1:20]
#>  [1] 14.500646 15.591349  5.647906 13.402470 13.561531 16.851552 17.150410
#>  [8] 14.795187  7.407757 12.975508 18.332700 15.783209  6.809797  6.352819
#> [15] 16.615000 16.302892  5.574984 16.150465 15.843635 14.834422

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
fitted(mod_b)[1:20]
#>  [1] 0.70363536 0.90572875 0.01482219 0.39544147 0.53245238 0.98428451
#>  [7] 0.98792349 0.76673516 0.14039964 0.45187209 0.99777959 0.92563660
#> [13] 0.06771991 0.03763388 0.97539178 0.96761542 0.01344634 0.95359520
#> [19] 0.93105223 0.85936669
fitted(mod_b, type = "link")[1:20]
#>  [1]  0.8646697  2.2625636 -4.1966964 -0.4244957  0.1299923  4.1372680
#>  [7]  4.4043433  1.1899670 -1.8119747 -0.1931095  6.1078418  2.5215178
#> [13] -2.6222531 -3.2414902  3.6797588  3.3971523 -4.2955109  3.0228364
#> [19]  2.6029661  1.8100398
```
