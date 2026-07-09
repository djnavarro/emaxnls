# Predicting from Emax regression models

Generates predictions from a fitted Emax model, either at the original
data points or at new covariate and exposure values supplied via
`newdata`. Standard errors and confidence or prediction intervals can be
requested.

## Usage

``` r
# S3 method for class 'emaxlogistic'
predict(
  object,
  newdata = NULL,
  type = c("response", "link"),
  se.fit = FALSE,
  interval = "none",
  level = 0.95,
  ...
)

# S3 method for class 'emaxnls'
predict(
  object,
  newdata = NULL,
  se.fit = FALSE,
  interval = "none",
  level = 0.95,
  ...
)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- newdata:

  A named list or data frame in which to look for variables with which
  to predict. If `newdata` is missing the fitted values at the original
  data points are returned.

- type:

  For `emaxlogistic` objects: `"response"` (default) returns predicted
  probabilities; `"link"` returns the linear predictor on the logit
  scale. Ignored for `emaxnls` objects.

- se.fit:

  A switch indicating if standard errors are required.

- interval:

  A character string indicating if prediction intervals or a confidence
  interval on the mean responses are to be calculated. Can be `"none"`,
  `"confidence"`, or `"prediction"`.

- level:

  A numeric scalar between 0 and 1 giving the confidence level for the
  intervals (if any) to be calculated.

- ...:

  Ignored

## Value

The return value differs slightly depending on inputs. When
`se.fit = FALSE`, it produces a vector or matrix of predictions with
column names `fit`, `lwr` and `upr` if the `interval` argument is set.
When `se.fit = TRUE`, it returns a list with the following components:

- `fit`: vector or matrix as above

- `se.fit`: standard error of the predicted means

- `residual.scale`: residual standard deviation

- `df`: residual degrees of freedom

## Details

For `emaxlogistic` objects, when `interval` is set, the bounds are first
computed on the link scale and then passed through the inverse logit
transformation, ensuring they remain in the unit interval on the
probability scale.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)

# return a vector of predictions
predict(mod_c)[1:20]
#>  [1] 14.500646 15.591349  5.647906 13.402470 13.561531 16.851552 17.150410
#>  [8] 14.795187  7.407757 12.975508 18.332700 15.783209  6.809797  6.352819
#> [15] 16.615000 16.302892  5.574984 16.150465 15.843635 14.834422

# return a matrix with confidence intervals
predict(mod_c, interval = "confidence", se.fit = FALSE)
#> # A tibble: 400 × 3
#>      fit   lwr   upr
#>    <dbl> <dbl> <dbl>
#>  1 14.5  14.4  14.6 
#>  2 15.6  15.5  15.7 
#>  3  5.65  5.52  5.78
#>  4 13.4  13.3  13.5 
#>  5 13.6  13.5  13.6 
#>  6 16.9  16.7  17.0 
#>  7 17.2  17.0  17.3 
#>  8 14.8  14.7  14.9 
#>  9  7.41  7.31  7.51
#> 10 13.0  12.9  13.1 
#> # ℹ 390 more rows

# emaxlogistic predicted probabilities
mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)
predict(mod_b)[1:20]
#>  [1] 0.70363536 0.90572875 0.01482219 0.39544147 0.53245238 0.98428451
#>  [7] 0.98792349 0.76673516 0.14039964 0.45187209 0.99777959 0.92563660
#> [13] 0.06771991 0.03763388 0.97539178 0.96761542 0.01344634 0.95359520
#> [19] 0.93105223 0.85936669
predict(mod_b, type = "link")[1:20]
#>  [1]  0.8646697  2.2625636 -4.1966964 -0.4244957  0.1299923  4.1372680
#>  [7]  4.4043433  1.1899670 -1.8119747 -0.1931095  6.1078418  2.5215178
#> [13] -2.6222531 -3.2414902  3.6797588  3.3971523 -4.2955109  3.0228364
#> [19]  2.6029661  1.8100398
```
