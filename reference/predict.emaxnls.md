# Predicting from Emax regression models

Predicting from Emax regression models

## Usage

``` r
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

  An `emaxnls` object

- newdata:

  A named list or data frame in which to look for variables with which
  to predict. If `newdata` is missing the fitted values at the original
  data points are returned.

- se.fit:

  A switch indicating if standard errors are required.

- interval:

  A character string indicating if prediction intervals or a confidence
  interval on the mean responses are to be calculated. Can be "none",
  "confidence", or "prediction"

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

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# return a vector of predictions
pred <- predict(mod)
pred[1:20]
#>  [1] 14.500646 15.591349  5.647906 13.402470 13.561531 16.851552 17.150410
#>  [8] 14.795187  7.407757 12.975508 18.332700 15.783209  6.809797  6.352819
#> [15] 16.615000 16.302892  5.574984 16.150465 15.843635 14.834422

# return a matrix with confidence intervals
predict(mod, interval = "confidence", se.fit = FALSE)
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
```
