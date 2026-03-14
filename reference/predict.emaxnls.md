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
