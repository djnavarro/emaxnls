# Log-likelihood for an Emax regression model

Log-likelihood for an Emax regression model

## Usage

``` r
# S3 method for class 'emaxnls'
logLik(object, REML = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` object

- REML:

  For `nls` objects only `REML = FALSE` is supported

- ...:

  Ignored

## Value

Returns an object of class `logLik`. This is a number with at least one
attribute, "df" (degrees of freedom), giving the number of (estimated)
parameters in the model.
