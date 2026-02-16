# Akaike information criterion / Bayesian information criterion

Akaike information criterion / Bayesian information criterion

## Usage

``` r
# S3 method for class 'emaxnls'
AIC(object, ..., k = 2)

# S3 method for class 'emaxnls'
BIC(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Optionally, more fitted model objects

- k:

  Penalty per parameter in the AIC

## Value

If just one object is provided, a numeric value with the corresponding
AIC (or BIC). If multiple objects are provided, a data.frame with rows
corresponding to the objects and columns representing the number of
parameters in the model (df) and the AIC or BIC.
