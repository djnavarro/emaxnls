# Summary of a logistic Emax regression model

Summary of a logistic Emax regression model

## Usage

``` r
# S3 method for class 'emaxlogistic'
summary(object, conf_level = 0.95, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxlogistic` object

- conf_level:

  Confidence level for interval estimates

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  the original scale?

- ...:

  Ignored

## Value

A data frame containing a table of parameter estimates with
z-statistics, p-values, and confidence intervals
