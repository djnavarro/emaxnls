# Confidence intervals for Emax regression model parameters

Confidence intervals for Emax regression model parameters

## Usage

``` r
# S3 method for class 'emaxnls'
confint(object, parm = NULL, level = 0.95, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` object

- parm:

  A specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  `parm = NULL`, all parameters are considered.

- level:

  The confidence level required

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A matrix (or vector) with columns giving lower and upper confidence
limits for each parameter. These will be labeled as (1-level)/2 and 1 -
(1-level)/2 in % (by default 2.5% and 97.5%).
