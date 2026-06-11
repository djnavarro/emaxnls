# Summary of an Emax regression model

Summary of an Emax regression model

## Usage

``` r
# S3 method for class 'emaxnls'
summary(object, conf_level = 0.95, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` object

- conf_level:

  Confidence level for interval estimates

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A data frame or tibble containing a table of parameter estimates and
other statistical summaries. Please note that the
[`summary()`](https://rdrr.io/r/base/summary.html) method is
experimental (moreso than other methods), and the return value may be
modified in future releases as the package matures.

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Error in new.env(parent = parent): 'enclos' must be an environment

summary(mod)
#> Error: object 'mod' not found
summary(mod, conf_level = 0.99)
#> Error: object 'mod' not found
summary(mod, back_transform = TRUE)
#> Error: object 'mod' not found
```
