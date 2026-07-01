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

## Examples

``` r
mod_0 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
mod_1 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# calculate AIC for individual models
AIC(mod_0)
#> [1] 1281.131
AIC(mod_1)
#> [1] 603.6431

# calculate AIC for a sequence of models
AIC(mod_0, mod_1)
#>        df       AIC
#> mod_0 397 1281.1314
#> mod_1 396  603.6431

# calculate BIC for individual models
BIC(mod_0)
#> [1] 1297.097
BIC(mod_1)
#> [1] 623.6004

# calculate BIC for a sequence of models
BIC(mod_0, mod_1)
#>        df       BIC
#> mod_0 397 1297.0973
#> mod_1 396  623.6004
```
