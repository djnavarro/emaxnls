# Akaike information criterion / Bayesian information criterion

Computes AIC or BIC for one or more fitted Emax models. Lower values
indicate a better-fitting model; values are only meaningful in
comparison to other models fitted to the same response variable and
dataset.

## Usage

``` r
# S3 method for class 'emaxlogistic'
AIC(object, ..., k = 2)

# S3 method for class 'emaxlogistic'
BIC(object, ...)

# S3 method for class 'emaxnls'
AIC(object, ..., k = 2)

# S3 method for class 'emaxnls'
BIC(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Optionally, more fitted model objects

- k:

  Penalty per parameter in the AIC

## Value

If just one object is provided, a numeric value with the corresponding
AIC (or BIC). If multiple objects are provided, a data.frame with rows
corresponding to the objects and columns representing the number of
parameters in the model (`df`) and the AIC or BIC.

## Details

AIC applies a penalty of `2 * k` to minus twice the log-likelihood,
where `k` is the number of estimated parameters. BIC applies
`log(n) * k`, making it more conservative than AIC in large samples.
When multiple models are passed, any non-converging models are dropped
with a warning.

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

# emaxlogistic models
mod_b0 <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
mod_b1 <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
AIC(mod_b0, mod_b1)
#>        df      AIC
#> mod_b0  3 444.4336
#> mod_b1  4 339.4698
BIC(mod_b0, mod_b1)
#>        df      BIC
#> mod_b0  3 456.4080
#> mod_b1  4 355.4356
```
