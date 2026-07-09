# Construct Emax prediction function from model object

Extracts a customizable prediction function from a fitted Emax model,
allowing predictions to be evaluated at arbitrary data and parameter
values.

## Usage

``` r
emax_fun(mod, ...)

# S3 method for class 'emaxlogistic'
emax_fun(mod, ...)

# S3 method for class 'emaxnls'
emax_fun(mod, ...)
```

## Arguments

- mod:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

A function with arguments `param` and `data` that evaluates the Emax
model at the supplied (or default) parameter values and data. For
`emaxnls` objects the return values are on the response scale; for
`emaxlogistic` objects they are predicted probabilities in \\(0, 1)\\.

## Details

The extracted function accepts `data` and `param` arguments. Both
default to the values used when fitting the model. When supplying custom
values, `data` must contain all variables used by the model, and `param`
must be a named numeric vector whose names exactly match those returned
by `coef(mod)`.

**Scale of predictions.** For `emaxnls` objects the returned function
produces predictions on the response scale (the same scale as the
outcome variable). For `emaxlogistic` objects the structural Emax model
is parameterized on the logit scale —
`logit(p) = E0 + Emax * x / (x + EC50)` — but `emax_fun()` applies the
inverse-logit transformation before returning, so predictions are on the
probability scale. This is consistent with the default behavior of
[`fitted()`](https://emaxnls.djnavarro.net/reference/fitted.md) and
[`predict()`](https://emaxnls.djnavarro.net/reference/predict.md) for
`emaxlogistic` objects. If you need the linear predictor (logit scale)
directly, use `fitted(object, type = "link")` or
`predict(object, type = "link")`.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md),
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)

## Examples

``` r

mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)

if (emax_converged(mod_c)) {

  par <- coef(mod_c)
  mod_fn <- emax_fun(mod_c)
  
  # apply the function to a few rows of the original data
  mod_fn(data = emax_df[120:125, ], param = par)
  
  # adjust the parameters and re-evaluate
  new_par <- par
  new_par["E0_Intercept"] <- 0
  mod_fn(data = emax_df[120:125, ], param = new_par)

}
#> [1]  9.595929  3.412750  8.208382 10.674149  1.200782  8.567196

# for emaxlogistic, the returned function gives probabilities
mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)

if (emax_converged(mod_b)) {
  mod_fn_b <- emax_fun(mod_b)
  mod_fn_b(data = emax_df[120:125, ])
}
#> [1] 0.73738728 0.40712302 0.38375750 0.92637415 0.03314224 0.39944030
```
