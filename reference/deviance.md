# Model deviance for an Emax regression model

Returns a scalar measure of the overall lack of fit. The two model
classes use different definitions of deviance that reflect their
different likelihoods. Both measures decrease as the fit improves and
are used internally by
[`anova()`](https://emaxnls.djnavarro.net/reference/anova.md) for model
comparison.

## Usage

``` r
# S3 method for class 'emaxlogistic'
deviance(object, ...)

# S3 method for class 'emaxnls'
deviance(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

A numeric scalar. For `emaxnls` objects, returns the residual sum of
squares. For `emaxlogistic` objects, returns the binomial deviance
(`-2 * logLik`).

## Examples

``` r
# emaxnls deviance (residual sum of squares)
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)
deviance(mod_c)
#> [1] 103.306

# emaxlogistic deviance (binomial deviance)
mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)
deviance(mod_b)
#> [1] 331.4698
```
