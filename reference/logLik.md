# Log-likelihood for an Emax regression model

Evaluates the log-likelihood of a fitted Emax model at the maximum
likelihood estimates. The returned object is compatible with
[`AIC()`](https://emaxnls.djnavarro.net/reference/AIC.md),
[`BIC()`](https://rdrr.io/r/stats/AIC.html), and likelihood ratio tests
via [`anova()`](https://emaxnls.djnavarro.net/reference/anova.md).

## Usage

``` r
# S3 method for class 'emaxlogistic'
logLik(object, REML = FALSE, ...)

# S3 method for class 'emaxnls'
logLik(object, REML = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- REML:

  For `emaxnls` objects only `REML = FALSE` is supported. Ignored for
  `emaxlogistic` objects.

- ...:

  Ignored

## Value

An object of class `logLik` with at least one attribute, `"df"` (degrees
of freedom), giving the number of estimated parameters in the model.

## Details

For `emaxnls` objects, the log-likelihood is computed under the
assumption of normally distributed errors, as returned by
`stats::logLik.nls()`. For `emaxlogistic` objects, it is the binomial
log-likelihood evaluated at the fitted probabilities. The `logLik`
object carries `df` (number of parameters) and `nobs` attributes.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)
logLik(mod_c)
#> 'log Lik.' -296.8216 (df=5)

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)
logLik(mod_b)
#> 'log Lik.' -165.7349 (df=4)
```
