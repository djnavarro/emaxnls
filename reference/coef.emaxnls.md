# Coefficients for an Emax regression

Returns the named vector of fitted parameter values. Parameters
involving a log transformation (logEC50, logHill) are returned on the
log scale by default, which is the scale on which they are estimated.

## Usage

``` r
# S3 method for class 'emaxnls'
coef(object, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A named numeric vector of parameter estimates

## Details

Setting `back_transform = TRUE` exponentiates logEC50 and logHill and
drops the `log` prefix from their names, expressing them on the
concentration scale rather than the log-concentration scale on which
they are estimated. [`confint()`](https://rdrr.io/r/stats/confint.html)
and [`vcov()`](https://rdrr.io/r/stats/vcov.html) are not affected by
this argument and always return results on the log-concentration scale.
The [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md)
method also accepts `back_transform = TRUE` and applies the same
transformation to its coefficient table, including the confidence
interval columns.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)

# coefficients on the estimation scale
coef(mod_c)
#>          E0_cnt_a      E0_Intercept    Emax_Intercept logEC50_Intercept 
#>         0.4861467         5.0548076         9.9697250         8.2688405 

# coefficients with log-scale parameters back-transformed
coef(mod_c, back_transform = TRUE)
#>       E0_cnt_a   E0_Intercept Emax_Intercept EC50_Intercept 
#>      0.4861467      5.0548076      9.9697250   3900.4236542 

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df,
  opts = emax_logistic_options(max_time = 10)
)
coef(mod_b)
#>          E0_cnt_a      E0_Intercept    Emax_Intercept logEC50_Intercept 
#>         0.6587629        -5.0003872         8.1156731         9.7832270 
```
