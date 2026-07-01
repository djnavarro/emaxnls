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

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# 95% confidence interval on the estimation scale
confint(mod)
#>                        2.5%      97.5%
#> E0_cnt_a          0.4634245  0.5088686
#> E0_Intercept      4.9055018  5.2041150
#> Emax_Intercept    9.7525268 10.1914700
#> logEC50_Intercept 8.1908985  8.3454640

# 90% confidence interval on the estimation scale
confint(mod, level = 0.9)
#>                          5%        95%
#> E0_cnt_a          0.4670913  0.5052018
#> E0_Intercept      4.9295962  5.1800202
#> Emax_Intercept    9.7872831 10.1553642
#> logEC50_Intercept 8.2035751  8.3331788

# 95% confidence interval with log-scale parameters back-transformed
confint(mod, back_transform = TRUE)
#>                        2.5%        97.5%
#> E0_cnt_a          0.4634245    0.5088686
#> E0_Intercept      4.9055018    5.2041150
#> Emax_Intercept    9.7525268   10.1914700
#> EC50_Intercept 3607.9625359 4211.0361735
```
