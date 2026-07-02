# Estimate parameters for a logistic Emax regression model

Fits a logistic Emax regression model for a binary response variable
using iterative reweighted least squares (IRLS). For continuous
outcomes, use
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
instead.

## Usage

``` r
emax_logistic(
  structural_model,
  covariate_model,
  data,
  init = NULL,
  opts = NULL
)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each specifying a covariate model for a
  structural parameter

- data:

  A data frame that includes all relevant variables

- init:

  Initial values and bounds for parameters. See
  [`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md)

- opts:

  Model fitting and optimization options. See
  [`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md)

## Value

An object of class `emaxlogistic` (which also inherits from `emaxnls`)

## Details

The structural Emax model is placed on the log-odds (logit) scale:

`logit(p) = E0 + Emax * x / (x + EC50)` (hyperbolic)

`logit(p) = E0 + Emax * x^h / (x^h + EC50^h)` (sigmoidal)

Estimation uses iterative reweighted least squares (IRLS). At each outer
iteration a weighted NLS problem is solved using working weights and a
working response derived from the current parameter estimates. This is
equivalent to Fisher scoring and produces maximum likelihood estimates
at convergence.

The interface mirrors the
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
function for continuous response models: the `structural_model` and
`covariate_model` arguments have the same specification, including
support for sigmoidal models via a `logHill` term. The response variable
in `structural_model` must be a binary (0/1) numeric vector.

## See also

[`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md),
[`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md),
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)

## Examples

``` r
emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_2 
#>   Emax type:      hyperbolic 
#>   Response type:  binary (logit link)
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:  400 
#>   Residual df:   396 
#>   Deviance:      331.4698 
#>   AIC:           339.4698 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error  lower  upper
#> 1 E0_cnt_a             0.659    0.0800  0.501  0.816
#> 2 E0_Intercept        -5.00     0.578  -6.14  -3.87 
#> 3 Emax_Intercept       8.12     2.27    5.08  17.6  
#> 4 logEC50_Intercept    9.78     0.518   8.89  11.0  
#> 
#> Use summary() for hypothesis tests.
```
