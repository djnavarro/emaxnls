# Estimate parameters for an Emax regression model

Fits an Emax regression model for a continuous response variable using
nonlinear least squares. For binary outcomes, use
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
instead.

## Usage

``` r
emax_nls(structural_model, covariate_model, data, init = NULL, opts = NULL)
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
  [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

- opts:

  Model fitting and optimization options. See
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)

## Value

An object of class `emaxnls`

## Details

Pass a two-sided formula to `structural_model` to specify the response
and exposure variables (e.g., `response ~ exposure`), and a list of
formulas to `covariate_model` to specify covariates. At a minimum the
covariate model requires formulas for E0, Emax, and logEC50. A formula
like `E0 ~ age + group` includes `age` and `group` as covariates on the
baseline response; use `Emax ~ 1` when no covariates are to be added for
a parameter.

To fit a sigmoidal Emax model (estimating the Hill parameter), include a
formula for `logHill` in `covariate_model`, e.g. `logHill ~ 1`. Without
this term a hyperbolic model is fitted. Interaction terms in the
covariate model are not currently supported.

Starting values are constructed automatically via
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
unless the `init` argument is supplied manually. Three optimization
algorithms are available; see
[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
for details.

## See also

[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md),
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

## Examples

``` r
emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6431 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.
 
```
