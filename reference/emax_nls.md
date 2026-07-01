# Estimate parameters for an Emax regression model

Estimate parameters for an Emax regression model

## Usage

``` r
emax_nls(structural_model, covariate_model, data, init = NULL, opts = NULL)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each of specifying a covariate model for
  a structural parameter

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

The `emax_nls()` function is the workhorse function for estimating an
Emax regression model. Pass a two-sided formula to the
`structural_model` argument to specify the exposure variable and the
response variable (e.g., `response ~ exposure`), and pass a list of
formulas to the `covariate_model` argument to specify covariates of
interest. At a minimum the covariate model requires specification of the
covariate model for the E0 parameter, the Emax parameter, and the
logEC50 parameter. For example, a formula like `E0 ~ age + group` would
indicate that `age` and `group` should both be included as covariates on
the baseline response E0. When no covariates are to be added, use a
formula like `Emax ~ 1`.

The `emax_nls()` function can support sigmoidal emax models as well as
hyperbolic models. To build a sigmoidal model (where the Hill parameter)
is estimated from the data, the `covariate_model` argument must also
include a formula for the `logHill` parameter. For instance, if the
covariate model includes `logHill ~ 1`, the model will estimate the
value of the Hill parameter (with no covariates on it) from the data
set.

At present, `emax_nls()` does not support binary response variables, nor
is it possible to specify interaction terms in the covariate model.

When estimating model parameters, the `init` argument can be used to
specify the starting values for the optimization. If unspecified, the
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
function is used to automatically guess sensible starting values. Please
see the documentation of that function for additional details on
manually specifying the initial values.

The `emax_nls()` function currently supports three optimization methods:
the Gauss-Newton algorithm, the Levenberg-Marquardt algorithm, and the
'nl2sol' algorithm from the Port library. For more information on how to
customize the optimization procedure, please see the documentation for
[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md).

## See also

[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md),
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

## Examples

``` r
emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
#>   Emax type: hyperbolic 
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 
 
```
