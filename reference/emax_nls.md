# Emax model with arbitrary covariates (does not support interactions)

Emax model with arbitrary covariates (does not support interactions)

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

  A data frame

- init:

  Initial values and bounds for parameters. See
  [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

- opts:

  Model fitting and optimization options. See
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)

## Value

An object of class `emaxnls`

## Examples

``` r
emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Error in new.env(parent = parent): 'enclos' must be an environment
 
```
