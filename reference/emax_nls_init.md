# Construct an initial guess for the Emax model parameters

Construct an initial guess for the Emax model parameters

## Usage

``` r
emax_nls_init(structural_model, covariate_model, data)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each of specifying a covariate model for
  a structural parameter

- data:

  A data frame

## Value

A data frame

## Examples

``` r
# use a heuristic to construct sensible start values, and plausible
# upper and lower bounds within which the estimate is expected to fall 
emax_nls_init(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> Error in new.env(parent = parent): 'enclos' must be an environment

# compare to the values estimated:
coef(emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
))
#> Error in new.env(parent = parent): 'enclos' must be an environment
```
