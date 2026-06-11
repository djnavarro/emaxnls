# Stepwise covariate modeling for Emax regression

Stepwise covariate modeling for Emax regression

## Usage

``` r
emax_scm_forward(mod, candidates, threshold = 0.01, seed = NULL)

emax_scm_backward(mod, candidates, threshold = 0.001, seed = NULL)

emax_scm_history(mod)
```

## Arguments

- mod:

  An `emaxnls` object

- candidates:

  A list of candidate covariates

- threshold:

  Threshold for addition or removal

- seed:

  Seed for the RNG state

## Value

An object of class `emaxnls`

## Examples

``` r
base_model <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#> Error in eval(fs[[i]][[3]], envir = default_env): object 'term' not found

covariate_list <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)

mm <- emax_scm_forward(
  mod = base_model,
  candidates = covariate_list, 
  threshold = .01
)
#> Error: object 'base_model' not found
final_mod <- emax_scm_backward(
  mod = mm,
  candidates = covariate_list, 
  threshold = .001
) 
#> Error: object 'mm' not found

final_mod
#> Error: object 'final_mod' not found

emax_scm_history(final_mod)
#> Error: object 'final_mod' not found
```
