# Add or remove a covariate term from an Emax regression

Add or remove a covariate term from an Emax regression

## Usage

``` r
emax_add_term(mod, formula)

emax_remove_term(mod, formula)
```

## Arguments

- mod:

  An `emaxnls` object

- formula:

  A formula such as E0 ~ AGE

## Value

An object of class `emaxnls`

## Examples

``` r
mod_0 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)
#> Error in value[[i]]: subscript out of bounds
mod_1 <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), emax_df)
#> Error in value[[i]]: subscript out of bounds

emax_add_term(mod_0, E0 ~ cnt_a)
#> Error: object 'mod_0' not found
emax_remove_term(mod_1, E0 ~ cnt_a)
#> Error: object 'mod_1' not found
```
