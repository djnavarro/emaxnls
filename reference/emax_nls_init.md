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
#> # A tibble: 3 × 5
#>   parameter covariate start  lower upper
#>   <chr>     <chr>     <dbl>  <dbl> <dbl>
#> 1 E0        Intercept  9.73  0.528  18.9
#> 2 Emax      Intercept  7.74 -1.47   16.9
#> 3 logEC50   Intercept  8.69  6.63   10.8

# compare to the values estimated:
coef(emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
))
#>          E0_cnt_a      E0_Intercept    Emax_Intercept logEC50_Intercept 
#>         0.4861467         5.0548075         9.9697250         8.2688405 
```
