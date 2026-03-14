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
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> # A tibble: 4 × 5
#>   parameter covariate start  lower upper
#>   <chr>     <chr>     <dbl>  <dbl> <dbl>
#> 1 E0        Intercept  9.57 -0.163 19.3 
#> 2 E0        cnt_a      0    -7.85   7.85
#> 3 Emax      Intercept  7.69 -2.03  17.4 
#> 4 logEC50   Intercept  8.63  6.59  10.7 

# compare to the values estimated:
coef(emax_nls(
  structural_model = response_1 ~ exposure_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
))
#>      E0_Intercept          E0_cnt_a    Emax_Intercept logEC50_Intercept 
#>          4.993052          0.498213         10.002568          8.265336 
```
