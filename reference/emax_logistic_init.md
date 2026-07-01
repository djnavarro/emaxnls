# Construct an initial guess for logistic Emax model parameters

Constructs a data frame of starting values and parameter bounds for the
logistic Emax model, using the same heuristic approach as
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
applied to the empirical logit scale rather than the raw response.

## Usage

``` r
emax_logistic_init(structural_model, covariate_model, data)
```

## Arguments

- structural_model:

  A two-sided formula of the form response ~ exposure

- covariate_model:

  A list of two-sided formulas, each specifying a covariate model for a
  structural parameter

- data:

  A data frame

## Value

A data frame with columns `parameter`, `covariate`, `start`, `lower`,
and `upper`

## See also

[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md),
[`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md)

## Examples

``` r
emax_logistic_init(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
#> # A tibble: 4 × 5
#>   parameter covariate start  lower upper
#>   <chr>     <chr>     <dbl>  <dbl> <dbl>
#> 1 E0        cnt_a      0    -25.6   25.6
#> 2 E0        Intercept -6.56 -48.8   35.7
#> 3 Emax      Intercept 19.6  -22.7   61.8
#> 4 logEC50   Intercept  8.91   7.07  10.8
```
