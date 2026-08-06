# Construct an initial guess for the Emax model parameters

Constructs a data frame of starting values and parameter bounds for the
Emax NLS optimization, using heuristics derived from the data.

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

## Details

The [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
function requires that the user specify the initial values for the model
parameters. Specifically, it expects to be supplied with a data frame
with columns named `parameter`, `covariate`, and `start`. If a bounded
optimization method is used (e.g. if the "port" method is used), the
data frame also needs to have columns named `lower` and `upper`. The
data frame should contain one row per parameter. In most cases the user
does not need to define this manually, because `emax_nls_init()` can use
heuristics to make a sensible guess about what to use as starting
values. By default this is what
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
relies upon, automatically calling `emax_nls_init()` using the
appropriate values for the `structural_model`, the `covariate_model`,
and the `data`.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md),
[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)

## Examples

``` r
# use a heuristic to construct sensible start values, and plausible
# upper and lower bounds within which the estimate is expected to fall 
emax_nls_init(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
#> # A tibble: 4 × 5
#>   parameter covariate start  lower upper
#>   <chr>     <chr>     <dbl>  <dbl> <dbl>
#> 1 E0        cnt_a      0    -7.84   7.84
#> 2 E0        Intercept  9.73  0.528 18.9 
#> 3 Emax      Intercept  7.74 -1.47  16.9 
#> 4 logEC50   Intercept  8.69  6.63  10.8 

# compare to the values estimated:
coef(emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
))
#>          E0_cnt_a      E0_Intercept    Emax_Intercept logEC50_Intercept 
#>         0.4861467         5.0548076         9.9697250         8.2688405 
```
