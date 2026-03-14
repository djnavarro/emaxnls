# Stepwise covariate modelling for Emax regression

Stepwise covariate modelling for Emax regression

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
base_model <- emax_nls(response_1 ~ exposure_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)

covariate_list <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)

mm <- emax_scm_forward(
  mod = base_model,
  candidates = covariate_list, 
  threshold = .01
)
final_mod <- emax_scm_backward(
  mod = mm,
  candidates = covariate_list, 
  threshold = .001
) 

final_mod
#> Structural model:
#> 
#>   Exposure:  exposure_1 
#>   Response:  response_1 
#>   Emax type: hyperbolic 
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Coefficient table:
#> 
#>   label             estimate std_error t_statistic   p_value ci_lower ci_upper
#> 1 E0_Intercept         4.99     0.0740        67.5 3.21e-219    4.85     5.14 
#> 2 E0_cnt_a             0.498    0.0113        44.2 4.30e-155    0.476    0.520
#> 3 Emax_Intercept      10.0      0.104         96.3 7.23e-277    9.80    10.2  
#> 4 logEC50_Intercept    8.27     0.0366       226.  0            8.19     8.34 

emax_scm_history(final_mod)
#> # A tibble: 32 × 11
#>    iteration attempt step       action term_tested  model_tested model_converged
#>        <int>   <int> <chr>      <chr>  <chr>        <chr>        <lgl>          
#>  1         0       0 base model NA     NA           E0 ~ 1, Ema… TRUE           
#>  2         1       1 forward    add    Emax ~ cnt_b E0 ~ 1, Ema… TRUE           
#>  3         1       2 forward    add    E0 ~ bin_e   E0 ~ 1 + bi… TRUE           
#>  4         1       3 forward    add    Emax ~ cnt_a E0 ~ 1, Ema… TRUE           
#>  5         1       4 forward    add    Emax ~ bin_e E0 ~ 1, Ema… TRUE           
#>  6         1       5 forward    add    E0 ~ bin_d   E0 ~ 1 + bi… TRUE           
#>  7         1       6 forward    add    E0 ~ cnt_a   E0 ~ 1 + cn… TRUE           
#>  8         1       7 forward    add    Emax ~ bin_d E0 ~ 1, Ema… TRUE           
#>  9         1       8 forward    add    Emax ~ cnt_c E0 ~ 1, Ema… TRUE           
#> 10         1       9 forward    add    E0 ~ cnt_c   E0 ~ 1 + cn… TRUE           
#> # ℹ 22 more rows
#> # ℹ 4 more variables: term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>,
#> #   model_updated <lgl>
```
