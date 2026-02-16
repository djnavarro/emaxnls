# Stepwise covariate modelling for Emax regression

Stepwise covariate modelling for Emax regression

## Usage

``` r
emax_forward(
  mod,
  candidates,
  threshold = 0.01,
  quiet = FALSE,
  history = TRUE,
  seed = NULL
)

emax_backward(
  mod,
  candidates,
  threshold = 0.001,
  quiet = FALSE,
  history = TRUE,
  seed = NULL
)

emax_history(mod)
```

## Arguments

- mod:

  An `emaxnls` object

- candidates:

  A list of candidate covariates

- threshold:

  Threshold for addition or removal

- quiet:

  When quiet=TRUE messages and warnings are suppressed

- history:

  When history=TRUE the sequence of models tested is stored

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

final_mod <- base_model |> 
  emax_forward(candidates = covariate_list, threshold = .01) |> 
  emax_backward(candidates = covariate_list, threshold = .001) 
#> try add: E0 ~ bin_d
#> try add: Emax ~ cnt_a
#> try add: E0 ~ cnt_a
#> try add: E0 ~ bin_e
#> try add: Emax ~ cnt_b
#> try add: Emax ~ bin_d
#> try add: E0 ~ cnt_b
#> try add: Emax ~ bin_e
#> try add: Emax ~ cnt_c
#> try add: E0 ~ cnt_c
#> try add: E0 ~ bin_e
#> try add: Emax ~ bin_e
#> try add: E0 ~ bin_d
#> try add: Emax ~ bin_d
#> try add: E0 ~ cnt_b
#> try add: Emax ~ cnt_c
#> try add: Emax ~ cnt_b
#> try add: Emax ~ cnt_a
#> try add: E0 ~ cnt_c
#> try add: Emax ~ cnt_a
#> try add: Emax ~ cnt_b
#> try add: Emax ~ cnt_c
#> try add: E0 ~ bin_e
#> try add: E0 ~ cnt_b
#> try add: E0 ~ cnt_c
#> try add: E0 ~ bin_d
#> try add: Emax ~ bin_d
#> try remove: E0 ~ cnt_a
#> try remove: Emax ~ bin_e
#> try remove: E0 ~ cnt_a

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
#> 
#> Variance-covariance matrix:
#> 
#>                   E0_Intercept E0_cnt_a Emax_Intercept logEC50_Intercept
#> E0_Intercept           0.00548 -6.2e-04       -2.2e-03           4.3e-04
#> E0_cnt_a              -0.00062  1.3e-04        4.2e-05           2.5e-05
#> Emax_Intercept        -0.00224  4.2e-05        1.1e-02           2.6e-03
#> logEC50_Intercept      0.00043  2.5e-05        2.6e-03           1.3e-03

emax_history(final_mod)
#> # A tibble: 32 × 9
#>    iteration attempt step       action term_tested  model_tested model_converged
#>        <int>   <int> <chr>      <chr>  <chr>        <chr>        <lgl>          
#>  1         0       0 base model NA     NA           E0 ~ 1, Ema… FALSE          
#>  2         1       1 forward    add    E0 ~ bin_d   E0 ~ 1 + bi… TRUE           
#>  3         1       2 forward    add    Emax ~ cnt_a E0 ~ 1, Ema… TRUE           
#>  4         1       3 forward    add    E0 ~ cnt_a   E0 ~ 1 + cn… TRUE           
#>  5         1       4 forward    add    E0 ~ bin_e   E0 ~ 1 + bi… TRUE           
#>  6         1       5 forward    add    Emax ~ cnt_b E0 ~ 1, Ema… TRUE           
#>  7         1       6 forward    add    Emax ~ bin_d E0 ~ 1, Ema… TRUE           
#>  8         1       7 forward    add    E0 ~ cnt_b   E0 ~ 1 + cn… TRUE           
#>  9         1       8 forward    add    Emax ~ bin_e E0 ~ 1, Ema… TRUE           
#> 10         1       9 forward    add    Emax ~ cnt_c E0 ~ 1, Ema… TRUE           
#> # ℹ 22 more rows
#> # ℹ 2 more variables: term_p_value <dbl>, model_updated <lgl>
```
