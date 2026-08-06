# Stepwise covariate modeling for Emax regression

Performs stepwise covariate modeling by forward addition
(`emax_scm_forward()`), backward elimination (`emax_scm_backward()`), or
both in sequence. Use `emax_scm_history()` to retrieve the history of
all models tested during the procedure.

## Usage

``` r
emax_scm_forward(
  mod,
  candidates,
  threshold = 0.01,
  criterion = "p-value",
  seed = NULL
)

emax_scm_backward(
  mod,
  candidates,
  threshold = 0.001,
  criterion = "p-value",
  seed = NULL
)

emax_scm_history(mod)
```

## Arguments

- mod:

  An `emaxnls` object

- candidates:

  A list of candidate covariates

- threshold:

  Threshold for addition or removal. Used only when
  `criterion = "p-value"` (the default); ignored otherwise.

- criterion:

  Model selection criterion. One of `"p-value"` (default), `"aic"`, or
  `"bic"`.

- seed:

  Seed for the RNG state

## Value

An object of class `emaxnls`

## Details

The `candidates` argument must be a named list whose names correspond to
structural parameters (e.g. `E0`, `Emax`) and whose values are character
vectors of covariate names to consider. See the examples for an
illustration.

Three model selection criteria are available via the `criterion`
argument:

- `"p-value"` (default): a term is added if its ANOVA p-value falls
  below `threshold` (forward) or removed if its p-value exceeds
  `threshold` (backward). When multiple candidates satisfy the
  threshold, the one with the most extreme p-value is chosen.

- `"aic"`: a term is added (forward) or removed (backward) if doing so
  strictly decreases AIC. When multiple candidates improve AIC, the one
  yielding the lowest AIC is chosen.

- `"bic"`: same as `"aic"` but using BIC as the criterion.

When `criterion` is `"aic"` or `"bic"`, the `threshold` argument has no
effect and is ignored.

The history returned by `emax_scm_history()` always records AIC and BIC
for every model tested (columns `model_aic` and `model_bic`), regardless
of which criterion was used for selection. The `criterion` column
records which criterion drove each forward or backward step.

The `seed` argument controls the RNG state for any stochastic components
of the procedure. It is currently experimental and may be removed in
future releases.

Every model tested during the procedure is stored internally in the
returned object. Use `emax_scm_history()` to extract this record.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)

## Examples

``` r
base_model <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)

covariate_list <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)

# add covariates to the base model using forward addition (p-value criterion)
forward_model <- emax_scm_forward(
  mod = base_model,
  candidates = covariate_list, 
  threshold = .01
)
forward_model
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6431 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.

# remove covariates from the forward model using backward deletion
final_model <- emax_scm_backward(
  mod = forward_model,
  candidates = covariate_list, 
  threshold = .001
) 
final_model
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6431 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.

# show the history of all models tested, including which criterion was used
emax_scm_history(final_model)
#> # A tibble: 22 × 13
#>    iteration attempt step       criterion action term_tested  model_tested      
#>        <int>   <int> <chr>      <chr>     <chr>  <chr>        <chr>             
#>  1         0       0 base model NA        NA     NA           E0 ~ 1, Emax ~ 1,…
#>  2         1       1 forward    p-value   add    E0 ~ cnt_b   E0 ~ 1 + cnt_b, E…
#>  3         1       2 forward    p-value   add    E0 ~ bin_e   E0 ~ 1 + bin_e, E…
#>  4         1       3 forward    p-value   add    Emax ~ cnt_b E0 ~ 1, Emax ~ 1 …
#>  5         1       4 forward    p-value   add    E0 ~ cnt_c   E0 ~ 1 + cnt_c, E…
#>  6         1       5 forward    p-value   add    E0 ~ cnt_a   E0 ~ 1 + cnt_a, E…
#>  7         1       6 forward    p-value   add    E0 ~ bin_d   E0 ~ 1 + bin_d, E…
#>  8         1       7 forward    p-value   add    Emax ~ cnt_c E0 ~ 1, Emax ~ 1 …
#>  9         1       8 forward    p-value   add    Emax ~ bin_d E0 ~ 1, Emax ~ 1 …
#> 10         1       9 forward    p-value   add    Emax ~ bin_e E0 ~ 1, Emax ~ 1 …
#> # ℹ 12 more rows
#> # ℹ 6 more variables: model_converged <lgl>, convergence_reason <chr>,
#> #   term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>, model_updated <lgl>

# AIC-based forward addition
forward_aic <- emax_scm_forward(
  mod = base_model,
  candidates = covariate_list,
  criterion = "aic"
)

# BIC-based backward elimination
final_bic <- emax_scm_backward(
  mod = forward_aic,
  candidates = covariate_list,
  criterion = "bic"
)

emax_scm_history(final_bic)
#> # A tibble: 22 × 13
#>    iteration attempt step       criterion action term_tested  model_tested      
#>        <int>   <int> <chr>      <chr>     <chr>  <chr>        <chr>             
#>  1         0       0 base model NA        NA     NA           E0 ~ 1, Emax ~ 1,…
#>  2         1       1 forward    aic       add    E0 ~ cnt_c   E0 ~ 1 + cnt_c, E…
#>  3         1       2 forward    aic       add    Emax ~ bin_e E0 ~ 1, Emax ~ 1 …
#>  4         1       3 forward    aic       add    Emax ~ cnt_c E0 ~ 1, Emax ~ 1 …
#>  5         1       4 forward    aic       add    E0 ~ cnt_a   E0 ~ 1 + cnt_a, E…
#>  6         1       5 forward    aic       add    E0 ~ cnt_b   E0 ~ 1 + cnt_b, E…
#>  7         1       6 forward    aic       add    E0 ~ bin_d   E0 ~ 1 + bin_d, E…
#>  8         1       7 forward    aic       add    Emax ~ cnt_b E0 ~ 1, Emax ~ 1 …
#>  9         1       8 forward    aic       add    Emax ~ bin_d E0 ~ 1, Emax ~ 1 …
#> 10         1       9 forward    aic       add    E0 ~ bin_e   E0 ~ 1 + bin_e, E…
#> # ℹ 12 more rows
#> # ℹ 6 more variables: model_converged <lgl>, convergence_reason <chr>,
#> #   term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>, model_updated <lgl>

# example using binary outcomes
base_model_logistic <- emax_nls(
  structural_model = rsp_2 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df,
  opts = emax_nls_options(max_time = 10)
)
forward_model_logistic <- emax_scm_forward(
  mod = base_model_logistic,
  candidates = covariate_list, 
  threshold = .01
)
final_model_logistic <- emax_scm_backward(
  mod = forward_model_logistic,
  candidates = covariate_list, 
  threshold = .001
)

final_model_logistic
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_2 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a + bin_d 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          395 
#>   Residual std. error:  0.3696 
#>   AIC:                  345.9516 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error   lower  upper
#> 1 E0_cnt_a            0.0932   0.00836  0.0767  0.110
#> 2 E0_bin_d            0.145    0.0373   0.0713  0.218
#> 3 E0_Intercept       -0.293    0.0566  -0.404  -0.182
#> 4 Emax_Intercept      0.930    0.145    0.707   1.29 
#> 5 logEC50_Intercept   9.27     0.381    8.60   10.0  
#> 
#> Use summary() for hypothesis tests.

emax_scm_history(final_model_logistic)
#> # A tibble: 31 × 13
#>    iteration attempt step       criterion action term_tested  model_tested      
#>        <int>   <int> <chr>      <chr>     <chr>  <chr>        <chr>             
#>  1         0       0 base model NA        NA     NA           E0 ~ 1, Emax ~ 1,…
#>  2         1       1 forward    p-value   add    Emax ~ bin_e E0 ~ 1, Emax ~ 1 …
#>  3         1       2 forward    p-value   add    E0 ~ cnt_c   E0 ~ 1 + cnt_c, E…
#>  4         1       3 forward    p-value   add    E0 ~ bin_d   E0 ~ 1 + bin_d, E…
#>  5         1       4 forward    p-value   add    E0 ~ bin_e   E0 ~ 1 + bin_e, E…
#>  6         1       5 forward    p-value   add    Emax ~ cnt_a E0 ~ 1, Emax ~ 1 …
#>  7         1       6 forward    p-value   add    E0 ~ cnt_b   E0 ~ 1 + cnt_b, E…
#>  8         1       7 forward    p-value   add    E0 ~ cnt_a   E0 ~ 1 + cnt_a, E…
#>  9         1       8 forward    p-value   add    Emax ~ cnt_c E0 ~ 1, Emax ~ 1 …
#> 10         1       9 forward    p-value   add    Emax ~ bin_d E0 ~ 1, Emax ~ 1 …
#> # ℹ 21 more rows
#> # ℹ 6 more variables: model_converged <lgl>, convergence_reason <chr>,
#> #   term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>, model_updated <lgl>
```
