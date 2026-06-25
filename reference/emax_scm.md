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

## Details

The emaxnls package supports stepwise covariate modeling via forward
addition and backward elimination. The `emax_scm_forward()` function
supports forward addition, the `emax_scm_backward()` function supports
backward elimination, and the syntax is designed to allow
forward-backward procedures by piping a base model to
`emax_scm_forward()` and then to `emax_scm_backward()`. In both cases,
the function takes an `emaxnls` regression object as the first argument,
as well as a list of candidate `covariates` to be considered for
addition (in the forward addition case) or deletion (backward
elimination). The input must be a named list, with the names
corresponding to the relevant structural parameter, and the values
should be character vector specifying covariates of interest. See the
examples for an illustration of how this argument should be specified.

As present, these functions only support stepwise regression using
p-values as the criterion for addition or deletion. The `threshold`
argument corresponds to the threshold p-value to be used. In future,
other methods (e.g., selection on the basis of AIC values) may be
supported.

The `seed` argument is used to control the RNG state for stochastic
components of the stepwise procedure. However, please note that the
`seed` argument is currently experimental, and may be removed in future
releases.

A key feature of the stepwise covariate modeling functions is that they
keep track of every tested model, and store information about this
history internally within the `emaxnls` object that gets returned. Use
the `emax_scm_history()` function to extract this history.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)

## Examples

``` r
base_model <- emax_nls(rsp_1 ~ exp_1, list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), emax_df)

covariate_list <- list(
  E0 = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)

# add covariates to the base model using forward addition
forward_model <- emax_scm_forward(
  mod = base_model,
  candidates = covariate_list, 
  threshold = .01
)
forward_model
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
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
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 

# remove covariates from the forward model using backward deletion
final_model <- emax_scm_backward(
  mod = forward_model,
  candidates = covariate_list, 
  threshold = .001
) 
final_model
#> Structural model:
#> 
#>   Exposure:  exp_1 
#>   Response:  rsp_1 
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
#> 1 E0_cnt_a             0.486    0.0116        42.1 3.63e-148    0.463    0.509
#> 2 E0_Intercept         5.05     0.0759        66.6 4.16e-217    4.91     5.20 
#> 3 Emax_Intercept       9.97     0.112         89.3 2.11e-264    9.75    10.2  
#> 4 logEC50_Intercept    8.27     0.0394       210.  0            8.19     8.35 

# show the history of all models tested during the forward addition
# step and the backward deletion step
emax_scm_history(final_model)
#> # A tibble: 22 × 11
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
#> # ℹ 12 more rows
#> # ℹ 4 more variables: term_p_value <dbl>, model_aic <dbl>, model_bic <dbl>,
#> #   model_updated <lgl>
```
