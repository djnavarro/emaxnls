# Analysis of variance for Emax regression models

Analysis of variance for Emax regression models

## Usage

``` r
# S3 method for class 'emaxnls'
anova(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Additional fitted model objects

## Value

Analysis of variance tables for a sequence of `emaxnls` models

## Examples

``` r

mod_0 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
mod_1 <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

anova(mod_0, mod_1)
#> Analysis of Variance Table
#> 
#> Model 1: rsp_1 ~ ((1 * E0_Intercept)) + exp_1 * ((1 * Emax_Intercept))/(exp_1 + exp((1 * logEC50_Intercept)))
#> Model 2: rsp_1 ~ ((cnt_a * E0_cnt_a) + (1 * E0_Intercept)) + exp_1 * ((1 * Emax_Intercept))/(exp_1 + exp((1 * logEC50_Intercept)))
#>   Res.Df Res.Sum Sq Df Sum Sq F value    Pr(>F)    
#> 1    397     564.77                                
#> 2    396     103.31  1 461.46  1768.9 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
