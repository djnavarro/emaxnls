# Analysis of variance for Emax regression models

Compares a sequence of nested Emax models. At least two model objects
must be provided; all must be of the same class.

## Usage

``` r
# S3 method for class 'emaxlogistic'
anova(object, ...)

# S3 method for class 'emaxnls'
anova(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Additional fitted model objects of the same class

## Value

For `emaxnls` objects, an analysis of variance table for a sequence of
models. For `emaxlogistic` objects, a data frame with columns `Df`,
`Deviance`, `Df_diff`, `LRT`, and `Pr(>Chi)`.

## Details

For `emaxnls` objects, calls
[`stats::anova()`](https://rdrr.io/r/stats/anova.html) on the underlying
`nls` objects to produce an ANOVA table for the sequence of models. For
`emaxlogistic` objects, computes a likelihood ratio chi-squared test
comparing nested models; the test statistic is the difference in
deviances and the reference distribution is chi-squared with degrees of
freedom equal to the difference in the number of parameters. The nesting
assumption is not checked; results are only interpretable when each
successive model genuinely adds parameters to the previous one.

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

# emaxlogistic: likelihood ratio test
mod_b0 <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
mod_b1 <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
anova(mod_b0, mod_b1)
#>      Df Deviance Df_diff    LRT  Pr(>Chi)    
#> [1,]  3   438.43                             
#> [2,]  4   331.47       1 106.96 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
