# Coefficients for an Emax regression

Coefficients for an Emax regression

## Usage

``` r
# S3 method for class 'emaxnls'
coef(object, back_transform = FALSE, ...)
```

## Arguments

- object:

  An `emaxnls` object

- back_transform:

  Should log-scaled parameters (logEC50, logHill) be back-transformed to
  original scale?

- ...:

  Ignored

## Value

A vector of coefficients

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)

# coefficients on the estimation scale
coef(mod)
#>          E0_cnt_a      E0_Intercept    Emax_Intercept logEC50_Intercept 
#>         0.4861467         5.0548076         9.9697250         8.2688405 

# coefficients with log-scale parameters back-transformed
coef(mod, back_transform = TRUE)
#>       E0_cnt_a   E0_Intercept Emax_Intercept EC50_Intercept 
#>      0.4861467      5.0548076      9.9697250   3900.4236542 
```
