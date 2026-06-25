# Residuals for an Emax regression

Residuals for an Emax regression

## Usage

``` r
# S3 method for class 'emaxnls'
residuals(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Ignored

## Value

Numeric vector of residuals

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
res <- residuals(mod)
res[1:20]
#>  [1]  1.169353898 -0.331348862  0.002093523 -0.942470064 -0.311530566
#>  [6] -0.051552000  0.249590372  1.074812535 -0.107757380 -0.155508320
#> [11] -0.142699949  0.566790807 -0.919796990 -0.562819131  0.645000441
#> [16]  0.567107657 -0.444984478  0.479534720  0.076365401 -0.194421634
```
