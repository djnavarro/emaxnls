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
#>  [1]  1.16935390 -0.33134886  0.00209353 -0.94247006 -0.31153056 -0.05155200
#>  [7]  0.24959037  1.07481254 -0.10775738 -0.15550832 -0.14269996  0.56679081
#> [13] -0.91979699 -0.56281913  0.64500044  0.56710765 -0.44498447  0.47953472
#> [19]  0.07636540 -0.19442164
```
