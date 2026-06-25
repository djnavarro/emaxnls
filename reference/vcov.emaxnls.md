# Variance-covariance matrix for an Emax regression

Variance-covariance matrix for an Emax regression

## Usage

``` r
# S3 method for class 'emaxnls'
vcov(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Ignored

## Value

A matrix

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
vcov(mod)
#>                        E0_cnt_a  E0_Intercept Emax_Intercept logEC50_Intercept
#> E0_cnt_a           1.335812e-04 -0.0006514396   3.017139e-05      2.760271e-05
#> E0_Intercept      -6.514396e-04  0.0057680906  -2.251611e-03      4.429919e-04
#> Emax_Intercept     3.017139e-05 -0.0022516112   1.247504e-02      3.115596e-03
#> logEC50_Intercept  2.760271e-05  0.0004429919   3.115596e-03      1.548477e-03
```
