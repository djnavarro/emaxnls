# Variance-covariance matrix for an Emax regression

Returns the estimated variance-covariance matrix of the model
parameters. The square roots of the diagonal entries are the parameter
standard errors.

## Usage

``` r
# S3 method for class 'emaxnls'
vcov(object, ...)
```

## Arguments

- object:

  An `emaxnls` or `emaxlogistic` object

- ...:

  Ignored

## Value

A square numeric matrix with rows and columns named by the model
parameters

## Details

For `emaxnls` objects, the matrix is derived from the Hessian of the NLS
objective at the parameter estimates (via `stats::vcov.nls()`). For
`emaxlogistic` objects, it is derived from the Jacobian of the IRLS
algorithm at convergence, which provides the correct asymptotic
covariance matrix under binomial sampling.

## Examples

``` r
mod_c <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
vcov(mod_c)
#>                        E0_cnt_a  E0_Intercept Emax_Intercept logEC50_Intercept
#> E0_cnt_a           1.335812e-04 -0.0006514396   3.017139e-05      2.760271e-05
#> E0_Intercept      -6.514396e-04  0.0057680906  -2.251611e-03      4.429919e-04
#> Emax_Intercept     3.017139e-05 -0.0022516112   1.247504e-02      3.115596e-03
#> logEC50_Intercept  2.760271e-05  0.0004429919   3.115596e-03      1.548477e-03

mod_b <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)
vcov(mod_b)
#>                        E0_cnt_a E0_Intercept Emax_Intercept logEC50_Intercept
#> E0_cnt_a           0.0063991938  -0.04021554      0.0415963      0.0009376435
#> E0_Intercept      -0.0402155409   0.33464607     -0.1360569      0.0667616964
#> Emax_Intercept     0.0415962975  -0.13605688      5.1426626      1.0714906773
#> logEC50_Intercept  0.0009376435   0.06676170      1.0714907      0.2680142874
```
