# Fitted values for an Emax regression

Fitted values for an Emax regression

## Usage

``` r
# S3 method for class 'emaxnls'
fitted(object, ...)
```

## Arguments

- object:

  An `emaxnls` object

- ...:

  Ignored

## Value

Numeric vector of fitted values

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
fit <- fitted(mod)
fit[1:20]
#>  [1] 14.500646 15.591349  5.647906 13.402470 13.561531 16.851552 17.150410
#>  [8] 14.795187  7.407757 12.975508 18.332700 15.783209  6.809797  6.352819
#> [15] 16.615000 16.302892  5.574984 16.150465 15.843635 14.834422
```
