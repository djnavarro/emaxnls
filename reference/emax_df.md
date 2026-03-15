# Sample simulated data for Emax exposure-response models with covariates.

Sample simulated data for Emax exposure-response models with covariates.

## Usage

``` r
emax_df
```

## Format

A data frame with columns:

- id:

  Identifier column

- dose:

  Nominal dose, units not specified

- exp_1:

  Exposure value, units and metric not specified

- exp_2:

  Exposure value, units and metric not specified, but different from
  exp_1

- rsp_1:

  Continuous response value (units not specified)

- rsp_2:

  Binary response value (group labels not specified)

- cnt_a:

  Continuous valued covariate

- cnt_b:

  Continuous valued covariate

- cnt_c:

  Continuous valued covariate

- bin_d:

  Binary valued covariate

- bin_e:

  Binary valued covariate

## Details

This simulated dataset is entirely synthetic. It is a generic data set
that can be used to illustrate Emax modeling. It contains variables
corresponding to dose and exposure, and includes both a continuous
response variable and a binary response variable. Three continuous
valued covariates are included, along with two binary covariates.

You can find the data generating code in the package source code, under
`R/data.R`

## Examples

``` r
emax_df
#> # A tibble: 400 × 11
#>       id  dose  exp_1  exp_2 rsp_1 rsp_2 cnt_a cnt_b cnt_c bin_d bin_e
#>    <int> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   300 38904. 34799. 18.7      1  8.23  2.89  4.87     1     1
#>  2     2   100  3521.  3260. 12.2      1  5.58  3.84  7.41     0     1
#>  3     3   100  9416   8074. 13.2      0  1.44  3.12  9.14     1     1
#>  4     4   300 25700. 24750. 17.4      1  7.02  8.25  7.96     1     1
#>  5     5   300 42142. 32825. 16.6      1  5.61  6.05  6.35     1     1
#>  6     6     0     0      0   6.33     0  2.46  3.36  8.29     0     1
#>  7     7     0     0      0   6.54     0  2.47  3.98  6.74     1     1
#>  8     8     0     0      0   7.45     0  4.03  2.86  9.38     1     1
#>  9     9   300 23364. 24189. 16.1      1  6.08  4.1   2.16     1     1
#> 10    10   100  5710.  5636. 14.2      1  6.1   5.68  3.55     1     1
#> # ℹ 390 more rows
```
