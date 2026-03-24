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

- cat_f:

  Categorical covariate

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
#> # A tibble: 400 × 12
#>       id  dose  exp_1  exp_2 rsp_1 rsp_2 cnt_a cnt_b cnt_c bin_d bin_e cat_f
#>    <int> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <fct>
#>  1     1   200 12332. 13004. 15.7      1  3.85  5.89  4.31     1     1 grp 1
#>  2     2   300 18232. 17244. 15.3      1  4.78  7.25  3.73     1     1 grp 1
#>  3     3     0     0      0   5.65     0  1.22  9.24  2.41     1     1 grp 1
#>  4     4   200  9394.  8839. 12.5      0  2.68  7.14  3.76     1     1 grp 2
#>  5     5   200  7088.  9827. 13.2      1  4.27  5.57  9.05     0     1 grp 2
#>  6     6   300 30402. 28483. 16.8      1  6.09  6.08  4.62     0     1 grp 1
#>  7     7   300 21679. 17137. 17.4      1  7.5   8.1   2.08     0     1 grp 3
#>  8     8   100 15506. 13377. 15.9      0  3.65  6.89  3.56     0     1 grp 1
#>  9     9     0     0      0   7.3      0  4.84  3.77  7.44     0     1 grp 2
#> 10    10   200  5331.  5251. 12.8      1  4.45  3.42  1.66     1     0 grp 3
#> # ℹ 390 more rows
```
