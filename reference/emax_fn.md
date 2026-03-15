# Construct Emax function from model object

Construct Emax function from model object

## Usage

``` r
emax_fn(mod)
```

## Arguments

- mod:

  An `emaxnls` object

## Value

A function with arguments `data` and `params`

## Examples

``` r
mod <- emax_nls(
  structural_model = rsp_1 ~ exp_1, 
  covariate_model = list(E0 ~ cnt_a, Emax ~ 1, logEC50 ~ 1), 
  data = emax_df
)
par <- coef(mod)

# customizable emax function with the same structural 
# model and same covariate model, defaulting to the 
# same data and parameters as the original model, but
# allowing user to pass their own data and parameters  
mod_fn <- emax_fn(mod)

# apply the function to a few rows of the original data
mod_fn(
  data = emax_df[120:125, ],
  param = par
)
#> [1] 14.455991 16.345386 15.472011  7.403739 13.178058 13.258412

# adjust the parameters
new_par <- par
new_par["E0_Intercept"] <- 0

# simulate the model with the adjusted parameters
mod_fn(
  data = emax_df[120:125, ],
  param = new_par
)
#> [1]  9.464150 11.353545 10.480169  2.411898  8.186216  8.266570
```
