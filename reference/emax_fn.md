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
  structural_model = response_1 ~ exposure_1, 
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
#> [1] 14.74993 14.06122 14.52765 13.90924 13.20033 11.45919

# adjust the parameters
new_par <- par
new_par["E0_Intercept"] <- 0

# simulate the model with the adjusted parameters
mod_fn(
  data = emax_df[120:125, ],
  param = new_par
)
#> [1] 9.756877 9.068172 9.534595 8.916192 8.207278 6.466142
```
