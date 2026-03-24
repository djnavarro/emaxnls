# Construct Emax prediction function from model object

Construct Emax prediction function from model object

## Usage

``` r
emax_fun(mod)
```

## Arguments

- mod:

  An `emaxnls` object

## Value

A function `f` with arguments `data` and `params`. The `data` argument
defaults to the data used to estimate the model, and the `params`
arugment defaults to the estimated parameter values. Both can be
customized, as long as `data` contains columns corresponding to each of
the variables used by the model, and `params` is a named numeric vector
of the appropriate length. The names for `params` must exactly match the
names of the vector returned by `coef(mod)`.

The return value for `f` is a numeric vector of model predictions for
each row in `data`, evaluated at parameters `params`.

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
mod_fn <- emax_fun(mod)

# apply the function to a few rows of the original data
mod_fn(
  data = emax_df[120:125, ],
  param = par
)
#> [1] 14.650737  8.467557 13.263190 15.728956  6.255590 13.622004

# adjust the parameters
new_par <- par
new_par["E0_Intercept"] <- 0

# simulate the model with the adjusted parameters
mod_fn(
  data = emax_df[120:125, ],
  param = new_par
)
#> [1]  9.595929  3.412750  8.208382 10.674149  1.200782  8.567196
```
