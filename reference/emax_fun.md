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
argument defaults to the estimated parameter values. Both can be
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
#> Error in new.env(parent = parent): 'enclos' must be an environment
par <- coef(mod)
#> Error: object 'mod' not found

# customizable emax function with the same structural 
# model and same covariate model, defaulting to the 
# same data and parameters as the original model, but
# allowing user to pass their own data and parameters  
mod_fn <- emax_fun(mod)
#> Error: object 'mod' not found

# apply the function to a few rows of the original data
mod_fn(
  data = emax_df[120:125, ],
  param = par
)
#> Error in mod_fn(data = emax_df[120:125, ], param = par): could not find function "mod_fn"

# adjust the parameters
new_par <- par
new_par["E0_Intercept"] <- 0
#> Error in new_par["E0_Intercept"] <- 0: object of type 'closure' is not subsettable

# simulate the model with the adjusted parameters
mod_fn(
  data = emax_df[120:125, ],
  param = new_par
)
#> Error in mod_fn(data = emax_df[120:125, ], param = new_par): could not find function "mod_fn"
```
