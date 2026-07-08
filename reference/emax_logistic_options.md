# Settings used to estimate a logistic Emax model

Constructs a settings object controlling the NLS optimizer and IRLS
convergence for
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md).
Pass the result to the `opts` argument of
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md).

## Usage

``` r
emax_logistic_options(
  optim_method = "gauss",
  optim_control = NULL,
  quiet = FALSE,
  na.action = getOption("na.action"),
  max_iter = 25,
  tol = 1e-06
)
```

## Arguments

- optim_method:

  Character string specifying the algorithm used for the weighted NLS
  step within each IRLS iteration. Supported options are `"gauss"`
  (default), `"port"`, and `"levenberg"`. See
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
  for details on each.

- optim_control:

  A list of arguments controlling the NLS optimizer.

- quiet:

  When `TRUE`, convergence warnings are suppressed.

- na.action:

  How should missing values in the data be handled?

- max_iter:

  Maximum number of IRLS outer iterations (default 25).

- tol:

  Convergence tolerance: IRLS stops when the change in binomial deviance
  between successive iterations falls below `tol` (default 1e-6).

## Value

A list of settings

## See also

[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md),
[`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md)

## Examples

``` r
# default options
emax_logistic_options()
#> $optim_method
#> [1] "gauss"
#> 
#> $optim_control
#> $optim_control$maxiter
#> [1] 50
#> 
#> $optim_control$tol
#> [1] 1e-05
#> 
#> $optim_control$minFactor
#> [1] 0.0009765625
#> 
#> $optim_control$printEval
#> [1] FALSE
#> 
#> $optim_control$warnOnly
#> [1] FALSE
#> 
#> $optim_control$scaleOffset
#> [1] 0
#> 
#> $optim_control$nDcentral
#> [1] FALSE
#> 
#> 
#> $quiet
#> [1] FALSE
#> 
#> $weights
#> NULL
#> 
#> $na.action
#> function (object, ...) 
#> UseMethod("na.omit")
#> <bytecode: 0x56015b6c31b8>
#> <environment: namespace:stats>
#> 
#> $max_iter
#> [1] 25
#> 
#> $tol
#> [1] 1e-06
#> 

# increase maximum IRLS iterations
emax_logistic_options(max_iter = 50)
#> $optim_method
#> [1] "gauss"
#> 
#> $optim_control
#> $optim_control$maxiter
#> [1] 50
#> 
#> $optim_control$tol
#> [1] 1e-05
#> 
#> $optim_control$minFactor
#> [1] 0.0009765625
#> 
#> $optim_control$printEval
#> [1] FALSE
#> 
#> $optim_control$warnOnly
#> [1] FALSE
#> 
#> $optim_control$scaleOffset
#> [1] 0
#> 
#> $optim_control$nDcentral
#> [1] FALSE
#> 
#> 
#> $quiet
#> [1] FALSE
#> 
#> $weights
#> NULL
#> 
#> $na.action
#> function (object, ...) 
#> UseMethod("na.omit")
#> <bytecode: 0x56015b6c31b8>
#> <environment: namespace:stats>
#> 
#> $max_iter
#> [1] 50
#> 
#> $tol
#> [1] 1e-06
#> 
```
