# Settings used to estimate Emax model

Constructs a settings object controlling the optimization algorithm and
other aspects of model fitting for
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md).
Pass the result to the `opts` argument of
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md).

## Usage

``` r
emax_nls_options(
  optim_method = "gauss",
  optim_control = NULL,
  quiet = FALSE,
  weights = NULL,
  na.action = getOption("na.action")
)
```

## Arguments

- optim_method:

  Character string specifying the algorithm used to solve the nonlinear
  least squares optimization problem. Supported options are "gauss" (the
  default), "port", and "levenberg". See details.

- optim_control:

  A list of arguments used to control the behavior of the optimization
  algorithm. Allowed values differ depending on which algorithm is used

- quiet:

  When `quiet=TRUE`, messages are suppressed

- weights:

  Numeric vector providing the weights for observations. When specified,
  weighted least squares is used

- na.action:

  How should missing values in the data be handled?

## Value

A list of settings

## Details

At present there are three supported values for `optim_method`:

- "gauss": Estimate parameters using the Gauss-Newton algorithm. This is
  equivalent to the using "default" option in
  [`nls()`](https://rdrr.io/r/stats/nls.html)

- "port": Estimate parameters using bounded optimization with the
  "nl2sol" algorithm from from the the Port library. Equivalent to
  "port" in [`nls()`](https://rdrr.io/r/stats/nls.html)

- "levenberg": Estimate parameters using the Levenberg-Marquardt
  algorithm. This is equivalent to using
  [`nlsLM()`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html) from the
  "minpack.lm" package.

Note that the Golub-Pereyra algorithm for partially linear least-squares
(i.e. the "plinear" option in
[`nls()`](https://rdrr.io/r/stats/nls.html)) is not currently supported
for Emax regression. Informal testing suggests it does not perform well
for these models, and rarely converges.

The `optim_control` argument mirrors the corresponding control arguments
for the respective optimization methods:

- For "gauss" and "port": the list should match the output of
  [`stats::nls.control()`](https://rdrr.io/r/stats/nls.control.html)

- For "levenberg": the list should match the output of
  [`minpack.lm::nls.lm.control()`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.control.html)

If `optim_control = NULL`, the default settings are used for the
relevant function.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md),
[`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)

## Examples

``` r
# default options
emax_nls_options()
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
#> <bytecode: 0x55ec91870fa8>
#> <environment: namespace:stats>
#> 

# switch to levenberg-marquardt
if (require("minpack.lm", quietly = TRUE)) emax_nls_options(optim_method = "levenberg")
#> $optim_method
#> [1] "levenberg"
#> 
#> $optim_control
#> $optim_control$ftol
#> [1] 1.490116e-08
#> 
#> $optim_control$ptol
#> [1] 1.490116e-08
#> 
#> $optim_control$gtol
#> [1] 0
#> 
#> $optim_control$diag
#> list()
#> 
#> $optim_control$epsfcn
#> [1] 0
#> 
#> $optim_control$factor
#> [1] 100
#> 
#> $optim_control$maxfev
#> integer(0)
#> 
#> $optim_control$maxiter
#> [1] 50
#> 
#> $optim_control$nprint
#> [1] 0
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
#> <bytecode: 0x55ec91870fa8>
#> <environment: namespace:stats>
#> 

```
