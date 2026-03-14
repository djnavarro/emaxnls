# Settings used to estimate Emax model

Settings used to estimate Emax model

## Usage

``` r
emax_nls_options(
  optim_method = "port",
  optim_control = NULL,
  quiet = FALSE,
  weights = NULL,
  na.action = options("na.action")
)
```

## Arguments

- optim_method:

  Character string specifying the algorithm used to solve the nonlinear
  least squares optimization problem. Supported pptions are "gauss",
  "port", and "levenberg". See details.

- optim_control:

  A list of arguments used to control the behavior of the optimization
  algorithm. Allowed values differ depending on which algorithm is used

- quiet:

  When `quiet=TRUE`, messages are suppressed

- weights:

  Numeric vector providing the weights for observations. When specified,
  weighted least squares is used

- na.action:

  How should missing values in the data be handled

  At present there are three supported values for `optim_method`:

  - "gauss": Estimate parameters using the Gauss-Newton algorithm. This
    is equivalent to the using "default" option in
    [`nls()`](https://rdrr.io/r/stats/nls.html)

  - "port": Estimate parameters using bounded optimization with the
    "nl2sol" algorithm from from the the Port library. Equivalent to
    "port" in [`nls()`](https://rdrr.io/r/stats/nls.html)

  - "levenberg": Estimate parameters using the Levenberg-Marquardt
    algorithm. This is equivalent to using `nlsLM()` from the
    "minpack.lm" package.

  Note that the Golub-Pereyra algorithm for partially linear
  least-squares (i.e. the "plinear" option in
  [`nls()`](https://rdrr.io/r/stats/nls.html)) is not currently
  supported for Emax regression. Informal testing suggests it does not
  perform well for these models, and rarely converges.

## Value

List
