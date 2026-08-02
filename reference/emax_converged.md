# Check Emax regression model for convergence status

Returns `TRUE` if the model converged during fitting and `FALSE`
otherwise. The reason for convergence or non-convergence is attached as
the `names` attribute of the return value, so it prints alongside the
logical result.

## Usage

``` r
emax_converged(mod)
```

## Arguments

- mod:

  An `emaxnls` object

## Value

A named logical scalar. The value is `TRUE` when the model converged and
`FALSE` otherwise. The `names` attribute holds a short description of
the outcome:

- `"converged"`: the optimizer reached a solution successfully.

- `"maximum time exceeded"`: the `max_time` limit set in
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
  /
  [`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md)
  was hit before the optimizer finished.

- `"maximum iterations exceeded"`: the optimizer ran out of iterations.
  This applies to the Gauss-Newton algorithm (when
  [`nls()`](https://rdrr.io/r/stats/nls.html) reports "number of
  iterations exceeded maximum") and to the Levenberg-Marquardt algorithm
  (when `nlsLM()` reports that the iteration count has reached
  `maxiter`). The iteration budget can be increased via the
  `optim_control` argument of
  [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md).

- Raw optimizer message: all other failures return the error message
  from the underlying optimizer directly. Common examples include a
  singular gradient matrix at the initial parameter estimates, the
  Gauss-Newton step factor collapsing below `minFactor`, and
  Port-algorithm convergence codes such as false convergence (code 8) or
  singular convergence (code 7). These messages are algorithm-specific
  and are passed through unchanged because no single label covers them
  accurately.

## See also

[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md),
[`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
