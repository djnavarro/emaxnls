# Settings used to estimate Emax model

Settings used to estimate Emax model

## Usage

``` r
emax_nls_settings(
  init = NULL,
  algorithm = "port",
  control = list(tol = 1e-08, minFactor = 1024^-4, maxiter = 2e+05, scaleOffset = 1,
    warnOnly = FALSE),
  ...
)
```

## Arguments

- init:

  Data frame specifying initial parameters (start, upper, lower)

- algorithm:

  Has same meaning as in nls() (allowed: "default", "plinear", "port")

- control:

  Has same meaning as in nls()

- ...:

  Other arguments passed to nls()

## Value

List
