# Changelog

## emaxnls 0.1.1.9000

- Adds
  [`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
  for fitting binary-outcome models with iterative reweighted least
  squares

- Redesigns
  [`print()`](https://emaxnls.djnavarro.net/reference/print.md) and
  [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md)
  methods for `emaxnls` and `emaxlogistic` objects:

  - [`print()`](https://emaxnls.djnavarro.net/reference/print.md) is now
    a concise model overview showing structural and covariate formulas,
    fit statistics (observations, residual df, sigma or deviance, AIC),
    and a coefficient table with estimates, standard errors, and
    confidence intervals. Hypothesis tests are not shown by
    [`print()`](https://emaxnls.djnavarro.net/reference/print.md); a
    footer line directs users to
    [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md). A
    `conf_level` argument controls the interval level (default 0.95).

  - [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md)
    gains three new arguments:

    - `suppress_nonsensical = TRUE`: suppresses the test statistic and
      p-value for `logEC50_Intercept` by default. The logEC50 intercept
      is estimated on the log-concentration scale, so testing
      `H0: logEC50 = 0` corresponds to testing EC50 = 1 on the
      concentration scale — a threshold with no pharmacometric meaning.
      The confidence interval for logEC50 is always reported. Pass
      `suppress_nonsensical = FALSE` to restore the raw test.
    - `p_adjust = "none"`: adjusts non-NA p-values via
      [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
      multiple-comparison correction; suppressed p-values are excluded
      from the adjustment set.
    - `simultaneous = FALSE`: when `TRUE`, computes simultaneous Wald
      confidence intervals using
      [`mvtnorm::qmvnorm()`](https://rdrr.io/pkg/mvtnorm/man/qmvnorm.html)
      on the correlation matrix from
      [`vcov()`](https://rdrr.io/r/stats/vcov.html), giving intervals
      with joint coverage at `conf_level` across all parameters
      simultaneously.

  - `summary(back_transform = TRUE)` now also sets the test statistic to
    `NA` for back-transformed parameters, consistent with the standard
    error already being `NA` on the back-transformed scale.

- [`confint()`](https://rdrr.io/r/stats/confint.html) now falls back to
  Wald intervals with a warning if profile likelihood computation fails
  (which can occur for sigmoidal models).

## emaxnls 0.1.1

CRAN release: 2026-06-30

- Expanded description of package.
- Fixes bug when
  [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
  is called manually
- Additional examples in documentation.
- Improves unit tests.

## emaxnls 0.1.0

- Initial CRAN submission.
