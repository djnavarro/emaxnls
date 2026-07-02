# emaxnls 0.1.1.9000

* Adds `emax_logistic()` for fitting binary-outcome models with iterative reweighted least squares

* Redesigns `print()` and `summary()` methods for `emaxnls` and `emaxlogistic` objects:

  - `print()` is now a concise model overview showing structural and covariate
    formulas, fit statistics (observations, residual df, sigma or deviance, AIC),
    and a coefficient table with estimates, standard errors, and confidence
    intervals. Hypothesis tests are not shown by `print()`; a footer line
    directs users to `summary()`. A `conf_level` argument controls the
    interval level (default 0.95).

  - `summary()` gains three new arguments:
    - `suppress_nonsensical = TRUE`: suppresses the test statistic and p-value
      for `logEC50_Intercept` by default. The logEC50 intercept is estimated on
      the log-concentration scale, so testing `H0: logEC50 = 0` corresponds to
      testing EC50 = 1 on the concentration scale — a threshold with no
      pharmacometric meaning. The confidence interval for logEC50 is always
      reported. Pass `suppress_nonsensical = FALSE` to restore the raw test.
    - `p_adjust = "none"`: adjusts non-NA p-values via `p.adjust()` for
      multiple-comparison correction; suppressed p-values are excluded from
      the adjustment set.
    - `simultaneous = FALSE`: when `TRUE`, computes simultaneous Wald confidence
      intervals using `mvtnorm::qmvnorm()` on the correlation matrix from
      `vcov()`, giving intervals with joint coverage at `conf_level` across
      all parameters simultaneously.

  - `summary(back_transform = TRUE)` now also sets the test statistic to `NA`
    for back-transformed parameters, consistent with the standard error already
    being `NA` on the back-transformed scale.

* `confint()` now falls back to Wald intervals with a warning if profile
  likelihood computation fails (which can occur for sigmoidal models).

# emaxnls 0.1.1

* Expanded description of package.
* Fixes bug when `emax_nls_init()` is called manually
* Additional examples in documentation.
* Improves unit tests.

# emaxnls 0.1.0

* Initial CRAN submission.
