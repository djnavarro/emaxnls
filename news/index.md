# Changelog

## emaxnls 0.1.1.9000

### New features

- Adds
  [`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
  for fitting binary-outcome Emax models using iterative reweighted
  least squares (IRLS), along with
  [`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md)
  and
  [`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md)
  for initialization and configuration. All standard S3 methods
  ([`coef()`](https://rdrr.io/r/stats/coef.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html),
  [`residuals()`](https://emaxnls.djnavarro.net/reference/residuals.md),
  [`fitted()`](https://emaxnls.djnavarro.net/reference/fitted.md),
  [`predict()`](https://emaxnls.djnavarro.net/reference/predict.md),
  [`anova()`](https://emaxnls.djnavarro.net/reference/anova.md),
  [`logLik()`](https://emaxnls.djnavarro.net/reference/logLik.md),
  [`AIC()`](https://emaxnls.djnavarro.net/reference/AIC.md),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`deviance()`](https://emaxnls.djnavarro.net/reference/deviance.md),
  [`simulate()`](https://emaxnls.djnavarro.net/reference/simulate.md))
  are supported for the new `emaxlogistic` class.

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

### Bug fixes

- Fixes [`AIC()`](https://emaxnls.djnavarro.net/reference/AIC.md) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) when called with multiple
  model arguments
  ([\#37](https://github.com/djnavarro/emaxnls/issues/37)).

- Fixes `na.action` parameter not being passed through correctly in
  [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
  and
  [`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
  ([\#38](https://github.com/djnavarro/emaxnls/issues/38)).

- Fixes
  [`predict()`](https://emaxnls.djnavarro.net/reference/predict.md)
  omitting `residual.scale` from the return value when `se.fit = TRUE`
  ([\#39](https://github.com/djnavarro/emaxnls/issues/39)).

- Fixes crashes in
  [`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md)
  and prevents `Inf` parameter bounds arising during initialisation
  ([\#40](https://github.com/djnavarro/emaxnls/issues/40)).

- Hardens `.nls_call()` to avoid cryptic errors when optimisation fails,
  and tightens argument validation in
  [`emax_fun()`](https://emaxnls.djnavarro.net/reference/emax_fun.md)
  ([\#41](https://github.com/djnavarro/emaxnls/issues/41)).

- Adds input validation for the binary response variable in
  [`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md),
  with informative errors for non-binary or out-of-range values
  ([\#42](https://github.com/djnavarro/emaxnls/issues/42)).

- Fixes validator error messages to reference public API parameter names
  rather than internal names
  ([\#43](https://github.com/djnavarro/emaxnls/issues/43)).

- Fixes `NaN` produced by `.binomial_deviance()` when predicted
  probabilities are exactly 0 or 1 (boundary cases)
  ([\#44](https://github.com/djnavarro/emaxnls/issues/44)).

- [`confint()`](https://rdrr.io/r/stats/confint.html) now accepts a
  `simultaneous` argument, mirroring
  [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md).
  Previously `confint(object, simultaneous = TRUE)` silently ignored the
  argument (swallowed by `...`) and returned pointwise intervals.
  Setting `simultaneous = TRUE` now returns simultaneous (joint) Wald
  intervals that match those reported by
  `summary(object, simultaneous = TRUE)`
  ([\#46](https://github.com/djnavarro/emaxnls/issues/46)).

## emaxnls 0.1.1

CRAN release: 2026-06-30

- Expanded description of package.
- Fixes bug when
  [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
  is called manually.
- [`anova()`](https://emaxnls.djnavarro.net/reference/anova.md) now
  warns rather than errors when fewer than two converged models are
  supplied.
- Additional examples in documentation.
- Improved unit tests.

## emaxnls 0.1.0

Initial CRAN submission. The package provides tools for fitting and
analysing Emax dose-response models via nonlinear least squares.

### Model fitting

- [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md)
  fits continuous-response Emax models using NLS, supporting both the
  hyperbolic (`E0 + Emax * x / (EC50 + x)`) and sigmoidal
  (`E0 + Emax * x^Hill / (EC50^Hill + x^Hill)`) model forms.

- [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
  configures the optimisation algorithm and control parameters. Three
  algorithms are supported via the `optim_method` argument: `"gauss"`
  (Gauss-Newton, default), `"port"` (bounded nl2sol), and `"levenberg"`
  (Levenberg-Marquardt via `minpack.lm`).

- [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
  generates starting values and parameter bounds automatically from the
  data, including support for categorical covariates. Users can also
  call it directly to inspect or override the initialisation before
  fitting.

### Covariate modeling

- Covariates can be added to any structural parameter (E0, Emax,
  logEC50, logHill) via a formula interface,
  e.g. `emax_nls(rsp ~ dose, data = df, E0 = ~ group + age)`.

- [`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  and
  [`emax_remove_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  update a fitted model by adding or removing a single covariate term
  without refitting from scratch.

### Stepwise covariate modeling (SCM)

- [`emax_scm_forward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  performs forward covariate selection, adding one term at a time while
  the likelihood-ratio test p-value stays below a threshold.

- [`emax_scm_backward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  performs backward elimination, removing terms while the p-value
  exceeds a threshold.

- [`emax_scm_history()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  returns the sequence of models tested during an SCM procedure,
  including AIC, BIC, and convergence status for each candidate.

### S3 methods

- [`print()`](https://emaxnls.djnavarro.net/reference/print.md) displays
  a brief model summary.

- [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md)
  produces a coefficient table with standard errors, confidence
  intervals, and hypothesis tests. A `back_transform = TRUE` argument
  re-expresses log-scaled parameters (logEC50, logHill) on their natural
  scales (EC50, Hill) in the output.

- [`coef()`](https://rdrr.io/r/stats/coef.html) extracts the parameter
  vector; supports `back_transform = TRUE`.

- [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the estimated
  variance-covariance matrix.

- [`confint()`](https://rdrr.io/r/stats/confint.html) computes
  profile-likelihood confidence intervals.

- [`residuals()`](https://emaxnls.djnavarro.net/reference/residuals.md)
  and [`fitted()`](https://emaxnls.djnavarro.net/reference/fitted.md)
  return model residuals and fitted values.

- [`predict()`](https://emaxnls.djnavarro.net/reference/predict.md)
  generates predictions, optionally with standard errors.

- [`anova()`](https://emaxnls.djnavarro.net/reference/anova.md) compares
  nested models by likelihood-ratio test.

- [`logLik()`](https://emaxnls.djnavarro.net/reference/logLik.md),
  [`AIC()`](https://emaxnls.djnavarro.net/reference/AIC.md),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`sigma()`](https://rdrr.io/r/stats/sigma.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html), and
  [`df.residual()`](https://emaxnls.djnavarro.net/reference/df.residual.md)
  return standard model-fit statistics.

- [`simulate()`](https://emaxnls.djnavarro.net/reference/simulate.md)
  draws Monte Carlo replicates by resampling parameters from the
  asymptotic normal distribution of the estimates (requires `mvtnorm`).

- [`emax_fun()`](https://emaxnls.djnavarro.net/reference/emax_fun.md)
  extracts a standalone prediction function from a fitted model that can
  be evaluated at arbitrary dose values and parameter vectors.

- [`emax_converged()`](https://emaxnls.djnavarro.net/reference/emax_converged.md)
  returns `TRUE` or `FALSE` indicating whether the optimiser converged.
  All S3 methods handle non-convergent models gracefully by returning an
  `emaxnls_null` object rather than erroring.

### Data

- `emax_df`: a synthetic dataset of 400 observations across four dose
  groups (0, 100, 200, 300), with a continuous response, a binary
  response, two exposure metrics, continuous and binary covariates, and
  a categorical covariate.
