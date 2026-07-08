# emaxnls 0.1.1.9000

## New features

* Adds `emax_logistic()` for fitting binary-outcome Emax models using iterative
  reweighted least squares (IRLS), along with `emax_logistic_init()` and
  `emax_logistic_options()` for initialization and configuration. All standard
  S3 methods (`coef()`, `vcov()`, `confint()`, `residuals()`, `fitted()`,
  `predict()`, `anova()`, `logLik()`, `AIC()`, `BIC()`, `deviance()`,
  `simulate()`) are supported for the new `emaxlogistic` class.

* Redesigns `print()` and `summary()` methods for `emaxnls` and `emaxlogistic`
  objects:

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

## Documentation

* Expands the `summary()` documentation to explain the relationship between
  the `p_adjust` and `simultaneous` arguments. The two are independent tools
  for multiplicity — `p_adjust` corrects the hypothesis-test p-values, while
  `simultaneous` widens the confidence intervals — and they use different
  machinery, so their reject/retain decisions need not agree. The new
  "Multiplicity: p-value adjustment versus simultaneous intervals" section
  spells out what each argument changes, why the adjusted p-values and the
  simultaneous intervals may disagree, and which tool to reach for (#47).

## Bug fixes

* Fixes `AIC()` and `BIC()` when called with multiple model arguments (#37).

* Fixes `na.action` parameter not being passed through correctly in
  `emax_nls()` and `emax_logistic()` (#38).

* Fixes `predict()` omitting `residual.scale` from the return value when
  `se.fit = TRUE` (#39).

* Fixes crashes in `emax_logistic_init()` and prevents `Inf` parameter bounds
  arising during initialisation (#40).

* Hardens `.nls_call()` to avoid cryptic errors when optimisation fails, and
  tightens argument validation in `emax_fun()` (#41).

* Adds input validation for the binary response variable in `emax_logistic()`,
  with informative errors for non-binary or out-of-range values (#42).

* Fixes validator error messages to reference public API parameter names rather
  than internal names (#43).

* Fixes `NaN` produced by `.binomial_deviance()` when predicted probabilities
  are exactly 0 or 1 (boundary cases) (#44).

* `confint()` now accepts a `simultaneous` argument, mirroring `summary()`.
  Previously `confint(object, simultaneous = TRUE)` silently ignored the
  argument (swallowed by `...`) and returned pointwise intervals. Setting
  `simultaneous = TRUE` now returns simultaneous (joint) Wald intervals that
  match those reported by `summary(object, simultaneous = TRUE)` (#46).


# emaxnls 0.1.1

* Expanded description of package.
* Fixes bug when `emax_nls_init()` is called manually.
* `anova()` now warns rather than errors when fewer than two converged models
  are supplied.
* Additional examples in documentation.
* Improved unit tests.


# emaxnls 0.1.0

Initial CRAN submission. The package provides tools for fitting and analysing
Emax dose-response models via nonlinear least squares.

## Model fitting

* `emax_nls()` fits continuous-response Emax models using NLS, supporting both
  the hyperbolic (`E0 + Emax * x / (EC50 + x)`) and sigmoidal
  (`E0 + Emax * x^Hill / (EC50^Hill + x^Hill)`) model forms.

* `emax_nls_options()` configures the optimisation algorithm and control
  parameters. Three algorithms are supported via the `optim_method` argument:
  `"gauss"` (Gauss-Newton, default), `"port"` (bounded nl2sol), and
  `"levenberg"` (Levenberg-Marquardt via `minpack.lm`).

* `emax_nls_init()` generates starting values and parameter bounds
  automatically from the data, including support for categorical covariates.
  Users can also call it directly to inspect or override the initialisation
  before fitting.

## Covariate modeling

* Covariates can be added to any structural parameter (E0, Emax, logEC50,
  logHill) via a formula interface, e.g. `emax_nls(rsp ~ dose, data = df, E0 = ~ group + age)`.

* `emax_add_term()` and `emax_remove_term()` update a fitted model by adding
  or removing a single covariate term without refitting from scratch.

## Stepwise covariate modeling (SCM)

* `emax_scm_forward()` performs forward covariate selection, adding one term
  at a time while the likelihood-ratio test p-value stays below a threshold.

* `emax_scm_backward()` performs backward elimination, removing terms while
  the p-value exceeds a threshold.

* `emax_scm_history()` returns the sequence of models tested during an SCM
  procedure, including AIC, BIC, and convergence status for each candidate.

## S3 methods

* `print()` displays a brief model summary.

* `summary()` produces a coefficient table with standard errors, confidence
  intervals, and hypothesis tests. A `back_transform = TRUE` argument
  re-expresses log-scaled parameters (logEC50, logHill) on their natural
  scales (EC50, Hill) in the output.

* `coef()` extracts the parameter vector; supports `back_transform = TRUE`.

* `vcov()` returns the estimated variance-covariance matrix.

* `confint()` computes profile-likelihood confidence intervals.

* `residuals()` and `fitted()` return model residuals and fitted values.

* `predict()` generates predictions, optionally with standard errors.

* `anova()` compares nested models by likelihood-ratio test.

* `logLik()`, `AIC()`, `BIC()`, `sigma()`, `nobs()`, and `df.residual()`
  return standard model-fit statistics.

* `simulate()` draws Monte Carlo replicates by resampling parameters from
  the asymptotic normal distribution of the estimates (requires `mvtnorm`).

* `emax_fun()` extracts a standalone prediction function from a fitted model
  that can be evaluated at arbitrary dose values and parameter vectors.

* `emax_converged()` returns `TRUE` or `FALSE` indicating whether the
  optimiser converged. All S3 methods handle non-convergent models gracefully
  by returning an `emaxnls_null` object rather than erroring.

## Data

* `emax_df`: a synthetic dataset of 400 observations across four dose groups
  (0, 100, 200, 300), with a continuous response, a binary response, two
  exposure metrics, continuous and binary covariates, and a categorical
  covariate.
