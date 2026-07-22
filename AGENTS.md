# emaxnls — Project Memory

This file is loaded automatically by Posit Assistant at the start of
every conversation. It captures project context so the assistant can
help without needing repeated explanation.

------------------------------------------------------------------------

## What this package does

`emaxnls` implements Emax dose-response regression models for
pharmacokinetic/pharmacodynamic analysis. It supports:

- **Continuous responses** via nonlinear least squares
  ([`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md))
- **Binary responses** via logistic Emax regression
  ([`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md))
- **Stepwise covariate modelling** (SCM) via
  [`emax_scm_forward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  /
  [`emax_scm_backward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
- **Simulation** from fitted models
  ([`simulate()`](https://emaxnls.djnavarro.net/reference/simulate.md))

The Emax model has parameters `E0` (baseline), `Emax` (maximum effect),
`EC50` (half-maximal concentration), and optionally `Hill` (sigmoidal
shape). Parameters `EC50` and `Hill` are estimated on the log scale
internally (`logEC50`, `logHill`), with back-transformation available
via `back_transform = TRUE`.

The package is on CRAN at version 0.1.1; the development version is
0.1.1.9000. It also integrates with the pre-CRAN **erplots** package
(GitHub: `djnavarro/erplots`) for ggplot2-based visualisation of Emax
models (see
[`erplots::er_plot_add_model()`](https://erplots.djnavarro.net/reference/er_plot_add_model.html)
etc.).  
Package website: <https://emaxnls.djnavarro.net/>

------------------------------------------------------------------------

## Package structure

    R/                      # Source code
      api.R                 # Main user-facing functions (emax_nls, emax_logistic, options, init)
      emaxnls-class.R       # S3 class construction for emaxnls
      emaxnls-methods.R     # S3 methods: coef, vcov, confint, residuals, fitted, predict, etc.
      emaxnls-printing.R    # print() and summary()
      emaxnls-init.R        # Starting value and bounds generation
      emaxnls-options.R     # Option configuration
      emaxnls-predict.R     # Predictions with optional SE
      emaxnls-simulate.R    # Monte Carlo simulation from fitted models
      emaxnls-scm.R         # Stepwise covariate modelling
      emaxnls-update.R      # emax_add_term() / emax_remove_term()
      emaxlogistic-*.R      # Binary response equivalents of each of the above
      er-methods.R          # erplots interface: er_predict/er_simulate/er_summary + .onLoad()
      data.R                # emax_df dataset documentation
      utils-*.R             # Internal helpers: validators, mappers, safe wrappers

    tests/testthat/         # Test suite (testthat edition 3)
      helper-platform.R     # Shared helpers: skip_if_not_converged(), test_nls_opts(), etc.
      test-*.R              # One file per feature area

    vignettes/articles/     # Four long-form articles
      fitting-emax-models.Rmd
      fitting-logistic-emax-models.Rmd
      simulating-from-emax-models.Rmd
      stepwise-covariate-modelling.Rmd

    dev/                    # Developer-focused notes (not part of the package)
      # Informal notes to track ongoing investigations, platform quirks, etc.

------------------------------------------------------------------------

## Key classes

| Class          | Description                                              |
|----------------|----------------------------------------------------------|
| `emaxnls`      | Fitted continuous Emax model                             |
| `emaxlogistic` | Fitted binary/logistic Emax model                        |
| `emaxnls_null` | Returned when model fails to converge (graceful failure) |

Models expose the standard S3 interface:
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`residuals()`](https://emaxnls.djnavarro.net/reference/residuals.md),
[`fitted()`](https://emaxnls.djnavarro.net/reference/fitted.md),
[`predict()`](https://emaxnls.djnavarro.net/reference/predict.md),
[`simulate()`](https://emaxnls.djnavarro.net/reference/simulate.md),
[`logLik()`](https://emaxnls.djnavarro.net/reference/logLik.md),
[`AIC()`](https://emaxnls.djnavarro.net/reference/AIC.md),
[`BIC()`](https://rdrr.io/r/stats/AIC.html),
[`anova()`](https://emaxnls.djnavarro.net/reference/anova.md),
[`print()`](https://emaxnls.djnavarro.net/reference/print.md),
[`summary()`](https://emaxnls.djnavarro.net/reference/summary.md),
[`sigma()`](https://rdrr.io/r/stats/sigma.html),
[`deviance()`](https://emaxnls.djnavarro.net/reference/deviance.md),
[`nobs()`](https://rdrr.io/r/stats/nobs.html),
[`df.residual()`](https://emaxnls.djnavarro.net/reference/df.residual.md).

When erplots is loaded, models also respond to
[`erplots::er_predict()`](https://erplots.djnavarro.net/reference/er_model_interface.html),
[`erplots::er_simulate()`](https://erplots.djnavarro.net/reference/er_model_interface.html),
and
[`erplots::er_summary()`](https://erplots.djnavarro.net/reference/er_model_interface.html)
(registered lazily via `.onLoad()` in `R/er-methods.R`; no hard
dependency on erplots).

------------------------------------------------------------------------

## Naming conventions

- **Public API functions**: no prefix, `snake_case` (e.g.,
  [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md),
  [`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md))
- **Internal/private functions**: dot prefix (e.g., `.emax_nls()`,
  `.validate_formula()`)
- **Safe wrappers**: `.safe_fn()`, `.quiet_fn()`, `.nls_safe()`,
  `.nls_lm_safe()`
- **Assertions/validators**: `.assert()`, `.validate_*()`
- **File naming**: `{class}-{concern}.R` for class-specific files (e.g.,
  `emaxnls-methods.R`), `utils-{concern}.R` for shared utilities

------------------------------------------------------------------------

## Optimization algorithms

Three algorithms are available via `optim_method`:

| Value | Algorithm | Notes |
|----|----|----|
| `"gauss"` (default) | Gauss-Newton | Delegates to [`nls()`](https://rdrr.io/r/stats/nls.html) |
| `"port"` | nl2sol (Port library) | Delegates to `nls(..., algorithm = "port")`, supports bounds |
| `"levenberg"` | Levenberg-Marquardt | Delegates to [`minpack.lm::nls.lm()`](https://rdrr.io/pkg/minpack.lm/man/nls.lm.html) |

All fittings support a `max_time` argument (in seconds) to prevent
hangs. The default in tests is 10 seconds via `test_nls_opts()`.

------------------------------------------------------------------------

## Testing conventions

- Framework: testthat 3.0.0 edition
- Test helpers live in `tests/testthat/helper-platform.R`
- Use `test_nls_opts()` / `test_logistic_opts()` when fitting models
  inside tests — these set `max_time = 10` to avoid hangs
- Use `skip_if_not_converged(mod)` to skip a test block when a model
  fails to converge (acceptable on some platforms/compilers)
- Platform helpers: `is_clang()`, `is_gcc15()`, `gcc_version()`,
  `mvtnorm_usable()`
- `mvtnorm` (used for simultaneous confidence intervals) has runtime
  failures on some platforms; the package degrades gracefully when it is
  unavailable
- `test-er-methods.R` gates all tests on
  `skip_if_not_installed("erplots")` — no erplots needed for the base
  test suite to pass

------------------------------------------------------------------------

## Key dependencies

| Package | Role |
|----|----|
| `minpack.lm` | Levenberg-Marquardt optimiser |
| `Deriv` | Symbolic differentiation for gradient computation |
| `mvtnorm` | Multivariate normal sampling for simultaneous CIs |
| `rlang` | Error/condition signalling |
| `stats`, `utils` | Base R (NLS, formula handling, etc.) |
| `tibble` | Suggested (optional); package works without it |
| `erplots` | Suggested (optional, pre-CRAN); enables `er_plot` visualisation pipeline |

------------------------------------------------------------------------

## Data

`emax_df` is a built-in synthetic dataset with 400 observations used in
examples and tests: - Continuous and binary response variables -
Multiple exposure metrics - Continuous, binary, and categorical
covariates - Dose groups: 0, 100, 200, 300

------------------------------------------------------------------------

## Formula interface

``` r

# Structural model: response ~ exposure
emax_nls(response ~ exposure, data = df)

# Covariate model: list of two-sided formulas, one per parameter
emax_nls(
  response ~ exposure,
  covariate_model = list(
    E0     ~ cov1 + cov2,
    Emax   ~ cov1,
    logEC50 ~ cov1
  ),
  data = df
)
```

------------------------------------------------------------------------

## Development workflow

- Documentation generated with roxygen2 (version 8.0.0, markdown
  enabled)
- CI/CD via GitHub Actions: R CMD check, test coverage (Codecov),
  pkgdown build, rhub
- Spell checking via `spelling` (custom words in `inst/WORDLIST`)
- README generated from `README.Rmd`
- **Use US English spelling** in all code comments, roxygen
  documentation, vignettes, and other prose (e.g. “behavior” not
  “behaviour”, “color” not “colour”, “modeling” not “modelling”)
- **Code style by context**: the package itself has minimal
  dependencies, so `@examples` blocks and any other code embedded in
  roxygen documentation must use base R only — no tidyverse. Articles in
  `vignettes/articles/` are website-only and are not part of the
  package; tidyverse style is preferred there.

### Common commands

| Task | Command |
|----|----|
| Regenerate documentation | `devtools::document()` |
| Run test suite | `devtools::test()` |
| Build pkgdown site | [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html) |

### Generated files — do not edit directly

`NAMESPACE` and all files under `man/` are generated by roxygen2. Never
edit them directly; edit the roxygen source in `R/` and regenerate with
`devtools::document()`.

------------------------------------------------------------------------

## Assistant preferences

### Autonomy

- For **large or structural changes** (touching multiple files, renaming
  public functions, reorganising classes), propose a plan and ask for
  approval before editing files. Act directly on small, well-scoped
  changes.
- When making non-trivial changes, **explain the reasoning** only when
  the decision is non-obvious or involves a tradeoff. Skip explanation
  for routine fixes.
- After making code changes, **run `devtools::test()`** to verify
  nothing is broken.
- When a change warrants a `NEWS.md` entry, **suggest what to add** but
  do not write it — leave changelog management to the developer.

### Dependencies

- The package has a minimal-dependency philosophy. **Ask before adding
  any new dependency** to either `Imports` or `Suggests` in
  `DESCRIPTION`.

### Roxygen documentation

- Every exported function should have `@returns` and `@examples`.
- Add `@seealso` only when there is a closely related function worth
  cross-referencing.
- Markdown formatting is enabled; use it (backticks, bold, etc.) in
  roxygen prose.

### Commit messages

- Use **freeform imperative mood**: “Add x”, “Fix y”, “Remove z”. No
  conventional-commit prefix required.
