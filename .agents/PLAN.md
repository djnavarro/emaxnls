# emaxnls development plan

This document tracks scoped-out future development for emaxnls -- work
that's been thought about but not done, or deliberately deferred. It is
not a changelog: once an item here is completed, its write-up should
move to [.agents/HISTORY.md](HISTORY.md) and be removed from this file
rather than marked "done" in place. Items are grouped by target release.

## Development version (0.1.1.9000)

### Merge `feature/er-simulate-sim-resp`

The `feature/er-simulate-sim-resp` branch widens `er_simulate.emaxnls()`
and `er_simulate.emaxlogistic()` to return an optional `sim_resp` column
alongside the existing `fit_resp` column. `sim_resp` is a full
response-scale draw that includes observation-level noise (not just
parameter uncertainty), which `er_vpc_add_simulated(model = ...)` in
erplots requires to build a VPC.

The implementation reuses the same noise models already used by
`simulate.emaxnls()` / `simulate.emaxlogistic()` (`.emax_resample()` /
`.emax_logistic_resample()`): `Normal(fit_resp, sigma(model))` for
`emaxnls`, `Bernoulli(fit_resp)` for `emaxlogistic`. See
[.agents/HISTORY.md](HISTORY.md) for the design rationale (additive
extension rather than a new generic). See `?er_model_interface` in
erplots for the full updated contract.

Once this branch is merged, the next step is a new CRAN release (see
below).

## Next CRAN release

### Register `er_predict`/`er_simulate`/`er_summary` S3 methods in NAMESPACE

The CRAN 0.1.1 release does not register the `er_predict()`,
`er_simulate()`, or `er_summary()` S3 methods that erplots relies on.
They are registered lazily via `.onLoad()` (in `R/er-methods.R`) only
when erplots is loaded, but they are absent from `NAMESPACE` --
confirmed by inspecting the installed NAMESPACE from CRAN. erplots works
around this by requiring `emaxnls >= 0.1.1.9000` in its `DESCRIPTION`,
forcing r-universe's resolver to use the GitHub dev version instead of
the CRAN release. This workaround should be removed once a new CRAN
release ships with the methods properly registered.

Check whether the methods need to be in `NAMESPACE` (via
`S3method(er_predict, emaxnls)` etc., added by roxygen2) or whether the
`.onLoad()` lazy-registration approach is sufficient for CRAN -- the key
question is whether CRAN's checks expect registered generics from
`Suggests` packages to appear in `NAMESPACE`.
