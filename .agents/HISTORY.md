# emaxnls design history

This file is a condensed historical record of completed design
decisions: what was tried, what was rejected, and why. It exists for
context in future sessions, not as a changelog or PR log -- step-by-step
implementation narrative (file-by-file diffs, exact test-pass counts,
staged PR sequencing) has generally been trimmed in favor of the
decisions themselves; see git history for that level of detail if it's
ever needed. Entries are in roughly chronological order. Current-state
facts that came out of this history (what the API looks like today) live
in `AGENTS.md`, not here.

## `sim_resp`: extending `er_simulate()` rather than adding a new generic

erplots' `er_vpc_add_simulated(model = ...)` needs a full response-scale
draw -- including observation-level noise -- to build a VPC. The
question was how to supply it from `emaxnls`.

Two approaches were considered:

1. **A new generic** (e.g. `er_simulate_response()`): keeps the noise
   draw entirely separate from `er_simulate()`'s existing
   parameter-uncertainty-only `fit_resp` output.
2. **Additive extension to `er_simulate()`**: the method may return an
   optional `sim_resp` column alongside `fit_resp`; callers that don't
   need it ignore it, and `er_vpc_add_simulated()` errors informatively
   if `sim_resp` is absent rather than silently treating `fit_resp` as
   a noisy draw.

Option 2 was chosen, mirroring the same decision made on the erplots
side (see erplots' `.agents/HISTORY.md`, "er_vpc_plot() and the sim_resp
extension to er_simulate()"): both `emaxnls` and `emaxlogistic` already
had `stats::simulate()` methods computing both the expected response and
a full response draw in one pass, so extending `er_simulate()`'s return
value was the smaller, more natural change. Adding a fourth generic would
have required every current and future model-interface implementer to
provide a second method for a concept already implicit in `er_simulate()`.

The implementation reuses the existing noise models from
`simulate.emaxnls()` / `simulate.emaxlogistic()`:
`.emax_resample()` draws from `Normal(fit_resp, sigma(model))` for
`emaxnls`, and `.emax_logistic_resample()` draws from
`Bernoulli(fit_resp)` for `emaxlogistic`.

Merged in PR #67.
