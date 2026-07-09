# rhub Platform Timing Analysis

## The Pattern

There are two very distinct groups:

| Group | Platforms | Test suite (s) | Examples total (s) |
|-------|-----------|---------------:|-----------------:|
| **SLOW** | c23, clang16–20, gcc15, noremap | 1,780 – 2,778 | 813 – 1,164 |
| **fast** | everything else | 8 – 35 | 2 – 11 |
| *(valgrind)* | valgrind | 785 | — |

The slow platforms are roughly **200× slower** than the fast ones. Valgrind is slow as expected
(memory instrumentation), but the 8 genuinely-slow platforms are another matter entirely.

> **Summary of root cause (updated after deeper investigation):** The slowness is primarily
> an **R regression** present in R-devel snapshots built around mid-March 2026 (r89623–r89629),
> fixed by April 2026 (r89874). The "clang16–20" platform names are a red herring — R on
> those platforms was compiled with GCC 11.4.0, identical to the fast clang21/22 platforms.
> The only difference is the R snapshot. The `gcc15` anomaly is real but secondary: GCC 15
> compiles R in a way that exposes the regression, while GCC 13/14 (same old snapshot) do
> not. See the "Revised diagnosis" section below.

---

## What's slow

Within the slow runs, it's not all operations — it's specifically **NLS model fitting**.
Examples like `emax_logistic_options` or `emax_df` (which don't fit models) take milliseconds
even on slow platforms. But anything that calls `nls()` — `fitted`, `logLik`, `coef`,
`summary`, and especially `emax_scm` — is 200× slower. This is a computational bottleneck
inside the optimizer, not a package loading or R infrastructure issue.

---

## Which optimizer is responsible

**Evidence points to base R's `nls()`, not `minpack.lm::nls.lm()`.**

Every example uses `optim_method = "gauss"` (the default), which routes to `nls()`. There
are no examples that use `"levenberg"` (`nls.lm()`), so `nls.lm()` is untested on the slow
platforms and we cannot say whether it would be fast or slow there.

In `test-optim-method.R`, all three methods — `"gauss"`, `"port"`, and `"levenberg"` — are
skipped on clang and gcc15 platforms because they fail to converge (a separate, pre-existing
toolchain issue). This means:

- The convergence failures are the reason for the skips, not the slowness per se.
- `nls.lm()` behaviour on these platforms remains unknown.
- **`"port"` is also skipped**, and `port` uses `nls(..., algorithm = "port")` — still base
  R, just a different internal algorithm. The fact that `port` fails alongside `gauss`
  suggests the issue is not specific to the Gauss-Newton algorithm within `nls()`, but
  something more fundamental about how these platforms handle `nls()` as a whole (or the
  underlying C-level numerical routines it calls).

The slow timings in the bulk of the test suite come from tests that use `test_nls_opts()`
with the default `optim_method = "gauss"`, where models run slowly but do not fail outright.
Several examples in `emaxnls-methods.R` also do not set `max_time`, so they can run
unbounded — which is why some example timings (e.g. `fitted`: 25s, `logLik`: 27s on c23)
far exceed the 10s cap used in tests.

---

## Revised diagnosis

Checking the compiler and R revision recorded in each platform's `00check.log` overturns
most of the earlier analysis.

### The R revision is the primary driver

Every slow platform was running an R-devel snapshot from mid-March 2026:

| Platform | R revision | Compiled by | Speed |
|----------|-----------|-------------|-------|
| c23, clang18, clang20, noremap | r89623 (2026-03-14) | GCC 11.4.0 (Ubuntu) | SLOW |
| clang16, clang17, clang19, gcc15 | r89629 (2026-03-15) | GCC 11.4.0 (Ubuntu) or GCC 15 (Red Hat) | SLOW |
| gcc13, gcc14 | r89629 (2026-03-15) | GCC 13/14 (Red Hat) | **fast** |
| ubuntu-gcc12 | r89874 (2026-04-13) | GCC 11.4.0 (Ubuntu) | **fast** |
| clang21, clang22, donttest, … | r90185+ (2026-06-21+) | GCC 11.4.0 (Ubuntu) | **fast** |
| atlas, nosuggests, gcc-asan, mkl | r90185+ | GCC 15 (Red Hat) | **fast** |

The regression was present in R-devel around r89623–r89629 (mid-March 2026) and was fixed
by r89874 (April 2026). All platforms running newer snapshots are fast regardless of their
compiler.

### The clang version was a red herring

All platforms named `clang16` through `clang22` compiled R with **GCC 11.4.0 on Ubuntu
22.04** — the same compiler on the same OS. The "clang" label refers to the version of
clang available in the container (for compiling R packages that have C code), not the
compiler used to build R itself. The difference between the slow clang16–20 platforms and
the fast clang21–22 platforms is entirely explained by the R revision: old snapshot vs
new snapshot.

### The gcc15 anomaly

For the Red Hat container platforms (gcc13 through gcc16), R was compiled with the
corresponding GCC version. Here gcc13 and gcc14 with old R are fast, while gcc15 with old
R is slow. All Red Hat platforms with new R are fast (atlas, nosuggests, gcc-asan, mkl,
valgrind all use GCC 15 and are fine). This suggests that GCC 15 compiled R in a way that
**exposed** the regression in r89629, while GCC 13 and 14 compiled R in a way that did not.
Whether this is a code-generation difference, an inlining difference, or something else in
GCC 15 interacting badly with the buggy R code is unknown.

### c23, noremap, and the Ubuntu GCC 11 platforms

These platforms are all running old R (r89623) on Ubuntu with GCC 11. They are slow for the
same underlying reason as clang16–20 (old R snapshot) — the `c23` and `noremap` labels
describe additional compilation flags, but the slowness is not specific to those flags. This
supersedes the earlier hypothesis that the gcc15/c23 pairing was explained by GCC 15
defaulting to C23: while that is true as a compiler fact, it is not what caused the
slowness here.

### What is the R regression?

The bug was introduced somewhere between r89439 (2026-02-19, Intel platform — fast) and
r89623 (2026-03-14 — slow), and fixed between r89629 and r89874. Identifying the exact
commit would require bisecting R-devel in that range against `nls()` performance — out of
scope for now, but the window is narrow (~5 weeks of commits).

---

## Practical strategy for running rhub checks

### Routine checks

For day-to-day pre-release checks, use a small curated subset that covers the three major
OS families, a sanitizer build, and the no-suggested-packages scenario:

```r
rhub::rhub_check(platforms = c(
  "ubuntu-release",  # R release on Ubuntu (fast, stable baseline)
  "windows",         # Windows R-devel
  "macos-arm64",     # macOS ARM R-devel
  "clang-asan",      # address sanitizer (catches memory issues in R internals)
  "nosuggests"       # verifies graceful degradation without suggested packages
))
```

All five platforms currently run up-to-date R snapshots and complete in under 35 seconds.

### Full pre-CRAN check

Run the full platform matrix once before submitting to CRAN:

```r
# Check which platforms have current R snapshots before running
rhub::rhub_platforms()

# Run all platforms except rchk (always fails for reasons unrelated to the package)
rhub::rhub_check(platforms = setdiff(rhub::rhub_platforms()$name, "rchk"))
```

With `max_time = 10` now set on all examples and tests, even the slow containers are
bounded in how long they can run.

### Platforms to be aware of

As of July 2026, the following containers are still running stale R-devel snapshots from
mid-March 2026 (r89623–r89629) and will be slow (~20–45 minutes each) until the container
images are refreshed:

| Platform | Snapshot | Typical test time |
|----------|----------|-------------------|
| `c23`, `noremap` | r89623 (2026-03-14) | ~40 min |
| `clang16`–`clang20`, `gcc15` | r89629 (2026-03-15) | ~30–45 min |
| `gcc13`, `gcc14` | r89629 (2026-03-15) | ~15 s (fast — GCC 13/14 don't expose the R bug) |
| `intel` | r89439 (2026-02-19) | ~12 s (fast) |

Check `rhub::rhub_platforms()` before a full run to see whether these have been updated.
Once they move to r89874 or later, they should return to normal speed.

---

## Data

Detailed per-platform timings are in `platform-timings.csv` (same folder). Columns:

| Column | Description |
|--------|-------------|
| `platform` | rhub platform name (shortened) |
| `group` | `"SLOW"`, `"fast"`, or `"valgrind (expected slow)"` |
| `test_elapsed_s` | Elapsed time for full test suite (seconds) |
| `ex_total_s` | Total elapsed time across all examples (seconds) |

---

*Last updated: 2026-07-10*
