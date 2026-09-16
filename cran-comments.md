This is a submission of emaxnls version 0.2.0 (updated from 0.1.1).

Kind regards
Danielle Navarro

## Local R CMD check results

Maintainer: 'Danielle Navarro <djnavarro@protonmail.com>'

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Rhub tests

Checked on the following architectures (skipping compiler/sanitiser-only
variants, since emaxnls has no compiled code of its own): linux,
macos, macos-arm64, windows, ubuntu-release, donttest, nosuggests.

linux, windows, ubuntu-release, donttest, and nosuggests all passed
cleanly. The two macOS jobs failed for reasons unrelated to emaxnls,
confirmed on two independent reruns:

- **macos-arm64**: fails in dependency setup, before emaxnls is built or
  checked. The `minpack.lm` dependency (which has Fortran source) fails
  to compile because R-devel on this runner image expects a Fortran
  compiler at `/opt/R/flang-23/bin/flang`, which is not installed on the
  image. This is a provisioning gap in the rhub/GitHub Actions
  macOS-arm64 runner, not an issue with emaxnls.
- **macos-intel**: hangs indefinitely partway through dependency
  installation. `pak` installs successfully, but `pak::repo_status()`
  reports the macOS binary CRAN mirror as unreachable
  (`ok = FALSE`, `ping = NA`), and no further log output appears for
  10+ minutes while sibling jobs in the same run complete in 2-3
  minutes. This looks like an rhub-side networking/mirror issue, not a
  package problem.

Given both failures occur before emaxnls's own code is exercised and
reproduce across reruns, macOS coverage for this submission relies on
CRAN's own `check_mac_release()` / win-builder infrastructure below
rather than rhub's macOS runners.

## CRAN infrastructure tests

- `devtools::check_win_devel()`: OK, no errors/warnings/notes.
- `devtools::check_win_release()`: OK, no errors/warnings/notes.
- `devtools::check_win_oldrelease()`: OK, no errors/warnings/notes.
- `devtools::check_mac_release()` (macOS 4.6.1, arm64/M1): OK, no errors/warnings/notes.

All four checks were re-run after erplots' initial CRAN acceptance to
confirm a transient issue had cleared: at first, all four reported the
same INFO, `Package suggested but not available for checking:
'erplots'`, because erplots (published to CRAN 2026-09-09) had not yet
had binaries built for every platform. On re-run, the three win-builder
checks no longer show the INFO now that erplots' Windows binaries are
published; `check_mac_release()` still shows it because erplots'
`r-release-macosx-arm64` binary specifically remains unbuilt on CRAN.
This is expected and not a concern, since erplots is only used
optionally via `Suggests` and the INFO is purely about CRAN's binary
build queue for a different package, not a defect in emaxnls.

These clean results on CRAN's own Windows and macOS build machines
corroborate that the macOS failures observed on rhub (noted above) are
specific to rhub's runner images and not indicative of a problem with
emaxnls on macOS.
