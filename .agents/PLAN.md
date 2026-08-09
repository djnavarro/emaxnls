# emaxnls development plan

This document tracks scoped-out future development for emaxnls -- work
that's been thought about but not done, or deliberately deferred. It is
not a changelog: once an item here is completed, its write-up should
move to [.agents/HISTORY.md](HISTORY.md) and be removed from this file
rather than marked "done" in place. Items are grouped by target release.

## Next CRAN release

### Remove `Remotes: djnavarro/erplots` from DESCRIPTION before CRAN submission

**Blocked on erplots 0.1 reaching CRAN.** emaxnls must not be submitted
until erplots is available on CRAN.

erplots is already listed in `Suggests` in `DESCRIPTION`, and the lazy
`.onLoad()` registration in `R/er-methods.R` (the `vctrs::s3_register()`
idiom) is the correct CRAN-compliant approach for S3 methods whose
generics live in a `Suggests` package -- no NAMESPACE changes are needed.
The only pre-submission change required in emaxnls is removing the
`Remotes: djnavarro/erplots` line from `DESCRIPTION` (CRAN does not
allow `Remotes:` entries pointing to non-CRAN sources).

Once that line is removed and erplots is on CRAN, the erplots-side
workaround (`Requires: emaxnls (>= 0.1.1.9000)` in erplots'
`DESCRIPTION`, which forces r-universe to use the GitHub dev version
of emaxnls) can also be dropped from erplots.
