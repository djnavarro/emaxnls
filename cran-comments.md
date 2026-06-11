This is a new package, designed to support emax regression models in R, commonly used in pharmacometric modeling. It has been checked on rhub and on cran infrastructure. Test coverage is slightly restricted on CI (82%), primarily because model convergence varies across architectures so some test results are difficult to predict on CI, but the local test runs cover 95% of code.

My apologies in advance if I have forgotten something.

Kind regards
Danielle Navarro

## Local R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Rhub results 

Tested on all architectures available on Rhub. The only failure is for rchk, which I believe to be innocuous as the package does not contain compiled code.

https://github.com/djnavarro/emaxnls/actions/runs/27332706889

## CRAN infrastructure tests

- `devtools::check_win_devel()`
- `devtools::check_win_release()`
- `devtools::check_win_oldrelease()`
- `devtools::check_mac_release()`

In each case the logs show only the "new submission" note, and a query about the spelling of the word "Emax" in the DESCRIPTION file. The spelling is correct, including capitalization: it is the standard way the model is referred to in the pharmacometric literature. 