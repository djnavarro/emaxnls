This is a resubmission of a new package designed to support emax regression models in R, commonly used in pharmacometric modeling (version updated from 0.1 to 0.1.1). The resubmission addresses the primary concern with the original submission, namely that the Description field in DESCRIPTION was not sufficiently detailed. I have also taken the opportunity to improve documentation and the unit tests in the resubmission.

Kind regards
Danielle Navarro

## Local R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Rhub results 

Checked on the following architectures: 

linux, m1-san, macos, macos-arm64, windows, atlas, c23, clang-asan, clang-ubsan, clang16, clang17, clang18, clang19, clang20, clang21, clang22, donttest, gcc-asan, gcc13, gcc14, gcc15, gcc16, intel, lto, mkl, nold, noremap, nosuggests, ubuntu-clang, ubuntu-gcc12, ubuntu-next, ubuntu-release, valgrind, vnu

## CRAN infrastructure tests

- `devtools::check_win_devel()`
- `devtools::check_win_release()`
- `devtools::check_win_oldrelease()`
- `devtools::check_mac_release()`

In each case the logs show only the "new submission" note, and a query about the spelling of the word "Emax" in the DESCRIPTION file. The spelling is correct, including capitalization: it is the standard way the model is referred to in the pharmacometric literature. 