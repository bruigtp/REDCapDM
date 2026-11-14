# Contributing to REDCapDM

Thank you for helping to improve REDCapDM! This guide explains how to report a problem and how to propose a change.

## Reporting a problem

Before opening an issue, check that the problem also happens with the development version:

```r
remotes::install_github("bruigtp/REDCapDM", ref = "development")
```

Then open an [issue](https://github.com/bruigtp/REDCapDM/issues/new/choose) with:

* A **minimal reproducible example**: the smallest code that shows the problem. If possible, use `covican`, the dataset included in the package, or a small made-up dataset.
* The **complete message** of the error or warning, and what you expected to happen.
* The output of `sessionInfo()`.

**Never share real patient data, REDCap API tokens or links to your REDCap project** in an issue.

## Proposing a change

* For anything beyond a small fix (a typo, a broken link), open an issue first to discuss the change.
* Work on a branch created from `development` and open the pull request against `development`: `main` is the released version.
* Document the functions with [roxygen2](https://roxygen2.r-lib.org/) and run `devtools::document()` after changing the documentation.
* Add or update the tests ([testthat](https://testthat.r-lib.org/), 3rd edition). If you change a printed report or a message on purpose, review the snapshots with `testthat::snapshot_review()`.
* The errors, warnings and messages of the package use `rd_abort()`, `rd_warn()` and `rd_inform()`, so that they are formatted with cli and have the classes `REDCapDM_error`, `REDCapDM_warning` and `REDCapDM_message`.
* Add a bullet to the first section of `NEWS.md` describing the change.
* Before opening the pull request, check that `devtools::test()` and `devtools::check()` pass without errors, warnings or notes.

## Code of Conduct

Please note that the REDCapDM project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
