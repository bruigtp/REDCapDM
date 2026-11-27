## R CMD check results

0 errors | 0 warnings | 0 notes

The local check on Windows gives one note, `checking for detritus in the temp directory ... 'lastMiKTeXException'`, which comes from the local MiKTeX installation that builds the PDF manual. It doesn't appear on win-builder or on GitHub Actions.

## Test environments

* Local: Windows 11 x64, R 4.6.2 (ucrt), with `devtools::check(remote = TRUE, manual = TRUE)`: 0 errors, 0 warnings, 1 note (the MiKTeX file above).
* win-builder: R-devel (2026-11-17 r90926 ucrt) and R-release: Status OK.
* GitHub Actions: macOS (R release), Windows (R release) and Ubuntu (R devel, release and oldrel-1): all passing.

## Changes in this version

This is a minor release (1.1.0) with new features and bug fixes, described in NEWS.md.

The changes that can affect existing code are listed under "Breaking changes" at the top of NEWS.md. The main ones:

* `library(REDCapDM)` no longer attaches other packages (dplyr, purrr, stringr, ...).
* REDCapR and openxlsx moved from Imports to Suggests. The functions that need them (reading from the REDCap API or from .xlsx files, and `rd_export()`) check that they are installed with `rlang::check_installed()`. The examples, tests and vignettes run without them.
* The query reports of `rd_query()` and `rd_event()` have the unique event name in `Event` and its label in a new `EventLabel` column. `check_queries()` also accepts the reports created with previous versions.
* The `filter` of `rd_insert_na()` must name the columns it uses, so that their events can be checked: a filter whose columns can't be determined stops with an error.

The argument `addTo` of `rd_query()` and `rd_event()` is deprecated in favour of `add_to` (with lifecycle) and still works.

## Reverse dependencies

There are currently no reverse dependencies on CRAN (checked with `tools::package_dependencies("REDCapDM", reverse = TRUE)`).
