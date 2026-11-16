## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11, R 4.6.2, with `devtools::check(remote = TRUE, manual = TRUE)`: 0 errors, 0 warnings. Its only note, a `lastMiKTeXException` file in the temporary directory, comes from the local MiKTeX installation that builds the PDF manual.
* GitHub Actions: macOS (R release), Windows (R release) and Ubuntu (R devel, release and oldrel-1).
* win-builder: R devel and R release: Status OK.

## Changes in this version

This is a minor release (1.1.0) with new features and bug fixes, described in NEWS.md.

Two changes can affect existing code. Both are announced at the top of NEWS.md:

* `library(REDCapDM)` no longer attaches other packages (dplyr, purrr, stringr, ...).
* REDCapR and openxlsx moved from Imports to Suggests. The functions that need them (reading from the REDCap API or from .xlsx files, and `rd_export()`) check that they are installed with `rlang::check_installed()`. The examples, tests and vignettes run without them.

The argument `addTo` of `rd_query()` and `rd_event()` is deprecated in favour of `add_to` (with lifecycle) and still works.

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
