## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11, R 4.6.2.
* GitHub Actions: macOS (R release), Windows (R release) and Ubuntu (R devel, release and oldrel-1).
* win-builder: R devel. TODO: add the result.

## Changes in this version

This is a minor release (1.2.0) with new features and bug fixes, described in NEWS.md.

Two changes can affect existing code. Both are announced at the top of NEWS.md:

* `library(REDCapDM)` no longer attaches other packages (dplyr, purrr, stringr, ...).
* REDCapR and openxlsx moved from Imports to Suggests. The functions that need them (reading from the REDCap API or from .xlsx files, and `rd_export()`) check that they are installed with `rlang::check_installed()`. The examples, tests and vignettes run without them.

The argument `addTo` of `rd_query()` and `rd_event()` is deprecated in favour of `add_to` (with lifecycle) and still works.

## Reverse dependencies

TODO: add the result of the reverse dependency check.
