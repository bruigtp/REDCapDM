# Changelog

## REDCapDM 1.2.0

### Breaking changes

- [`library(REDCapDM)`](https://bruigtp.github.io/REDCapDM/) no longer
  attaches other packages (dplyr, purrr, stringr, labelled, …). Load the
  packages you use in your own code,
  e.g. [`library(dplyr)`](https://dplyr.tidyverse.org) to use
  [`filter()`](https://rdrr.io/r/stats/filter.html) or `%>%`.
- REDCapR and openxlsx are now suggested packages: they are only needed
  to read data from the REDCap API or `.xlsx` files and to export
  queries with
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md).
  These functions ask to install them when they are missing.

### New features

- New
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  function to visualise missing data per variable (barplot) or per event
  and variable (heatmap in longitudinal projects). With `by = "record"`
  it shows the missing values record by record. The `events` and `forms`
  arguments choose what is plotted, long labels are shortened
  (`max_label_length`) and the events where a variable is not collected
  are left blank.
- New `rm_mdc` argument in
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  to remove REDCap Missing Data Codes (default `TRUE`).
- With `redcap_data(rm_mdc = FALSE)`, the Missing Data Codes of
  validated text fields (numbers, dates, …) are kept in a new
  `<field>_mdc` column, so that the field keeps its type.
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md),
  [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  and
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  handle these columns like the `.factor` ones.
- New `forms` argument in
  [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  to remove every variable of a form.
- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  reports the number of different values of each calculated field.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  returns `eval_data`, the evaluation next to the record identifier,
  event and repetition of each row.
- [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  also updates the `.factor` version of the variables.
- [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
  warns when the dictionary logic was already translated to R with
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md).
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  translates logic that uses the same variable in different events
  (e.g. `[visit_2_arm_1][weight] - [visit_1_arm_1][weight]`).
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  returns a project without checkbox fields unchanged, with a warning,
  instead of an error.
- The `addTo` argument of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  is now called `add_to`. `addTo` still works, with a deprecation
  warning.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  and
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  return a list of class `REDCapDM_report`: printed in the console, it
  shows the number of queries per variable and event (per state in
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md))
  instead of the whole list. `$queries`, `$results`, `add_to` and
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
  work as before.

### Repeated instruments and repeating events

- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  now only checks each variable in the rows where it is collected. In
  projects with repeated instruments, empty fields of other instruments
  are no longer reported as queries.
- The `Repetition` column of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  is now correct for every row, and query links open the right instance.
- [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  uses the event and the repetition to match queries, so the same
  variable in different events or instances is tracked separately.
- [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  computes missingness only over the rows where each variable is
  collected.
- Repeating events are now detected internally.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  translates and evaluates logic in projects with repeated instruments
  and repeating events: the logic of a repeating instrument is evaluated
  in its instances and, as in REDCap, the variables of the other forms
  take the value of the base row of the same record and event. As a
  result,
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  now works with repeated instruments, and
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
  apply and translate their branching logic.
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  evaluates the branching logic of checkboxes and
  [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  no longer skips the recalculation, the checkbox branching logic and
  the dictionary translation in projects with repeated instruments.
- `rd_missplot(by = "record")` shows each repetition as a row and tells
  apart the cells of variables not collected in that row.

### Bug fixes

- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  failed when `addTo` received the output of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  or
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md).
- `rd_split(wide = TRUE)` failed when it was called as
  [`REDCapDM::rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  without loading the package.
- The `filter` and `expression` of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  and the `filter` of
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  and
  [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md),
  can use objects created inside a function or a loop: before, only the
  objects of the global environment were found. They are no longer run
  with `eval(parse())`, so an “x” inside a text value of `expression`
  (e.g. `x == 'x'`) is no longer replaced by the variable, and filters
  can’t use the internal objects of the functions. As before, several
  conditions can be separated by commas and `data$var` refers to the
  data.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  explains the mistake when one of several filters (one per variable)
  can’t be evaluated.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md),
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  and
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  gave wrong results when the variables had already been converted to
  factors with
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md).
- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  applied twice duplicated the `_recalc` fields.
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  and
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  gave wrong results with tibbles, and
  [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  couldn’t name the options of data without variable labels.
- [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  failed in non-longitudinal projects.
- `rd_query(report_zeros = TRUE)` failed when every variable had
  queries.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  and
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  turned alphanumeric record identifiers into `NA`; identifiers are now
  sorted naturally.
- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  assigned the DAG of the first record to every query, failed without
  `redcap_event_name.factor` and could pair events with the wrong
  description. It now explains that it needs a longitudinal project.
- [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  failed when the record identifier was not called `record_id`.
- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  reported a calculated field as different from REDCap when the values
  were equal but the variable had a label.
- `rd_split(wide = TRUE)` returned several rows per record for repeated
  instruments in non-longitudinal projects.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  with several variables checked each variable also in the events of the
  other ones.
- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  reported records of other arms as missing the event in projects with
  several arms.
- [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  failed with descriptive fields in the dictionary.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  no longer warns about an incomplete final line when reading the event
  file.
- Fixed a stray “FALSE” at the end of a
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  warning.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  translated `datediff()` differently from REDCap: it now returns the
  absolute difference unless the last argument is `true`, accepts any
  date format (`"ymd"`, `"mdy"`…), uses `"M"` for months and `"m"` for
  minutes, and understands `"today"`.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  (and so
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md),
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md))
  translated several REDCap functions wrongly:
  [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) returned a single
  value for the whole dataset,
  [`sum()`](https://rdrr.io/r/base/sum.html) didn’t ignore blank values,
  `rounddown(x, d)` rounded instead of rounding down and several
  `rounddown()` in the same logic were mistranslated.
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html), `roundup()`,
  `mod()`, `exponential()`, `isnumber()`, `contains()` and
  [`length()`](https://rdrr.io/r/base/length.html) are now translated
  too.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  ignored the branching logic that uses a variable of another event
  (e.g. `[baseline_arm_1][sex] = '2'`) when the variable was checked
  alone, and checked every row when no row met the branching logic.
- [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  could remove `record_id`, also through `pattern`.
- [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
  reported success when the folder of the file didn’t exist.
- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  failed with event labels.
- [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
  failed after
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
  when the only factors were yes/no or true/false fields.
- `redcap_data(rm_mdc = FALSE)`,
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  and
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  failed or warned in projects without records.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  no longer runs the [`library(Hmisc)`](https://hbiostat.org/R/Hmisc/)
  and [`graphics.off()`](https://rdrr.io/r/grDevices/dev.html) lines of
  the R script exported by REDCap: it failed when Hmisc wasn’t
  installed, attached Hmisc (masking
  e.g. [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html))
  and closed the open plots.
- knitr and lubridate are now declared dependencies: several functions
  failed when knitr wasn’t installed, and the translation of
  `datediff()` needed lubridate. kableExtra is optional: without it the
  HTML reports are not styled.

### Clearer messages

- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  explains when a file is not found, when the csv file read by the R
  script is not in its folder and when the event file is not the
  instrument-event mapping of the project. It warns when variables of
  the data are not in the dictionary and when the project has no records
  yet.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  names the variables that don’t exist (and how to check a checkbox),
  explains why an expression can’t be evaluated, warns when a text
  variable is compared with a number and checks `negate`,
  `report_zeros`, `by_dag` and `add_to`.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  names the variables it can’t find and shows the translated logic and
  the R error when it can’t be evaluated.
- [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  warns when `pattern` matches no variable,
  [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  when no row meets the filter, and
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
  and
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  when `exclude` has unknown variables.
- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  asks for the `event`,
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  explains why it can’t split by event,
  [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  asks for `delete_pattern` before splitting the data,
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  checks that it receives query reports,
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  checks `max_vars` and the number of rows, and
  [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  checks `checkbox_labels`.
- Every function checks its arguments: a wrong value (e.g. `NULL`, `NA`,
  a vector instead of a single value or a text instead of TRUE/FALSE)
  stops with a message that names the argument and what it expects,
  instead of an internal R error.
- [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  never removes the REDCap columns of events and repetitions through
  `pattern`, and
  [`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md)
  and
  [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  work with a dataset without rows.
- Every function checks that the dictionary has the columns it needs,
  and
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  names the variables of the data that are not in the dictionary.
- Errors about wrong arguments and the informative messages use cli:
  they show the function that was called and the value received, and are
  formatted in the console. Errors, warnings and messages have the
  classes `REDCapDM_error` (`REDCapDM_error_argument` for wrong
  arguments), `REDCapDM_warning` and `REDCapDM_message`.
- The informative messages (e.g. the progress of
  [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md))
  can be hidden with `options(REDCapDM.quiet = TRUE)`.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  and
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  report all their errors and warnings with cli. Problems with the files
  have the class `REDCapDM_error_file`, and problems with the REDCap API
  the class `REDCapDM_error_api`, which keeps the original error of the
  API as its parent.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  informs with a message, instead of a warning, when it applies the
  branching logic automatically.
- All the other functions also report their errors and warnings with
  cli, naming the variables, forms and events involved. Logic that can’t
  be translated or evaluated has the class `REDCapDM_error_logic` (with
  the translated logic and the R error), a dataset that doesn’t match
  its dictionary the class `REDCapDM_error_data`, and a folder that
  doesn’t exist in
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
  the class `REDCapDM_error_file`. A filter of
  [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  that can’t be evaluated keeps the original R error as its parent.

### Documentation

- The vignette is split into four: getting started
  ([`vignette("REDCapDM")`](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.md)),
  missing data
  ([`vignette("missing-data")`](https://bruigtp.github.io/REDCapDM/articles/missing-data.md)),
  data processing
  ([`vignette("data-processing")`](https://bruigtp.github.io/REDCapDM/articles/data-processing.md))
  and queries
  ([`vignette("query-management")`](https://bruigtp.github.io/REDCapDM/articles/query-management.md)).
- The reference of the website groups the functions by task, and the
  help of each function links to the related functions.
- New contributing guide, code of conduct and issue templates on GitHub.
  The README states the R version the package needs (4.1 or later).
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
  [`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md),
  [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md),
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md),
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md),
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md),
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  and
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  are now stable (their arguments and results won’t change without a
  deprecation period).
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  remains experimental.

### Internal changes

- Snapshot tests keep the text of the printed reports and of the main
  messages, and new tests cover chained functions and less common
  arguments.
- Code cleanup following lintr: the conditions of `if()` use `&&` and
  `||`, and the query codes are numbered with
  [`dplyr::row_number()`](https://dplyr.tidyverse.org/reference/row_number.html),
  which also works with zero rows.

## REDCapDM 1.0-1

### Bug fixes

- Fixed a bug in `rd_checkbox` that mis-evaluated the branching logic of
  certain checkboxes if they had previously been transformed with
  `rd_factor`

### New features

- Added a `sep` argument to the
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  function, allowing users to specify the field separator when reading
  CSV dictionary files.

## REDCapDM 1.0-0

### Bug fixes

- Addressed multiple bugs across various functions to enhance stability
  and reliability.

- Improved error handling to provide clearer messages when issues arise.

- Fixed inconsistencies in data transformation outputs for certain edge
  cases.

### Changes & New features

- The `rd_transform` function has been split into several smaller
  subprocess functions. This allows users to:

  - Apply each transformation step independently.

  - Inspect intermediate results at each stage.

  - Have precise control over every change made to their data.

- By exposing the individual steps as separate functions, users can now
  customize the data transformation pipeline to better fit their REDCap
  datasets.

- Updated function descriptions and examples illustrate how to use the
  subprocesses effectively, making it easier to integrate REDCapDM into
  data management workflows.

- Some older functions related to data transformation have now been
  deprecated. Users are encouraged to transition to the new subprocess
  functions for more flexibility and maintainability.

  
  

## REDCapDM 0.9-9

### Bug fixes

- Added an informative message to the *redcap_data()* function when it
  is unable to replicate the factor version of a variable. This only
  applies to cases where the user is reading the data via an API
  connection. \[Issue
  [\#7](https://github.com/bruigtp/REDCapDM/issues/7) in github\]
- Added an informative message to the *redcap_data()* function when
  someone tries to use another type of file in the *data_path* argument
  instead of an R file. \[Issue
  [\#7](https://github.com/bruigtp/REDCapDM/issues/7) in github\]

### Changes

- Added a weblink in the description to redirect users to the published
  article in BMC Medical Research Methodology.

  
  

## REDCapDM 0.9-8

### Bug fixes

- Added an informative message to the *redcap_data()* function when
  someone tries to use a URI without specifying the token or vice versa.
  \[Issue [\#6](https://github.com/bruigtp/REDCapDM/issues/6) in
  github\]
- Added an informative message to the *redcap_data()* function when it
  is unable to establish a secure connection due to an SSL certificate
  problem. \[Issue [\#6](https://github.com/bruigtp/REDCapDM/issues/6)
  in github\]
- Added examples to the *redcap_data()* function. \[Issue
  [\#6](https://github.com/bruigtp/REDCapDM/issues/6) in github\]
- Modification of the argument `delete_vars` in the *redcap_data()*
  function to `delete_pattern` to accurately describe its purpose.
  \[Issue [\#6](https://github.com/bruigtp/REDCapDM/issues/6) in
  github\]
- Added a new argument to the *redcap_data()* function named
  `delete_vars` which allows users to specify variables to be removed.
  \[Issue [\#6](https://github.com/bruigtp/REDCapDM/issues/6) in
  github\]

  
  

## REDCapDM 0.9-7

### Bug fixes

- Error in *redcap_data()* function while using the API connection to
  import data. \[Solves Issue
  [\#4](https://github.com/bruigtp/REDCapDM/issues/4) in github\]

  
  

## REDCapDM 0.9-6

### Changes

- Update of the links leading to the Github page.

  
  

## REDCapDM 0.9-5

### Changes

- Huge improvement in reading and transforming data through the API
  connection. Now the final output will be the same as the one resulting
  from using the default files exported from REDCap.

  
  

## REDCapDM 0.9-0

### Changes to functions

- Added a new argument (filter_field) to the *redcap_data()* function
  that allows the user to select which fields to import into R, instead
  of having to import all fields of the project.

### Bug fixes

- Fixed an error when transforming data imported through an API
  connection (rd_transform).

  
  

## REDCapDM 0.8-5

### Bug fixes

- Fixed a major error when importing data through an API connection.

  
  

## REDCapDM 0.8-0

### Changes to functions

- The *redcap_data()* function can now read data from xlsx files.

### Minor Changes

- Small bugs fixed in some of the functions

- Added new vignettes for a simpler use of the functions

  
  

## REDCapDM 0.7-0

2023-06-02

### Changes to functions

- The *rd_transform()* function will now also transform the branching
  logic in the dictionary into R logic.

- The *rd_query()* will now use the information of the event-form to
  automatically detect the events where the variables are present.

- The *rd_query()* will now automatically apply the branching logic of
  the variable before generating queries.

### Minor Changes

- Remove descriptive variables from the dictionary in the
  *redcap_data()* function.

- Improve the resulting summary of the *rd_query()* function.

- New error added to the *rd_query()* function when trying to use a
  variable that is not in the dataset.

- New error added to the *rd_query()* function when the logic of the
  filter was using incorrected logic.

- The *rd_export()* function will now create a themed excel file.

### Bug fixes

- Minor error fixed in the summary report of the *rd_query()* function
  when there were multiple variables with zero queries.

- Error fixed in the *check_queries()* function when there were several
  ‘Miscorrected’ queries.

- We now take into account REDCap projects with repeated instruments.

  
  

## REDCapDM 0.6-0

2023-03-31

### Changes to functions

- Reduced number of dependencies of the package.

### Bug fixes

- Fixed a bug in the *rd_transform()* function in the branching logic
  evaluation of checkboxes from non longitudinal projects.

  
  

## REDCapDM 0.5-0

2023-03-24

### Changes to functions

- New function *rd_export()* to export the identified queries to an
  excel file.
- The *redcap_data()* function can read the relative path to the working
  directory in the path arguments.
- Now *rd_transform()* will keep all the labels of the variables in the
  transformed data. Therefore, the *keep_labels* argument has been
  removed.
- *rd_transform()* now will also return the *event_form* element in the
  output.
- New argument *exclude_recalc* added to the *rd_transform()* function
  to exclude specified calculated fields from being recalculated. It may
  reduce the execution time in projects with complex calculated fields.
- The *filter* argument in the *rd_query()* and *rd_event()* functions
  now works as a vector so a different filter can be applied to each
  variable.
- Minor changes to the *rd_query()* function.

### Bug fixes

- Fixed bugs in several functions resulting from testing in special
  project structures of REDCap.

  
  

## REDCapDM 0.4-0

2023-03-03

### Changes to functions

- New *link* argument of the functions *rd_query()* and *rd_event()* to
  generate the link where the query can be found in REDCap. \[Solves
  Issue [\#2](https://github.com/bruigtp/REDCapDM/issues/2) in github\]

### Bug fixes

- Bug resolved in the summary of queries, when the data did not present
  events (non-longitudinal projects).

  
  

## REDCapDM 0.3-0

2023-02-24

### Changes to functions

- The *redcap_data()* function, when used to read data through an API
  connection, will automatically read the event-form mapping without
  specifying the *event_path* argument. This will only be applied on
  longitudinal REDCap projects (more than one event).
- Adaption of the *rd_transform()* function to non-longitudinal REDCap
  projects which only contain one event.
- The *rd_transform()* function will now also delete, by default,
  variables with the pattern “\_timestamp” in addition to the
  “\_complete” pattern.
- Change in the names of the categories of the different queries when
  comparing two reports using the *check_queries()*.
- Creation of a new category of query in the *check_queries()* output
  (“Miscorrected”) that represents those queries that are different but
  belong to the same variable of the same record identifier in both the
  old and new reports.
- Adaptation of the summary of the *check_queries()* and *rd_event()*
  functions to the RStudio viewer tab.
- Customization of the title of the summary of queries returned by the
  *check_queries()* function.

### Bug fixes

- Bug resolved in the *redcap_data()* function when trying to read the
  dictionary of a specific structure of a REDCap project.

  
  

## REDCapDM 0.2-0

2023-02-17

### Changes to functions

- New *event_path* argument of the function *redcap_data()* to read the
  event-form mapping of the REDCap project.
- For all functions except *redcap_data()* we can now pass a list with
  the data, dictionary and event in the first argument of the function.
- *rd_transform()* will now take into account the different events that
  might be present in the variables of the logic for the recalculation
  of calculated fields and the evaluation of branching logics. This also
  applies for *rd_rlogic()*.
- As a consequence of this last improvement that applies in longitudinal
  REDCap projects (with more than one event) now we have to specify not
  only the data and dictionary but also the event-form mapping.
- New *checkbox_na* argument in *rd_transform()* to specify if the
  values of the checkbox will be converted to missing not only when the
  branching logic is missing but also when it’s not fullfilled (if
  *TRUE*).
- New *by_dag* argument in *rd_query()* to filter the output by each
  Data Access Group (DAG).
- Summary of the generated queries ( *\$results* ) will now include the
  event and the type of query and will be displayed in the RStudio
  viewer tab.

### Bug fixes

- The output of the *redcap_data()* function was not consistent if we
  read the data using the files exported from REDCap or the API
  connection. \[Solves Issue
  [\#1](https://github.com/bruigtp/REDCapDM/issues/1) in github\]
- We changed the use of the *subset()* function by the *filter()*
  function because of undesired behaviours of this function when
  applying a non-existent expression.

  
  

## REDCapDM 0.1-1

2023-01-30

- Help documentation has been altered.
- Improvement of the vignette.

  
  

## REDCapDM 0.1-0

2022-12-20
