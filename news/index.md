# Changelog

## REDCapDM 1.1.0

### Breaking changes

- [`library(REDCapDM)`](https://bruigtp.github.io/REDCapDM/) no longer
  attaches other packages (dplyr, purrr, stringr, labelled, …): load the
  packages you use in your own code,
  e.g. [`library(dplyr)`](https://dplyr.tidyverse.org).
- REDCapR and openxlsx are now suggested packages: they are only needed
  to read from the REDCap API or from `.xlsx` files and to export
  queries with
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md),
  which ask to install them when they are missing.
- REDCapDM now requires dplyr 1.1.0 or later, and reading from the
  REDCap API requires REDCapR 1.2.0 or later (older versions only return
  the events of the first arm).
- The reports of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  have the unique event name (e.g. `enrollment_arm_1`) in `Event` and
  its label in a new `EventLabel` column. `Event` had the label, so
  events with the same label (e.g. the same visit in several arms) were
  merged, and renaming an event in REDCap solved its queries.
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  also accepts the reports created with previous versions.
- The `addTo` argument of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  is now called `add_to`. `addTo` still works, with a deprecation
  warning.
- The `filter` of
  [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  must name the columns it uses (e.g. `age > 30`, `data$age` or
  `data[, "age"]`), so that their events can be checked: a filter whose
  columns can’t be determined (e.g. `dplyr::pull(data, "age")` or
  `get("age")`) stops with an error.

### New features

- New
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  function to visualise the missing data per variable (barplot), per
  event and variable (heatmap in longitudinal projects) or record by
  record (`by = "record"`). First version by Ferran Perez Sanchez
  ([\#19](https://github.com/bruigtp/REDCapDM/issues/19)).
- Repeated instruments and repeating events are supported:
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  and
  [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  only use each variable in the rows where it is collected, the
  `Repetition` column and the links of the queries point to the right
  instance,
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  matches the queries by event and repetition, and
  [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  evaluates the logic in each instance, as REDCap, also with
  `[current-instance]`. As a result,
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md),
  [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
  and
  [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  also work in these projects.
- New `rm_mdc` and `mdc` arguments in
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  to remove the Missing Data Codes of REDCap and of the project
  (`rm_mdc = TRUE`, the default), or to keep them (`rm_mdc = FALSE`) in
  a new `<field>_mdc` column next to each field, so that the field keeps
  its type. With the API, the codes of the project are read
  automatically.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  (and so
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md),
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  and
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md))
  translates many more REDCap functions: `isblankormissingcode()`,
  `contains()`, `not_contain()`, `starts_with()`, `ends_with()`,
  `left()`, `right()`, `mid()`,
  [`find()`](https://rdrr.io/r/utils/apropos.html), `replace_text()`,
  [`length()`](https://rdrr.io/r/base/length.html), `concat()`,
  `concat_ws()`, `upper()`, `lower()`, `trim()`,
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html), `stdev()`,
  `roundup()`, `mod()`, `exponential()`, `isnumber()`, `isinteger()`,
  `month()` and `day()`, and `datediff()` with `"now"`, dates with time
  and time fields. It also translates the same variable in different
  events (e.g. `[visit_2_arm_1][weight] - [visit_1_arm_1][weight]`).
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  returns `eval_data`, the evaluation next to the record identifier,
  event and repetition of each row.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  and
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  return a report of class `REDCapDM_report`: printed in the console, it
  shows the number of queries per variable and event (per state in
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)).
  `$queries`, `$results`, `add_to` and
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
  work as before.
- New `forms` argument in
  [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  to remove every variable of a form.
- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  reports the number of different values of each calculated field,
  [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  also updates the `.factor` version of the variables, and
  [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  returns a project without checkboxes unchanged, with a warning.
- The functions take the record identifier from the dictionary (its
  first field) when it isn’t called `record_id`, whatever its position
  in the data.
- `filter` and `expression` can use objects created inside a function or
  a loop, and they are no longer run with `eval(parse())`.

### Bug fixes

- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  keeps the text of the values that look like numbers or missing values,
  with the API and with the exported files: text fields (e.g. `"0034"`,
  `"1.50"` or `"+34600111222"`), the text `"NA"`, the codes of options
  written as `01` or `1.0`, `redcap_survey_identifier`, and record
  identifiers with leading zeros or too long to be numbers.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  with the API gives each row the labels of its event, repeated
  instrument and data access group, and each field its label, by their
  codes and names: they were joined by position, so a change of the
  project while it was read could move them to other rows or fields.
  Text fields and the record identifier keep their labels, and yes/no,
  true/false and status fields get their `.factor` version.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  with the exported files doesn’t run the
  [`library(Hmisc)`](https://hbiostat.org/R/Hmisc/) and
  [`graphics.off()`](https://rdrr.io/r/grDevices/dev.html) lines of the
  R script of REDCap, reads the projects that repeat events but not
  instruments, keeps the spaces of the labels with line breaks and
  accepts the paths between quotes copied by Windows.
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  only removes the HTML tags of the labels (the texts such as `<18` and
  `&` are kept), and gives the same dictionary from the API and from the
  exported files (empty texts in the empty cells, without spaces at the
  start or the end).
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  translates the REDCap logic as REDCap evaluates it: `!=`, `AND` and
  `OR` in capitals or across lines, negative numbers (e.g. `[a]<-3` or
  `[a] < '-3'`), texts between quotes that look like operators or
  functions (e.g. `[x] = 'a=b'`), blank values (different from any value
  with `<>`), `if()` with a condition without variables, functions
  written with spaces before their parenthesis, and fields whose names
  aren’t valid in R (e.g. `if`).
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  evaluates `datediff()` as REDCap: it failed, and now gives the
  absolute difference unless the last argument is `true`, with any date
  format, `"M"` for months and `"m"` for minutes, `"today"`, and years
  and months of 365.2425 and 30.44 days.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md):
  [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) are computed row by
  row (they gave one value for the whole data),
  [`sum()`](https://rdrr.io/r/base/sum.html) ignores blank values,
  `rounddown()` works and rounds down, and the dates chosen by `if()`,
  [`min()`](https://rdrr.io/r/base/Extremes.html) or
  [`max()`](https://rdrr.io/r/base/Extremes.html) stay dates.
- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md)
  takes the value of `[event][field]` from the record of each row, also
  when the identifier isn’t called `record_id`, without overwriting
  other fields, and works with variables already converted into factors
  with
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md).
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  applies the branching logic that uses variables of other events, of
  checkbox options (e.g. `variables = "symptoms___1"`), written in R
  (also with `[[` or comments) or without fields (e.g. `1 = 1`), and
  checks each variable only in its own events.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  keeps the texts of the expression and the values in the messages of
  the queries, and the characters `<`, `>` and `&` of the labels,
  describes `<=` and `>=` as “less than or equal to” and “greater than
  or equal to”, and keeps apart in `results` the variables whose long
  names begin in the same way.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  works with `report_zeros = TRUE` when every variable has queries, with
  several data access groups when a variable has no queries, with events
  without records, and with `by_dag = TRUE` keeps the records without a
  data access group. It checks `link$event_id` and accepts events given
  by their label.
- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  and
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  keep the record identifiers as text when they aren’t plain numbers
  (e.g. `"007"` was `7`, and alphanumeric identifiers were `NA`), write
  them without scientific notation (e.g. `1e+05-1`) and sort them
  naturally.
- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  gives each query the data access group of its record, only reports the
  records of the arm of the event, works with event labels, without
  `redcap_event_name.factor`, with `add_to` and with events without
  records, and takes in `query_name` one message for all the events or
  one per event.
- [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)
  marks as `Miscorrected` only the new queries of a field that had a
  query in the old report, keeps one pending query when only its link
  changed, tells apart queries of different fields with the same
  description and texts with carriage returns, reads the same text in
  different encodings as the same query, and works when neither report
  has queries.
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
  gives each option a valid and unique name, keeps the logic that uses
  the options with the codes 0 and 1 (also as numbers,
  e.g. `[cb(1)] + [cb(2)] >= 2`), translates the options with letters or
  negative codes, and works with tibbles, with options written as labels
  and with branching logic that can’t be evaluated (the options are left
  unchanged, with a warning). It stops when the checkboxes were already
  transformed.
- [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
  and
  [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  keep the labels with quotes valid in the logic, read the options
  written without a space after the comma, translate the codes of yes/no
  and true/false fields, and keep in REDCap syntax the logic that uses
  factors as numbers or variables of other events.
- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  leaves blank the calculations without a numeric result (e.g. a
  division by zero), compares the values as numbers (also with labels),
  updates the `_recalc` fields that it added before instead of
  duplicating them, and never overwrites a field of the project called
  `_recalc`.
- [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  keeps the record identifier (also with another name) and the fields
  called `id`, gives one row per record with `wide = TRUE`, chooses the
  rows of each repeating instrument by its code, keeps the rows of the
  forms that only repeat in some events and ignores the descriptive
  fields.
- [`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md)
  sets to `NA`, with a warning, the values that aren’t dates (they
  stopped the function or turned the next dates into `NA`), keeps the
  seconds of the datetimes, and works when the data doesn’t have every
  date field.
- [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md)
  never removes the record identifier, removes the variables of
  `pattern` from the dictionary without distinguishing upper and lower
  case, removes a checkbox from the dictionary when all its options are
  removed and accepts a variable with its `.factor` version in `vars`.
- [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
  keeps the codes of a field whose labels are of several codes, converts
  the `.factor` columns that are text, warns instead of stopping when
  `exclude` has every variable and removes the `.factor` columns without
  their variable, also of the REDCap columns.
- [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md)
  checks the events of every column of the filter, also chosen with
  `data$x`, `data[["x"]]` or `data[, "x"]`, takes the record identifier
  as present in every event, accepts filters with functions and objects
  of the session, and works in non-longitudinal projects.
- [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
  stops when the folder doesn’t exist or the sheet name isn’t valid in
  Excel, writes the links of factor columns, keeps the format of dates
  when the cells are centered and accepts the data frame of queries.
- [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
  works when the record identifier isn’t called `record_id` (also when
  its form has no event) and with `wide = FALSE`.
- All the functions work with grouped data
  ([`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  dictionaries with factor columns and fields with the name of an
  internal object (e.g. `data` or `event`). knitr and lubridate are now
  declared dependencies.

### Clearer messages

- Every function checks its arguments, and a wrong value stops with a
  message that names the argument and what it expects, instead of an
  internal R error.
- Errors, warnings and messages use cli and have classes
  (`REDCapDM_error`, with `_argument`, `_data`, `_file`, `_api` and
  `_logic`, `REDCapDM_warning` and `REDCapDM_message`). The informative
  messages can be hidden with `options(REDCapDM.quiet = TRUE)`.
- The messages name the variables, forms and events involved: e.g. the
  variables that don’t exist, the fields of the dictionary that aren’t
  in the data, how to remove missing instruments with
  `rd_delete_vars(forms = ...)`, or why a logic can’t be translated
  (with the translated logic and the R error).
- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  explains when a file is not found or can’t be read (e.g. a dictionary
  not encoded in UTF-8), and warns when variables of the data aren’t in
  the dictionary or the project has no records.

### Documentation

- The vignette is split into four: getting started
  ([`vignette("REDCapDM")`](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.md)),
  missing data
  ([`vignette("missing-data")`](https://bruigtp.github.io/REDCapDM/articles/missing-data.md)),
  data processing
  ([`vignette("data-processing")`](https://bruigtp.github.io/REDCapDM/articles/data-processing.md))
  and queries
  ([`vignette("query-management")`](https://bruigtp.github.io/REDCapDM/articles/query-management.md)).
- The reference of the website groups the functions by task, the help of
  each function links to the related ones, and the examples can be run
  as they are.
- New contributing guide, code of conduct and issue templates on GitHub.
- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
  [`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md),
  [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md),
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md),
  [`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md),
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md),
  [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md)
  and
  [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
  are now stable.
  [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
  remains experimental.

### Internal changes

- Much larger test suite, with snapshot tests of the printed reports and
  the main messages, and code cleanup following lintr.

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

- Update of the links leading to the GitHub page.

  
  

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
  filter was using incorrect logic.

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
  branching logic is missing but also when it’s not fulfilled (if
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
