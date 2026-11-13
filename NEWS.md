# REDCapDM (development version)

## New features

- New `rd_missplot()` function to visualise missing data per variable (barplot) or per event and variable (heatmap in longitudinal projects). With `by = "record"` it shows the missing values record by record. The `events` and `forms` arguments choose what is plotted, long labels are shortened (`max_label_length`) and the events where a variable is not collected are left blank.
- New `rm_mdc` argument in `redcap_data()` to remove REDCap Missing Data Codes (default `TRUE`).
- With `redcap_data(rm_mdc = FALSE)`, the Missing Data Codes of validated text fields (numbers, dates, ...) are kept in a new `<field>_mdc` column, so that the field keeps its type. `rd_missplot()`, `rd_delete_vars()` and `rd_split()` handle these columns like the `.factor` ones.
- New `forms` argument in `rd_delete_vars()` to remove every variable of a form.
- `rd_recalculate()` reports the number of different values of each calculated field.
- `rd_rlogic()` returns `eval_data`, the evaluation next to the record identifier, event and repetition of each row.
- `rd_insert_na()` also updates the `.factor` version of the variables.
- `rd_factor()` warns when the dictionary logic was already translated to R with `rd_dictionary()`.
- `rd_rlogic()` translates logic that uses the same variable in different events (e.g. `[visit_2_arm_1][weight] - [visit_1_arm_1][weight]`).
- `rd_checkbox()` returns a project without checkbox fields unchanged, with a warning, instead of an error.

## Repeated instruments and repeating events

- `rd_query()` now only checks each variable in the rows where it is collected. In projects with repeated instruments, empty fields of other instruments are no longer reported as queries.
- The `Repetition` column of `rd_query()` is now correct for every row, and query links open the right instance.
- `check_queries()` uses the event and the repetition to match queries, so the same variable in different events or instances is tracked separately.
- `rd_missplot()` computes missingness only over the rows where each variable is collected.
- Repeating events are now detected internally.
- `rd_rlogic()` translates and evaluates logic in projects with repeated instruments and repeating events: the logic of a repeating instrument is evaluated in its instances and, as in REDCap, the variables of the other forms take the value of the base row of the same record and event. As a result, `rd_recalculate()` now works with repeated instruments, and `rd_query()` and `rd_dictionary()` apply and translate their branching logic.
- `rd_checkbox()` evaluates the branching logic of checkboxes and `rd_transform()` no longer skips the recalculation, the checkbox branching logic and the dictionary translation in projects with repeated instruments.
- `rd_missplot(by = "record")` shows each repetition as a row and tells apart the cells of variables not collected in that row.

## Bug fixes

- `rd_rlogic()`, `rd_recalculate()` and `rd_query()` gave wrong results when the variables had already been converted to factors with `rd_factor()`.
- `rd_recalculate()` applied twice duplicated the `_recalc` fields.
- `rd_checkbox()` and `rd_recalculate()` gave wrong results with tibbles, and `rd_checkbox()` couldn't name the options of data without variable labels.
- `rd_insert_na()` failed in non-longitudinal projects.
- `rd_query(report_zeros = TRUE)` failed when every variable had queries.
- `rd_query()`, `rd_event()` and `check_queries()` turned alphanumeric record identifiers into `NA`; identifiers are now sorted naturally.
- `rd_event()` assigned the DAG of the first record to every query, failed without `redcap_event_name.factor` and could pair events with the wrong description. It now explains that it needs a longitudinal project.
- `rd_transform()` failed when the record identifier was not called `record_id`.
- `rd_recalculate()` reported a calculated field as different from REDCap when the values were equal but the variable had a label.
- `rd_split(wide = TRUE)` returned several rows per record for repeated instruments in non-longitudinal projects.
- `rd_query()` with several variables checked each variable also in the events of the other ones.
- `rd_event()` reported records of other arms as missing the event in projects with several arms.
- `rd_split()` failed with descriptive fields in the dictionary.
- `redcap_data()` no longer warns about an incomplete final line when reading the event file.
- Fixed a stray "FALSE" at the end of a `rd_query()` warning.
- `rd_rlogic()` translated `datediff()` differently from REDCap: it now returns the absolute difference unless the last argument is `true`, accepts any date format (`"ymd"`, `"mdy"`...), uses `"M"` for months and `"m"` for minutes, and understands `"today"`.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()` and `rd_dictionary()`) translated several REDCap functions wrongly: `min()` and `max()` returned a single value for the whole dataset, `sum()` didn't ignore blank values, `rounddown(x, d)` rounded instead of rounding down and several `rounddown()` in the same logic were mistranslated. `mean()`, `median()`, `roundup()`, `mod()`, `exponential()`, `isnumber()`, `contains()` and `length()` are now translated too.
- `rd_query()` ignored the branching logic that uses a variable of another event (e.g. `[baseline_arm_1][sex] = '2'`) when the variable was checked alone, and checked every row when no row met the branching logic.
- `rd_delete_vars()` could remove `record_id`, also through `pattern`.
- `rd_export()` reported success when the folder of the file didn't exist.
- `rd_event()` failed with event labels.
- `rd_dictionary()` failed after `rd_factor()` when the only factors were yes/no or true/false fields.
- `redcap_data(rm_mdc = FALSE)`, `rd_split()` and `rd_missplot()` failed or warned in projects without records.

## Clearer messages

- `redcap_data()` explains when a file is not found, when the csv file read by the R script is not in its folder and when the event file is not the instrument-event mapping of the project. It warns when variables of the data are not in the dictionary and when the project has no records yet.
- `rd_query()` names the variables that don't exist (and how to check a checkbox), explains why an expression can't be evaluated, warns when a text variable is compared with a number and checks `negate`, `report_zeros`, `by_dag` and `addTo`.
- `rd_rlogic()` names the variables it can't find and shows the translated logic and the R error when it can't be evaluated.
- `rd_delete_vars()` warns when `pattern` matches no variable, `rd_insert_na()` when no row meets the filter, and `rd_factor()` and `rd_recalculate()` when `exclude` has unknown variables.
- `rd_event()` asks for the `event`, `rd_split()` explains why it can't split by event, `rd_transform()` asks for `delete_pattern` before splitting the data, `check_queries()` checks that it receives query reports, `rd_missplot()` checks `max_vars` and the number of rows, and `rd_checkbox()` checks `checkbox_labels`.
- Every function checks its arguments: a wrong value (e.g. `NULL`, `NA`, a vector instead of a single value or a text instead of TRUE/FALSE) stops with a message that names the argument and what it expects, instead of an internal R error.
- `rd_delete_vars()` never removes the REDCap columns of events and repetitions through `pattern`, and `rd_dates()` and `rd_rlogic()` work with a dataset without rows.
- Every function checks that the dictionary has the columns it needs, and `rd_split()` names the variables of the data that are not in the dictionary.

# REDCapDM 1.0-1

## Bug fixes

- Fixed a bug in `rd_checkbox` that mis-evaluated the branching logic of certain checkboxes if they had previously been transformed with `rd_factor`

## New features

-  Added a `sep` argument to the `redcap_data()` function, allowing users to specify the field separator when reading CSV dictionary files.

# REDCapDM 1.0-0

## Bug fixes

- Addressed multiple bugs across various functions to enhance stability and reliability.

- Improved error handling to provide clearer messages when issues arise.

- Fixed inconsistencies in data transformation outputs for certain edge cases.

## Changes & New features

- The `rd_transform` function has been split into several smaller subprocess functions. This allows users to:

    - Apply each transformation step independently.

    - Inspect intermediate results at each stage.

    - Have precise control over every change made to their data.

- By exposing the individual steps as separate functions, users can now customize the data transformation pipeline to better fit their REDCap datasets.

- Updated function descriptions and examples illustrate how to use the subprocesses effectively, making it easier to integrate REDCapDM into data management workflows.

- Some older functions related to data transformation have now been deprecated. Users are encouraged to transition to the new subprocess functions for more flexibility and maintainability.

<br>
<br>

# REDCapDM 0.9-9

## Bug fixes

-   Added an informative message to the *redcap_data()* function when it is unable to replicate the factor version of a variable. This only applies to cases where the user is reading the data via an API connection. [Issue #7 in github]
-   Added an informative message to the *redcap_data()* function when someone tries to use another type of file in the *data_path* argument instead of an R file. [Issue #7 in github]

## Changes

-   Added a weblink in the description to redirect users to the published article in BMC Medical Research Methodology.

<br>
<br>

# REDCapDM 0.9-8

## Bug fixes

-   Added an informative message to the *redcap_data()* function when someone tries to use a URI without specifying the token or vice versa. [Issue #6 in github]
-   Added an informative message to the *redcap_data()* function when it is unable to establish a secure connection due to an SSL certificate problem. [Issue #6 in github]
-   Added examples to the *redcap_data()* function. [Issue #6 in github]
-   Modification of the argument `delete_vars` in the *redcap_data()* function to `delete_pattern` to accurately describe its purpose. [Issue #6 in github]
-   Added a new argument to the *redcap_data()* function named `delete_vars` which allows users to specify variables to be removed. [Issue #6 in github]

<br>
<br>

# REDCapDM 0.9-7

## Bug fixes

-   Error in *redcap_data()* function while using the API connection to import data. [Solves Issue #4 in github]

<br>
<br>

# REDCapDM 0.9-6

## Changes

-   Update of the links leading to the Github page.

<br>
<br>

# REDCapDM 0.9-5

## Changes

-   Huge improvement in reading and transforming data through the API connection. Now the final output will be the same as the one resulting from using the default files exported from REDCap.

<br>
<br>


# REDCapDM 0.9-0

## Changes to functions

-   Added a new argument (filter_field) to the *redcap_data()* function that allows the user to select which fields to import into R, instead of having to import all fields of the project.

## Bug fixes

-   Fixed an error when transforming data imported through an API connection (rd_transform).

<br>
<br>


# REDCapDM 0.8-5

## Bug fixes

-   Fixed a major error when importing data through an API connection.

<br>
<br>


# REDCapDM 0.8-0

## Changes to functions

-   The *redcap_data()* function can now read data from xlsx files.

## Minor Changes

-   Small bugs fixed in some of the functions

-   Added new vignettes for a simpler use of the functions

<br>
<br>


# REDCapDM 0.7-0

2023-06-02

## Changes to functions

-   The *rd_transform()* function will now also transform the branching logic in the dictionary into R logic.

-   The *rd_query()* will now use the information of the event-form to automatically detect the events where the variables are present.

-   The *rd_query()* will now automatically apply the branching logic of the variable before generating queries.

## Minor Changes

-   Remove descriptive variables from the dictionary in the *redcap_data()* function.

-   Improve the resulting summary of the *rd_query()* function.

-   New error added to the *rd_query()* function when trying to use a variable that is not in the dataset.

-   New error added to the *rd_query()* function when the logic of the filter was using incorrected logic.

-   The *rd_export()* function will now create a themed excel file.

## Bug fixes

-   Minor error fixed in the summary report of the *rd_query()* function when there were multiple variables with zero queries.

-   Error fixed in the *check_queries()* function when there were several 'Miscorrected' queries.

-   We now take into account REDCap projects with repeated instruments.

<br>
<br>


# REDCapDM 0.6-0

2023-03-31

## Changes to functions

-   Reduced number of dependencies of the package.

## Bug fixes

-   Fixed a bug in the *rd_transform()* function in the branching logic evaluation of checkboxes from non longitudinal projects.

<br>
<br>


# REDCapDM 0.5-0

2023-03-24

## Changes to functions

-   New function *rd_export()* to export the identified queries to an excel file.
-   The *redcap_data()* function can read the relative path to the working directory in the path arguments.
-   Now *rd_transform()* will keep all the labels of the variables in the transformed data. Therefore, the *keep_labels* argument has been removed.
-   *rd_transform()* now will also return the *event_form* element in the output.
-   New argument *exclude_recalc* added to the *rd_transform()* function to exclude specified calculated fields from being recalculated. It may reduce the execution time in projects with complex calculated fields.
-   The *filter* argument in the *rd_query()* and *rd_event()* functions now works as a vector so a different filter can be applied to each variable.
-   Minor changes to the *rd_query()* function.

## Bug fixes

-   Fixed bugs in several functions resulting from testing in special project structures of REDCap.

<br>
<br>


# REDCapDM 0.4-0

2023-03-03

## Changes to functions

-   New *link* argument of the functions *rd_query()* and *rd_event()* to generate the link where the query can be found in REDCap. [Solves Issue #2 in github]

## Bug fixes

-   Bug resolved in the summary of queries, when the data did not present events (non-longitudinal projects).

<br>
<br>


# REDCapDM 0.3-0

2023-02-24

## Changes to functions

-   The *redcap_data()* function, when used to read data through an API connection, will automatically read the event-form mapping without specifying the *event_path* argument. This will only be applied on longitudinal REDCap projects (more than one event).
-   Adaption of the *rd_transform()* function to non-longitudinal REDCap projects which only contain one event.
-   The *rd_transform()* function will now also delete, by default, variables with the pattern "\_timestamp" in addition to the "\_complete" pattern.
-   Change in the names of the categories of the different queries when comparing two reports using the *check_queries()*.
-   Creation of a new category of query in the *check_queries()* output ("Miscorrected") that represents those queries that are different but belong to the same variable of the same record identifier in both the old and new reports.
-   Adaptation of the summary of the *check_queries()* and *rd_event()* functions to the RStudio viewer tab.
-   Customization of the title of the summary of queries returned by the *check_queries()* function.

## Bug fixes

-   Bug resolved in the *redcap_data()* function when trying to read the dictionary of a specific structure of a REDCap project.

<br>
<br>

# REDCapDM 0.2-0

2023-02-17

## Changes to functions

-   New *event_path* argument of the function *redcap_data()* to read the event-form mapping of the REDCap project.
-   For all functions except *redcap_data()* we can now pass a list with the data, dictionary and event in the first argument of the function.
-   *rd_transform()* will now take into account the different events that might be present in the variables of the logic for the recalculation of calculated fields and the evaluation of branching logics. This also applies for *rd_rlogic()*.
-   As a consequence of this last improvement that applies in longitudinal REDCap projects (with more than one event) now we have to specify not only the data and dictionary but also the event-form mapping.
-   New *checkbox_na* argument in *rd_transform()* to specify if the values of the checkbox will be converted to missing not only when the branching logic is missing but also when it's not fullfilled (if *TRUE*).
-   New *by_dag* argument in *rd_query()* to filter the output by each Data Access Group (DAG).
-   Summary of the generated queries ( *\$results* ) will now include the event and the type of query and will be displayed in the RStudio viewer tab.

## Bug fixes

-   The output of the *redcap_data()* function was not consistent if we read the data using the files exported from REDCap or the API connection. [Solves Issue #1 in github]
-   We changed the use of the *subset()* function by the *filter()* function because of undesired behaviours of this function when applying a non-existent expression.

<br>
<br>

# REDCapDM 0.1-1

2023-01-30

-   Help documentation has been altered.
-   Improvement of the vignette.

<br>
<br>

# REDCapDM 0.1-0

2022-12-20
