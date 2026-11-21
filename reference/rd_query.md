# Identification of Queries

**\[stable\]**

Detects and summarizes queries in a REDCap dataset based on specified
expressions, filters, or a defined branching logic. Useful for
identifying missing values, out-of-range values, or values that do not
meet predefined criteria.

## Usage

``` r
rd_query(
  project = NULL,
  variables = NA,
  expression = NA,
  negate = FALSE,
  event = NA,
  filter = NA,
  add_to = NA,
  variables_names = NA,
  query_name = NA,
  instrument = NA,
  report_title = NA,
  report_zeros = FALSE,
  by_dag = FALSE,
  link = list(),
  data = NULL,
  dic = NULL,
  event_form = NULL,
  addTo = deprecated()
)
```

## Arguments

- project:

  A list containing the REDCap data, dictionary, and event mapping
  (expected
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  output). Overrides `data`, `dic`, and `event_form`.

- variables:

  Character vector of variable names to check for queries.

- expression:

  Character vector of R conditions, written as text, to evaluate for
  each variable, where `x` stands for the variable (e.g. `"is.na(x)"` or
  `"x > 100"`). Several conditions separated by commas must all be met.
  Besides the columns of the data, they can use objects created in your
  session or in your function.

- negate:

  Logical, if `TRUE`, identifies values that **do not** meet the
  condition. Default is `FALSE`.

- event:

  REDCap event(s) to analyze in longitudinal projects. If it isn't
  given, the events where the form of each variable is collected are
  taken from `event_form`; without `event_form`, all the events are
  analyzed (with a warning), which can overestimate the number of
  queries.

- filter:

  Optional filter of the rows to check, written as text (e.g.
  `"age > 18"`), such as the branching logic of a variable translated by
  [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md).
  Give one filter, or one per variable. Several conditions separated by
  commas must all be met, and the filter can use objects created in your
  session or in your function.

- add_to:

  Optional output of a previous `rd_query()` or
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  to which the new queries are appended.

- variables_names:

  Optional character vector of descriptions for each variable. Defaults
  to the variables labels in the dictionary.

- query_name:

  Optional character vector describing each query. Defaults to a
  standard format:
  `The value is [value] and it should not be [expression]`.

- instrument:

  Optional REDCap instrument(s) for each variable. Defaults to the
  instrument reported in the dictionary.

- report_title:

  Optional string specifying the title of the final report. Defaults to
  `"Report of queries"`.

- report_zeros:

  Logical, include variables with zero queries in the report. Default is
  `FALSE`.

- by_dag:

  Logical, split results by Data Access Group (DAG). Default is `FALSE`.

- link:

  Optional list containing project information (`domain`,
  `redcap_version`, `proj_id`, `event_id`) to generate clickable links
  for each query.

- data:

  A `data.frame` or `tibble` with the REDCap dataset.

- dic:

  A `data.frame` with the REDCap dictionary.

- event_form:

  Only applicable for longitudinal projects (presence of events).
  Event-to-form mapping for longitudinal projects.

- addTo:

  **\[deprecated\]** Use `add_to` instead.

## Value

A list of class `REDCapDM_report`, which prints a short summary (see
[`print.REDCapDM_report()`](https://bruigtp.github.io/REDCapDM/reference/print.REDCapDM_report.md)),
containing:

- queries:

  A data frame or a list of data frames (if `by_dag = TRUE`) with
  detailed query information for each record.

- results:

  A formatted report (HTML table) summarizing total queries per
  variable, event, and DAG if applicable.

## Details

The function performs the following steps:

- Applies user-specified expressions to the selected variables to detect
  queries.

- Optionally negates the expressions to find values that **do not**
  satisfy the condition.

- Handles REDCap branching logic, converting it into R-compatible
  expressions for evaluation.

- Applies additional user-specified filters before identifying queries.

- Generates structured query results with metadata including:

  - Identifier (record_id)

  - DAG (if present)

  - Event and Instrument

  - Field, Repetition, Description, Query statement

  - Optional link to REDCap entry

- Optionally combines results with previous query outputs using
  `add_to`.

- Produces a summarized report, optionally including variables with zero
  queries.

- Provides warnings for variables with branching logic that could not be
  automatically evaluated.

## See also

Other query functions:
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md),
[`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md),
[`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Identify missing values for multiple variables
result <- rd_query(covican,
  variables = c("copd", "age"),
  expression = c("is.na(x)", "x %in% NA"),
  event = "baseline_visit_arm_1"
)
result$results

# Identify values exceeding a threshold
result <- rd_query(covican,
  variables = "age",
  expression = "x > 20",
  event = "baseline_visit_arm_1"
)

# Apply a filter to select subset of data
result <- rd_query(covican,
  variables = "potassium",
  expression = "is.na(x)",
  event = "baseline_visit_arm_1",
  filter = "available_analytics == '1'"
)
} # }
```
