# Identify Missing Events in REDCap Data

**\[stable\]**

Helps identify records in a REDCap longitudinal project that are missing
one or more specified events. Because REDCap typically omits empty
events from exports, an event that contains no data for a record will
not appear. This function finds those absent events and returns a
per-record query table and a summarized HTML report.

## Usage

``` r
rd_event(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  event,
  filter = NA,
  query_name = NA,
  addTo = NA,
  report_title = NA,
  report_zeros = FALSE,
  link = list()
)
```

## Arguments

- project:

  A list containing the REDCap data, dictionary, and event mapping
  (expected
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  output). Overrides `data`, `dic`, and `event_form`.

- data:

  A `data.frame` or `tibble` with the REDCap dataset.

- dic:

  A `data.frame` with the REDCap dictionary.

- event_form:

  Only applicable for longitudinal projects (presence of events).
  Event-to-form mapping for longitudinal projects.

- event:

  Character vector with one or more REDCap event names to check for
  missing records.

- filter:

  Optional. A single character string containing a filter expression to
  subset the dataset before checking for missing events. Example:
  `"age >= 18"`.

- query_name:

  Optional character vector describing each query. Defaults to a
  standard format: `The event (event_name) is missing`.

- addTo:

  Optional data frame from a previous query report to which the new
  results can be appended.

- report_title:

  Optional string specifying the title of the final report. Defaults to
  `"Report of queries"`.

- report_zeros:

  Logical, include variables with zero queries in the report. Default is
  `FALSE`.

- link:

  Optional list containing project information (`domain`,
  `redcap_version`, `proj_id`, `event_id`) to generate clickable links
  for each query.

## Value

A named list with two elements:

- `queries`:

  A data frame listing records missing the specified events. Columns:
  `Identifier`, `DAG`, `Event`, `Instrument`, `Field`, `Repetition`,
  `Description`, `Query`, `Code`, and optionally `Link`. If no queries
  are found this will be an empty data frame with the expected columns.

- `results`:

  An HTML table (knitr::kable styled with kableExtra) summarising the
  number of missing records per event. Returned as
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) (HTML).

## Examples

``` r
# Minimal reproducible example
data0 <- data.frame(
  record_id = c("100-1", "100-2", "200-1"),
  redcap_event_name = c("baseline_arm_1", "baseline_arm_1", "follow_up_arm_1"),
  redcap_event_name.factor = factor(c("Baseline", "Baseline", "Follow-up")),
  stringsAsFactors = FALSE
)

# Suppose we want to check that every record has the follow-up event
res <- rd_event(
  data = data0,
  dic = data.frame(),       # placeholder dictionary
  event = "follow_up_arm_1",
  report_zeros = TRUE
)

# Records missing the event:
res$queries
#>   Identifier DAG           Event Instrument Field Repetition Description
#> 1      100-1   - follow_up_arm_1          -     -          -   Follow-up
#> 2      100-2   - follow_up_arm_1          -     -          -   Follow-up
#>                               Query    Code
#> 1 The event 'Follow-up' is missing. 100-1-1
#> 2 The event 'Follow-up' is missing. 100-2-1

# HTML summary (in RMarkdown or Viewer)
res$results
#> <table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>Report of queries</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:center;border-bottom: 1px solid grey"> Events </th>
#>    <th style="text-align:center;border-bottom: 1px solid grey"> Description </th>
#>    <th style="text-align:center;border-bottom: 1px solid grey"> Total </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:center;"> follow_up_arm_1 </td>
#>    <td style="text-align:center;"> Follow-up </td>
#>    <td style="text-align:center;"> 2 </td>
#>   </tr>
#> </tbody>
#> </table>
```
