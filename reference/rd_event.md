# Identify Missing Events in REDCap Data

**\[stable\]**

Helps identify records in a REDCap longitudinal project that are missing
one or more specified events. Because REDCap typically omits empty
events from exports, an event that contains no data for a record will
not appear. This function finds those absent events (not whether the
events are complete) and returns a per-record query table and a
summarized HTML report.

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
  add_to = NA,
  report_title = NA,
  report_zeros = FALSE,
  link = list(),
  addTo = deprecated()
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

  Optional. A single character string containing a filter expression
  that selects the records to check. Example: `"age >= 18"`. It can use
  objects created in your session or in your function. Whether each
  selected record has the event is checked with all its data (also the
  rows that the filter doesn't select).

- query_name:

  Optional character vector describing each query: one message for all
  the events, or one message for each event of `event` (in its order).
  Defaults to a standard format with the unique event name:
  `The event 'event_name' is missing.`, also for the events with an `NA`
  message.

- add_to:

  Optional output of a previous
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)
  or `rd_event()` to which the new queries are appended. Its queries
  must be a data frame: the output of `rd_query(by_dag = TRUE)`, whose
  queries are a list of data frames, can't be used.

- report_title:

  Optional string specifying the title of the final report. Defaults to
  `"Report of queries"`.

- report_zeros:

  Logical, include events with zero queries in the report. Default is
  `FALSE`.

- link:

  Optional list containing project information (`domain`,
  `redcap_version`, `proj_id`) to generate a link for each query, which
  opens the home page of the record in REDCap (unlike
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  it doesn't use `event_id`).

- addTo:

  **\[deprecated\]** Use `add_to` instead.

## Value

A list of class `REDCapDM_report`, which prints a short summary (see
[`print.REDCapDM_report()`](https://bruigtp.github.io/REDCapDM/reference/print.REDCapDM_report.md)),
with two elements:

- `queries`:

  A data frame listing records missing the specified events. Columns:
  `Identifier` (the record identifier, `record_id` or else the first
  field of the dictionary, which `filter` can use by its name), `DAG`,
  `Event` (the unique event name), `EventLabel` (its label),
  `Instrument`, `Field`, `Repetition`, `Description`, `Query`, `Code`,
  and optionally `Link`. If no queries are found this will be an empty
  data frame with the expected columns.

- `results`:

  An HTML table (knitr::kable styled with kableExtra) summarising the
  number of missing records per event. Returned as
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) (HTML).

## See also

Other query functions:
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md),
[`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md),
[`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)

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
#>   Identifier DAG           Event EventLabel Instrument Field Repetition
#> 1      100-1   - follow_up_arm_1  Follow-up          -     -          -
#> 2      100-2   - follow_up_arm_1  Follow-up          -     -          -
#>   Description                                   Query    Code
#> 1   Follow-up The event 'follow_up_arm_1' is missing. 100-1-1
#> 2   Follow-up The event 'follow_up_arm_1' is missing. 100-2-1

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
