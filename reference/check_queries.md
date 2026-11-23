# Check for Changes Between Two Query Reports

**\[stable\]**

Compare an older query report (`old`) with a newer one (`new`) and
classify each query into one of four statuses:

- **Pending** — the same query is present in both reports (no change
  detected),

- **Solved** — the query was present in the old report but is absent
  from the new report,

- **New** — the query appears in the new report but was not present in
  the old report,

- **Miscorrected** — a special case where a query in the new report is
  marked as `New` but the old report has a query with the same
  `Identifier`, `Event`, `Repetition` and `Field` (each of them when
  both reports have it; `Description` when they don't have `Field`),
  suggesting a re-issued or modified query for the same record and
  field.

The function returns a detailed comparison table of queries with a
`Modification` factor (one of the four statuses) and an HTML summary
table showing counts per status.

## Usage

``` r
check_queries(old, new, report_title = NULL, return_viewer = TRUE)
```

## Arguments

- old:

  Data frame containing the previous (older) query report. Must include
  `Identifier`, `Description` and `Query` columns (character or factor).

- new:

  Data frame containing the newer query report. Must include
  `Identifier`, `Description` and `Query` columns (character or factor).
  If `new` contains a `Code` column, it will be removed at the start of
  processing.

- report_title:

  Optional single string used as the caption for the HTML summary table.
  Defaults to `"Comparison report"` when not supplied or when `NA`.

- return_viewer:

  Logical; if `TRUE` (default) an HTML table (knitr/kable + kableExtra)
  summarizing the counts per state is produced and returned in the
  `results` element of the returned list. If `FALSE`, no HTML viewer is
  produced (useful for non-interactive runs).

## Value

A list of class `REDCapDM_report`, which prints a short summary (see
[`print.REDCapDM_report()`](https://bruigtp.github.io/REDCapDM/reference/print.REDCapDM_report.md)),
with two elements:

- `queries`:

  A data frame containing all queries present in either `old` or `new`.
  A factor column `Modification` indicates the state for each row
  (levels: `Pending`, `Solved`, `Miscorrected`, `New`). The function
  also reassigns `Code` values so codes are consistent per `Identifier`.

- `results`:

  If `return_viewer = TRUE`, an HTML
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) (styled
  with `kableExtra`) summarising totals per state. If
  `return_viewer = FALSE`, this is `NULL`.

## Details

Requirements:

- Both `old` and `new` must be data frames.

- Both data frames must contain at least the following character
  columns: `Identifier`, `Description`, and `Query`.

- Every query must have an `Identifier` (the record identifier of the
  data).

- A `Code` column is optional; if present it will be preserved and
  considered for sorting and output. If `Code` exists in `new`, it is
  removed at the beginning of the routine to avoid conflicts with
  re-assigned codes.

The function merges the two reports, constructs composite keys used for
comparison, classifies each row into a modification state, detects and
re-labels `Miscorrected` cases, reassigns a `Code` per `Identifier` to
keep codes consistent, and returns a detailed dataset plus an optional
HTML summary viewer.

## Notes and edge cases

- **Column types:** If `Identifier`, `Description` or `Query` are
  factors, they will be used in the comparison — it is recommended to
  convert them to character prior to calling `check_queries()` to avoid
  factor-level mismatches.

- **Sorting:** The rows are sorted by `Identifier` in natural order
  (e.g. `"100-2"` before `"100-10"`). `Identifier` is returned as a
  number only when every identifier is a plain number without leading
  zeros (e.g. `"007"` is kept as text).

- **Identity of a query:** a query is the same in both reports when it
  has the same `Identifier`, `Event` and `Repetition` (when both reports
  have them), `Field` (when both reports have it; otherwise
  `Description`) and `Query`. `Event` is the unique event name and
  `Field` the name of the field: renaming an event, an arm or a field in
  REDCap doesn't solve its queries. Their labels (`EventLabel` and
  `Description`) and the other columns (e.g. `Link` or `DAG`) are taken
  from the new report, so a query whose label or link changed is still
  one pending query.

- **Reports created before `EventLabel` existed:** they have the label
  of the event in `Event`. When a single event of the new report has
  that label, it is replaced by its unique event name; a label of
  several events (e.g. the same visit in several arms) can't be told
  apart.

- **Miscorrected detection:** A `Miscorrected` label is assigned to a
  query that would otherwise be `New` when the old report has a query
  with the same `Identifier`, `Event`, `Repetition`, `Field` (each of
  them when both reports have it) and `Description` — this signals a
  likely re-issued or modified query (`Description` is only compared
  when the reports don't have `Field`). New queries without such a query
  in the old report are `New`, also when there are several of them for
  the same field.

## See also

Other query functions:
[`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md),
[`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md),
[`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)

## Examples

``` r
# Minimal reproducible example
old <- data.frame(
  Identifier = c("100-1", "100-2", "200-1"),
  Description = c("age check", "weight check", "lab miss"),
  Query = c("is.na(age)", "is.na(weight)", "missing lab result"),
  Code = c("100-1-1", "100-2-1", "200-1-1"),
  stringsAsFactors = FALSE
)

new <- data.frame(
  Identifier = c("100-1", "200-1", "300-1"),
  Description = c("age check", "lab miss", "new query"),
  Query = c("is.na(age)", "missing lab result (clarify)", "is.na(x)"),
  stringsAsFactors = FALSE
)

res <- check_queries(old = old, new = new, report_title = "My Query Comparison")
# detailed table
head(res$queries)
#>   Identifier  Description                        Query    Code Modification
#> 1      100-1    age check                   is.na(age) 100-1-1      Pending
#> 2      100-2 weight check                is.na(weight) 100-2-1       Solved
#> 3      200-1     lab miss           missing lab result 200-1-1       Solved
#> 4      200-1     lab miss missing lab result (clarify) 200-1-2 Miscorrected
#> 5      300-1    new query                     is.na(x) 300-1-1          New
# HTML summary (if in an RMarkdown or interactive viewer)
res$results
#> <table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>My Query Comparison</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:center;border-bottom: 1px solid grey"> State </th>
#>    <th style="text-align:center;border-bottom: 1px solid grey"> Total </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:center;"> Solved </td>
#>    <td style="text-align:center;"> 2 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> Pending </td>
#>    <td style="text-align:center;"> 1 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> Miscorrected </td>
#>    <td style="text-align:center;"> 1 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:center;"> New </td>
#>    <td style="text-align:center;"> 1 </td>
#>   </tr>
#> </tbody>
#> </table>
```
