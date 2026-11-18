# Export Queries to an Excel File

**\[stable\]**

Export a query dataset (e.g., from `rd_query` or `rd_event`) to an
`.xlsx` file. The function can optionally convert a column of URLs into
Excel hyperlinks and apply password protection to the worksheet.

## Usage

``` r
rd_export(
  project = NULL,
  queries = NULL,
  column = NULL,
  sheet_name = NULL,
  path = NULL,
  password = NULL
)
```

## Arguments

- project:

  A list containing the dataframe of queries and results (expected
  `rd_query` or `rd_event` output). Overrides `queries`. A data frame of
  queries is also accepted (e.g. `rd_export(report$queries)`).

- queries:

  A data frame of identified queries.

- column:

  Name of the column containing URLs to convert into hyperlinks. If
  `NULL`, hyperlinks are added only if a `Link` column exists.

- sheet_name:

  Name of the Excel sheet in the resulting `.xlsx` file. Default:
  `"Sheet1"`.

- path:

  File path for saving the `.xlsx` file. If `NULL`, the file is saved as
  `"example.xlsx"` in the working directory.

- password:

  Optional password to protect the worksheet from edits.

## Value

An `.xlsx` file written to the specified path.

## See also

Other query functions:
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md),
[`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md),
[`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md)

## Examples

``` r
if (requireNamespace("openxlsx", quietly = TRUE)) {
  result <- rd_query(covican, variables = "age", expression = "x > 80")
  rd_export(
    result,
    sheet_name = "My Queries",
    path = file.path(tempdir(), "queries.xlsx")
  )
}
#> ✔ The file has been successfully created in /tmp/Rtmpnvc0X6/queries.xlsx.
```
