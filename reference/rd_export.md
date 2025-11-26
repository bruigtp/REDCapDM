# Export Queries to an Excel File

**\[experimental\]**

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
  `rd_query` or `rd_event` output). Overrides `queries`.

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

## Examples

``` r
if (FALSE) { # \dontrun{
rd_export(
  queries = my_queries,
  column = "Link",
  sheet_name = "My Queries",
  path = "queries.xlsx"
)
} # }
```
