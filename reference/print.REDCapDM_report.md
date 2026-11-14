# Print a query report

Shows a short summary of the output of
[`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
[`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
or
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md):
the number of queries per variable and event (per state in
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md)).
The output is still a list: the queries are in `$queries` and the
formatted summary in `$results`.

## Usage

``` r
# S3 method for class 'REDCapDM_report'
print(x, ...)
```

## Arguments

- x:

  Output of
  [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md),
  [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md)
  or
  [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md).

- ...:

  Not used.

## Value

`x`, invisibly.

## Examples

``` r
report <- rd_query(covican, variables = c("copd", "age"), expression = c("is.na(x)", "x > 70"),
                   event = "baseline_visit_arm_1")
report
#> REDCapDM query report: 82 queries in 81 records
#> 
#>  Variable Event          Queries
#>  age      Baseline visit 76
#>  copd     Baseline visit  6
#> 
#> Each query is in `$queries` and the summary table in `$results`.
#> `rd_export()` saves the queries in an Excel file.
head(report$queries)
#> # A tibble: 6 × 9
#>   Identifier DAG       Event Instrument Field Repetition Description Query Code 
#>   <chr>      <chr>     <chr> <chr>      <chr> <chr>      <chr>       <chr> <chr>
#> 1 100-31     Hospital… Base… Demograph… age   -          Age         The … 100-…
#> 2 100-58     Hospital… Base… Comorbidi… copd  -          Chronic ob… The … 100-…
#> 3 100-58     Hospital… Base… Demograph… age   -          Age         The … 100-…
#> 4 100-65     Hospital… Base… Demograph… age   -          Age         The … 100-…
#> 5 100-66     Hospital… Base… Demograph… age   -          Age         The … 100-…
#> 6 100-72     Hospital… Base… Demograph… age   -          Age         The … 100-…
```
