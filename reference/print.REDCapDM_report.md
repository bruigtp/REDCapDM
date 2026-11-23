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
#> # A tibble: 6 × 10
#>   Identifier DAG        Event EventLabel Instrument Field Repetition Description
#>   <chr>      <chr>      <chr> <chr>      <chr>      <chr> <chr>      <chr>      
#> 1 100-31     Hospital … base… Baseline … Demograph… age   -          Age        
#> 2 100-58     Hospital … base… Baseline … Comorbidi… copd  -          Chronic ob…
#> 3 100-58     Hospital … base… Baseline … Demograph… age   -          Age        
#> 4 100-65     Hospital … base… Baseline … Demograph… age   -          Age        
#> 5 100-66     Hospital … base… Baseline … Demograph… age   -          Age        
#> 6 100-72     Hospital … base… Baseline … Demograph… age   -          Age        
#> # ℹ 2 more variables: Query <chr>, Code <chr>
```
