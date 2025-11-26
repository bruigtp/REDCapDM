# REDCapDM - Queries

  

This vignette provides a summary of the simple and common use of
[REDCapDM](https://github.com/bruigtp/REDCapDM) to identify
discrepancies in [REDCap](https://www.project-redcap.org/) data imported
into R.

  

## **Queries**

Queries are crucial for the accuracy and reliability of a
[REDCap](https://www.project-redcap.org/) dataset. They help identify
missing values, inconsistencies, and potential errors in the collected
data. The
[`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.html)
function allows you to generate queries using a specific expression.

### **1. Find missing values for a single variable**

To identify missing values in certain variables, simply provide the
relevant information to the `variables` and `expression` arguments. In
this scenario, the expression would be ‘is.na(x)’, where ‘x’ represents
the variable itself:

``` r
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")
```

> Note: For variables with branching logic, the function will
> automatically apply the associated branching logic or at least report
> it.

  

### **2. Find out-of-range values**

Alternatively, to identify outliers or observations that meet a certain
condition (for example, range):

``` r
example <- rd_query(covican_transformed,
                    variables = c("age", "potassium"),
                    expression = c("x > 80", "x > 4.2 & x < 4.3"),
                    event = "baseline_visit_arm_1")
```

    Warning: The branching logic of the following variables were applied
    automatically: potassium

> Note: if you supply fewer `expression` entries than `variables`, the
> first expression is repeated for all variables (the function warns
> you).

  

In both cases, the function returns a list containing a data frame
designed to aid you to locate each query in the REDCap project:

``` r
example$queries
```

[TABLE]

And a summary of the generated queries per specified variable for each
applied expression:

``` r
example$results
```

| Variables | Description |     Event      |                           Query                            | Total |
|:---------:|:-----------:|:--------------:|:----------------------------------------------------------:|:-----:|
|    age    |     Age     | Baseline visit |          The value should not be greater than 80           |  22   |
| potassium |  Potassium  | Baseline visit | The value should not be greater than 4.2 and less than 4.3 |   4   |

Report of queries

  

### **3. Longitudinal projects and branching logic**

For longitudinal projects, the
[`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.html)
allows you to check if a particular event is missing from a record in
the exported data. This happens in REDCap when there is no collected
data in a particular event from a record, as REDCap will not export the
corresponding row. To identify these cases, you can use the following
code:

``` r
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1")
```

  
  

## **Practical workflow: Find → Fix → Re-check**

After identifying queries, it is common practice to correct the original
dataset in REDCap and re-run the query process for a new query dataset.

The
[`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.html)
function allows you to compare the previous query dataset with the new
one:

``` r
check <- check_queries(old = example$queries, 
                       new = new_example$queries)
```

The output, in addition to the query data frame, now includes a summary
with the number of new, miscorrected, solved and pending queries:

``` r
# Print results
check$results
```

|    State     | Total |
|:------------:|:-----:|
|   Pending    |   7   |
|    Solved    |   4   |
| Miscorrected |   1   |
|     New      |   1   |

Comparison report

> Note: The “Miscorrected” category includes queries that belong to the
> same combination of record identifier and variable in both the old and
> new reports, but with a different reason. For instance, if a variable
> had a missing value in the old report, but in the new report shows a
> value outside the established range, it would be classified as
> “Miscorrected”.

  
  

## **Exporting query lists**

With the help of the
[`rd_export()`](https://bruigtp.github.io/REDCapDM/reference/rd_export.md)
function, you can export the identified queries to a `.xlsx` file of
your choice:

``` r
rd_export(example)
```

This is the simplest way to use the function and will create a file
named “example.xlsx” in your current working directory, but you can
customise this exported file:

``` r
rd_export(queries = example$queries,
          column = "Link",
          sheet_name = "Queries - Proyecto",
          path = "C:/User/Desktop/queries.xlsx",
          password = "123") 
```

In both cases, a message will be generated in the console informing you
that the file has been created and where it is located.

  
  

**For more information, consult the complete vignette available at:
<https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html>**

  
  
