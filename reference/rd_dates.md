# Transform Dates and Datetimes in REDCap Data

**\[experimental\]**

Converts date and datetime fields in a REDCap dataset to appropriate R
classes.

## Usage

``` r
rd_dates(project = NULL, data = NULL, dic = NULL, event_form = NULL)
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

## Value

A list with the following elements:

- data:

  The transformed dataset with date and datetime fields formatted as
  `Date` and `POSIXct`.

- dictionary:

  The original REDCap dictionary passed to the function.

- event_form:

  The original event-form mapping (if applicable).

- results:

  A summary of the transformations performed.

## Details

The function performs the following tasks:

- Detects date and datetime fields from the REDCap dictionary (`date_*`
  and `datetime_*` validation types).

- Converts date fields to `Date` class.

- Converts datetime fields to `POSIXct` class, treating empty strings as
  `NA`.

## Examples

``` r
result <- rd_dates(covican)
#> Warning: All date and datetime variables are already in the correct format. No transformation applied.
transformed_data <- result$data
```
