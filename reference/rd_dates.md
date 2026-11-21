# Transform Dates and Datetimes in REDCap Data

**\[stable\]**

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
  `NA`. The texts are read in UTC, whatever the time zone of the session
  (e.g. `TZ = "Europe/Paris"`): the time is kept as it is written in
  REDCap (e.g. `2024-01-05 10:30` is 10:30 UTC). The fields that are
  already `POSIXct` are left as they are.

- Each value is converted on its own (`YYYY-MM-DD` or `YYYY/MM/DD`, with
  or without time): a value that isn't a date or a datetime (e.g. a
  Missing Data Code that wasn't removed) is set to `NA`, and a warning
  lists the fields and the values.

## See also

Other process functions:
[`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
[`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md),
[`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md),
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md),
[`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md),
[`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md),
[`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md),
[`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md),
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)

## Examples

``` r
result <- rd_dates(covican)
#> All date and datetime variables are already in the correct format. No
#> transformation applied.
transformed_data <- result$data
```
