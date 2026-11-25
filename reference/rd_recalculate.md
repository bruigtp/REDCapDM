# Recalculate and Verify Calculated Fields in REDCap Data

**\[stable\]**

Recalculates fields marked as calculated in the REDCap dictionary,
compares them with the original values, and reports discrepancies. If
any differences are found, new recalculated fields are added to the
dataset and dictionary with `_recalc` appended to the names.

## Usage

``` r
rd_recalculate(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  exclude = NULL
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

- exclude:

  Optional. Character vector of field names to exclude from
  recalculation.

## Value

A list with:

- data:

  The dataset with new `_recalc` fields for any differing calculated
  fields.

- dictionary:

  Updated dictionary including the new `_recalc` fields.

- event_form:

  The `event_form` passed in (if applicable).

- results:

  Summary report of the recalculation process, including tables of
  discrepancies.

## Details

- Fields of type `calc` in the dictionary are recalculated. As in
  REDCap, a calculation without a numeric result (e.g. a division by
  zero) is blank (`NA`).

- Recalculated values are compared with the original values, both
  rounded to three decimals (e.g. 1.0004 and 1 are equal).

- If differences exist, new fields `[field_name]_recalc` are added to
  the dataset and dictionary, with the label of the original field
  followed by `(Recalculated)`. Running the function again on its output
  updates these fields instead of duplicating them.

- The fields added by this function are recognised because they are
  calculated fields with the same calculation as the original field and,
  when the dictionary has the column `field_label`, its label followed
  by `(Recalculated)` (or by `(Recalculate)`, the label of previous
  versions of the package, which are also recognised). No other field or
  column is overwritten: if `[field_name]_recalc` is already one of
  them, the recalculated values are saved in `[field_name]_recalc_1` (or
  `_2`, ...) with a warning.

- Works for single-event projects; for longitudinal projects,
  `event_form` must be provided.

- The calculation of each field is translated to R with
  [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md):
  calculations that can't be translated (e.g. with smart variables or
  functions that it doesn't know) are not recalculated, and
  `Transcribed?` is `No`.

- The original and recalculated values are compared as numbers when both
  are numbers, also written as text (e.g. `'100000'`, the result of
  `if()` with texts) or as `TRUE`/`FALSE` (the 1/0 of a condition),
  rounded to 3 decimals: the comparison doesn't depend on how R writes
  the numbers.

- A calculated field of the dictionary that isn't in the data can't be
  compared: a warning is shown, and its `Is equal?` and
  `Different values` are `NA`.

- Existing generated columns are refreshed even when the original values
  now agree with the calculation. If a formula cannot be translated or
  evaluated, its previously generated column is set to `NA` with a
  warning. Excluded fields and their generated columns are left
  unchanged.

- Unsupported formulas are reported as non-transcribed in `results`;
  they do not stop the recalculation of other fields. Restrictions on
  repeated-source references are those of
  [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md).

## See also

Other process functions:
[`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
[`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md),
[`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md),
[`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md),
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md),
[`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md),
[`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md),
[`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md),
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)

## Examples

``` r
# Recalculate all calculated fields
if (FALSE) { # \dontrun{
results <- rd_recalculate(
  data = covican$data,
  dic = covican$dictionary,
  event_form = covican$event_form
)
} # }

# Recalculate but exclude some variables
if (FALSE) { # \dontrun{
results <- rd_recalculate(
  project = covican,
  exclude = c("age", "screening_fail_crit")
)
} # }
```
