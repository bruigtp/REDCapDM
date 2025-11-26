# Recalculate and Verify Calculated Fields in REDCap Data

**\[experimental\]**

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

- Fields of type `calc` in the dictionary are recalculated.

- Recalculated values are compared with the original values.

- If differences exist, new fields `[field_name]_recalc` are added to
  the dataset and dictionary.

- Works for single-event projects; for longitudinal projects,
  `event_form` must be provided.

- Fields with incomplete branching logic or smart variables may fail to
  recalculate.

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
