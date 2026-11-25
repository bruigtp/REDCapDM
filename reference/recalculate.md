# Recalculate REDCap Calculated Fields

This function recalculates each calculated field if the logic can be
transcribed to R. Note that calculated fields containing smart
variables, or variables of another repeating instrument or repeating
event without their instance, cannot be transcribed (the variables of
other events, e.g. `[visit_2_arm_1][weight]`, and
`[event][field][current-instance]` can).

The function returns the dataset and dictionary with the recalculated
variables appended (named as the original field plus `_recalc`), along
with a summary table of the recalculation results.

## Usage

``` r
recalculate(data, dic, event_form = NULL, exclude_recalc = NULL)
```

## Arguments

- data:

  Data frame containing data from REDCap.

- dic:

  Data frame containing the dictionary read from REDCap.

- event_form:

  Data frame containing the correspondence of each event with each form.

- exclude_recalc:

  Character vector with the names of the variables that should not be
  recalculated. Useful for projects with time-consuming recalculations
  for certain calculated fields.
