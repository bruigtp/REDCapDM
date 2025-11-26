# Transformation of Checkboxes with Branching Logic

This function inspects all the checkboxes in the study to determine if
they have a branching logic. If a branching logic is present and its
result is missing, the function will input a missing value into the
checkbox. If `checkbox_na` is `TRUE`, the function will additionally
input a missing value when the branching logic isn't satisfied, not just
when it is missing. If a branching logic cannot be found or the logic
cannot be transcribed due to the presence of smart variables, the
variable is added to a list of reviewable variables that will be
printed.

The function returns the dataset with the transformed checkboxes and a
table summarizing the results.

## Usage

``` r
transform_checkboxes(data, dic, event_form = NULL, checkbox_na = FALSE)
```

## Arguments

- data:

  Data frame containing data from REDCap.

- dic:

  Data frame containing the dictionary read from REDCap.

- event_form:

  Data frame containing the correspondence of each event with each form.

- checkbox_na:

  Logical indicating if values of checkboxes with branching logic should
  be set to missing only when the branching logic is missing (`FALSE`),
  or also when the branching logic is not satisfied (`TRUE`). The
  default is `FALSE`.
