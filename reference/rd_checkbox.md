# Transform Checkbox Variables in a REDCap Project

**\[experimental\]**

This function is used to process checkbox variables in a REDCap dataset.
By default, it changes their default categories ("Unchecked" and
"Checked") to new ones ("No" and "Yes). Optionally, the function can
also evaluate the branching logic for checkbox fields and adjust the
data and dictionary accordingly.

## Usage

``` r
rd_checkbox(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  checkbox_labels = c("No", "Yes"),
  checkbox_names = TRUE,
  na_logic = "none"
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

- checkbox_labels:

  Character vector of length 2 for labels of unchecked/checked values.
  Default: `c("No", "Yes")`.

- checkbox_names:

  Logical. If `TRUE` (default), checkbox columns are renamed using
  choice labels.

- na_logic:

  Controls how missing values are set based on branching logic. Must be
  one of `"none"` (do nothing), `"missing"` (set to `NA` only when the
  logic evaluation is `NA`), or `"eval"` (set to `NA` when the logic
  evaluates to `FALSE`). Defaults to `"none"`.

## Value

A list with:

- data:

  Transformed dataset with checkbox fields as factors and optionally
  renamed.

- dictionary:

  Updated dictionary with checkbox fields expanded and optionally
  renamed.

- event_form:

  The `event_form` passed in (if applicable).

- results:

  Summary of transformations and any fields needing review.

## Details

- Checkbox columns are expected in REDCap wide format (`field___code`).

- Branching logic evaluation requires `event_form` for longitudinal
  projects.

- Names are cleaned and truncated to 60 characters; uniqueness is
  enforced.

- Fields that cannot be evaluated are listed in `results`.

## Examples

``` r
# Basic usage with a project object
res <- rd_checkbox(covican)

# With custom labels
res <- rd_checkbox(data = covican$data,
                   dic = covican$dictionary,
                   checkbox_labels = c("Not present", "Present"))
#> Warning: Branching logic evaluation could not be performed because the project contains multiple events and the event-form correspondence was not specified. Please provide the `event_form` argument to enable branching logic evaluation.

# Keep original checkbox names
res <- rd_checkbox(covican, checkbox_names = FALSE)

# Longitudinal project with NA logic
res <- rd_checkbox(data = covican$data,
                   dic = covican$dictionary,
                   event_form = covican$event_form,,
                   na_logic = "eval")
```
