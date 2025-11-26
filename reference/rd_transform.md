# Transformation of the Raw Data

**\[stable\]**

Transforms the raw REDCap data read by the `redcap_data` function. The
function runs in one-step pipeline all functions dedicated to processing
the data and returns the transformed data and dictionary, along with a
summary of each step done.

## Usage

``` r
rd_transform(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  checkbox_labels = c("No", "Yes"),
  na_logic = "none",
  exclude_recalc = NULL,
  exclude_factor = NULL,
  delete_vars = NULL,
  delete_pattern = NULL,
  final_format = "raw",
  which_event = NULL,
  which_form = NULL,
  wide = NULL
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

- na_logic:

  Controls how missing values are set based on the branching logic of a
  checkbox. Must be one of `"none"` (do nothing), `"missing"` (set to
  `NA` only when the logic evaluation is `NA`), or `"eval"` (set to `NA`
  when the logic evaluates to `FALSE`). Defaults to `"none"`.

- exclude_recalc:

  Optional. Character vector of field names to exclude from
  recalculation.

- exclude_factor:

  Optional character vector of variable names (use original names
  **without** the `.factor` suffix) to exclude from conversion.

- delete_vars:

  Optional. A character vector of variable names to remove from both the
  dataset and dictionary.

- delete_pattern:

  Optional. A character vector of regular expression patterns. Variables
  matching these patterns will be removed from the dataset and
  dictionary.

- final_format:

  Character string indicating the final format of the data. Options are
  `raw`, `by_event` or `by_form`. `raw` (default) returns the
  transformed data in its original structure, `by_event` returns it as a
  nested data frame by event, and `by_form` returns it as a nested data
  frame by form.

- which_event:

  Character. If `final_format = "by_event"`, return only this event.

- which_form:

  Character. If `final_format = "by_form"`, return only this form.

- wide:

  Logical. If `TRUE` (for form-based splits), repeated instances are
  returned in wide format. Defaults to `FALSE`.

## Value

A list with elements:

- data:

  Transformed data (data.frame or nested list when split).

- dictionary:

  Updated dictionary data.frame.

- event_form:

  Event–form mapping (if applicable).

- results:

  Character summary of transformation steps performed.

## Examples

``` r
# Minimal usage (project object or data + dictionary)
trans <- rd_transform(covican)
#> ⏳ Transformation in progress...
#> Warning: All date and datetime variables are already in the correct format. No transformation applied.

# Custom checkbox labels
trans <- rd_transform(covican,
                      checkbox_labels = c("Not present", "Present"))
#> ⏳ Transformation in progress...
#> Warning: All date and datetime variables are already in the correct format. No transformation applied.

# Return only a single form (wide)
trans <- rd_transform(covican,
                      final_format = "by_form",
                      which_form = "laboratory_findings",
                      wide = TRUE)
#> ⏳ Transformation in progress...
#> Warning: All date and datetime variables are already in the correct format. No transformation applied.
```
