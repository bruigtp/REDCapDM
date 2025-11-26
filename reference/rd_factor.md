# Convert Variables to Factors in a REDCap Dataset

**\[experimental\]**

Converts variables in a REDCap dataset with associated `.factor` columns
into actual factor variables, while allowing the exclusion of specific
variables. Ensures consistency with the dataset and preserves variable
labels.

## Usage

``` r
rd_factor(
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

  Optional character vector of variable names (use original names
  **without** the `.factor` suffix) to exclude from conversion.

## Value

A list with the following elements:

- data:

  The transformed dataset with `.factor` columns applied as factors.

- dictionary:

  The dictionary used (unchanged).

- event_form:

  The event-form mapping used (if applicable).

- results:

  A brief text summary of the transformation.

## Details

The function looks for columns ending in `.factor` and replaces the
original variable values with those `.factor` values (converted to
factors). It preserves variable labels. The `exclude` argument must
contain base variable names (no `.factor` suffix); if any `.factor`
names are passed to `exclude` the function will throw an informative
error. The columns `redcap_event_name`, `redcap_repeat_instrument` and
`redcap_data_access_group` (and their `.factor` counterparts) are
handled specially to avoid altering event or access-group data.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- rd_factor(covican)
result <- rd_factor(covican, exclude = c("available_analytics", "urine_culture"))
transformed_data <- result$data
} # }
```
