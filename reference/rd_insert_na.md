# Insert Missing Values Using a Filter

**\[stable\]**

Sets selected variables to `NA` when a filter condition is satisfied.
Useful for managing checkboxes or other fields without explicit
gatekeeper questions.

## Usage

``` r
rd_insert_na(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  vars,
  filter
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

- vars:

  Character vector of variable names to set to `NA`.

- filter:

  A single logical expression (as string). Rows where the filter
  evaluates to `TRUE` will have the corresponding `vars` set to `NA`.

## Value

A list with:

- data:

  The dataset with `NA` inserted where the filter applies.

- dictionary:

  The unchanged dictionary.

- event_form:

  The `event_form` passed in (if applicable).

- results:

  Summary message of the changes applied.

## Details

- Each variable is only updated in rows/events where both the variable
  and filter are present.

- For longitudinal projects, `event_form` must be provided for proper
  event-level filtering.

- Only one filter expression is allowed.

- Variables and filter columns must exist in both `data` and
  `dictionary`.

## Examples

``` r
# Set 'potassium' to NA where age < 65
if (FALSE) { # \dontrun{
data <- rd_insert_na(
  data = covican$data,
  dic = covican$dictionary,
  vars = "potassium",
  filter = "age < 65"
)
table(data$potassium)
} # }
```
