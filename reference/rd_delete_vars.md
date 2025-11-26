# Delete Variables from REDCap Dataset and Dictionary

**\[experimental\]**

Deletes selected variables from a REDCap dataset and its dictionary,
keeping them consistent and preserving variable labels.

## Usage

``` r
rd_delete_vars(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  vars = NULL,
  pattern = NULL
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

  Optional. A character vector of variable names to remove from both the
  dataset and dictionary.

- pattern:

  Optional. A character vector of regular expression patterns. Variables
  matching these patterns will be removed from the dataset and
  dictionary.

## Value

A list containing:

- data:

  The updated dataset with specified variables removed.

- dictionary:

  The updated REDCap dictionary.

- event_form:

  The original event-form mapping (if applicable).

- results:

  A summary message describing the variable removal operation.

## Details

- Ensure that at least one of `vars` or `pattern` is specified.

- Removes specified variables and their factor versions (e.g.,
  `variable.factor`) from the dataset.

- Removes matching variables from the dictionary.

- Warns about factor versions of variables matching patterns,
  recommending use of
  [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
  if necessary.

## Examples

``` r
# Delete specific variables by name
result <- rd_delete_vars(
  project = covican,
  vars = c("potassium", "leuk_lymph")
)

# Delete variables matching patterns
result <- rd_delete_vars(
  data = covican$data,
  dic = covican$dictionary,
  pattern = c("_complete$", "_other$")
)
```
