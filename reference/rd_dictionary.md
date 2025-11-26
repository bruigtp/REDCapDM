# Transform the data dictionary and handle branching logic

**\[experimental\]**

Updates a REDCap data dictionary by converting branching logic and
calculation expressions into valid R expressions. This ensures that
conditional display rules and calculated fields in the dictionary can be
programmatically evaluated with the dataset. Any variables that cannot
be converted are reported in the results.

## Usage

``` r
rd_dictionary(project = NULL, data = NULL, dic = NULL, event_form = NULL)
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

  The original dataset passed to the function.

- dictionary:

  The updated data dictionary with modified branching logic and
  calculations.

- event_form:

  The original event-form mapping (if applicable).

- results:

  A summary of the transformations, including any variables with
  unconverted branching logic.

## Details

The function performs the following tasks:

- Evaluates and transforms branching logic expressions into valid R
  expressions using `rd_rlogic`.

- Evaluates and converts calculation expressions for fields of type
  `calc`.

- Generates a results summary table listing any variables that could not
  be converted.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- rd_dictionary(covican)
print(result$results)
updated_dic <- result$dictionary
} # }
```
