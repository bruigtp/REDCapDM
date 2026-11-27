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
  evaluates to `TRUE` will have the corresponding `vars` set to `NA`. It
  can use objects created in your session or in your function, columns
  chosen with `.data[[field]]` (with `field` one of these objects) or be
  a logical vector of your session with one value per row (e.g.
  `filter = "eligible"`).

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

- If the variable has a `.factor` version, it is updated too.

- For longitudinal projects, `event_form` must be provided for proper
  event-level filtering.

- Only one filter expression is allowed.

- The columns of the filter must be known to check their events: use
  their names (e.g. `age > 30`), `data$age`, `data[["age"]]`,
  `.data[[field]]`, `data[, "age"]` or `data[, 3]`. A filter that uses
  the data in another way (e.g. `dplyr::pull(data, "age")` or
  `rev(data)[, 2]`), finds objects by their name or evaluates code (e.g.
  `get("age")` or `eval(parse(text = "age"))`), or whose result depends
  on other columns of the data (e.g. through a function of your session
  that reads them) stops with an error.

- Variables and filter columns must exist in both `data` and
  `dictionary`: their `.factor` and `_mdc` versions and the options of a
  checkbox (e.g. `cb___1`) are checked with their field. The objects of
  your session used in the filter don't need to be in them.

## See also

Other process functions:
[`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md),
[`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md),
[`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md),
[`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md),
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md),
[`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md),
[`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md),
[`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md),
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)

## Examples

``` r
# Set 'potassium' to NA where age < 65
result <- rd_insert_na(
  covican,
  vars = "potassium",
  filter = "age < 65"
)
#> Warning: The variable `potassium` is present in more events than the filter. Only rows
#> in common events ("baseline_visit_arm_1") will be transformed.
table(is.na(result$data$potassium))
#> 
#> FALSE  TRUE 
#>   188   154 
```
