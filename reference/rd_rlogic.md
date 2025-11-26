# Translate REDCap Logic to R Logic

**\[stable\]**

Converts a REDCap logic expression into R-compatible logic. Processes
one logic expression (`logic`) for one target variable (`var`) at a
time. Supports common REDCap operators (`and`, `or`, `=`, `<`, `>`,
etc.) and handles event-specific logic in longitudinal projects. Logic
involving smart variables or repeated instruments may require manual
review.

## Usage

``` r
rd_rlogic(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  logic,
  var
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

- logic:

  A single REDCap logic string (e.g.,
  `"if([exc_1]='1' or [inc_1]='0', 1, 0)"`).

- var:

  A single string specifying the target variable the logic applies to.

## Value

A list with:

- rlogic:

  The translated R-compatible logic as a string.

- eval:

  The evaluation of the translated logic on the provided dataset,
  filtered by event if applicable.

## Details

- Translates REDCap operators and functions into R equivalents:

  - `=` → `==`, `<>` → `!=`, `and` → `&`, `or` → `|`.

  - Converts functions like `if()`, `rounddown()`, `datediff()`,
    [`sum()`](https://rdrr.io/r/base/sum.html) to R equivalents.

- Handles date transformations and empty strings (`''`) → `NA`.

- Adjusts logic for longitudinal data using `event_form` if provided.

- Evaluates the translated R logic against the dataset and returns the
  results.

- Logic with repeated instruments, smart variables, or multiple events
  per variable may require manual inspection.

## Examples

``` r
# Translate a single REDCap logic expression for one variable
covican |>
  rd_rlogic(
    logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0', 1, 0)",
    var = "screening_fail_crit"
  )
#> $rlogic
#> [1] "ifelse(data$exc_1=='1' | data$inc_1=='0' | data$inc_2=='0' | data$inc_3=='0', 1, 0)"
#> 
#> $eval
#>   [1]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#>  [26] NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#>  [51] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0  0  0  0 NA  0  0  0  0  0  0  0
#>  [76]  0  0  0  0  0  0  0  0  0  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [101] NA  0 NA  0 NA  0 NA  0 NA  1  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  1
#> [126]  0 NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0  0  0 NA  0 NA  0 NA
#> [151]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [176]  0 NA  0  0 NA  0 NA  0 NA  0  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA
#> [201]  0 NA  0 NA  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [226]  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [251] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> [276]  0 NA  0 NA  0 NA  0 NA  0 NA  1  0 NA  1  0 NA  0 NA  0  0 NA  0  0 NA  0
#> [301] NA  0  0 NA  0  0 NA  0 NA  0  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0
#> [326] NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA  0 NA
#> 
```
