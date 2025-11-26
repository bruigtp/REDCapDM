# Handle Project Arguments

This helper function processes the `project` argument in the several
other functions. It extracts data, dictionary, event form, and results
from the project object while handling potential duplication and
providing warnings when arguments are provided redundantly.

## Usage

``` r
check_proj(project, data = NULL, dic = NULL, event_form = NULL)
```

## Arguments

- project:

  A list or object containing `data`, `dictionary`, and optionally
  `event_form` and `results`.

- data:

  A data frame (optional) that may be overridden if provided in the
  `project` object.

- dic:

  A data dictionary (optional) that may be overridden if provided in the
  `project` object.

- event_form:

  An optional event-form object that may be overridden if provided in
  the `project`.
