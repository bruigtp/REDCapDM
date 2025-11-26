# Convert Variables to Factors

This function converts all variables in the dataset to factors, except
those specified in the `exclude` parameter.

## Usage

``` r
to_factor(data, dic, exclude = NULL)
```

## Arguments

- data:

  Data frame containing the REDCap data.

- dic:

  Data frame containing the REDCap dictionary.

- exclude:

  Character vector specifying the names of variables that should not be
  converted to factors. If `NULL`, all variables will be converted.
