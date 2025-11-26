# Creation of a Data Frame with Variables from a Specified Form

This function generates a nested dataset containing only the variables
associated with each form, using the provided data, dictionary, and
event-form mapping. You can choose to return data for a specific form.

## Usage

``` r
split_form(data, dic, event_form = NULL, which = NULL, wide = FALSE)
```

## Arguments

- data:

  Data frame containing data from REDCap.

- dic:

  Data frame containing the dictionary read from REDCap.

- event_form:

  Data frame containing the correspondence of each event with each form.

- which:

  Character string specifying a form if only data for that form is
  desired.

- wide:

  Logical indicating if the dataset should be returned in a wide format
  (`TRUE`) or long format (`FALSE`).
