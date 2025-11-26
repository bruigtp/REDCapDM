# Creation of a Data Frame with Variables from All Forms of a Specified Event

This function generates a nested dataset filtered by each event,
containing only the variables associated with each event. It uses the
provided data, dictionary, and event-form mapping. You can choose to
return data for a specific event.

## Usage

``` r
split_event(data, dic, event_form, which = NULL)
```

## Arguments

- data:

  Data frame containing data from REDCap.

- dic:

  Data frame containing the dictionary read from REDCap.

- event_form:

  Data frame containing the correspondence of each event with each form.

- which:

  Character string specifying an event if only data for that event is
  desired.
