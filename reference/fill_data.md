# Fill Rows with Values from One Event

This function fills all rows in the dataset with the value of a
particular variable in a specified event. It is an auxiliary function
used in the `rd_rlogic` function.

## Usage

``` r
fill_data(which_event, which_var, data)
```

## Arguments

- which_event:

  String specifying the name of the event.

- which_var:

  String specifying the name of the variable.

- data:

  Dataset containing the REDCap data.
