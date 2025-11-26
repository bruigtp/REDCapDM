# Change Checkboxes Names to Option Names

This function updates the names of checkboxes in the dataset and
dictionary to reflect the names of their options.

## Usage

``` r
checkbox_names(data, dic, labels, checkbox_labels = c("No", "Yes"))
```

## Arguments

- data:

  Dataset containing the REDCap data.

- dic:

  Dataset containing the REDCap dictionary.

- labels:

  Named character vector with the names of the variables in the data and
  their corresponding REDCap labels.

- checkbox_labels:

  Character vector specifying the names for the two options of each
  checkbox variable. The default is `c('No', 'Yes')`.
