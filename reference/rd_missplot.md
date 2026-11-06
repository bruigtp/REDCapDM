# Plot Missing Data Overview (Barplot or Longitudinal Heatmap)

**\[experimental\]**

Generates a visual summary of missing data for variables in a REDCap
dataset or list structure. For cross-sectional (single-event) studies,
it displays a barplot showing the proportion or count of missing values
per variable. For longitudinal (multi-event) studies, it generates a
heatmap illustrating missingness across events and variables.

## Usage

``` r
rd_missplot(
  data,
  sort_by_missingness = TRUE,
  max_vars = NULL,
  show_percentages = TRUE,
  use_field_labels = TRUE,
  drop_complete = TRUE
)
```

## Arguments

- data:

  A `data frame` or a list containing a `data` element retrieved from
  REDCap.

- sort_by_missingness:

  Logical. Variables are ordered from highest to lowest missingness.
  Default `TRUE`.

- max_vars:

  Integer. If specified, only the top `max_vars` variables are plotted
  (by missingness, or by original column order if
  `sort_by_missingness = FALSE`). Default `NULL` (show all variables).

- show_percentages:

  Logical. Displays missingness as percentages. Default `TRUE`.

- use_field_labels:

  Logical. Uses field labels from the dictionary for axis labels instead
  of variable names. Default `TRUE`.

- drop_complete:

  Logical. Variables with 0 missing values are excluded from the plot.
  Default `TRUE`.

## Value

A `ggplot2` object representing either a missingness barplot or a
longitudinal missingness heatmap.

## Details

The function automatically detects whether the input dataset contains
longitudinal event information. If `redcap_event_name` is detected in
the data columns, a missingness heatmap grouped by event is produced.
