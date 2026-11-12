# Plot Missing Data Overview (Barplot, Longitudinal Heatmap or Record Map)

**\[experimental\]**

Generates a visual summary of missing data for variables in a REDCap
dataset or list structure. With `by = "vars"` (default), cross-sectional
(single-event) studies get a barplot showing the proportion or count of
missing values per variable, and longitudinal (multi-event) studies get
a heatmap illustrating missingness across events and variables. With
`by = "record"`, the plot shows one row per record and one column per
variable, with the missing values painted.

## Usage

``` r
rd_missplot(
  data,
  sort_by_missingness = TRUE,
  max_vars = NULL,
  show_percentages = TRUE,
  use_field_labels = TRUE,
  drop_complete = TRUE,
  max_label_length = 40,
  by = c("vars", "record"),
  events = NULL,
  forms = NULL
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

  Logical. Displays missingness as percentages. Only used with
  `by = "vars"`. Default `TRUE`.

- use_field_labels:

  Logical. Uses field labels from the dictionary for axis labels instead
  of variable names. Default `TRUE`.

- drop_complete:

  Logical. Variables with 0 missing values are excluded from the plot.
  Default `TRUE`.

- max_label_length:

  Integer. Maximum number of characters of the axis labels. Use `NULL`
  to keep them complete. Default `40`.

- by:

  Character. `"vars"` (default) summarises the missing values of each
  variable; `"record"` shows them record by record.

- events:

  Character vector. Only for longitudinal projects: events to plot, by
  their unique name (e.g. `"baseline_arm_1"`) or the name shown in the
  plot (e.g. `"Baseline"`). Use `"all"` to plot every event. Default
  `NULL`: every event with `by = "vars"` and the first event with
  `by = "record"`.

- forms:

  Character vector. Only plot the variables of these forms (instrument
  names as in the `form_name` column of the dictionary). Needs the
  dictionary. Default `NULL` (every form).

## Value

A `ggplot2` object representing a missingness barplot, a longitudinal
missingness heatmap or a record-level missingness map.

## Details

The function automatically detects whether the input dataset contains
longitudinal event information. If `redcap_event_name` is detected in
the data columns, a missingness heatmap grouped by event is produced.
When the event-form mapping is available (`event_form` element of the
list), each variable is only evaluated in the events where its form is
collected; the other cells of the heatmap are left blank.

With `by = "record"`, longitudinal projects get one panel per event,
each with the variables collected in that event. By default only the
first event is plotted (with a warning); use `events` to choose others.
Records without data in an event are shown in grey in that event's
panel. Every row is labelled with its record identifier.

## Examples

``` r
if (FALSE) { # \dontrun{
# Missing values per variable and event
rd_missplot(covican)

# Missing values record by record in the baseline laboratory findings
rd_missplot(covican, by = "record", events = "baseline_visit_arm_1",
            forms = "laboratory_findings")
} # }
```
