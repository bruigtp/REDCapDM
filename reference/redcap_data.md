# Read REDCap data

**\[stable\]**

Import REDCap data into R either from REDCap's exported R file or
directly via the REDCap API. The function returns a list with the
dataset, the project dictionary (metadata) and, for longitudinal
projects, the instrument–event mapping (`event_form`) when available.

## Usage

``` r
redcap_data(
  data_path = NA,
  dic_path = NA,
  event_path = NA,
  uri = NA,
  token = NA,
  sep = ",",
  filter_field = NULL,
  survey_fields = FALSE,
  rm_mdc = TRUE,
  mdc = NULL
)
```

## Arguments

- data_path:

  Path to exported R file (use with `dic_path`).

- dic_path:

  Path to the dictionary file (CSV or XLSX; use with `data_path`).

- event_path:

  Path to the event-form mapping file (CSV or XLSX) for longitudinal
  projects (downloadable via the `Designate Instruments for My Events`
  tab within the `Project Setup` section of REDCap).

- uri:

  REDCap API base URI (use with `token`).

- token:

  REDCap API token (use with `uri`).

- sep:

  Character string specifying the field separator for the exported
  dictionary CSV file. Default `","`.

- filter_field:

  Optional character vector of field names to request from the API. It
  must include the record identifier (the first field of the project,
  e.g. `record_id`).

- survey_fields:

  Logical; include survey-related fields when pulling via API. Default
  `FALSE`.

- rm_mdc:

  Logical. Remove Missing Data Codes (default `TRUE`). With `FALSE`,
  validated text and categorical fields keep their original missing
  codes in `<field>_mdc` columns. Validated text fields contain `NA` in
  those rows; categorical fields also retain an additional category in
  their `.factor` version.

- mdc:

  Optional Missing Data Codes of the project, besides the default ones
  of REDCap (`NI`, `UNK`, `NASK`, ...), which are always used: a vector
  of codes, e.g. `c("-999", "-888")`. A code can have a label, written
  as `code = label` (e.g. `c("-999" = "Not applicable", "-888")`): with
  `rm_mdc = FALSE`, the `.factor` version of the multiple-choice fields
  shows the label instead of the code. Numeric codes are also found in
  numeric fields, also when written in another way (e.g. `-999.0` for
  `-999`). With the API, the codes defined in the project (Project Setup
  \> Additional customizations \> Missing Data Codes) are added
  automatically. As with the default codes, they are searched in the
  validated text fields (numbers, dates, ...), the multiple-choice
  fields and the checkboxes, so they must not be values that the fields
  can take (e.g. the code of an option).

## Value

A list with:

- `data`: Imported dataset.

- `dictionary`: Variable dictionary (project meta data).

- `event_form`: Event-form mapping for longitudinal projects (if
  applicable).

## Details

Two import modes are supported:

- **Exported files** — use `data_path` (REDCap R export) and `dic_path`
  (dictionary CSV/XLSX).

- **API** — use `uri` and `token` to pull data and metadata directly
  from REDCap.

If the project is longitudinal, provide `event_path` (instrument–event
mapping) or the function will attempt to fetch it from the API when
using API mode.

With the API, the values and their labels are read with two requests.
The record identifiers are read as text in both of them (long
identifiers such as `9007199254740993`, or `100000000000000`, are not
changed), and the identifier is a number only when its text is that
number, as with the exported files. The labels of the events
(`redcap_event_name.factor`), repeated instruments and data access
groups are given to each row by its code (the unique event name,
instrument or data access group), and they are checked with the metadata
of the project (its events, arms, instruments and data access groups):
if the records changed between the two requests, the labels are read
again, and the function stops if they keep changing. When that metadata
can't be read, those columns have the codes instead of the labels, with
a warning (except a single code with a single label in all the rows,
e.g. the data access group of a user assigned to a group).

**Steps for using exported data in REDCap:**

1.  Use the REDCap *Export Data* function and choose *R Statistical
    Software* format.

2.  REDCap generates:

    - A CSV file with observations.

    - An R script to format variables for import.

3.  Ensure the exported files, dictionary, and event mapping (if any)
    are in the same directory.

## Note

To use other package functions effectively, include the `dic_path`
argument to load the project dictionary.

- Use either exported-files mode (`data_path` + `dic_path`) **or** API
  mode (`uri` + `token`) — not both.

- For exported files, REDCap's R export is required for `data_path`.
  Dictionary and event files must be CSV or XLSX.

## Examples

``` r
if (FALSE) { # \dontrun{
# From exported files
out <- redcap_data(
  data_path = "project_export.r",
  dic_path  = "project_dictionary.csv",
  event_path = "instrument_event_map.csv"
)

# From API
out_api <- redcap_data(
  uri   = "https://redcap.example.org/api/",
  token = "REPLACE_WITH_TOKEN"
)
} # }
```
