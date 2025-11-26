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
  filter_field = NULL,
  survey_fields = FALSE
)
```

## Arguments

- data_path:

  Path to exported R file (use with `dic_path`).

- dic_path:

  Path to the dictionary file (CSV or XLSX; use with `data_path`)..

- event_path:

  Path to the event-form mapping file (CSV or XLSX) for longitudinal
  projects (downloadable via the `Designate Instruments for My Events`
  tab within the `Project Setup` section of REDCap).

- uri:

  REDCap API base URI (use with `token`).

- token:

  REDCap API token (use with `uri`).

- filter_field:

  Optional character vector of field names to request from the API.

- survey_fields:

  Logical; include survey-related fields when pulling via API. Default
  `FALSE`.

## Value

A list with:

- `data`: Imported dataset.

- `dictionary`: Variable dictionary (project metadata).

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
