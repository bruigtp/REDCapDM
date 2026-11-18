# REDCapDM

  
  

## **Introduction**

The REDCapDM package provides a comprehensive toolkit for managing data
exported from REDCap. It supports both importing REDCap data (from files
or via API), transforming and cleaning the data according to the
project’s dictionary and logic, and generating query reports for data
validation. In particular, REDCapDM can identify missing or out-of-range
values and track changes in identified queries across data versions.

  
  

## **Functions**

All main functions are listed below (and described in detail in the
examples):

### Import data

- [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md):
  Read REDCap data into R.

### Explore data

- [`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md):
  Plot the missing values per variable and event, or record by record.

### Process data

- [`rd_dates()`](https://bruigtp.github.io/REDCapDM/reference/rd_dates.md):
  Standardize date and datetime fields.

- [`rd_delete_vars()`](https://bruigtp.github.io/REDCapDM/reference/rd_delete_vars.md):
  Remove specified variables (by name, pattern or form).

- [`rd_recalculate()`](https://bruigtp.github.io/REDCapDM/reference/rd_recalculate.md):
  Recompute calculated fields and compare with REDCap values.

- [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md):
  Replace numeric multiple-choice columns with their factor version.

- [`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md):
  Expand checkbox responses with custom labels and rename `var___1`
  columns (REDCap style) to `var_option`.

- [`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md):
  Split dataset by form or event.

- [`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md):
  Manually set specified variables to missing based on a logical filter.

- [`rd_rlogic()`](https://bruigtp.github.io/REDCapDM/reference/rd_rlogic.md):
  Translate REDCap branching or calculation logic into R syntax.

- [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md):
  Update dictionary (translation of REDCap logic into R syntax) to
  reflect transformed data and logic.

Or we can use all these functions at once:

- [`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md):
  One-step pipeline to clean and preprocess the raw REDCap data.

### Queries

- [`rd_query()`](https://bruigtp.github.io/REDCapDM/reference/rd_query.md):
  Apply expressions to identify data queries/issues.

- [`rd_event()`](https://bruigtp.github.io/REDCapDM/reference/rd_event.md):
  Report missing/incomplete events per record (longitudinal).

- [`check_queries()`](https://bruigtp.github.io/REDCapDM/reference/check_queries.md):
  Compare two query reports to track changes made.

- `rd_export`: Export query/report tables to an Excel (.xlsx) file.

Each group of functions is explained in detail in its own vignette:
[`vignette("missing-data")`](https://bruigtp.github.io/REDCapDM/articles/missing-data.md),
[`vignette("data-processing")`](https://bruigtp.github.io/REDCapDM/articles/data-processing.md)
and
[`vignette("query-management")`](https://bruigtp.github.io/REDCapDM/articles/query-management.md).

  
  

## **Installation**

The release version can be installed from CRAN:

``` r

install.packages("REDCapDM")
```

The development version can be installed from GitHub:

``` r

install.packages("remotes") # Run this line if the 'remotes' package isn't installed already.
remotes::install_github("bruigtp/REDCapDM")
```

## **Built-in dataset**

For the following examples, we will use a random sample of the COVICAN
study which is included in the package. COVICAN is an international,
multicentre cohort study of cancer patients with COVID-19 to describe
the epidemiology, risk factors, and clinical outcomes of co-infections
and superinfections in onco-hematological patients with COVID-19.

We can load the built-in dataset by typing:

``` r

library(REDCapDM)

data(covican)
```

The structure of this dataset is:

    List of 3
     $ data      :'data.frame': 342 obs. of  56 variables:
     $ dictionary:'data.frame': 21 obs. of  18 variables:
     $ event_form:'data.frame': 9 obs. of  3 variables:

The first element in the list is a data frame containing all the data.
The second element is a data frame with the information in the
dictionary of the project about each field. The third and final element
is a data frame containing the correspondence of each event with each
form.

Some of the variables in the dataset are:

| Name | Description | Categories |
|:---|:---|:---|
| record_id | Identifier of each record |  |
| redcap_event_name | Auto-generated name of the events |  |
| redcap_data_access_group | Auto-generated name of each center |  |
| inc_1 | Patients older than 18 years | No ; Yes |
| inc_2 | Cancer patients | No ; Yes |
| inc_3 | Diagnosed of COVID-19 | No ; Yes |
| exc_1 | Solid tumour remission \>1 year | No ; Yes |
| screening_fail_crit | Indicator of non-compliance with inclusion and exclusion criteria | Compliance ; Non-compliance |
| d_birth | Date of birth (y-m-d) |  |
| d_admission | Date of first visit (y-m-d) |  |
| age | Age |  |
| dm | Indicator of diabetes | No ; Yes |
| type_dm | Type of diabetes | No complications ; End-organ diabetes-related disease |
| copd | Indicator of chronic pulmonary disease | No ; Yes |
| fio2 | Fraction of inspired oxygen (%) |  |
| available_analytics | Indicator of blood test available | No ; Yes |
| potassium | Potassium (mmol/L) |  |
| resp_rate | Respiratory rate (bpm) |  |
| leuk_lymph | Indicator of leukemia or lymphoma | No ; Yes |
| acute_leuk | Indicator of acute leukemia | No ; Yes |

  

## **Usage**

The package structure can be divided into three main components: reading
raw data, processing data and identifying queries. Typically, after
collecting data in REDCap, we will have to follow these three components
in order to have a final validated dataset for analysis. We will provide
a complete basic user guide on how to perform each one of these steps
using the package’s functions. For the processing of the data and query
identification, we will use the `covican` built-in dataset as an
example.

### **Read data**

#### **redcap_data**

The
[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
function allows users to easily import data from a REDCap project into
R.

In order to read exported data from REDCap, we first need to download
the data and dictionary from the REDCap project in R format. We can then
use the arguments `data_path` and `dic_path` to designate the local path
where we have stored the R file and the dictionary from the REDCap
project:

``` r

dataset <- redcap_data(data_path = "C:/Users/username/example.r",
                       dic_path = "C:/Users/username/example_dictionary.csv")
```

> Note: The R and data CSV file exported from REDCap must be located in
> the same directory.

If the REDCap project is longitudinal (contains more than one event)
then a third element should be specified with the correspondence of each
event with each form of the project. This csv file can be downloaded in
the REDCap of the project following these steps: *Project Setup* \<
*Designate Instruments for My Events* \< *Download instrument-event
mappings (CSV)*. Then, it has to be specified using the argument
`event_path`:

``` r

dataset <- redcap_data(data_path = "C:/Users/username/example.r",
                       dic_path = "C:/Users/username/example_dictionary.csv",
                       event_path = "C:/Users/username/events.csv")
```

> Note: if the project is longitudinal and the event-form file is not
> provided using the `event_path` argument, some steps of the processing
> can not be performed.

Another way to read data exported from a REDCap project is using an API
connection. To do this, we can use the arguments `uri` and `token` which
respectively refer to the uniform resource identifier of the REDCap
project and the user-specific string that serves as the password:

``` r

dataset_api <- redcap_data(uri = "https://redcap.example.org/api/",
                           token = Sys.getenv("REDCAP_TOKEN"))
```

In this case, there is no need to specify the event-form file since the
function will download it automatically using the API connection, if the
project is longitudinal.

> **Warning**: Please keep in mind that the API token gives you special
> access to the REDCap project and that it should not be shared with
> other people. Instead of writing it in your scripts, save it in your
> `.Renviron` file (e.g. with `usethis::edit_r_environ()`, adding a line
> `REDCAP_TOKEN=...`) and read it with `Sys.getenv("REDCAP_TOKEN")`, as
> in the example above.

The
[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
function returns a list with three elements: imported data, dictionary
and event-form mapping(if included).

If the project uses REDCap’s Missing Data Codes (e.g. `NI`, `UNK`,
`ASKU`), by default (`rm_mdc = TRUE`) they are replaced by missing
values (`NA`) and a warning is shown. With `rm_mdc = FALSE`, they are
kept: in the multiple-choice variables as an additional category, and in
the validated text fields (numbers, dates, …) in a new `<field>_mdc`
column next to the field, which keeps its type and has `NA` in those
rows. A warning lists the fields with codes:

``` r

dataset <- redcap_data(data_path = "C:/Users/username/example.r",
                       dic_path = "C:/Users/username/example_dictionary.csv",
                       rm_mdc = FALSE)
```

If the project has its own Missing Data Codes, give them in the `mdc`
argument, optionally with their labels (with the API, the codes defined
in the project are read automatically). They are handled like the
default codes of REDCap, which are always used:

``` r

dataset <- redcap_data(data_path = "C:/Users/username/example.r",
                       dic_path = "C:/Users/username/example_dictionary.csv",
                       mdc = c("-999" = "Not applicable", "-888" = "Unknown"))
```

  
  

### **Explore data**

The
[`rd_missplot()`](https://bruigtp.github.io/REDCapDM/reference/rd_missplot.md)
function plots the missing values of the dataset, per variable and event
or record by record. See
[`vignette("missing-data")`](https://bruigtp.github.io/REDCapDM/articles/missing-data.md).

  

### **Process**

Given any data imported from REDCap with
[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md),
this would be the pipeline of an entire processing workflow:

``` r

dataset |> 
    rd_delete_vars(pattern = c("_complete", "_timestamp")) |>
    rd_dates() |>
    rd_recalculate() |> 
    rd_checkbox() |> 
    rd_factor() |> 
    rd_dictionary() |> 
    rd_split(by = "event") # use "form" if not longitudinal
```

All functions are optional and should only be used at the user’s
discretion when necessary. The order of some functions can also be
exchanged. For example, for `covican` there are no variables to delete
and dates are already processed, so the pipeline would be simplified:

``` r

covican_transformed <- covican |> 
    rd_recalculate() |> 
    rd_checkbox() |> 
    rd_factor() |> 
    rd_dictionary() |> 
    rd_split(by = "event") 

covican_transformed$results
#> 1. Recalculating calculated fields and saving them as '[field_name]_recalc'. (rd_recalculate)
#> 
#> 
#> | Total calculated fields | Non-transcribed fields | Recalculated different fields |
#> |:-----------------------:|:----------------------:|:-----------------------------:|
#> |            2            |         0 (0%)         |            1 (50%)            |
#> 
#> 
#> |     field_name      | Transcribed? | Is equal? | Different values |
#> |:-------------------:|:------------:|:---------:|:----------------:|
#> |         age         |     Yes      |   FALSE   |        1         |
#> | screening_fail_crit |     Yes      |   TRUE    |        0         |
#> 
#> 2. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. For checkboxes that have a branching logic, when the logic is missing their values will be set to missing. (rd_checkbox)
#> 
#> Table: Checkbox variables advisable to be reviewed
#> 
#> | Variables without any branching logic |
#> |:-------------------------------------:|
#> |        type_underlying_disease        |
#> 
#> 3. Replacing original variables for their factor version. (rd_factor)
#> 
#> 4. Converting every branching logic in the dictionary into R logic. (rd_dictionary)
#> 
#> 5. Final arrangment of the data by event. (rd_split)
```

Each of these functions, and
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md),
which does all the steps at once, is explained in detail in
[`vignette("data-processing")`](https://bruigtp.github.io/REDCapDM/articles/data-processing.md).

  

### **Queries**

Queries are very important to ensure the accuracy and reliability of a
REDCap dataset. The collected data may contain missing values,
inconsistencies, or other potential errors that need to be identified in
order to correct them later.

The functions to identify queries, check missing events, follow up the
corrections and export the reports are explained in detail in
[`vignette("query-management")`](https://bruigtp.github.io/REDCapDM/articles/query-management.md).
