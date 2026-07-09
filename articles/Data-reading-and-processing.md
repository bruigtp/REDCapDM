# Data reading and processing

  

This vignette provides a summary of the straightforward and common use
of [REDCapDM](https://github.com/bruigtp/REDCapDM) to interact with
[REDCap](https://www.project-redcap.org/) data.

  

## **Read REDCap data into R**

`REDCapDM` allows you to import REDCap data either from local files or
via an API connection. Both methods return a structured list containing
your dataset, dictionary, and event-form mapping (for longitudinal
projects).

### **1. Using local files**

Ensure you have exported the following files from REDCap and placed them
in the same directory:

1.  **R data file** – contains your REDCap dataset (`.rds` or `.RData`)

2.  **Dictionary file** – CSV file containing variable metadata

3.  **Event-form mapping file** (only for longitudinal projects) – CSV
    describing which forms belong to each event

Then, provide the paths to these files using the
[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
function:

``` r

dataset <- redcap_data(
  data_path = "C:/Users/username/example.r",
  dic_path = "C:/Users/username/example_dictionary.csv",
  event_path = "C:/Users/username/events.csv" # optional for longitudinal projects
)
```

⚠️ Tip: If your project is not longitudinal, you can omit `event_path`.

### **2. Using API connection**

You can also import data directly from REDCap using your project’s **API
token** and **URL**:

``` r

dataset_api <- redcap_data(
  uri = "https://redcap.com/api/",
  token = "YOUR_API_TOKEN_HERE"
)
```

This method automatically retrieves the event-form mapping for
longitudinal projects.

⚠️ Tip: Keep your API token secure and do not share it publicly.

### **3. Understanding the output**

[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.html)
returns a list with three elements:

| Element | Description |
|----|----|
| `data` | Imported dataset containing all REDCap variables |
| `dictionary` | Metadata for each variable, including labels, types, and branching logic |
| `event_form` | Mapping between events and forms (only for longitudinal projects) |

  
  

## **Process and transform REDCap data**

After importing your dataset into R, `REDCapDM` provides several
functions to **process and transform your data** efficiently. The main
function is
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.html),
which performs multiple operations in one step.

### **1. Core transformations**

[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
performs the following actions:

| Transformation | Description |
|----|----|
| Variable deletion | Remove specific variables or patterns |
| Recalculate fields | Update REDCap calculated fields |
| Checkbox transformation | Convert checkbox variables into separate variables with option names |
| Factor conversion | Replace original variables with their factor version |
| Branching logic conversion | Translate REDCap logic into R logic for easier processing |

### **2. Basic usage**

The minimum required inputs are the dataset and dictionary. For
longitudinal projects, include the event-form mapping for full
functionality.

You can provide these inputs either as a **single list** (output of
[`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md))
or as **separate arguments**:

``` r

#Option A: Provide the full project list
covican_transformed <- rd_transform(covican)

#Option B: Provide individual arguments
covican_transformed <- rd_transform(
  data = covican$data, 
  dic = covican$dictionary, 
  event_form = covican$event_form
)
```

The output is a list containing:

- **data**: Transformed dataset

- **dictionary**: Updated variable dictionary

- **event_form**: Event-form mapping (if applicable)

- **results**: Summary of all transformations

You can view the transformation results:

``` r

#Print the results of the transformation
covican_transformed$results
```

    1. Removing selected variables

    2. Deleting variables that contain some patterns

    3. Recalculating calculated fields and saving them as '[field_name]_recalc'


    | Total calculated fields | Non-transcribed fields | Recalculated different fields |
    |:-----------------------:|:----------------------:|:-----------------------------:|
    |            2            |         0 (0%)         |            1 (50%)            |


    |     field_name      | Transcribed? | Is equal? |
    |:-------------------:|:------------:|:---------:|
    |         age         |     Yes      |   FALSE   |
    | screening_fail_crit |     Yes      |   TRUE    |

    4. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options.

    Table: Checkbox variables advisable to be reviewed

    | Variables without any branching logic |
    |:-------------------------------------:|
    |        type_underlying_disease        |

    5. Replacing original variables for their factor version

    6. Converting every branching logic in the dictionary into R logic

### **3. Splitting by event**

For longitudinal projects, you can split the transformed dataset by
event using the `final_format = "by_event"` argument:

``` r

dataset <- rd_transform(covican,
                        final_format = "by_event")
```

Where the transformed dataset is a tibble object, containing data frames
for each event in the REDCap project.

``` r

dataset$data
#> # A tibble: 2 × 2
#>   events                   df             
#>   <chr>                    <list>         
#> 1 baseline_visit_arm_1     <df [190 × 35]>
#> 2 follow_up_visit_da_arm_1 <df [152 × 9]>
```

### **4. Splitting by form**

Or, alternatively, it can be split by form using
`final_format = "by_form"` argument:

``` r

dataset <- rd_transform(covican,
                        final_format = "by_form")
```

Where the tibble object is composed by data frames corresponding to each
form in the REDCap project.

``` r

dataset$data
#> # A tibble: 7 × 3
#>   form                        events    df             
#>   <chr>                       <list>    <list>         
#> 1 inclusionexclusion_criteria <chr [1]> <df [190 × 10]>
#> 2 demographics                <chr [1]> <df [190 × 9]> 
#> 3 comorbidities               <chr [1]> <df [190 × 10]>
#> 4 cancer                      <chr [1]> <df [190 × 16]>
#> 5 vital_signs                 <chr [2]> <df [342 × 7]> 
#> 6 laboratory_findings         <chr [2]> <df [342 × 7]> 
#> 7 microbiological_studies     <chr [1]> <df [190 × 6]>
```

  
  

## **Quick troubleshooting checklist**

1.  Did you export the variable dictionary from REDCap? Without it,
    label conversions and checkbox expansions are error-prone.
2.  Are API calls failing? Verify `uri`, `token` and your project’s
    export permissions.

**For more information, consult the complete vignette available at:
<https://bruigtp.github.io/REDCapDM/articles/REDCapDM.html>**

  
  
