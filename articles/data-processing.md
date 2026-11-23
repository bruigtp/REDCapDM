# Process REDCap data

The examples use `covican`, the dataset included in the package
(described in
[`vignette("REDCapDM")`](https://bruigtp.github.io/REDCapDM/articles/REDCapDM.md)).

For a project assembled from separate data frames, keep the record
identifier as the first field of the dictionary, even if its column is
moved in the dataset. The processing functions use that field when the
identifier is not named `record_id`.

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
exchanged. All the functions that can be used in each step of a
processing workflow are detailed below.

## **rd_delete_vars**

This function removes unwanted variables from both a REDCap dataset and
its dictionary. This is especially useful for eliminating automatically
generated fields such as form completion flags (`*_complete`) or
timestamps (`*_timestamp`).

You can delete variables by specifying their exact names, by using
regular expression patterns or by choosing whole forms:

``` r

# Option A: delete by variable name
covican_deleted <- covican |> 
  rd_delete_vars(vars = c("potassium", "leuk_lymph"))

# Option B: delete by regex pattern (here, the inclusion and exclusion criteria)
covican_deleted <- covican |> 
  rd_delete_vars(pattern = c("^inc_", "^exc_"))

# Option C: delete every variable of a form
covican_deleted <- covican |> 
  rd_delete_vars(forms = "laboratory_findings")
```

When variables are deleted:

- They are removed from both the dataset and dictionary.

- Factor versions of deleted variables (if present) are also removed.

- If a pattern doesn’t match any variable, a warning tells you that
  nothing was removed. The columns that REDCap uses for the record
  identifier, the events and the repetitions are never removed by a
  pattern.

- With `forms`, the checkbox options, recalculated versions (`_recalc`)
  and the `_complete`/`_timestamp` fields of the form are removed too,
  and the form is removed from the event-form mapping. The record
  identifier is always kept.

  

## **rd_dates**

This function is designed to process and standardize `date` and
`datetime` fields in a REDCap dataset. In REDCap projects, date and
datetime fields can sometimes be stored as character strings, which can
make analyses difficult. This function detects which fields should be
dates/datetimes from the REDCap dictionary and converts them to `Date`
and `POSIXct`, respectively. Datetimes written as text are parsed in
UTC, preserving the clock time written in REDCap; existing `POSIXct`
columns keep their time zone.

``` r

covican_dates <- covican |> 
  rd_dates()
```

Quick verification example:

``` r

# Simulate a character date since covican already has the dates in the correct format
covican_dates <- covican
covican_dates$data <- covican_dates$data |> 
  dplyr::mutate(d_birth = as.character(d_birth))
# Check class before conversion

class(covican_dates$data$d_birth)
#> [1] "character"

# Check class after conversion
covican_dates <- covican_dates |> 
  rd_dates()
class(covican_dates$data$d_birth)
#> [1] "Date"
```

After this transformation, all `date` and `datetime` variables are
standardized and ready for analysis in R.

  

## **rd_recalculate**

This function identifies calculated fields in a REDCap project,
translates their calculation logic into R, recalculates the values, and
compares the results with the values stored in REDCap.

It then produces a structured report that helps users detect
discrepancies between REDCap’s stored calculations and the values
recalculated in R.

``` r

covican_recalc <- covican |> 
  rd_recalculate()

# Print recalculation results
covican_recalc$results
```

    ## Recalculating calculated fields and saving them as '[field_name]_recalc'. (rd_recalculate)
    ## 
    ## 
    ## | Total calculated fields | Non-transcribed fields | Fields with different recalculated values |
    ## |:-----------------------:|:----------------------:|:-----------------------------------------:|
    ## |            2            |         0 (0%)         |                  1 (50%)                  |
    ## 
    ## 
    ## |     field_name      | Transcribed? | Is equal? | Different values |
    ## |:-------------------:|:------------:|:---------:|:----------------:|
    ## |         age         |     Yes      |   FALSE   |        1         |
    ## | screening_fail_crit |     Yes      |   TRUE    |        0         |

The `results` object includes:

- A summary report outlining the total number of calculated fields, how
  many were successfully transcribed into R, and how many showed
  differences between the REDCap values and the recalculated ones.

- A field-level report listing each calculated field, whether the logic
  was successfully converted to R, whether the recalculated values match
  the originals and how many values are different.

You can also exclude specific fields from recalculation (e.g., complex
multi-event calculations) to reduce computation time and avoid
unnecessary warnings.

``` r

# Exclude specific variables from recalculation
covican_recalc <- covican |> 
  rd_recalculate(exclude = "screening_fail_crit")

covican_recalc$results
```

    ## Recalculating calculated fields and saving them as '[field_name]_recalc'. (rd_recalculate)
    ## 
    ## 
    ## | Total calculated fields | Non-transcribed fields | Fields with different recalculated values |
    ## |:-----------------------:|:----------------------:|:-----------------------------------------:|
    ## |            1            |         0 (0%)         |                 1 (100%)                  |
    ## 
    ## 
    ## | field_name | Transcribed? | Is equal? | Different values |
    ## |:----------:|:------------:|:---------:|:----------------:|
    ## |    age     |     Yes      |   FALSE   |        1         |

After running this function:

- For each calculated field whose recalculated values are different from
  the values of REDCap, a new variable with the suffix `_recalc` is
  added to the dataset, placed immediately after the original variable
  and containing the recalculated values.

- The data dictionary is updated with a corresponding entry, where the
  original variable label is extended with `"(Recalculated)"` to make
  these fields easy to identify.

The recalculation gives the same results if the variables used in the
calculations have already been converted to factors with
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md).
Running the function again on its own output updates the `_recalc`
variables instead of duplicating them.

  

## **rd_checkbox**

This function cleans and restructures REDCap checkbox fields. It
converts the default `"Unchecked/Checked"` categories of checkbox
responses created by REDCap into user-specified labels (default
`"No"/"Yes"`) and renames the `varname___code` variables (original
REDCap structure) to readable names based on the text of the checkbox
options. Additionally, it updates the dictionary to match the new
variable names. This includes choices, calculations, and branching
logic.

``` r

# Default transformation: "No"/"Yes" labels & renamed variables
cb <- covican |> 
  rd_checkbox()

str(cb$data$underlying_disease_hemato_acute_myeloid_leukemia.factor)
```

    ##  Factor w/ 2 levels "No","Yes": 1 NA 1 NA 1 NA 1 NA 1 NA ...
    ##  - attr(*, "label")= chr ""

For example, consider the checkbox field of the type of underlying
disease present in the `covican` dataset. Originally, the variables were
named `type_underlying_disease___0` and `type_underlying_disease___1`,
while the option labels were ‘Haematological cancer’ and ‘Solid tumour’.
After running the function, the variables are renamed to
`type_underlying_disease_haematological_cancer` and
`type_underlying_disease_solid_tumour`, reflecting the option text in a
readable format.

To preserve the original REDCap-style names (e.g., `varname___1`,
`varname___2`) instead of renaming variables based on option text:

``` r

# use the argument checkbox_names to choose the final format of the variable names
cb <- covican |> 
  rd_checkbox(checkbox_names = FALSE)

str(cb$data$underlying_disease_hemato___1)
```

    ##  num [1:342] 0 NA 0 NA 0 NA 0 NA 0 NA ...
    ##  - attr(*, "label")= chr "Specify underlying disease (choice=Acute myeloid leukemia)"

In this case, each option becomes a field of the dictionary of its own
and its values are already the labels, so running
[`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
again on the result stops with an error instead of converting them
again.

If a checkbox field has a branching logic, the `na_logic` argument
controls how missing values are set, with the following options:

- `"none"`: do not set `NA` based on branching logic.

- `"missing"` (default): set `NA` only where the branching logic
  evaluation is `NA`.

- `"eval"`: set `NA` where the branching logic evaluates to `FALSE` or
  `NA` (i.e., logic not satisfied or missing).

``` r

cb <- covican |> 
  rd_checkbox(na_logic = "eval")
```

By default, checkbox factors are labeled `"No"` and `"Yes"`, but you can
specify alternative labels:

``` r

cb <- covican |> 
  rd_checkbox(checkbox_labels = c("Absent", "Present"))

str(cb$data$underlying_disease_hemato_acute_myeloid_leukemia.factor)
```

    ##  Factor w/ 2 levels "Absent","Present": 1 NA 1 NA 1 NA 1 NA 1 NA ...
    ##  - attr(*, "label")= chr ""

  

## **rd_factor**

This function converts categorical variables in a REDCap dataset into R
factors by replacing each original variable (numeric version) with its
corresponding `.factor` version created by REDCap. If a `.factor` column
is text instead of a factor (e.g. after modifying the data), it is
converted to a factor with the levels in the order of the options of the
dictionary.

``` r

factored <- covican |> 
  rd_factor()

# Checking class of the variable
str(factored$data$available_analytics)
```

    ##  Factor w/ 2 levels "No","Yes": 2 2 2 2 2 1 2 NA 2 1 ...
    ##  - attr(*, "label")= chr "Blood test available? (+/- 72h)"

If you need to keep certain variables in their raw form, you can list
them in the `exclude` argument. This prevents those variables from being
replaced (including their `.factor` version) while still allowing the
rest of the dataset to be converted.

``` r

factored <- covican |> 
  rd_factor(exclude = c("available_analytics", "urine_culture"))

# The excluded variables keep both versions
str(factored$data$available_analytics)
```

    ##  'labelled' int [1:342] 1 1 1 1 1 0 1 NA 1 0 ...
    ##  - attr(*, "label")= chr "Blood test available? (+/- 72h)"

``` r

str(factored$data$available_analytics.factor)
```

    ##  Factor w/ 2 levels "No","Yes": 2 2 2 2 2 1 2 NA 2 1 ...
    ##  - attr(*, "label")= chr ""

> Note: the function automatically excludes these system variables from
> conversion: `redcap_event_name`, `redcap_repeat_instrument`,
> `redcap_data_access_group`. These variables are retained as-is to
> avoid interfering with longitudinal event mappings or user access
> groups.

After conversion only the cleaned factor variables remain in the
dataset, the original numeric version of those variables is dropped.

> Note: use
> [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
> before
> [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md).
> The branching logic and calculations translated to R by
> [`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
> compare the variables with their original codes, so they stop working
> once the variables are factors. If
> [`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
> detects this situation, it shows a warning.

  

## **rd_dictionary**

When working with REDCap exports, the data dictionary contains field
metadata, branching logic, and calculation rules written in REDCap
logic. The
[`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md)
function refreshes branching logic and calculations, translating them
from REDCap logic into R logic, and ensures the dictionary remains
consistent with the cleaned dataset.

``` r

# Update dictionary after cleaning
dict_result <- covican |>
  rd_checkbox() |>
  rd_factor() |>
  rd_dictionary()
```

When we transform the dictionary:

- Updates branching logic expressions so they match factor labels rather
  than numeric codes.

- Converts calculations and logic into R-friendly expressions.

- Reports any fields where branching logic or calculations could not be
  converted.

  

## **rd_split**

After preparing your dataset, you may want to work with only one form or
one event at a time. The
[`rd_split()`](https://bruigtp.github.io/REDCapDM/reference/rd_split.md)
function separates your dataset accordingly.

- **By form**

For non-longitudinal projects (or longitudinal projects with an
`event_form` mapping), you can split the dataset into smaller datasets
based on forms.

``` r

forms_data <- covican |>
  rd_split(by = "form")

forms_data$data
```

    ## # A tibble: 7 × 3
    ##   form                        events    df             
    ##   <chr>                       <list>    <list>         
    ## 1 inclusionexclusion_criteria <chr [1]> <df [190 × 14]>
    ## 2 demographics                <chr [1]> <df [190 × 8]> 
    ## 3 comorbidities               <chr [1]> <df [190 × 15]>
    ## 4 cancer                      <chr [1]> <df [190 × 27]>
    ## 5 vital_signs                 <chr [2]> <df [342 × 7]> 
    ## 6 laboratory_findings         <chr [2]> <df [342 × 8]> 
    ## 7 microbiological_studies     <chr [1]> <df [190 × 7]>

The result is a tibble with one row per form, and the dataset of each
form is in its list-column `df`. For example, the dataset of the first
form:

``` r

head(forms_data$data$df[[1]])
```

    ##   record_id    redcap_event_name redcap_data_access_group
    ## 1     100-6 baseline_visit_arm_1              hospital_11
    ## 2    100-13 baseline_visit_arm_1              hospital_11
    ## 3    100-16 baseline_visit_arm_1              hospital_11
    ## 4    100-31 baseline_visit_arm_1              hospital_11
    ## 5    100-34 baseline_visit_arm_1              hospital_11
    ## 6    100-36 baseline_visit_arm_1              hospital_11
    ##   redcap_event_name.factor redcap_data_access_group.factor inc_1 inc_2 inc_3
    ## 1           Baseline visit                     Hospital 11     1     1     1
    ## 2           Baseline visit                     Hospital 11     1     1     1
    ## 3           Baseline visit                     Hospital 11     1     1     1
    ## 4           Baseline visit                     Hospital 11     1     1     1
    ## 5           Baseline visit                     Hospital 11     1     1     1
    ## 6           Baseline visit                     Hospital 11     1     1     1
    ##   exc_1 screening_fail_crit inc_1.factor inc_2.factor inc_3.factor exc_1.factor
    ## 1     0                   0          Yes          Yes          Yes           No
    ## 2     0                   0          Yes          Yes          Yes           No
    ## 3     0                   0          Yes          Yes          Yes           No
    ## 4     0                   0          Yes          Yes          Yes           No
    ## 5     0                   0          Yes          Yes          Yes           No
    ## 6     0                   0          Yes          Yes          Yes           No

If repeated entries exist, you can reshape the data into wide format:

``` r

forms_data <- covican |>
  rd_split(by = "form", wide = TRUE)
```

With `wide = TRUE`, the result also has the column
`max_repeated_instance`, the highest repetition of each form.

> Note: For longitudinal projects, the column `events` has the events
> where each form is collected.

> Note: the columns of the data must be fields of the dictionary, their
> `.factor` and `_mdc` versions, the options of the checkbox fields or
> the `.factor` version of the status of the forms. Any other column
> stops the function with an error that names it, so that it isn’t left
> out of the result without notice.

- **By event**

For longitudinal projects, you can also split the data by event. The
function uses the `event_form` mapping to assign variables correctly to
each event:

``` r

events_data <- covican |>
  rd_split(by = "event")

events_data$data
```

    ## # A tibble: 2 × 2
    ##   events                   df             
    ##   <chr>                    <list>         
    ## 1 baseline_visit_arm_1     <df [190 × 56]>
    ## 2 follow_up_visit_da_arm_1 <df [152 × 10]>

If you want to extract only one form or event, use the `which` argument:

``` r

# Example by form
baseline_data <- covican |>
  rd_split(by = "form", which = "demographics")

head(baseline_data$data)
```

    ##   record_id    redcap_event_name redcap_data_access_group
    ## 1     100-6 baseline_visit_arm_1              hospital_11
    ## 2    100-13 baseline_visit_arm_1              hospital_11
    ## 3    100-16 baseline_visit_arm_1              hospital_11
    ## 4    100-31 baseline_visit_arm_1              hospital_11
    ## 5    100-34 baseline_visit_arm_1              hospital_11
    ## 6    100-36 baseline_visit_arm_1              hospital_11
    ##   redcap_event_name.factor redcap_data_access_group.factor    d_birth
    ## 1           Baseline visit                     Hospital 11 1963-10-05
    ## 2           Baseline visit                     Hospital 11 1959-10-04
    ## 3           Baseline visit                     Hospital 11 1951-04-08
    ## 4           Baseline visit                     Hospital 11 1944-08-08
    ## 5           Baseline visit                     Hospital 11 1954-12-02
    ## 6           Baseline visit                     Hospital 11 1952-05-08
    ##   d_admission age
    ## 1  2020-04-12  56
    ## 2  2020-02-13  60
    ## 3  2020-03-08  68
    ## 4  2020-02-23  75
    ## 5  2020-06-15  65
    ## 6  2020-02-25  67

  

## **rd_insert_na**

This is an auxiliary function that can be used to set some values of a
variable(s) to missing if a certain logic is fulfilled. It can be used,
for example, to insert missing values on those checkboxes that do not
have a branching logic, as mentioned earlier. For instance, we can
transform the checkboxes with the
[`rd_checkbox()`](https://bruigtp.github.io/REDCapDM/reference/rd_checkbox.md)
function and then use this function to set the values of the checkbox
*type_underlying_disease_haematological_cancer* to missing when the age
is less than 65 years old:

``` r

cb <- covican |> 
  rd_checkbox()

#Before inserting missing values
table(cb$data$type_underlying_disease_haematological_cancer)
```


      0   1 
    103  87 

``` r

#Run with this function
cb2 <- covican |> 
  rd_checkbox() |> 
  rd_insert_na(vars = "type_underlying_disease_haematological_cancer",
               filter = "age < 65")

#After inserting missing values
table(cb2$data$type_underlying_disease_haematological_cancer)
```


     0  1 
    65 50 

If the variable has a `.factor` version, it is also set to missing, so
that a later
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
doesn’t bring the values back.

> Note that both the variable to be transformed
> (`type_underlying_disease_haematological_cancer`) and the variable
> included in the filter (`age`) are in the same event. If the variable
> to be transformed and the filter didn’t have any event in common then
> the transformation would give an error. Furthermore, if the variable
> to be transformed was in more events than the filter, only the rows of
> the events in common would be converted.

  

## **rd_rlogic**

This is also an auxiliary function that transforms the REDCap logic into
logic that can be evaluated in R. This function is used internally in
multiple functions, for example,
[`rd_dictionary()`](https://bruigtp.github.io/REDCapDM/reference/rd_dictionary.md).
It returns a list with:

- `rlogic`: the transformed logic.

- `eval`: the result of the evaluation of the logic in R, one value per
  row of the dataset.

- `eval_data`: a data frame with the record identifier, the event (and
  repetition, if any) and `eval`, to identify the record of each
  evaluated value.

> This function doesn’t modify the dataset, so it has to be used outside
> the transform workflow.

Let’s see how it transforms the logic of one of the calculated fields in
the built-in dataset:

``` r

logic_trans <- covican |> 
  rd_rlogic(logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)",
            var = "screening_fail_crit")

logic_trans$rlogic
```

    [1] "ifelse(data$exc_1=='1' | data$inc_1=='0' | data$inc_2=='0' | data$inc_3=='0',1,0)"

``` r

head(logic_trans$eval_data)
```

      record_id        redcap_event_name eval
    1     100-6     baseline_visit_arm_1    0
    2     100-6 follow_up_visit_da_arm_1   NA
    3    100-13     baseline_visit_arm_1    0
    4    100-13 follow_up_visit_da_arm_1   NA
    5    100-16     baseline_visit_arm_1    0
    6    100-16 follow_up_visit_da_arm_1   NA

The variables of the logic that have already been converted to factors
with
[`rd_factor()`](https://bruigtp.github.io/REDCapDM/reference/rd_factor.md)
are evaluated with their original codes, so the result is the same
before and after the conversion.

  

## **rd_transform**

Alternatively, you can do all these steps at once using the
[`rd_transform()`](https://bruigtp.github.io/REDCapDM/reference/rd_transform.md)
function, except
[`rd_insert_na()`](https://bruigtp.github.io/REDCapDM/reference/rd_insert_na.md):
its filters are specific to each project, so apply it separately.

``` r

covican_transformed <- rd_transform(covican)

#Print the results of the transformation
covican_transformed$results
```

    1. Removing selected variables

    2. Deleting variables that contain some patterns

    3. Recalculating calculated fields and saving them as '[field_name]_recalc'


    | Total calculated fields | Non-transcribed fields | Fields with different recalculated values |
    |:-----------------------:|:----------------------:|:-----------------------------------------:|
    |            2            |         0 (0%)         |                  1 (50%)                  |


    |     field_name      | Transcribed? | Is equal? | Different values |
    |:-------------------:|:------------:|:---------:|:----------------:|
    |         age         |     Yes      |   FALSE   |        1         |
    | screening_fail_crit |     Yes      |   TRUE    |        0         |

    4. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options.

    Table: Checkbox variables advisable to be reviewed

    | Variables without any branching logic |
    |:-------------------------------------:|
    |        type_underlying_disease        |

    5. Replacing original variables for their factor version

    6. Converting every branching logic in the dictionary into R logic

Using the arguments of the function we can perform all the different
type of transformations described until now.
