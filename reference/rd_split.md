# Split a REDCap dataset by form or event

**\[experimental\]**

Splits a REDCap dataset into separate datasets by **form** or **event**
using the data dictionary. Supports both longitudinal and
non-longitudinal projects and can return wide or long formats for
repeated measures.

## Usage

``` r
rd_split(
  project = NULL,
  data = NULL,
  dic = NULL,
  event_form = NULL,
  which = NULL,
  by = "form",
  wide = FALSE
)
```

## Arguments

- project:

  A list containing the REDCap data, dictionary, and event mapping
  (expected
  [`redcap_data()`](https://bruigtp.github.io/REDCapDM/reference/redcap_data.md)
  output). Overrides `data`, `dic`, and `event_form`.

- data:

  A `data.frame` or `tibble` with the REDCap dataset.

- dic:

  A `data.frame` with the REDCap dictionary.

- event_form:

  Only applicable for longitudinal projects (presence of events).
  Event-to-form mapping for longitudinal projects.

- which:

  Optional. A single form or event to extract. If not provided, all
  forms or events are returned.

- by:

  Character. Criteria to split the dataset: `"form"` (default) or
  `"event"`.

- wide:

  Logical. If `TRUE` (for form-based splits), repeated instances are
  returned in wide format. Defaults to `FALSE`.

## Value

Depending on `which` and `wide`:

- data:

  A `data.frame` or a list of `data.frames` representing the split
  datasets.

- dictionary:

  The original REDCap dictionary.

- event_form:

  The original event-form mapping (if applicable).

- results:

  A summary message of the splitting operation.

## Details

- Handles checkbox variables and REDCap default variables (`_complete`,
  `_timestamp`) appropriately.

- For form-based splits in longitudinal projects, uses `event_form` to
  map variables to events.

- Wide format expands repeated instances into multiple columns per
  record.

- Filtering by `which` allows extracting a single form or event.

- Projects with repeated instruments are handled by filtering on the
  `redcap_repeat_instrument` variable.

## Examples

``` r
# Split by form and return wide format
result <- covican |>
  rd_split(by = "form", wide = TRUE)

print(result)
#> $data
#> # A tibble: 7 × 4
#>   form                        events    max_repeated_instance df                
#>   <chr>                       <list>                    <dbl> <list>            
#> 1 inclusionexclusion_criteria <chr [1]>                     1 <df [190 × 10]>   
#> 2 demographics                <chr [1]>                     1 <df [190 × 4]>    
#> 3 comorbidities               <chr [1]>                     1 <df [190 × 11]>   
#> 4 cancer                      <chr [1]>                     1 <df [190 × 23]>   
#> 5 vital_signs                 <chr [2]>                     2 <tibble [190 × 5]>
#> 6 laboratory_findings         <chr [2]>                     2 <tibble [190 × 7]>
#> 7 microbiological_studies     <chr [1]>                     1 <df [190 × 3]>    
#> 
#> $dictionary
#>                    field_name                   form_name
#> 2                   record_id inclusionexclusion_criteria
#> 3                       inc_1 inclusionexclusion_criteria
#> 4                       inc_2 inclusionexclusion_criteria
#> 5                       inc_3 inclusionexclusion_criteria
#> 7                       exc_1 inclusionexclusion_criteria
#> 11        screening_fail_crit inclusionexclusion_criteria
#> 20                d_admission                demographics
#> 22                    d_birth                demographics
#> 24                        age                demographics
#> 46                         dm               comorbidities
#> 47                    type_dm               comorbidities
#> 49                       copd               comorbidities
#> 60                 leuk_lymph               comorbidities
#> 61                 acute_leuk               comorbidities
#> 107   type_underlying_disease                      cancer
#> 108 underlying_disease_hemato                      cancer
#> 212                      fio2                 vital_signs
#> 216                 resp_rate                 vital_signs
#> 235       available_analytics         laboratory_findings
#> 249                 potassium         laboratory_findings
#> 413             urine_culture     microbiological_studies
#>                                   section_header field_type
#> 2                                                      text
#> 3   <center><h6>Inclusion criteria</center></h6>      radio
#> 4                                                     radio
#> 5                                                     radio
#> 7   <center><h6>Exclusion criteria</center></h6>      radio
#> 11                                                     calc
#> 20                                                     text
#> 22                                                     text
#> 24                                                     calc
#> 46               <center>CHARLSON INDEX</center>      radio
#> 47                                                    radio
#> 49                                                    radio
#> 60                                                    radio
#> 61                                                    radio
#> 107                                                checkbox
#> 108       <center>Haematological cancer</center>   checkbox
#> 212                                                    text
#> 216                                                    text
#> 235                                                   radio
#> 249                                                    text
#> 413                                                   radio
#>                                                                                                                                                                         field_label
#> 2                                                                                                                                                                         Record ID
#> 3                                                                                                                                                      Patients older than 18 years
#> 4                                                                                                                                                                   Cancer patients
#> 5   Diagnosed of COVID-19 (Clinical respiratory infection (fever, cough and/or dyspnoea), diarrhea, vomiting or other symptoms; and presence of a positive detection of SARS-CoV-2)
#> 7                                                                                                                                                    Solid tumour remission >1 year
#> 11                                                                                                   Screening failure por incumplimiento de los criterios de inclusion y exclusión
#> 20                                                                                                                                                              Date of first visit
#> 22                                                                                                                                                                    Date of birth
#> 24                                                                                                                                                                              Age
#> 46                                                                                                                       Diabetes (treated with insulin or antidiabetic oral drugs)
#> 47                                                                                                                                                                 Type of diabetes
#> 49                                                                                                                                            Chronic obstructive pulmonary disease
#> 60                                                                                                                                          Leukaemia or Lymphoma (include myeloma)
#> 61                                                                                                                                                                  Acute leukaemia
#> 107                                                                                                                                                      Type of underlying disease
#> 108                                                                                                                                                      Specify underlying disease
#> 212                                                                                                                                                                            FiO2
#> 216                                                                                                                                                                Respiratory rate
#> 235                                                                                                                                                 Blood test available? (+/- 72h)
#> 249                                                                                                                                                                      Potassium 
#> 413                                                                                                                                                                   Urine culture
#>                                                                                                                                                                                                                                                                         choices_calculations_or_slider_labels
#> 2                                                                                                                                                                                                                                                                                                            
#> 3                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 4                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 5                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 7                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 11                                                                                                                                                                                                                                           if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)
#> 20                                                                                                                                                                                                                                                                                                           
#> 22                                                                                                                                                                                                                                                                                                           
#> 24                                                                                                                                                                                                                                                   rounddown(datediff([d_birth],[d_admission],"y","dmy"),0)
#> 46                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 47                                                                                                                                                                                                    1, No complications | 2, End-organ diabetes-related disease (neuropathy, nefropathy, retinopathy, etc.)
#> 49                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 60                                                                                                                                                                                                                                                                                             0, No | 2, Yes
#> 61                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 107                                                                                                                                                                                                                                                                0, Haematological cancer | 1, Solid tumour
#> 108 1, Acute myeloid leukemia | 2, Myelodysplastic syndrome | 3, Chronic myeloid leukaemia | 4, Acute lymphoblastic leukaemia | 5, Hodgkin lymphoma | 6, NonHodgkin lymphoma | 7, Multiple myeloma | 8, Myelofibrosis | 9, Aplastic anaemia | 10, Chronic lymphocytic leukaemia | 11, Amyloidosis | 12, Other
#> 212                                                                                                                                                                                                                                                                                                          
#> 216                                                                                                                                                                                                                                                                                                          
#> 235                                                                                                                                                                                                                                                                                            0, No | 1, Yes
#> 249                                                                                                                                                                                                                                                                                                          
#> 413                                                                                                                                                                                                                                                                                     0, Not done | 1, Done
#>     field_note text_validation_type_or_show_slider_number text_validation_min
#> 2                                                                            
#> 3                                                                            
#> 4                                                                            
#> 5                                                                            
#> 7                                                                            
#> 11                                                                           
#> 20                                               date_dmy                    
#> 22                                               date_dmy                    
#> 24                                                                           
#> 46                                                                           
#> 47                                                                           
#> 49                                                                           
#> 60                                                                           
#> 61                                                                           
#> 107                                                                          
#> 108                                                                          
#> 212          %                                     number                  21
#> 216        bpm                                    integer                   4
#> 235                                                                          
#> 249     mmol/L                                     number                   1
#> 413                                                                          
#>     text_validation_max identifier       branching_logic_show_field_only_if
#> 2                                                                          
#> 3                                                                          
#> 4                                                                          
#> 5                                                                          
#> 7                                                                          
#> 11                                                                         
#> 20                                                                         
#> 22                                                                         
#> 24                                                                         
#> 46                                                                         
#> 47                                                                 [dm]='1'
#> 49                                                                         
#> 60                                                                         
#> 61                                                         [leuk_lymph]='2'
#> 107                                                                        
#> 108                                        [type_underlying_disease(0)]='1'
#> 212                 100                                                    
#> 216                  65                 [event-name]='baseline_visit_arm_1'
#> 235                                [event-name]<>'ongoing_data_colle_arm_1'
#> 249                  14                           [available_analytics]='1'
#> 413                                     [event-name]='baseline_visit_arm_1'
#>     required_field custom_alignment question_number_surveys_only
#> 2                                                               
#> 3                                                               
#> 4                                                               
#> 5                                                               
#> 7                                                               
#> 11                                                              
#> 20                                                              
#> 22                                                              
#> 24                                                              
#> 46                                                              
#> 47                                                              
#> 49                                                              
#> 60                                                              
#> 61                                                              
#> 107                                                             
#> 108                                                             
#> 212                                                             
#> 216                                                             
#> 235                                                             
#> 249                                                             
#> 413                                                             
#>     matrix_group_name matrix_ranking field_annotation
#> 2                                                    
#> 3           inclusion                                
#> 4           inclusion                                
#> 5           inclusion                                
#> 7                 exc                                
#> 11                                            @HIDDEN
#> 20                                                   
#> 22                                                   
#> 24                                                   
#> 46                                                   
#> 47                                                   
#> 49                                                   
#> 60                                                   
#> 61                                                   
#> 107                                                  
#> 108                                                  
#> 212                                                  
#> 216                                                  
#> 235                                                  
#> 249                                                  
#> 413                                                  
#> 
#> $event_form
#>   arm_num        unique_event_name                        form
#> 1       1     baseline_visit_arm_1 inclusionexclusion_criteria
#> 2       1     baseline_visit_arm_1                demographics
#> 3       1     baseline_visit_arm_1               comorbidities
#> 4       1     baseline_visit_arm_1                      cancer
#> 5       1     baseline_visit_arm_1                 vital_signs
#> 6       1     baseline_visit_arm_1         laboratory_findings
#> 7       1     baseline_visit_arm_1     microbiological_studies
#> 8       1 follow_up_visit_da_arm_1                 vital_signs
#> 9       1 follow_up_visit_da_arm_1         laboratory_findings
#> 
#> $results
#> Final arrangment of the data by form. (rd_split)
#> 

# Split by event (long format)
result <- covican |>
  rd_split(by = "event")

print(result)
#> $data
#> # A tibble: 2 × 2
#>   events                   df             
#>   <chr>                    <list>         
#> 1 baseline_visit_arm_1     <df [190 × 56]>
#> 2 follow_up_visit_da_arm_1 <df [152 × 10]>
#> 
#> $dictionary
#>                    field_name                   form_name
#> 2                   record_id inclusionexclusion_criteria
#> 3                       inc_1 inclusionexclusion_criteria
#> 4                       inc_2 inclusionexclusion_criteria
#> 5                       inc_3 inclusionexclusion_criteria
#> 7                       exc_1 inclusionexclusion_criteria
#> 11        screening_fail_crit inclusionexclusion_criteria
#> 20                d_admission                demographics
#> 22                    d_birth                demographics
#> 24                        age                demographics
#> 46                         dm               comorbidities
#> 47                    type_dm               comorbidities
#> 49                       copd               comorbidities
#> 60                 leuk_lymph               comorbidities
#> 61                 acute_leuk               comorbidities
#> 107   type_underlying_disease                      cancer
#> 108 underlying_disease_hemato                      cancer
#> 212                      fio2                 vital_signs
#> 216                 resp_rate                 vital_signs
#> 235       available_analytics         laboratory_findings
#> 249                 potassium         laboratory_findings
#> 413             urine_culture     microbiological_studies
#>                                   section_header field_type
#> 2                                                      text
#> 3   <center><h6>Inclusion criteria</center></h6>      radio
#> 4                                                     radio
#> 5                                                     radio
#> 7   <center><h6>Exclusion criteria</center></h6>      radio
#> 11                                                     calc
#> 20                                                     text
#> 22                                                     text
#> 24                                                     calc
#> 46               <center>CHARLSON INDEX</center>      radio
#> 47                                                    radio
#> 49                                                    radio
#> 60                                                    radio
#> 61                                                    radio
#> 107                                                checkbox
#> 108       <center>Haematological cancer</center>   checkbox
#> 212                                                    text
#> 216                                                    text
#> 235                                                   radio
#> 249                                                    text
#> 413                                                   radio
#>                                                                                                                                                                         field_label
#> 2                                                                                                                                                                         Record ID
#> 3                                                                                                                                                      Patients older than 18 years
#> 4                                                                                                                                                                   Cancer patients
#> 5   Diagnosed of COVID-19 (Clinical respiratory infection (fever, cough and/or dyspnoea), diarrhea, vomiting or other symptoms; and presence of a positive detection of SARS-CoV-2)
#> 7                                                                                                                                                    Solid tumour remission >1 year
#> 11                                                                                                   Screening failure por incumplimiento de los criterios de inclusion y exclusión
#> 20                                                                                                                                                              Date of first visit
#> 22                                                                                                                                                                    Date of birth
#> 24                                                                                                                                                                              Age
#> 46                                                                                                                       Diabetes (treated with insulin or antidiabetic oral drugs)
#> 47                                                                                                                                                                 Type of diabetes
#> 49                                                                                                                                            Chronic obstructive pulmonary disease
#> 60                                                                                                                                          Leukaemia or Lymphoma (include myeloma)
#> 61                                                                                                                                                                  Acute leukaemia
#> 107                                                                                                                                                      Type of underlying disease
#> 108                                                                                                                                                      Specify underlying disease
#> 212                                                                                                                                                                            FiO2
#> 216                                                                                                                                                                Respiratory rate
#> 235                                                                                                                                                 Blood test available? (+/- 72h)
#> 249                                                                                                                                                                      Potassium 
#> 413                                                                                                                                                                   Urine culture
#>                                                                                                                                                                                                                                                                         choices_calculations_or_slider_labels
#> 2                                                                                                                                                                                                                                                                                                            
#> 3                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 4                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 5                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 7                                                                                                                                                                                                                                                                                              0, No | 1, Yes
#> 11                                                                                                                                                                                                                                           if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)
#> 20                                                                                                                                                                                                                                                                                                           
#> 22                                                                                                                                                                                                                                                                                                           
#> 24                                                                                                                                                                                                                                                   rounddown(datediff([d_birth],[d_admission],"y","dmy"),0)
#> 46                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 47                                                                                                                                                                                                    1, No complications | 2, End-organ diabetes-related disease (neuropathy, nefropathy, retinopathy, etc.)
#> 49                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 60                                                                                                                                                                                                                                                                                             0, No | 2, Yes
#> 61                                                                                                                                                                                                                                                                                             0, No | 1, Yes
#> 107                                                                                                                                                                                                                                                                0, Haematological cancer | 1, Solid tumour
#> 108 1, Acute myeloid leukemia | 2, Myelodysplastic syndrome | 3, Chronic myeloid leukaemia | 4, Acute lymphoblastic leukaemia | 5, Hodgkin lymphoma | 6, NonHodgkin lymphoma | 7, Multiple myeloma | 8, Myelofibrosis | 9, Aplastic anaemia | 10, Chronic lymphocytic leukaemia | 11, Amyloidosis | 12, Other
#> 212                                                                                                                                                                                                                                                                                                          
#> 216                                                                                                                                                                                                                                                                                                          
#> 235                                                                                                                                                                                                                                                                                            0, No | 1, Yes
#> 249                                                                                                                                                                                                                                                                                                          
#> 413                                                                                                                                                                                                                                                                                     0, Not done | 1, Done
#>     field_note text_validation_type_or_show_slider_number text_validation_min
#> 2                                                                            
#> 3                                                                            
#> 4                                                                            
#> 5                                                                            
#> 7                                                                            
#> 11                                                                           
#> 20                                               date_dmy                    
#> 22                                               date_dmy                    
#> 24                                                                           
#> 46                                                                           
#> 47                                                                           
#> 49                                                                           
#> 60                                                                           
#> 61                                                                           
#> 107                                                                          
#> 108                                                                          
#> 212          %                                     number                  21
#> 216        bpm                                    integer                   4
#> 235                                                                          
#> 249     mmol/L                                     number                   1
#> 413                                                                          
#>     text_validation_max identifier       branching_logic_show_field_only_if
#> 2                                                                          
#> 3                                                                          
#> 4                                                                          
#> 5                                                                          
#> 7                                                                          
#> 11                                                                         
#> 20                                                                         
#> 22                                                                         
#> 24                                                                         
#> 46                                                                         
#> 47                                                                 [dm]='1'
#> 49                                                                         
#> 60                                                                         
#> 61                                                         [leuk_lymph]='2'
#> 107                                                                        
#> 108                                        [type_underlying_disease(0)]='1'
#> 212                 100                                                    
#> 216                  65                 [event-name]='baseline_visit_arm_1'
#> 235                                [event-name]<>'ongoing_data_colle_arm_1'
#> 249                  14                           [available_analytics]='1'
#> 413                                     [event-name]='baseline_visit_arm_1'
#>     required_field custom_alignment question_number_surveys_only
#> 2                                                               
#> 3                                                               
#> 4                                                               
#> 5                                                               
#> 7                                                               
#> 11                                                              
#> 20                                                              
#> 22                                                              
#> 24                                                              
#> 46                                                              
#> 47                                                              
#> 49                                                              
#> 60                                                              
#> 61                                                              
#> 107                                                             
#> 108                                                             
#> 212                                                             
#> 216                                                             
#> 235                                                             
#> 249                                                             
#> 413                                                             
#>     matrix_group_name matrix_ranking field_annotation
#> 2                                                    
#> 3           inclusion                                
#> 4           inclusion                                
#> 5           inclusion                                
#> 7                 exc                                
#> 11                                            @HIDDEN
#> 20                                                   
#> 22                                                   
#> 24                                                   
#> 46                                                   
#> 47                                                   
#> 49                                                   
#> 60                                                   
#> 61                                                   
#> 107                                                  
#> 108                                                  
#> 212                                                  
#> 216                                                  
#> 235                                                  
#> 249                                                  
#> 413                                                  
#> 
#> $event_form
#>   arm_num        unique_event_name                        form
#> 1       1     baseline_visit_arm_1 inclusionexclusion_criteria
#> 2       1     baseline_visit_arm_1                demographics
#> 3       1     baseline_visit_arm_1               comorbidities
#> 4       1     baseline_visit_arm_1                      cancer
#> 5       1     baseline_visit_arm_1                 vital_signs
#> 6       1     baseline_visit_arm_1         laboratory_findings
#> 7       1     baseline_visit_arm_1     microbiological_studies
#> 8       1 follow_up_visit_da_arm_1                 vital_signs
#> 9       1 follow_up_visit_da_arm_1         laboratory_findings
#> 
#> $results
#> Final arrangment of the data by event. (rd_split)
#> 
```
