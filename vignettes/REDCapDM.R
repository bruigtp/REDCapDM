## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
rm(list = ls())
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(magrittr)
library(purrr)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
data(covican)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
str(covican, max.level = 1)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
descr <- c("Identifier of each record", "",
           "Auto-generated name of the events", "",
           "Auto-generated name of each center", "",
           "Patients older than 18 years", "No ; Yes",
           "Cancer patients", "No ; Yes",
           "Diagnosed of COVID-19", "No ; Yes",
           "Solid tumour remission >1 year", "No ; Yes",
           "Indicator of non-compliance with inclusion and exclusion criteria", "Compliance ; Non-compliance",
           "Date of birth (y-m-d)", "",
           "Date of first visit (y-m-d)", "",
           "Age", "",
           "Indicator of diabetes", "No ; Yes",
           "Type of diabetes", "No complications ; End-organ diabetes-related disease",
           "Indicator of chronic pulmonary disease", "No ; Yes",
           "Fraction of inspired oxygen (%)", "",
           "Indicator of blood test available", "No ; Yes",
           "Potassium (mmol/L)", "",
           "Respiratory rate (bpm)", "",
           "Indicator of leukemia or lymphoma", "No ; Yes",
           "Indicator of acute leukemia", "No ; Yes")

vars <- data.frame("Name" = names(covican$data %>% dplyr::select(record_id:acute_leuk)),
                   "Description" = descr[seq(1, length(descr), 2)],
                   "Categories" = descr[seq(2, length(descr), 2)])

kable(vars) %>% 
  kableExtra::row_spec(0, bold=TRUE) %>% 
  kableExtra::kable_styling(full_width = F)

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
#  dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                         dic_path = "C:/Users/username/example_dictionary.csv")

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
#  dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#                         dic_path = "C:/Users/username/example_dictionary.csv",
#                         event_path = "C:/Users/username/events.csv")

## ----eval=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
#  dataset_api <- redcap_data(uri = "https://redcap.idibell.cat/api/",
#                             token = "55E5C3D1E83213ADA2182A4BFDEA")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
#Option A: list object 
covican_transformed <- rd_transform(covican)

#Option B: separately with different arguments
covican_transformed <- rd_transform(data = covican$data, 
                                    dic = covican$dictionary, 
                                    event_form = covican$event_form)

#Print the results of the transformation
covican_transformed$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_event")

#To print the results
dataset$results

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
dataset$data

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_form")

#To print the results
dataset$results

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
dataset$data

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        checkbox_labels = c("N", "Y"))

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        checkbox_na = TRUE)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        exclude_recalc = "age")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        exclude_to_factor = "dm")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        delete_vars = c("inc_", "exc_"))

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_event",
                        which_event = "baseline_visit_arm_1")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------

dataset <- rd_transform(covican,
                        final_format = "by_form",
                        which_form = "demographics")

data <- dataset$data

names(data)

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
dataset <- rd_transform(covican,
                        final_format = "by_form",
                        which_form = "laboratory_findings",
                        wide = TRUE)

head(dataset$data)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
logic_trans <- rd_rlogic(covican,
                         logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)",
                         var = "screening_fail_crit")

str(logic_trans)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
#Raw transformation of the data:
dataset <- rd_transform(covican)

data <- dataset$data

#Before inserting missings
table(data$type_underlying_disease_haematological_cancer)

#Run the function
data2 <- rd_insert_na(dataset,
                      event_form = covican$event_form,
                      vars = "type_underlying_disease_haematological_cancer",
                      filter = "age < 65")

#After inserting missings
table(data2$type_underlying_disease_haematological_cancer)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
kable(head(example$queries)) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::kable_styling()

example$results

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA----------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
covican_transformed$dictionary$branching_logic_show_field_only_if[covican_transformed$dictionary$field_name %in% "potassium"] <- "[available_analytics][current-instance]=‘1’"

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("age", "copd", "potassium"),
                    expression = c("is.na(x)", "is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("potassium"),
                    expression = c("is.na(x)"),
                    event = "baseline_visit_arm_1",
                    filter = c("available_analytics=='Yes'"))

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(variables="age",
                    expression="x>70",
                    event="baseline_visit_arm_1",
                    dic=covican_transformed$dictionary,
                    data=covican_transformed$data)

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables=c("age", "copd"),
                    expression=c("x > 70", "x == 'Yes'"),
                    event="baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables="age",
                    expression="(x>70 & x<80) | is.na(x)",
                    event="baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd","age","dm"),
                    expression = "is.na(x)",
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")

# Printing results
example$results

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
my_list <- subset(covican_transformed, !names(covican_transformed) %in% "event_form")

example <- rd_query(my_list,
                    variables = "copd",
                    expression = "is.na(x)")

# Printing results
example$results

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA----------------------
example<- rd_query(covican_transformed,
                   variables = c("copd"),
                   variables_names = c("Chronic obstructive pulmonary disease (Yes/No)"),
                   expression = c("is.na(x)"),
                   query_name = c("COPD is a missing value."),
                   instrument = c("Admission"),
                   event = "baseline_visit_arm_1")

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
kable(example$queries[1,]) %>% kableExtra::row_spec(0,bold=TRUE) %>% kableExtra::kable_styling()

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)",
                    negate = TRUE,
                    event = "baseline_visit_arm_1")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example2 <- rd_query(covican_transformed,
                     variables = "age",
                     expression = "is.na(x)",
                     event = "baseline_visit_arm_1",
                     addTo = example)

# Printing results
example2$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x<20"),
                    event = "baseline_visit_arm_1",
                    report_title = "Missing COPD values in the baseline event")

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x < 20"),
                    event = "baseline_visit_arm_1",
                    report_zeros = TRUE)

# Printing results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "x>60"),
                    event = "baseline_visit_arm_1",
                    by_dag = TRUE)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
# Printing results
example$results$`Hospital 2`

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
knitr::include_graphics("files/ProjectHome.png")

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
knitr::include_graphics("files/EventsID.png", dpi = 150)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_query(covican_transformed,
                    variables = "age",
                    expression = "x>89",
                    event = "baseline_visit_arm_1",
                    link = list(domain = "redcappre.idibell.cat",
                                redcap_version = "13.1.9",
                                proj_id = 800,
                                event_id = c("baseline_visit_arm_1" = 811, "follow_up_visit_da_arm_1" = 812)))

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
# Printing results
example$queries$Link

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1")

# Print results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1",
                    filter = "screening_fail_crit==0")

# Print results
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = c("baseline_visit_arm_1","follow_up_visit_da_arm_1"),
                    filter = "screening_fail_crit==0",
                    report_zeros = TRUE)

# Print results
example$results

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")
new_example <- example
new_example$queries <- as.data.frame(new_example$queries)
new_example$queries <- new_example$queries[c(1:5, 10:11),] # We take only some of the previously created queries
new_example$queries[nrow(new_example$queries)+1,] <- c("100-79", "Hospital 11", "Baseline visit", "Comorbidities", "copd", "-", "Chronic obstructive pulmonary disease", "The value is NA and it should not be missing", "100-79-4") # we create a new query
new_example$queries[nrow(new_example$queries)+1,] <- c("105-56", "Hospital 5", "Baseline visit", "Demographics", "age", "-", "Age", "The value is 80 and it should not be >70", "105-56-2")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
check <- check_queries(old = example$queries, 
                       new = new_example$queries)

# Print results
check$results

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
example <- rbind(head(check$queries, 4), 
                 check$queries %>% dplyr::filter(Modification == "Modified") %>% dplyr::filter(row_number()==1))
kable(example) %>% kableExtra::row_spec(0,bold=TRUE) %>% kableExtra::kable_styling()

## ----message=FALSE, warning=FALSE, comment=NA, include=FALSE------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
#  rd_export(example)

## ----message=FALSE, warning=FALSE, comment=NA, eval=FALSE---------------------
#  rd_export(queries = example$queries,
#            column = "Link",
#            sheet_name = "Queries - Proyecto",
#            path = "C:/User/Desktop/queries.xlsx",
#            password = "123")

