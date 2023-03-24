## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
rm(list = ls())
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(Hmisc)
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
  kableExtra::row_spec(0,bold=TRUE) %>% 
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

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset$data

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_form")

#To print the results
dataset$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset$data

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        checkbox_labels = c("N", "Y"))

