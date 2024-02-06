## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
rm(list = ls())
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(magrittr)
library(purrr)

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

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
#Print the results of the transformation
covican_transformed$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_event")

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
dataset$data

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
dataset <- rd_transform(covican,
                        final_format = "by_form")

## ----message=FALSE, warning=FALSE, comment="#>", collapse = TRUE--------------
dataset$data

