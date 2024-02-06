## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
rm(list = ls())
library(REDCapDM) 
library(kableExtra)
library(knitr)
library(dplyr)
library(magrittr)
library(purrr)

covican_transformed <- rd_transform(covican)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
example <- rd_query(covican_transformed,
                    variables = "copd",
                    expression = "is.na(x)")

## ----message=FALSE, warning=TRUE, comment=NA----------------------------------
example <- rd_query(covican_transformed,
                    variables = c("age", "potassium"),
                    expression = c("x > 80", "x > 4.2 & x < 4.3"),
                    event = "baseline_visit_arm_1")

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA, results='hide'------
example$queries

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
kable(head(example$queries, 2)) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::kable_styling()

## ----echo=TRUE, message=FALSE, warning=FALSE, comment=NA----------------------
example$results

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
example <- rd_event(covican_transformed,
                    event = "follow_up_visit_da_arm_1")

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
example <- rd_query(covican_transformed,
                    variables = c("copd", "age"),
                    expression = c("is.na(x)", "is.na(x)"),
                    event = "baseline_visit_arm_1")
new_example <- example
new_example$queries <- as.data.frame(new_example$queries)
new_example$queries <- new_example$queries[c(1:5, 10:11),] # We take only some of the previously created queries
new_example$queries[nrow(new_example$queries) + 1,] <- c("100-79", "Hospital 11", "Baseline visit", "Comorbidities", "copd", "-", "Chronic obstructive pulmonary disease", "The value is NA and it should not be missing", "100-79-4") # we create a new query
new_example$queries[nrow(new_example$queries) + 1, ] <- c("105-56", "Hospital 5", "Baseline visit", "Demographics", "age", "-", "Age", "The value is 80 and it should not be >70", "105-56-2")

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
check <- check_queries(old = example$queries, 
                       new = new_example$queries)

## ----message=FALSE, warning=FALSE, comment=NA---------------------------------
# Print results
check$results

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

