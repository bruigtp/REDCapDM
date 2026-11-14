# the query reports print a summary

    Code
      report
    Output
      REDCapDM query report: 82 queries in 81 records
      
       Variable Event          Queries
       age      Baseline visit 76
       copd     Baseline visit  6
      
      Each query is in `$queries` and the summary table in `$results`.
      `rd_export()` saves the queries in an Excel file.
    Code
      events
    Output
      REDCapDM query report: 38 queries in 38 records
      
       Event                    Queries
       follow_up_visit_da_arm_1 38
      
      Each query is in `$queries` and the summary table in `$results`.
      `rd_export()` saves the queries in an Excel file.
    Code
      control
    Output
      REDCapDM query control: 82 queries in 81 records
      
       State        Queries
       Pending       3
       Solved       79
       Miscorrected  0
       New           0
      
      Each query is in `$queries` and the summary table in `$results`.
      `rd_export()` saves the queries in an Excel file.
    Code
      none
    Output
      REDCapDM query report: no queries.
    Code
      by_dag
    Output
      REDCapDM query report: 6 queries in 6 records
      
       Variable Event          Queries
       copd     Baseline visit 6
      
      Each query is in `$queries` and the summary table in `$results`.
      `rd_export()` saves the queries in an Excel file.

# the main errors and warnings keep their format

    Code
      rd_query(covican, variables = "copd", expression = "is.na(x)", negate = "yes")
    Condition
      Error in `rd_query()`:
      ! `negate` must be TRUE or FALSE, not a string.

---

    Code
      rd_query(covican, variables = "agee", expression = "is.na(x)")
    Condition
      Error in `rd_query()`:
      ! Specified variables do not exist in the dataset: "agee".
      i Please review the `variables` argument.

---

    Code
      rd_delete_vars(covican, vars = c("copd", "nope1"))
    Condition
      Error in `rd_delete_vars()`:
      ! The following variables are not present in the dataset: `nope1`.
      i Please remove them from the `vars` argument.

---

    Code
      rd_split(covican, by = "form", which = "nope")
    Condition
      Error in `rd_split()`:
      ! The form specified in the `which` argument was not found in this project.
      i Please select one of the available REDCap forms: "inclusionexclusion_criteria", "demographics", "comorbidities", "cancer", "vital_signs", "laboratory_findings", and "microbiological_studies".

---

    Code
      rd_rlogic(covican, logic = "[nope] = '1'", var = "age")
    Condition
      Error in `rd_rlogic()`:
      ! REDCap logic contains variables that are not found in the database (possibly a smart variable or an event that doesn't exist): `[nope]`

---

    Code
      rd_event(covican, event = "nope")
    Condition
      Error in `rd_event()`:
      ! One or more specified events do not exist in the dataset. Please review the `event` argument.

---

    Code
      res <- rd_recalculate(covican, exclude = "copd")
    Condition
      Warning:
      The following variables of `exclude` are not calculated fields of the dictionary: `copd`.

