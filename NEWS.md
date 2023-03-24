# REDCapDM 0.5-0

2023-03-24

## Changes to functions

- New function _rd_export()_ to export the identified queries to an excel file.
- The _redcap_data()_ function can read the relative path to the working directory in the path arguments.
- Now _rd_transform()_ will keep all the labels of the variables in the transformed data. Therefore, the _keep_labels_ argument has been removed.
- _rd_transform()_ now will also return the _event_form_ element in the output.
- New argument _exclude_recalc_ added to the _rd_transform()_ function to exclude specified calculated fields from being recalculated. It may reduce the execution time in projects with complex calculated fields.
- The _filter_ argument in the _rd_query()_ and _rd_event()_ functions now works as a vector so a different filter can be applied to each variable.
- Minor changes to the _rd_query()_ function.

## Bug fixes

- Fixed bugs in several functions resulting from testing in special project structures of REDCap.

# REDCapDM 0.4-0

2023-03-03

## Changes to functions

- New _link_ argument of the functions _rd_query()_ and _rd_event()_ to generate the link where the query can be found in REDCap. [Solves Issue #2 in github]

## Bug fixes

- Bug resolved in the summary of queries, when the data did not present events (non-longitudinal projects).

# REDCapDM 0.3-0

2023-02-24

## Changes to functions

- The _redcap_data()_ function, when used to read data through an API connection, will automatically read the event-form mapping without specifying the _event_path_ argument. This will only be applied on longitudinal REDCap projects (more than one event).
- Adaption of the _rd_transform()_ function to non-longitudinal REDCap projects which only contain one event.
- The _rd_transform()_ function will now also delete, by default, variables with the pattern "_timestamp" in addition to the "_complete" pattern.
- Change in the names of the categories of the different queries when comparing two reports using the _check_queries()_.
- Creation of a new category of query in the _check_queries()_ output ("Miscorrected") that represents those queries that are different but belong to the same variable of the same record identifier in both the old and new reports.
- Adaptation of the summary of the _check_queries()_ and _rd_event()_ functions to the RStudio viewer tab.
- Customization of the title of the summary of queries returned by the _check_queries()_ function.

## Bug fixes

- Bug resolved in the _redcap_data()_ function when trying to read the dictionary of a specific structure of a REDCap project.

# REDCapDM 0.2-0

2023-02-17

## Changes to functions

- New _event_path_ argument of the function _redcap_data()_ to read the event-form mapping of the REDCap project.
- For all functions except _redcap_data()_ we can now pass a list with the data, dictionary and event in the first argument of the function.
- _rd_transform()_ will now take into account the different events that might be present in the variables of the logic for the recalculation of calculated fields and the evaluation of branching logics. This also applies for _rd_rlogic()_.
- As a consequence of this last improvement that applies in longitudinal REDCap projects (with more than one event) now we have to specify not only the data and dictionary but also the event-form mapping.
- New _checkbox_na_ argument in _rd_transform()_ to specify if the values of the checkbox will be converted to missing not only when the branching logic is missing but also when it's not fullfilled (if _TRUE_).
- New _by_dag_ argument in _rd_query()_ to filter the output by each Data Access Group (DAG).
- Summary of the generated queries ( _$results_ ) will now include the event and the type of query and will be displayed in the RStudio viewer tab.

## Bug fixes

- The output of the _redcap_data()_ function was not consistent if we read the data using the files exported from REDCap or the API connection. [Solves Issue #1 in github]
- We changed the use of the _subset()_ function by the _filter()_ function because of undesired behaviours of this function when applying a non-existent expression.

# REDCapDM 0.1-1

2023-01-30

- Help documentation has been altered.
- Improvement of the vignette.

# REDCapDM 0.1-0

2022-12-20
