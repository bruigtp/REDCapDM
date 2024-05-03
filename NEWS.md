# REDCapDM 0.9-9

## Bug fixes

-   Added an informative message to the *redcap_data()* function when it is unable to replicate the factor version of a variable. This only applies to cases where the user is reading the data via an API connection. [Issue #7 in github]
-   Added an informative message to the *redcap_data()* function when someone tries to use another type of file in the *data_path* argument instead of an R file. [Issue #7 in github]

## Changes

-   Added a weblink in the description to redirect users to the published article in BMC Medical Research Methodology.

<br>
<br>

# REDCapDM 0.9-8

## Bug fixes

-   Added an informative message to the *redcap_data()* function when someone tries to use a URI without specifying the token or vice versa. [Issue #6 in github]
-   Added an informative message to the *redcap_data()* function when it is unable to establish a secure connection due to an SSL certificate problem. [Issue #6 in github]
-   Added examples to the *redcap_data()* function. [Issue #6 in github]
-   Modification of the argument `delete_vars` in the *redcap_data()* function to `delete_pattern` to accurately describe its purpose. [Issue #6 in github]
-   Added a new argument to the *redcap_data()* function named `delete_vars` which allows users to specify variables to be removed. [Issue #6 in github]

<br>
<br>

# REDCapDM 0.9-7

## Bug fixes

-   Error in *redcap_data()* function while using the API connection to import data. [Solves Issue #4 in github]

<br>
<br>

# REDCapDM 0.9-6

## Changes

-   Update of the links leading to the Github page.

<br>
<br>

# REDCapDM 0.9-5

## Changes

-   Huge improvement in reading and transforming data through the API connection. Now the final output will be the same as the one resulting from using the default files exported from REDCap.

<br>
<br>


# REDCapDM 0.9-0

## Changes to functions

-   Added a new argument (filter_field) to the *redcap_data()* function that allows the user to select which fields to import into R, instead of having to import all fields of the project.

## Bug fixes

-   Fixed an error when transforming data imported through an API connection (rd_transform).

<br>
<br>


# REDCapDM 0.8-5

## Bug fixes

-   Fixed a major error when importing data through an API connection.

<br>
<br>


# REDCapDM 0.8-0

## Changes to functions

-   The *redcap_data()* function can now read data from xlsx files.

## Minor Changes

-   Small bugs fixed in some of the functions

-   Added new vignettes for a simpler use of the functions

<br>
<br>


# REDCapDM 0.7-0

2023-06-02

## Changes to functions

-   The *rd_transform()* function will now also transform the branching logic in the dictionary into R logic.

-   The *rd_query()* will now use the information of the event-form to automatically detect the events where the variables are present.

-   The *rd_query()* will now automatically apply the branching logic of the variable before generating queries.

## Minor Changes

-   Remove descriptive variables from the dictionary in the *redcap_data()* function.

-   Improve the resulting summary of the *rd_query()* function.

-   New error added to the *rd_query()* function when trying to use a variable that is not in the dataset.

-   New error added to the *rd_query()* function when the logic of the filter was using incorrected logic.

-   The *rd_export()* function will now create a themed excel file.

## Bug fixes

-   Minor error fixed in the summary report of the *rd_query()* function when there were multiple variables with zero queries.

-   Error fixed in the *check_queries()* function when there were several 'Miscorrected' queries.

-   We now take into account REDCap projects with repeated instruments.

<br>
<br>


# REDCapDM 0.6-0

2023-03-31

## Changes to functions

-   Reduced number of dependencies of the package.

## Bug fixes

-   Fixed a bug in the *rd_transform()* function in the branching logic evaluation of checkboxes from non longitudinal projects.

<br>
<br>


# REDCapDM 0.5-0

2023-03-24

## Changes to functions

-   New function *rd_export()* to export the identified queries to an excel file.
-   The *redcap_data()* function can read the relative path to the working directory in the path arguments.
-   Now *rd_transform()* will keep all the labels of the variables in the transformed data. Therefore, the *keep_labels* argument has been removed.
-   *rd_transform()* now will also return the *event_form* element in the output.
-   New argument *exclude_recalc* added to the *rd_transform()* function to exclude specified calculated fields from being recalculated. It may reduce the execution time in projects with complex calculated fields.
-   The *filter* argument in the *rd_query()* and *rd_event()* functions now works as a vector so a different filter can be applied to each variable.
-   Minor changes to the *rd_query()* function.

## Bug fixes

-   Fixed bugs in several functions resulting from testing in special project structures of REDCap.

<br>
<br>


# REDCapDM 0.4-0

2023-03-03

## Changes to functions

-   New *link* argument of the functions *rd_query()* and *rd_event()* to generate the link where the query can be found in REDCap. [Solves Issue #2 in github]

## Bug fixes

-   Bug resolved in the summary of queries, when the data did not present events (non-longitudinal projects).

<br>
<br>


# REDCapDM 0.3-0

2023-02-24

## Changes to functions

-   The *redcap_data()* function, when used to read data through an API connection, will automatically read the event-form mapping without specifying the *event_path* argument. This will only be applied on longitudinal REDCap projects (more than one event).
-   Adaption of the *rd_transform()* function to non-longitudinal REDCap projects which only contain one event.
-   The *rd_transform()* function will now also delete, by default, variables with the pattern "\_timestamp" in addition to the "\_complete" pattern.
-   Change in the names of the categories of the different queries when comparing two reports using the *check_queries()*.
-   Creation of a new category of query in the *check_queries()* output ("Miscorrected") that represents those queries that are different but belong to the same variable of the same record identifier in both the old and new reports.
-   Adaptation of the summary of the *check_queries()* and *rd_event()* functions to the RStudio viewer tab.
-   Customization of the title of the summary of queries returned by the *check_queries()* function.

## Bug fixes

-   Bug resolved in the *redcap_data()* function when trying to read the dictionary of a specific structure of a REDCap project.

<br>
<br>

# REDCapDM 0.2-0

2023-02-17

## Changes to functions

-   New *event_path* argument of the function *redcap_data()* to read the event-form mapping of the REDCap project.
-   For all functions except *redcap_data()* we can now pass a list with the data, dictionary and event in the first argument of the function.
-   *rd_transform()* will now take into account the different events that might be present in the variables of the logic for the recalculation of calculated fields and the evaluation of branching logics. This also applies for *rd_rlogic()*.
-   As a consequence of this last improvement that applies in longitudinal REDCap projects (with more than one event) now we have to specify not only the data and dictionary but also the event-form mapping.
-   New *checkbox_na* argument in *rd_transform()* to specify if the values of the checkbox will be converted to missing not only when the branching logic is missing but also when it's not fullfilled (if *TRUE*).
-   New *by_dag* argument in *rd_query()* to filter the output by each Data Access Group (DAG).
-   Summary of the generated queries ( *\$results* ) will now include the event and the type of query and will be displayed in the RStudio viewer tab.

## Bug fixes

-   The output of the *redcap_data()* function was not consistent if we read the data using the files exported from REDCap or the API connection. [Solves Issue #1 in github]
-   We changed the use of the *subset()* function by the *filter()* function because of undesired behaviours of this function when applying a non-existent expression.

<br>
<br>

# REDCapDM 0.1-1

2023-01-30

-   Help documentation has been altered.
-   Improvement of the vignette.

<br>
<br>

# REDCapDM 0.1-0

2022-12-20
