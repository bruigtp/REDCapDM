# REDCapDM 1.1.0

## Breaking changes

- `library(REDCapDM)` no longer attaches other packages (dplyr, purrr, stringr, labelled, ...). Load the packages you use in your own code, e.g. `library(dplyr)` to use `filter()` or `%>%`.
- REDCapR and openxlsx are now suggested packages: they are only needed to read data from the REDCap API or `.xlsx` files and to export queries with `rd_export()`. These functions ask to install them when they are missing.
- REDCapDM now requires dplyr 1.1.0 or later (released in January 2023).
- Reading data from the REDCap API needs REDCapR 1.2.0 or later: older versions only return the events of the first arm of the project.

## New features

- New `rd_missplot()` function to visualise missing data per variable (barplot) or per event and variable (heatmap in longitudinal projects). With `by = "record"` it shows the missing values record by record. The `events` and `forms` arguments choose what is plotted, long labels are shortened (`max_label_length`) and the events where a variable is not collected are left blank. First version by Ferran Perez Sanchez (#19).
- New `rm_mdc` argument in `redcap_data()` to remove REDCap Missing Data Codes (default `TRUE`). It warns that the codes were removed only when the data has them (also a checked option of a code in a checkbox) and the codes of the project weren't given in `mdc`.
- With `redcap_data(rm_mdc = FALSE)`, the Missing Data Codes of validated text fields (numbers, dates, ...) are kept in a new `<field>_mdc` column, so that the field keeps its type. `rd_missplot()`, `rd_delete_vars()` and `rd_split()` handle these columns like the `.factor` ones.
- New `mdc` argument in `redcap_data()` for the Missing Data Codes of the project, besides the default ones of REDCap (e.g. `mdc = c("-999", "-888" = "Not applicable")`): they are removed, or kept with `rm_mdc = FALSE`, like the default ones, and with a label the `.factor` version shows it instead of the code. Numeric codes are also found in numeric fields, also when written in another way (e.g. `-999.0` for `-999`). With the API, the codes defined in the project are added automatically.
- New `forms` argument in `rd_delete_vars()` to remove every variable of a form.
- `rd_recalculate()` reports the number of different values of each calculated field.
- `rd_rlogic()` returns `eval_data`, the evaluation next to the record identifier, event and repetition of each row.
- `rd_insert_na()` also updates the `.factor` version of the variables.
- `rd_factor()` warns when the dictionary logic was already translated to R with `rd_dictionary()`.
- `rd_rlogic()` translates logic that uses the same variable in different events (e.g. `[visit_2_arm_1][weight] - [visit_1_arm_1][weight]`).
- `rd_checkbox()` returns a project without checkbox fields unchanged, with a warning, instead of an error.
- The `addTo` argument of `rd_query()` and `rd_event()` is now called `add_to`. `addTo` still works, with a deprecation warning.
- `rd_query()`, `rd_event()` and `check_queries()` return a list of class `REDCapDM_report`: printed in the console, it shows the number of queries per variable and event (per state in `check_queries()`) instead of the whole list. `$queries`, `$results`, `add_to` and `rd_export()` work as before.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()` and `rd_dictionary()`) translates more REDCap functions: `isblankormissingcode()`, `not_contain()`, `starts_with()`, `ends_with()`, `left()`, `right()`, `mid()`, `find()`, `replace_text()`, `concat()`, `concat_ws()`, `upper()`, `lower()`, `trim()`, `stdev()`, `isinteger()`, `month()` and `day()`. As in REDCap, the text functions use a blank value as an empty text. `datediff()` also understands `"now"` and dates with time (e.g. `'2024-01-05 10:30'`).

## Repeated instruments and repeating events

- `rd_query()` now only checks each variable in the rows where it is collected. In projects with repeated instruments, empty fields of other instruments are no longer reported as queries.
- The `Repetition` column of `rd_query()` is now correct for every row, and query links open the right instance.
- `check_queries()` uses the event and the repetition to match queries, so the same variable in different events or instances is tracked separately.
- `rd_missplot()` computes missingness only over the rows where each variable is collected.
- Repeating events are now detected internally.
- `rd_rlogic()` translates and evaluates logic in projects with repeated instruments and repeating events: the logic of a repeating instrument is evaluated in its instances and, as in REDCap, the variables of the other forms take the value of the base row of the same record and event. As a result, `rd_recalculate()` now works with repeated instruments, and `rd_query()` and `rd_dictionary()` apply and translate their branching logic.
- `rd_checkbox()` evaluates the branching logic of checkboxes and `rd_transform()` no longer skips the recalculation, the checkbox branching logic and the dictionary translation in projects with repeated instruments.
- `rd_missplot(by = "record")` shows each repetition as a row and tells apart the cells of variables not collected in that row.

## Bug fixes

- `rd_query()`, `rd_event()` and `rd_insert_na()` evaluate `filter` and `expression` without the class of the variable labels, which some functions don't accept: e.g. `dplyr::between(age, 17, 18)` stopped with an error. They only see the columns of the project: a filter that selects the variables by their type (e.g. `if_all(where(is.numeric), ~ .x < 5)`) also took the internal row counters, so the result depended on the order of the rows. In the `expression` of `rd_query()`, `x` is the variable except where it is the argument of a function (e.g. `vapply(x, function(x) is.na(x), logical(1))`).
- `rd_rlogic()` leaves as they are the texts between quotes that look like operators, functions or fields (e.g. `[x] = 'a=b'`, `'if(x)'` or `'[age]'`): they were translated, and the comparison was wrong or stopped with an error. Quotes and backslashes inside a text can be escaped as in R (e.g. `'O\'Brien'`), and two apostrophes inside a text are kept (e.g. `"L''Z"` was compared with `"LNAZ"`).
- `rd_dictionary()` and `rd_transform()` write the labels with an apostrophe between double quotes (e.g. `[name]="O'Brien"`), and escape the labels with both quotes or with backslashes (e.g. `[name]='O\'Brien "A"'`): the logic was broken, and `rd_query()` could report queries of other options.
- `check_queries()` keeps one pending query when only its link (or another column that doesn't identify it) changed, also when it is a factor (e.g. a DAG with a new level); it reported it twice. The field is now part of the identity of a query (when both reports have it), so two fields with the same description are not mixed up, and queries without `Identifier` stop with an error.
- `rd_insert_na()` accepts filters with R functions (e.g. `abs(score) > 1`), parentheses (e.g. `(age > 30)` in longitudinal projects), objects of the session (also their components, e.g. `settings$threshold`), formulas of purrr (e.g. `purrr::map_lgl(age, ~ .x > 30)`), columns chosen with `.data[[field]]` and logical vectors of the session (e.g. `filter = "eligible"`); it took the functions as variables or didn't find the variable.
- `rd_dictionary()` and `rd_transform()` keep in REDCap syntax, with the codes of the options, the logic that uses as a number a variable already converted into a factor (e.g. `[score] >= 3`, `[a] + [b]`, or `[a] = '1' or [a] >= 3`): the R logic compared the factor as a number, with warnings such as "'>=' not meaningful for factors", and `rd_query()` lost the queries of those fields. They also read the options written without a space after the comma (e.g. `5,Always`): they warned "Expected 2 pieces" and the logic with that option was wrong.
- `rd_checkbox()` gives a name that the logic can use to an option whose new name already exists (e.g. `cb_one_1` instead of `cb_one.1`, which `rd_rlogic()` couldn't read, so `rd_query()` checked every record), also with the fields of the dictionary, and keeps apart two options with the same clean name (e.g. `A-B` and `A B`): both pointed to the second one.
- `check_queries()` tells apart queries whose texts have carriage returns (e.g. the description `Age\rRule` with the query `Check`, and `Age` with `Rule\rCheck`): they were taken for the same query. The same text in different encodings (e.g. UTF-8 and Latin-1) is the same query: it was reported as solved and new.
- `rd_recalculate()` and `rd_rlogic()` round very large numbers without turning them into `Inf` (e.g. `1e308` in `round()`, `rounddown()`, `roundup()` and in the comparison of `rd_recalculate()`), so their differences are detected, and keep the value with too many decimals (e.g. `round([a], 309)` gave NaN). With so many negative decimals that the scale is 0 (e.g. `round(12, -324)`), the result is 0. Without decimals (e.g. a blank field as the number of decimals), the result is blank.
- `round()` in `rd_rlogic()` and `rd_recalculate()` rounds down the values below the half (e.g. `round(1.49999999)` gave 2 and `round(0.49999999)` gave 1), and keeps rounding the halves away from zero. The value is compared with the half written with these decimals as R reads it (as the data), so the halves stored with the error of the binary numbers are halves (e.g. `round(2.675, 2)` is 2.68, while 2.675 is stored as 2.67499999999999982), and the other values aren't (e.g. `round(0.4999999999999999)` is 0).
- `roundup()` and `rounddown()` in `rd_rlogic()` keep the values that already have their decimals (e.g. `roundup(1.1, 2)` gave 1.11 and `rounddown(0.29, 2)` gave 0.28, because of the error of the binary numbers): the result is the smallest or largest number with these decimals, as R reads it (as the data), that isn't smaller or larger than the value. Large integers and very small numbers are kept (e.g. `2^50` or `rounddown(1e-24, 24)`), and a large number of decimals works with very small numbers (e.g. `roundup(1e-320, 309)` is 1e-309).
- `mod()` in `rd_rlogic()` gives the exact remainder of large numbers (e.g. `mod(2^54, 3)` gave 0 instead of 1, and with very different numbers, such as the largest number and 1, it could be infinite).
- `rd_checkbox()` only renames the options of the checkboxes in the branching logic and calculations, which keep the codes 0 and 1 (e.g. `[cb(1)] = '1'` is `[cb_one] = '1'`), and `rd_dictionary()` writes the labels when it translates the logic into R. The labels written by `rd_checkbox()` were read as codes when they looked like codes (e.g. `checkbox_labels = c("1", "0")` selected the unchecked options), and didn't work without `rd_factor()`.
- `rd_rlogic()` reads the text of a factor in `isnumber()` (it used the internal codes of the factor, so any value was a number), searches and replaces record by record when the text to find or the replacement is another field in `contains()`, `not_contain()`, `find()` and `replace_text()` (it used the value of the first record for all of them), keeps the empty texts `''` given to the text functions (e.g. `concat('', [text])` gave `NAabc`, and `replace_text([text], '-', '')` and `concat_ws('', ...)` failed), and `isblankormissingcode()` finds the empty texts that the text functions give (e.g. `isblankormissingcode(trim([text]))` of a field with spaces). An operation compared with an empty text (e.g. `trim([text]) = ''`) checks if its result is blank: it was always NA.
- `rd_rlogic()` reads a blank text to search, replace or separate as an empty text, as REDCap does: a blank field in `find()` stopped the whole evaluation, and in `replace_text()`, `contains()`, `not_contain()`, `starts_with()` and `ends_with()` it gave NA. An empty text to search leaves the text as it is in `replace_text()`, and `concat_ws()` uses the separator of each record (it used the first one for all of them).
- `rd_rlogic()` only reads the units of `datediff()` in its arguments: the last text of another function that is a unit (e.g. `if([sex] = '1', 'f', 'm')` or `ends_with([t], 's')`) was read as one and the logic couldn't be evaluated. It also accepts spaces around the unit (e.g. `datediff([a], [b], 'y' )`) and `false` as its last argument (e.g. `datediff([a], [b], 'd', false)` couldn't be evaluated).
- `rd_rlogic()` (and so `rd_recalculate()` and `rd_query()`) reads the dates of each `datediff()` with its own date format: with several formats in the same logic, all the dates were read with the first one (e.g. `datediff('01-02-2024', '02-02-2024', 'd', 'dmy') + datediff('02-01-2024', '02-02-2024', 'd', 'mdy')` gave 32 instead of 2). `'today'`, `'now'` and the dates are only read as dates in `datediff()` and in the comparisons of date fields (e.g. `[visit] < 'today'`): the texts of other functions were changed (e.g. `concat('today', ':', [text])` wrote the current date and `contains([text], 'now')` didn't find the text `now`).
- `rd_rlogic()` reads the functions written with spaces before their parenthesis (e.g. `if  (`, `datediff  (` or `year ([date])`): some of them couldn't be evaluated.
- `rd_rlogic()` reads the empty texts of the fields as blank values (e.g. in data modified after `redcap_data()`), so `[text] = ''`, `'' = [text]` and `isblankormissingcode([text])` give the same result, and works with logic without variables (e.g. `1` or `upper('abc')`) in data without rows.
- `rd_rlogic()` (and so `rd_recalculate()` and `rd_query()`) keeps the values of other events apart from the fields of the data: a field called like its auxiliary column (e.g. `value__base_arm_1` in `[visit_arm_1][value] - [base_arm_1][value] + [value__base_arm_1]`) was overwritten, and a field used with and without event (e.g. `[base_arm_1][value] + [value]`) took the value of the other event in every row. It also reads the values of other events when the record identifier isn't `record_id` (e.g. `study_id`): it stopped with an error, and `rd_recalculate()` left the field without recalculating.
- `rd_rlogic()` keeps the dates chosen by `if()` and the dates of `min()` and `max()` (e.g. `year(if([flag] = 1, [a], [b]))` and `year(min('', [a]))` gave 1970, and `month()`, `day()` and `datediff()` were also wrong), returns dates in longitudinal projects as in the others (`eval` and `eval_data` gave numbers), and reads the dates between parentheses in `datediff()` (e.g. `datediff(('01-02-2024'), '02-02-2024', 'd', 'dmy')` gave NA).
- `rd_rlogic()` compares with an empty text the functions translated with several instructions (e.g. `concat_ws([sep], [a], [b]) <> ''` or `roundup([n], 1) = ''`): the logic couldn't be evaluated.
- `rd_query()` translates the branching logic without fields written in REDCap syntax (e.g. `1 = 1` or `sum(1, 1) = 2`): it stopped with an error.
- `rd_query()` applies the branching logic translated into R by `rd_dictionary()` and `rd_transform()` that uses `contains()`, `not_contain()`, `find()`, `replace_text()`, `concat_ws()`, `roundup()` or `rounddown()`: it was read as REDCap logic and not applied, so the records where the field isn't shown were also checked.
- `rd_query()` keeps the texts between quotes and the raw strings of R of the expression (e.g. `r"(a > b)"`), and the values of the variable, in the messages of the queries: they were changed (e.g. `x == 'a>b'` was described as equal to `'agreater than b'`, `x == 'x'` as equal to `''`, and the value `"A  B"` was written `A B`), so two expressions with texts that became the same gave a single query, and `check_queries()` kept as `Pending` a query whose value had changed.
- `check_queries()` marks as `Miscorrected` only the new queries of a record and field (with the same `Identifier`, `Event`, `Repetition`, `Field` and `Description`) that had a query in the old report: several new queries of the same field were `Miscorrected` without any query in the old report. It also keeps the columns of the reports called `comp`, `comp2`, `n` or `cod`, which were replaced by its internal columns or removed.
- `rd_query()` applies the branching logic in R with double brackets (e.g. `data[['value']] == 0`) or with comments (e.g. `data$value == 0 # [note]`): it was read as REDCap logic and not applied.
- `rd_rlogic()` writes between backticks the fields whose name isn't a valid name in R (e.g. `if`, `in`, `TRUE` or `_x`, written as `` data$`if` ``): the logic couldn't be evaluated, so `rd_query()` didn't apply the branching logic and `rd_recalculate()` didn't recalculate the field. `rd_factor()` also finds these fields in the logic already translated to R.
- `rd_rlogic()` also translates the `<>` and `!=` inside the value of another `<>` (e.g. `[a] <> 0 + if([b] <> 1, 1, 0)`): a blank value gave NA instead of being different from any value.
- `rd_split()` keeps the rows of the forms that aren't repeating when the instrument of `redcap_repeat_instrument` is an empty text (e.g. read from a CSV file), and the normal rows of a form that only repeats in some events (they were removed).
- `rd_dictionary()` and `rd_checkbox()` don't change a comparison whose value is an operation (e.g. `[cb(1)] = 0 + 1` became `[cb_one]='No' + 1`), and `rd_dictionary()` keeps the logic of other events with the codes of the options, which `rd_rlogic()` and `rd_query()` read as codes: with labels that are codes of other options (e.g. code 1 with label `2`), the logic selected the other option.
- `rd_rlogic()` compares with `<>` and `!=` the whole operation of the value (e.g. `[a] <> 0 + 1` was `!(data$a %in% 0) + 1`), and reads as a code a label that is also a code.
- `rd_split()` works with repeating instruments when the data doesn't have the factor version of `redcap_repeat_instrument` (e.g. data that doesn't come from `redcap_data()`): it stopped with an error.
- `rd_dictionary()` and `rd_checkbox()` write the labels in the logic in one reading of it: a label that is the code of another option (e.g. code 1 with label `2`) was changed again, and the logic selected the other option. They also keep the texts between quotes (e.g. `[t] = '[cb(1)]'`) and read whole numbers (`[cb(1)] = 1.0` became `[cb_one]='Yes'.0`, and `[cb(2)] = 10` became `[cb_two]='Yes'0`).
- `rd_rlogic()` evaluates with the codes a variable that is a factor when the logic compares it with a label and also uses it as a number (e.g. `[score] = '2' or [score] + 1 >= 4`): the operation gave NA.
- `rd_recalculate()` leaves blank, as REDCap, the calculations without a numeric result (e.g. a division by zero): they were `Inf`, `-Inf` or `NaN`, and were reported as different from REDCap.
- `rd_split(wide = TRUE)` keeps the fields called `id`: it used this name for the number of each row of a record, which mixed records or removed the field.
- `rd_delete_vars()` removes from the dictionary the variables of `pattern` without distinguishing upper and lower case, as in the data (they were left in the dictionary), and accepts a variable together with its factor version or its column of Missing Data Codes in `vars` (e.g. `vars = c("status", "status.factor")` stopped with an error).
- `rd_export()` writes the links of a factor column (it wrote the internal codes of the factor, also as the destination of the links), and the dates and datetimes keep their format when the cells are centered (they were shown as numbers, e.g. `45351`).
- `rd_split()` keeps in every dataset a record identifier with another name than `record_id` (e.g. `subject` or `study_id`): some datasets didn't have it, and `wide = TRUE` stopped with an error. With `wide = TRUE`, the columns of a form with only one variable keep its name (e.g. `weight_1` instead of `1`).
- `rd_checkbox()`, `rd_dictionary()` and `rd_transform()` keep working the logic that uses the options of a checkbox as numbers (e.g. `[cb(1)] + [cb(2)] >= 2` or `sum([cb(1)], [cb(2)])`): `rd_checkbox()` only renames the options in it, with the codes 0 and 1, and `rd_dictionary()` keeps it in REDCap syntax. The logic was left half translated, or written as R logic of the factors, and `rd_query()` checked every record or none.
- `rd_rlogic()` translates `datediff()` of two time fields (e.g. `datediff([start], [end], 'h', 'hhmm', true)`), as REDCap does: it stopped with an error, or gave NA.
- `rd_query()` doesn't translate again a branching logic already written in R with brackets inside a text (e.g. `data$status == '[age]'`): it was taken for REDCap logic and the queries were wrong.
- `rd_checkbox()` creates the `.factor` version of a checkbox option that doesn't have it when the other options have it, so that every option gets the same labels, and it only warns that the checkboxes are already factors when they are.
- `rd_delete_vars()` removes from the dictionary a checkbox whose options were all removed, and never removes the record identifier, whatever its name (it only protected `record_id`).
- `rd_missplot()` doesn't plot the record identifier when it isn't called `record_id`, and `max_vars` must be a whole number or `Inf` (all the variables): `NaN` gave an R error and decimals were accepted.
- `rd_dates()` (and so `rd_transform()`) converts the dates one by one. A value that isn't a date (e.g. a Missing Data Code that wasn't removed) stopped it with an error, or turned the next dates into NA without a warning when it wasn't the first value; now it is set to NA and a warning lists the fields and the values. A value is a date only when it follows the whole format: a time out of range or extra text (e.g. `2024-02-29 99:00:00`, `2024-02-29 12:30:60`, `2024-02-29 24:00` or `2024-02-29 extra`) are not dates. Datetimes with and without seconds in the same field keep their seconds.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()` and `rd_dictionary()`) translates `and` and `or` written at the end or the start of a line, with Windows line breaks or tabs, in capitals (`AND`, `OR`) or next to a parenthesis: the logic couldn't be evaluated. It also leaves as they are the words `and` and `or` inside a text (e.g. `[x] = 'yes or no'`), which were translated.
- Fields with the name of an object used inside the functions work: with a field called `event`, `rd_query()` with the `event` argument found no queries; with a field called `data`, `rd_query()`, `rd_event()`, `rd_checkbox()` and `rd_transform()` stopped with an error; `rd_rlogic()` couldn't evaluate a logic that used a field called `datediff`; and `redcap_data()` left empty a field called `missing_codes`.
- `redcap_data()` with the exported files: the labels of the options with line breaks lost their spaces (e.g. "oralcorticosteroids(e.g."), because the R script exported by REDCap removes the line breaks. They take the label of the dictionary, as with the API.
- `redcap_data()` reads the paths between quotes, as Windows copies them with "Copy as path" (e.g. `'"C:\\Users\\...\\project.r"'`). It said that the file was not an R file.
- `redcap_data()` couldn't read the R script exported by REDCap of the projects that repeat events but not instruments: REDCap writes the options of the repeated instrument as `c("", "")` and the script fails in R (`invalid 'labels'`).
- `redcap_data()` with the API: an empty `redcap_data_access_group.factor` or `redcap_repeat_instrument.factor` (e.g. data access groups or repeating instruments without records yet) was logical instead of a factor, as with the exported files.
- `redcap_data()`: the text of the dictionary has no spaces or line breaks at the beginning or at the end (e.g. in the field annotations, notes or section headers), with the exported files and with the API. They differed: REDCapR only removes them from the text without quotes.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_dictionary()` and the branching logic of the other functions) gave the value of the first row to every row when the condition of an `if()` didn't use any variable (e.g. `if(1 < 2, [a], 0)`). Each row now gets its own value, as in REDCap.
- `rd_event()` stopped with "events do not exist in the dataset" for an event of the project that has no records yet (e.g. a new visit, or the events of an arm without records). All the records of its arm are now reported as missing it (none for an arm without records).
- `rd_query()`, `rd_event()` and `check_queries()` wrote round record identifiers in scientific notation in the query codes and links (e.g. `1e+05-1` for the record `100000`), and `check_queries()` sorted them wrongly.
- `rd_query()` described the expressions with `<=` and `>=` as "less than =" and "greater than =" in the queries and in the report. They are now "less than or equal to" and "greater than or equal to".
- `rd_event()` failed when `addTo` received the output of `rd_query()` or `rd_event()`.
- `rd_split(wide = TRUE)` failed when it was called as `REDCapDM::rd_split()` without loading the package.
- The `filter` and `expression` of `rd_query()`, and the `filter` of `rd_event()` and `rd_insert_na()`, can use objects created inside a function or a loop: before, only the objects of the global environment were found. They are no longer run with `eval(parse())`, so an "x" inside a text value of `expression` (e.g. `x == 'x'`) is no longer replaced by the variable, and filters can't use the internal objects of the functions. As before, several conditions can be separated by commas and `data$var` refers to the data.
- `rd_query()` explains the mistake when one of several filters (one per variable) can't be evaluated.
- `rd_rlogic()`, `rd_recalculate()` and `rd_query()` gave wrong results when the variables had already been converted to factors with `rd_factor()`.
- `rd_recalculate()` applied twice duplicated the `_recalc` fields.
- `rd_checkbox()` and `rd_recalculate()` gave wrong results with tibbles, and `rd_checkbox()` couldn't name the options of data without variable labels.
- `rd_insert_na()` failed in non-longitudinal projects.
- `rd_query(report_zeros = TRUE)` failed when every variable had queries.
- `rd_query()`, `rd_event()` and `check_queries()` turned alphanumeric record identifiers into `NA`; identifiers are now sorted naturally.
- `rd_event()` assigned the DAG of the first record to every query, failed without `redcap_event_name.factor` and could pair events with the wrong description. It now explains that it needs a longitudinal project.
- `rd_transform()` failed when the record identifier was not called `record_id`.
- `rd_recalculate()` reported a calculated field as different from REDCap when the values were equal but the variable had a label.
- `rd_split(wide = TRUE)` returned several rows per record for repeated instruments in non-longitudinal projects.
- `rd_query()` with several variables checked each variable also in the events of the other ones.
- `rd_event()` reported records of other arms as missing the event in projects with several arms.
- `rd_split()` failed with descriptive fields in the dictionary.
- `redcap_data()` no longer warns about an incomplete final line when reading the event file.
- Fixed a stray "FALSE" at the end of a `rd_query()` warning.
- `rd_rlogic()` translated `datediff()` differently from REDCap: it now returns the absolute difference unless the last argument is `true`, accepts any date format (`"ymd"`, `"mdy"`...), uses `"M"` for months and `"m"` for minutes, and understands `"today"`.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()` and `rd_dictionary()`) translated several REDCap functions wrongly: `min()` and `max()` returned a single value for the whole dataset, `sum()` didn't ignore blank values, `rounddown(x, d)` rounded instead of rounding down and several `rounddown()` in the same logic were mistranslated. `mean()`, `median()`, `roundup()`, `mod()`, `exponential()`, `isnumber()`, `contains()` and `length()` are now translated too.
- `rd_query()` ignored the branching logic that uses a variable of another event (e.g. `[baseline_arm_1][sex] = '2'`) when the variable was checked alone, and checked every row when no row met the branching logic.
- `rd_delete_vars()` could remove `record_id`, also through `pattern`.
- `rd_export()` reported success when the folder of the file didn't exist.
- `rd_event()` failed with event labels.
- `rd_dictionary()` failed after `rd_factor()` when the only factors were yes/no or true/false fields.
- `redcap_data(rm_mdc = FALSE)`, `rd_split()` and `rd_missplot()` failed or warned in projects without records.
- `redcap_data()` no longer runs the `library(Hmisc)` and `graphics.off()` lines of the R script exported by REDCap: it failed when Hmisc wasn't installed, attached Hmisc (masking e.g. `dplyr::summarize()`) and closed the open plots.
- knitr and lubridate are now declared dependencies: several functions failed when knitr wasn't installed, and the translation of `datediff()` needed lubridate. kableExtra is optional: without it the HTML reports are not styled.
- `rd_query()` failed in longitudinal projects with several DAGs when a variable had no queries.
- `rd_query()` didn't apply the branching logic of a checkbox field when one of its options was queried (e.g. `variables = "symptoms___1"`), and after `rd_checkbox(checkbox_names = FALSE)` it reported the logic as applied without applying it.
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()`, `rd_dictionary()` and `rd_checkbox()`) didn't translate the checkbox options whose code has letters or is negative (e.g. `[symptoms(A)]` or `[symptoms(-1)]`).
- `rd_checkbox()` reversed the logic `[field(code)] <> '1'` when it renamed the checkbox options in the dictionary, and didn't translate the codes with letters, the negative codes and the values in double quotes.
- `rd_rlogic()` counted calendar years and months in `datediff()`: as REDCap, it now counts years of 365.2425 days and months of 30.44 days (e.g. 365 days are 0.9993 years, so `rounddown(datediff(..., 'y'), 0)` gives 0).
- `redcap_data()` with the API: the text fields and the record identifier lost their labels, the yes/no, true/false and form status fields didn't get their factor version (so `rd_factor()` and `rd_transform()` didn't convert them, as they do with the exported files), and the fields of the forms without any event were not removed although the warning said so.
- `redcap_data()` with the exported files: the text fields that look like numbers lost their format when the R script of REDCap read them (e.g. a postal code or phone `"0034"` was read as `34`, and `"1.50"` as `1.5`); they now keep the text exported by REDCap. The record identifier also keeps its label when it has another name.
- `redcap_data()` with the API: the text fields that look like numbers lost their format too (e.g. a phone `"+34600111222"` became `34600111222`, and `"1.50"` became `1.5`). As with the exported files, they now keep the text from REDCap.
- `rd_rlogic()` failed with the operator `!=`, which REDCap accepts as `<>`.
- `rd_transform()` failed with `wide = FALSE` when `final_format` was not `"by_form"`.
- `rd_rlogic()` (and so `rd_query()`, `rd_checkbox()` and `rd_recalculate()`) treated `[x] <> '1'` as unknown when `x` was blank. As in REDCap, a blank value is different from any value, so the field with this branching logic is shown and checked.
- `rd_dictionary()` (and so `rd_transform()`) didn't translate the codes of the yes/no and true/false fields converted with `rd_factor()` (e.g. `[smoker] = '1'` must be `[smoker] = 'Yes'`), turned `[x] > 1` of a factor into `[x] <> 'label'`, and wrote the logic with variables of other events (e.g. `[baseline_arm_1][sex] = '2'`) as R logic of each row, which lost the event. This logic is now kept in REDCap syntax and listed in the results.
- `rd_query()` after `rd_transform()` didn't apply the branching logic that uses variables of other events, and checked every row of a field whose branching logic is never met (without `rd_transform()` there is nothing to check, as in REDCap).
- Grouped data (`dplyr::group_by()` without `ungroup()`) and dictionaries with factor columns (e.g. read with `stringsAsFactors = TRUE`) made several functions fail, and `rd_recalculate()` skipped calculated fields without warning. All functions now use them as plain data frames.
- `rd_checkbox(na_logic = "eval")` and `na_logic = "missing"` failed when the branching logic of a checkbox could not be evaluated, or applied to it the logic of the previous checkbox. Its options are now left unchanged, with a warning.
- `rd_rlogic()` returned `NA` in `datediff()` with `"now"` or with dates with time, translated the names of functions written inside a text (e.g. `[t] = 'sum(1,2)'`) and gave `mod()` of negative numbers with the sign of the divisor (REDCap gives `mod(-7, 3) = -1`).
- `rd_query()` removed the text between `<` and `>` of the labels (e.g. `Age (<18 or >90 years)` became `Age (90 years)`) and showed `&amp;` in the HTML report. `redcap_data()` and `rd_missplot()` also keep these characters, and only remove the HTML tags of the labels. `redcap_data()` also removes the HTML of the variable labels of the data (before, only of the dictionary), and no longer adds spaces where a tag was inside a word or a parenthesis (e.g. `Name (<i>full</i>)` is now `Name (full)`).
- `rd_rlogic()` (and so `rd_recalculate()`, `rd_query()` and `rd_dictionary()`) gave wrong results with negative numbers: `[a]<-3` (without spaces) was run as an assignment in R, and `[a] < '-3'` (with quotes) compared the values as text.
- `rd_query()`, `rd_event()` and `check_queries()` turned record identifiers with leading zeros into numbers (e.g. `"007"` became `7`, and its code `7-1`), so they didn't match the record in REDCap. They are now kept as text; identifiers that are plain numbers are still returned as numbers. The type is decided with all the records of the project, so all the reports of a project have the same type of `Identifier` (and can be combined with e.g. `dplyr::bind_rows()`), whichever records have queries.
- `rd_dates()`, `rd_recalculate()` and `rd_transform()` failed when the data didn't have every date field of the dictionary (e.g. with a selection of columns).
- `check_queries()` failed when neither report had queries (e.g. a project without errors in both reviews).
- `rd_query()` failed with an internal error when `link` had `event_id = NULL` (e.g. a template for longitudinal projects used in a classic project). Elements given as `NULL` are now ignored.
- `rd_factor()` (and so `rd_transform()`) stopped when `exclude` had every variable that can be converted to a factor (e.g. a project with a single radio field that should stay numeric). As when there are no such variables, it now warns and goes on.
- `rd_checkbox()` failed with projects read from the API when no checkbox had branching logic: the dictionary of the API has `NA` in its empty cells. `redcap_data()` now gives the same dictionary from the API as from the exported files, with empty texts in its empty cells.
- `rd_query()` without `event` failed in longitudinal projects when an event of the variable had no records yet (e.g. a follow-up visit that no one has reached, or an arm without records). It now checks the events with data.
- `rd_export()` reported success when `path` was a folder or `sheet_name` had one of `/ \ ? * : [ ]`, which Excel doesn't accept, and failed with an unclear error with more than 31 characters. It now stops and explains the problem.
- `rd_export()` accepts the data frame of queries as its first argument (e.g. `rd_export(report$queries)`): before, it said that there were no queries to export.
- `rd_transform()` keeps the record identifier when its form is not designated in any event of `event_form`: it was removed from the data and the dictionary with the other variables of the forms without events.
- The functions take the record identifier from the dictionary when it isn't called `record_id` (as `redcap_data()` calls it): it is the first field of the dictionary (e.g. `subject_id`), whatever its position in the data. With another column first (e.g. `age`, or a column added for an analysis that isn't in the dictionary), `rd_event()` and `rd_query()` reported the values of that column as identifiers, `rd_rlogic()` (and so `rd_recalculate()`) used it to match the events of each record, `rd_missplot()` labelled the records with it and plotted the identifier as a variable, and `rd_delete_vars(forms = ...)` removed the identifier. The first column is the identifier only when the first field of the dictionary isn't in the data.
- `rd_query()` and `rd_event()` keep the name of an identifier that isn't called `record_id`, so their `filter` can use it (e.g. `subject_id == '003'` or `.data$subject_id == '003'`): they renamed it `record_id`, so the filter failed, or used an object of the session with that name and found no queries.
- `rd_factor()` converts to factors the `.factor` columns that are text (e.g. after modifying the data), with the levels in the order of the options of the dictionary: the variables kept the labels as text.
- `rd_split()` only takes as factor versions the columns of fields of the dictionary, and as checkbox options the columns of its checkbox fields: other columns (e.g. `riskfactor`, `risk_factor`, `ghost___1` when `ghost` isn't a field, or `age___1` when `age` isn't a checkbox) were left out of the result without notice, and with `age___1` the field `age` was left out. Now they stop the function, as the other columns that aren't in the dictionary.
- `rd_checkbox()` stops with an error when the checkboxes were already transformed with `checkbox_names = FALSE` (their options keep their names, but their values are the labels and each option is a field of the dictionary): running it again on its result turned all their values into `NA` without notice (also the labels of their `.factor` versions), or inverted the answers with labels that look like codes (e.g. `checkbox_labels = c("1", "0")`).
- `rd_split()` takes the `.factor` versions of the checkbox options that `rd_checkbox(checkbox_names = FALSE)` keeps as fields of the dictionary (e.g. `cb___1.factor`): they stopped the function as unknown columns.
- `rd_checkbox()` (and so `rd_transform()`) takes as factor versions only the columns that end in `.factor`: an option with the code `factor` (e.g. `cb___factor`) was taken for one, so the values of the checkbox became `NA` or the function stopped with "'by' must specify a uniquely valid column".
- `rd_insert_na()` takes the record identifier as present in every event, whatever its form in the dictionary: a filter with the identifier (e.g. `record_id == '003'`, or `record_id == '003' & value > 6`) stopped saying that the variable and the filter didn't share any event, or that the variables of the filter belonged to different events.
- `rd_recalculate()` doesn't overwrite the fields of the project whose names end in `_recalc` (e.g. a field `q_recalc` when `q` is recalculated), nor a column of the data with that name: the recalculated values are saved in `q_recalc_1` (or `_2`, ...), with a warning. A calculated field of the project with such a name is now recalculated: it was skipped as if `rd_recalculate()` had added it. The fields added by `rd_recalculate()` are recognised because they are calculated fields with the same calculation as the original field and its label followed by `(Recalculated)` (or `(Recalculate)`, as in previous versions); without the column `field_label` in the dictionary, by the type and the calculation, so running the function again updates them instead of adding new ones.
- `rd_missplot()` keeps apart the events with the same name (e.g. `Baseline` in two arms): they were merged in the heatmap and in the record map. The plot adds their unique name (e.g. `Baseline (baseline_arm_1)`), also when the new name is the name of another event, and in `events` the shared name selects all of them. A field called `clean_event_name` gets its own missing values (it got the ones of the event names) and keeps its label, and every variable gets a different label also when the name added to a shared label gives another existing label (e.g. `Lab`, `Lab` and `Lab (a)`): the bars were merged or the plot failed with "factor level is duplicated".

## Clearer messages

- `redcap_data()` explains when the dictionary is not encoded in UTF-8 (e.g. a dictionary saved again with Excel as "CSV"), instead of failing with an internal error.
- The functions that use the event-form mapping explain an `event_form` that is not the one of `redcap_data()` (e.g. a number or a data frame without the columns `unique_event_name` and `form`), `rd_rlogic()` a `var` that is not in the dictionary of a longitudinal project and a logic that doesn't give one value per row, and `rd_query()` a `data` without columns. Before, they failed with internal R errors.
- `redcap_data()` explains when a file is not found, when the csv file read by the R script is not in its folder and when the event file is not the instrument-event mapping of the project. It warns when variables of the data are not in the dictionary and when the project has no records yet.
- `rd_query()` names the variables that don't exist (and how to check a checkbox), explains why an expression can't be evaluated, warns when a text variable is compared with a number and checks `negate`, `report_zeros`, `by_dag` and `add_to`.
- `rd_rlogic()` names the variables it can't find and shows the translated logic and the R error when it can't be evaluated.
- `rd_delete_vars()` warns when `pattern` matches no variable, `rd_insert_na()` when no row meets the filter, and `rd_factor()` and `rd_recalculate()` when `exclude` has unknown variables.
- `rd_event()` asks for the `event`, `rd_split()` explains why it can't split by event, `rd_transform()` asks for `delete_pattern` before splitting the data, `check_queries()` checks that it receives query reports, `rd_missplot()` checks `max_vars` and the number of rows, and `rd_checkbox()` checks `checkbox_labels`.
- Every function checks its arguments: a wrong value (e.g. `NULL`, `NA`, a vector instead of a single value or a text instead of TRUE/FALSE) stops with a message that names the argument and what it expects, instead of an internal R error.
- `rd_delete_vars()` never removes the REDCap columns of events and repetitions through `pattern`, and `rd_dates()` and `rd_rlogic()` work with a dataset without rows.
- Every function checks that the dictionary has the columns it needs, and `rd_split()` names the variables of the data that are not in the dictionary.
- Errors about wrong arguments and the informative messages use cli: they show the function that was called and the value received, and are formatted in the console. Errors, warnings and messages have the classes `REDCapDM_error` (`REDCapDM_error_argument` for wrong arguments), `REDCapDM_warning` and `REDCapDM_message`.
- The informative messages (e.g. the progress of `rd_transform()`) can be hidden with `options(REDCapDM.quiet = TRUE)`.
- `redcap_data()` and `rd_query()` report all their errors and warnings with cli. Problems with the files have the class `REDCapDM_error_file`, and problems with the REDCap API the class `REDCapDM_error_api`, which keeps the original error of the API as its parent.
- `rd_query()` informs with a message, instead of a warning, when it applies the branching logic automatically.
- All the other functions also report their errors and warnings with cli, naming the variables, forms and events involved. Logic that can't be translated or evaluated has the class `REDCapDM_error_logic` (with the translated logic and the R error), a dataset that doesn't match its dictionary the class `REDCapDM_error_data`, and a folder that doesn't exist in `rd_export()` the class `REDCapDM_error_file`. A filter of `rd_insert_na()` that can't be evaluated keeps the original R error as its parent.
- `redcap_data()` explains when the R script can't be run, ignores the empty rows of the dictionary (e.g. added by Excel) and warns when a field appears more than once in it.
- `rd_split()` and `rd_transform()` name the fields of the dictionary that are not in the data and, when whole instruments are missing (data exported with only some instruments), show how to remove them with `rd_delete_vars(forms = ...)`.
- `rd_query()` suggests an option of the checkbox that exists in the data (e.g. `symptoms___a`) when a checkbox field is given in `variables`.
- `rd_query()` no longer warns when one expression is given for several variables, the usual way to check them (it still warns when several expressions don't match the variables), and `rd_dates()` informs with a message, instead of a warning, that there are no dates to convert.
- The results of `rd_checkbox()` and `rd_transform()` describe the labels given in `checkbox_labels` and, with `checkbox_names = FALSE`, don't say that the checkboxes were renamed: they always said "No/Yes" and that the names were changed.
- `rd_recalculate()` shows `-` instead of a percentage (`NaN%`) in its summary when there are no calculated fields to recalculate (e.g. all of them in `exclude`) or none could be transcribed, and its last column is called "Fields with different recalculated values".
- Fixed typos in the reports of `rd_split()` and `rd_transform()` ("arrangement"), and in the label of the fields added by `rd_recalculate()` ("(Recalculated)").

## Documentation

- The vignette is split into four: getting started (`vignette("REDCapDM")`), missing data (`vignette("missing-data")`), data processing (`vignette("data-processing")`) and queries (`vignette("query-management")`).
- The reference of the website groups the functions by task, and the help of each function links to the related functions.
- New contributing guide, code of conduct and issue templates on GitHub. The README states the R version the package needs (4.1 or later).
- `rd_checkbox()`, `rd_dates()`, `rd_delete_vars()`, `rd_dictionary()`, `rd_export()`, `rd_factor()`, `rd_recalculate()` and `rd_split()` are now stable (their arguments and results won't change without a deprecation period). `rd_missplot()` remains experimental.
- The examples of `rd_insert_na()` and `rd_export()` can be run as they are.
- The screenshots of the queries vignette have alternative text, for screen readers.
- The help of `rd_dates()` explains that the datetimes written as text are read in UTC, whatever the time zone of the session (the time is kept as written in REDCap), and the help of `rd_recalculate()` that the label `(Recalculate)` of previous versions is also recognised.
- The help of `rd_split()` and `rd_transform()` explains that the data split by form or event is a tibble with the dataset of each form or event in the list-column `df` (e.g. `result$data$df[[1]]`), and the help of `rd_transform()` and the vignettes say which functions it runs: `rd_insert_na()` is applied separately.
- The help of `rd_missplot()` explains that `sort_by_missingness` and `max_vars` use the number of missing values (also when the plot shows percentages), and that the labels shared by several variables, which get the variable name, can be longer than `max_label_length`.

## Internal changes

- Snapshot tests keep the text of the printed reports and of the main messages, and new tests cover chained functions and less common arguments.
- Code cleanup following lintr: the conditions of `if()` use `&&` and `||`, and the query codes are numbered with `dplyr::row_number()`, which also works with zero rows.

# REDCapDM 1.0-1

## Bug fixes

- Fixed a bug in `rd_checkbox` that mis-evaluated the branching logic of certain checkboxes if they had previously been transformed with `rd_factor`

## New features

-  Added a `sep` argument to the `redcap_data()` function, allowing users to specify the field separator when reading CSV dictionary files.

# REDCapDM 1.0-0

## Bug fixes

- Addressed multiple bugs across various functions to enhance stability and reliability.

- Improved error handling to provide clearer messages when issues arise.

- Fixed inconsistencies in data transformation outputs for certain edge cases.

## Changes & New features

- The `rd_transform` function has been split into several smaller subprocess functions. This allows users to:

    - Apply each transformation step independently.

    - Inspect intermediate results at each stage.

    - Have precise control over every change made to their data.

- By exposing the individual steps as separate functions, users can now customize the data transformation pipeline to better fit their REDCap datasets.

- Updated function descriptions and examples illustrate how to use the subprocesses effectively, making it easier to integrate REDCapDM into data management workflows.

- Some older functions related to data transformation have now been deprecated. Users are encouraged to transition to the new subprocess functions for more flexibility and maintainability.

<br>
<br>

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

-   New error added to the *rd_query()* function when the logic of the filter was using incorrect logic.

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
-   New *checkbox_na* argument in *rd_transform()* to specify if the values of the checkbox will be converted to missing not only when the branching logic is missing but also when it's not fulfilled (if *TRUE*).
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
