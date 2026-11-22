#' Split a REDCap dataset by form or event
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Splits a REDCap dataset into separate datasets by **form** or **event** using the data dictionary. Supports both longitudinal and non-longitudinal projects and can return wide or long formats for repeated measures.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param which Optional. A single form or event to extract. If not provided, all forms or events are returned.
#' @param by Character. Criteria to split the dataset: `"form"` (default) or `"event"`.
#' @param wide Logical. If `TRUE` (for form-based splits), repeated instances are returned in wide format. Defaults to `FALSE`.
#'
#' @details
#' * Handles checkbox variables and REDCap default variables (`_complete`, `_timestamp`) appropriately.
#' * Columns of the data that aren't fields of the dictionary, nor the `.factor` or `_mdc` versions of its fields or the options of its checkbox fields (or the `.factor` version of the `_complete` field of a form), stop the function with an error that names them.
#' * For form-based splits in longitudinal projects, uses `event_form` to map variables to events.
#' * Wide format expands repeated instances into multiple columns per record.
#' * Filtering by `which` allows extracting a single form or event.
#' * Projects with repeated instruments are handled by filtering on the `redcap_repeat_instrument` variable.
#'
#' @return Depending on `which` and `wide`:
#' \describe{
#'   \item{data}{Without `which`, a tibble with one row per form (columns `form`, `events` in longitudinal projects, and `df`) or per event (columns `events` and `df`): the dataset of each form or event is in the list-column `df` (e.g. `result$data$df[[1]]`, or `result$data$df[result$data$form == "demographics"][[1]]`). With `which`, the `data.frame` of that form or event.}
#'   \item{dictionary}{The original REDCap dictionary.}
#'   \item{event_form}{The original event-form mapping (if applicable).}
#'   \item{results}{A summary message of the splitting operation.}
#' }
#'
#' @examples
#' # Split by form and return wide format
#' result <- covican |>
#'   rd_split(by = "form", wide = TRUE)
#'
#' print(result)
#'
#' # Split by event (long format)
#' result <- covican |>
#'   rd_split(by = "event")
#'
#' print(result)
#'
#' @family process functions
#' @export

rd_split <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, which = NULL, by = "form", wide = FALSE) {

  results <- NULL

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) || is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = c("field_name", "form_name"), dic_rows = TRUE)
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_strings(which, "which")
  check_flag(wide, "wide")

  # Error: invalid `by`
  if (!isTRUE(by %in% c("form", "event"))) {
    rd_abort("Invalid {.arg by} argument. Please specify either {.val form} or {.val event}.", class = "REDCapDM_error_argument")
  }

  # Error: splitting by event needs a longitudinal project and its event-form correspondence
  if (by == "event" && !"redcap_event_name" %in% names(data)) {
    rd_abort("The data has no events: use {.code by = 'form'} to split a project without events.")
  }

  if (by == "event" && is.null(event_form)) {
    rd_abort("The event-form correspondence ({.arg event_form}) is required to split the data by event.")
  }

  # Extract labels from the data to reapply later
  labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })

  # Check if the project has repeated instruments (an empty text, e.g. read from a CSV file, is no instrument)
  if ("redcap_repeat_instrument" %in% names(data)) {
    repeat_instrument <- any(!as.character(data$redcap_repeat_instrument) %in% c(NA, ""))
  } else {
    repeat_instrument <- FALSE
  }

  # The record identifier: record_id or else the first field of the dictionary (REDCap's record ID, e.g. `subject` or
  # `study_id`), see id_field(). Every dataset of the split keeps it
  id_var <- id_field(data, dic)

  # Identify basic REDCap variables present in the dataset
  basic_redcap_vars <- c(id_var, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "redcap_data_access_group", "redcap_event_name.factor", "redcap_repeat_instrument.factor", "redcap_data_access_group.factor", "redcap_survey_identifier")

  basic_redcap_vars <- intersect(basic_redcap_vars, names(data))

  # Check functions needed before splitting
  actions <- character()

  # Check for missing variables in the dataset compared to the dictionary
  vars_more <- setdiff(dic$field_name[!dic$field_type %in% "descriptive"], names(data)) # Descriptive fields are never exported with the data

  # Handle cases where variables in the dictionary are missing in the dataset
  if (length(vars_more) > 0) {

    # Special handling for "checkbox" variables
    check_vars <- dic |>
      dplyr::filter(.data$field_name %in% vars_more & .data$field_type == "checkbox") |>
      dplyr::pull(.data$field_name)

    other_check_vars <- setdiff(vars_more, check_vars)

    if (length(other_check_vars) > 0) {
      shown_vars <- utils::head(other_check_vars, 10)

      # Forms with none of their fields in the data (e.g. data exported with only some instruments)
      form_fields <- dic[!dic$field_type %in% "descriptive" & dic$field_name != id_var, c("field_name", "form_name")]
      missing_forms <- unique(form_fields$form_name[form_fields$field_name %in% other_check_vars])
      missing_forms <- missing_forms[vapply(missing_forms, function(f) all(form_fields$field_name[form_fields$form_name == f] %in% vars_more), logical(1))]
      forms_code <- paste0("rd_delete_vars(project, forms = ", paste(deparse(missing_forms), collapse = ""), ")")

      rd_abort(
        c(
          "{length(other_check_vars)} variable{?s} of the dictionary {?is/are} not in the data: {.var {shown_vars}}.",
          "i" = if (length(missing_forms) > 0) "If the data was exported without some instruments, remove them first: {.code {forms_code}}.",
          "i" = "Please ensure that the dictionary matches the dataset. Transformation has been halted."
        ),
        class = "REDCapDM_error_data"
      )
    }

  }

  # Check for extra variables in the dataset compared to the dictionary
  vars_less <- setdiff(names(data), dic$field_name) |>
    setdiff(basic_redcap_vars) # Exclude REDCap default variables not found in the dictionary

  # Handle cases where there are variables in the data that cannot be found in the dictionary
  if (length(vars_less) > 0) {

    patt_vars <- grep("_complete$|_timestamp$", vars_less, value = TRUE)

    # Special handling for "_complete" and "_timestamp" patterns
    if (length(patt_vars) > 0) {
      mss <- dplyr::case_when(
        any(grepl("_complete$", vars_less)) & any(grepl("_timestamp$", vars_less)) ~ "c('_complete', '_timestamp')",
        any(grepl("_complete$", vars_less)) ~ "'_complete'",
        any(grepl("_timestamp$", vars_less)) ~ "'_timestamp'"
      )

      # actions <- c(actions, stringr::str_glue("Transformation halted. Default REDCap variables ({mss}) are present in the dataset but not in the dictionary.\nTo proceed, use the `rd_delete_vars` function with `pattern = {mss}` to remove these variables before continuing."))
      actions <- c(actions, stringr::str_glue("Default REDCap variables ({mss}) detected in the dataset. Please, run: rd_delete_vars(..., pattern = {mss})"))
    }

    # Checkbox fields of the dictionary: only they have options `<field>___<code>` (without the type of the fields,
    # every field of the dictionary)
    checkbox_fields <- if ("field_type" %in% names(dic)) dic$field_name[dic$field_type %in% "checkbox"] else dic$field_name
    is_option <- function(x) grepl("___", x) & sub("___.*$", "", x) %in% checkbox_fields

    # Factor versions of the fields of the dictionary (and of their checkbox options) and of the status field of the
    # forms (`<form>_complete`, also of the forms that redcap_data() removed from the dictionary because they are in no
    # event)
    fact_base <- sub("\\.factor$", "", vars_less)
    # (an exact field of the dictionary first, e.g. `cb___1` after rd_checkbox(checkbox_names = FALSE))
    fact_vars <- vars_less[grepl("\\.factor$", vars_less) &
      (fact_base %in% dic$field_name | is_option(fact_base) | grepl("_complete\\.factor$", vars_less))]
    
    # Columns with the Missing Data Codes kept by redcap_data(rm_mdc = FALSE)
    fact_vars <- c(fact_vars, intersect(vars_less, mdc_columns(names(data), dic$field_name)))

    # Checkbox vars already identified in the previous step (vars_more): options of a checkbox field of the dictionary
    less_check <- vars_less[is_option(vars_less)]

    other_less_vars <- setdiff(vars_less, c(patt_vars, fact_vars, less_check))

    if (length(other_less_vars) > 0) {
      # General message for other cases
      shown_vars <- utils::head(other_less_vars, 10)
      rd_abort(
        c("There are extra variables in the dataset not found in the dictionary: {.var {shown_vars}}.", "i" = "Check that the dictionary is the one of this project. Transformation halted."),
        class = "REDCapDM_error_data"
      )
    }
  }


  # single stop with full list (if anything found)
  if (length(actions) > 0) {
    rd_abort(
      c(
        "Detected issues that must be resolved BEFORE running {.fn rd_split}:",
        stats::setNames(sprintf("%d. {actions[%d]}", seq_along(actions), seq_along(actions)), rep("x", length(actions))),
        "i" = "Suggested order: {.code list(data, dic, ...) |> rd_delete_vars(pattern = ...) |> rd_split(...)}"
      ),
      class = "REDCapDM_error_data"
    )
  }

  if (by == "form") {
    # Handle splitting by form
    form <- unique(dic$form_name)
    longitudinal <- "redcap_event_name" %in% names(data)

    if (longitudinal && is.null(event_form)) {
      rd_abort("The event-form correspondence is required to split the data by form in a longitudinal project. Please provide the {.arg event_form} argument to proceed.")
    }

    if (longitudinal) {
      ndata <- tibble::tibble("form" = form) |>
        dplyr::mutate(
          events = purrr::map(.data$form, ~ event_form$unique_event_name[event_form$form == .x]),
          vars = purrr::map(.data$form, ~{
            posible_vars <- dic$field_name[dic$form_name == .x]
            posible_vars <- c(posible_vars, paste0(posible_vars, ".factor"), paste0(posible_vars, "_mdc"))

            check_df <- tibble::tibble(check_data = names(data)[grep("___", names(data))]) |>
              dplyr::mutate(check_dic = gsub("___.*$", "", check_data)) |>
              dplyr::distinct_all() |>
              dplyr::filter(check_dic %in% posible_vars)

            if (nrow(check_df) > 0) {
              vars_dic <- c(unique(check_df$check_dic), paste0(unique(check_df$check_dic), ".factor"))
              posible_vars <- setdiff(posible_vars, vars_dic)
              posible_vars <- c(posible_vars, check_df$check_data)
            }

            intersect(names(data), posible_vars)
          }
          )
        ) |>
        dplyr::mutate(df = purrr::map2(
          .data$events,
          .data$vars,
          ~ data |>
            dplyr::filter(redcap_event_name %in% .x) |>
            dplyr::select(dplyr::any_of(unique(
              c(basic_redcap_vars, .y)
            )))
        ))
    } else {
      ndata <- tibble::tibble("form" = form) |>
        dplyr::mutate(
          vars = purrr::map(.data$form, ~ {
            posible_vars <- dic$field_name[dic$form_name == .x]
            posible_vars <- c(posible_vars, paste0(posible_vars, ".factor"), paste0(posible_vars, "_mdc"))

            check_df <- tibble::tibble(check_data = names(data)[grep("___", names(data))]) |>
              dplyr::mutate(check_dic = gsub("___.*$", "", check_data)) |>
              dplyr::distinct_all() |>
              dplyr::filter(check_dic %in% posible_vars)

            if (nrow(check_df) > 0) {
              vars_dic <- c(unique(check_df$check_dic), paste0(unique(check_df$check_dic), ".factor"))
              posible_vars <- setdiff(posible_vars, vars_dic)
              posible_vars <- c(posible_vars, check_df$check_data)
            }

            intersect(names(data), posible_vars)
          }),
          vars = purrr::map(.data$vars, ~ unique(c(basic_redcap_vars, .x)))
        ) |>
        dplyr::mutate(df = purrr::map(.data$vars, ~ data |>
          dplyr::select(dplyr::any_of(unique(c(basic_redcap_vars, .x))))))
    }

    if (repeat_instrument) {
      # The name of each repeating instrument in the data: its factor version (redcap_data()), or else its name
      instrument <- if ("redcap_repeat_instrument.factor" %in% names(data)) "redcap_repeat_instrument.factor" else "redcap_repeat_instrument"
      form_check <- data |>
        dplyr::distinct(dplyr::pick(dplyr::any_of(c("redcap_repeat_instrument", "redcap_repeat_instrument.factor"))))
      form_check$form_factor <- form_check[[instrument]]

      # Events where each form repeats: in the other events, its rows are the normal rows of the record (a form can repeat
      # only in some events)
      repeating_rows <- !as.character(data$redcap_repeat_instrument) %in% c(NA, "")
      repeat_events <- function(form) {
        if (!"redcap_event_name" %in% names(data)) {
          return(NULL)
        }
        unique(data$redcap_event_name[repeating_rows & data$redcap_repeat_instrument %in% form])
      }

      ndata <- ndata |>
        dplyr::left_join(form_check[c("redcap_repeat_instrument", "form_factor")], by = dplyr::join_by("form" == "redcap_repeat_instrument")) |>
        dplyr::relocate("form_factor", .after = form) |>
        dplyr::mutate(df = purrr::pmap(list(.data$form, .data$form_factor, .data$df), function(form, form_factor, df) {
          no_instrument <- as.character(df[[instrument]]) %in% c(NA, "")
          # (dplyr::slice() keeps the labels of the variables)
          if (is.na(form_factor)) {
            df |>
              dplyr::slice(which(.env$no_instrument)) |>
              dplyr::select(-dplyr::starts_with("redcap_repeat_instrument"))
          } else {
            events <- repeat_events(form)
            normal <- if (is.null(events)) rep(FALSE, nrow(df)) else no_instrument & !df$redcap_event_name %in% events
            rows_kept <- normal | (!no_instrument & as.character(df[[instrument]]) %in% as.character(form_factor))
            df |>
              dplyr::slice(which(.env$rows_kept)) |>
              dplyr::mutate(redcap_repeat_instrument = .data[[instrument]]) |>
              dplyr::select(-dplyr::any_of("redcap_repeat_instrument.factor"))
          }
        })) |>
        dplyr::select(-"form_factor")
    }

    if (wide) {
      ndata <- ndata |>
        dplyr::mutate(df = purrr::map(.data$df, ~{
          order_cols <- intersect(names(data), names(.x))

          .x |>
            dplyr::select(dplyr::any_of(c(basic_redcap_vars, order_cols)))
        }))

      ndata <- ndata |>
        dplyr::mutate(
          max_repeated_instance = purrr::map_dbl(
            .data$df,
            # Most rows of a record (0 when the form has no rows, e.g. a project without records)
            ~ max(0, tabulate(match(.x[[id_var]], unique(.x[[id_var]]))))
          ),
          df = purrr::pmap(list(.data$vars, .data$df, if (longitudinal) .data$events else as.list(rep(NA, dplyr::n()))), function(x, y, z) {

            y <- y |>
              dplyr::select(dplyr::any_of(c(id_var, setdiff(x, basic_redcap_vars))))

            # One row per record: the rows of a record (events, instances of a repeated instrument or event) become numbered columns
            if (dplyr::n_distinct(z) > 1 || any(duplicated(y[[id_var]]))) {

              # (the number of each row of a record, in a column whose name isn't one of the data, e.g. a field `id`)
              number <- utils::tail(make.unique(c(names(y), ".rd_number")), 1)
              y[[number]] <- stats::ave(seq_len(nrow(y)), match(y[[id_var]], unique(y[[id_var]])), FUN = seq_along)
              y <- y |>
                # (`<variable>_<number>`, also when the form has only one variable)
                tidyr::pivot_wider(names_from = dplyr::all_of(number), values_from = -dplyr::all_of(c(id_var, number)), names_glue = paste0("{.value}_{", number, "}"))
            }

            return(y)
          })
        ) |>
        dplyr::relocate("max_repeated_instance", .before = "vars")
    }
  } else if (by == "event") {

    # Handle splitting by event
    var_event <- tibble::tibble("form_name" = event_form$form) |>
      dplyr::mutate(
        redcap_event_name = purrr::map(.data$form_name, ~ event_form$unique_event_name[event_form$form == .x]),
        vars = purrr::map(.data$form_name, ~{
          posible_vars <- dic$field_name[dic$form_name == .x]
          posible_vars <- c(posible_vars, paste0(posible_vars, ".factor"), paste0(posible_vars, "_mdc"))

          check_df <- tibble::tibble(check_data = names(data)[grep("___", names(data))]) |>
            dplyr::mutate(check_dic = gsub("___.*$", "", check_data)) |>
            dplyr::distinct_all() |>
            dplyr::filter(check_dic %in% posible_vars)

          if (nrow(check_df) > 0) {
            vars_dic <- c(unique(check_df$check_dic), paste0(unique(check_df$check_dic), ".factor"))
            posible_vars <- setdiff(posible_vars, vars_dic)
            posible_vars <- c(posible_vars, check_df$check_data)
          }

          intersect(names(data), posible_vars)
        }
        )
      ) |>
      tidyr::unnest("redcap_event_name") |>
      tidyr::unnest("vars") |>
      dplyr::distinct(.data$redcap_event_name, field_name = .data$vars) |>
      dplyr::filter(.data$field_name != id_var) |>
      tibble::as_tibble()

    var_event_add <- data.frame(redcap_event_name = NA, field_name = basic_redcap_vars)
    var_event <- rbind(var_event_add, var_event)

    list_events <- stats::na.exclude(unique(var_event$redcap_event_name))

    ndata <- tibble::tibble("events" = list_events) |>
      dplyr::mutate(
        vars = purrr::map(
          .data$events,
          ~ var_event |>
            dplyr::filter(.data$redcap_event_name == .x) |>
            dplyr::pull("field_name")
        ),
        df = purrr::map2(
          .data$events,
          .data$vars,
          ~ data |>
            dplyr::filter(.data$redcap_event_name == .x) |>
            dplyr::select(dplyr::all_of(c(basic_redcap_vars, .y)))
        )
      )

  } else {
    rd_abort("Invalid {.arg by} argument. Please specify either {.val form} or {.val event}.", class = "REDCapDM_error_argument")
  }

  # Order columns
  if (!wide) {
    ndata <- ndata |>
      dplyr::mutate(df = purrr::map(.data$df, ~{
        order_cols <- intersect(names(data), names(.x))

        .x |>
          dplyr::select(dplyr::any_of(c(basic_redcap_vars, order_cols)))
      }))
  }

  # Handle the `which` argument if provided
  if (!is.null(which)) {
    if (length(which) > 1) {
      rd_warn("The {.arg which} argument is designed to specify a single form or event. Multiple inputs were provided; only the first will be used.")

      which <- which[1]
    }

    if (by == "form") {
      if(!which %in% form){
        rd_abort(c("The form specified in the {.arg which} argument was not found in this project.", "i" = "Please select one of the available REDCap forms: {.val {form}}."), class = "REDCapDM_error_argument")
      }
    }

    if (by == "event") {
      if(!which %in% list_events){
        rd_abort(c("The event specified in the {.arg which} argument was not found in this project.", "i" = "Please select one of the available REDCap events: {.val {list_events}}."), class = "REDCapDM_error_argument")
      }
    }


    ndata <- ndata |>
      dplyr::filter((if (by == "form") .data$form else .data$events) == which) |>
      dplyr::pull(.data$df) |>
      purrr::pluck(1)
  } else {
    ndata <- ndata |>
      dplyr::select(-"vars")
  }

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("Final arrangement of the data by {by}. (rd_split)\n"))
  } else {

    if(grepl("^[A-Z]", results[1])) {
      results[1] <- paste("1.", results[1])
    }

    last_val_res <- results |>
      stringr::str_extract("^(\n)?\\d+\\.") |>
      na.omit() |>
      dplyr::last() |>
      stringr::str_remove("\\.") |>
      as.numeric()

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Final arrangement of the data by {by}. (rd_split)\n"))
  }

  # Return the modified data, dictionary, event_form, and results
  list(
    data = ndata,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
