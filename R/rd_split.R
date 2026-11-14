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
#' * For form-based splits in longitudinal projects, uses `event_form` to map variables to events.
#' * Wide format expands repeated instances into multiple columns per record.
#' * Filtering by `which` allows extracting a single form or event.
#' * Projects with repeated instruments are handled by filtering on the `redcap_repeat_instrument` variable.
#'
#' @return Depending on `which` and `wide`:
#' \describe{
#'   \item{data}{A `data.frame` or a list of `data.frames` representing the split datasets.}
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

  # Check if the project has repeated instruments
  if ("redcap_repeat_instrument" %in% names(data)) {
    repeat_instrument <- dplyr::case_when(
      any(!is.na(data$redcap_repeat_instrument)) ~ TRUE,
      TRUE ~ FALSE
    )
  } else {
    repeat_instrument <- FALSE
  }

  # Identify basic REDCap variables present in the dataset
  basic_redcap_vars <- c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "redcap_data_access_group", "redcap_event_name.factor", "redcap_repeat_instrument.factor", "redcap_data_access_group.factor", "redcap_survey_identifier")

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
      rd_abort(c("There are variables in the dictionary that are not present in the dataset.", "i" = "Please ensure that the dictionary matches the dataset. Transformation has been halted."))
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

    fact_vars <- grep(".factor$", vars_less, value = TRUE)
    
    # Columns with the Missing Data Codes kept by redcap_data(rm_mdc = FALSE)
    fact_vars <- c(fact_vars, intersect(vars_less, mdc_columns(names(data), dic$field_name)))

    # Checkbox vars already identified in the previous step (vars_more)
    less_check <- grep("___", vars_less, value = TRUE)

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
      form_check <- data |>
        dplyr::distinct(dplyr::pick(dplyr::contains("redcap_repeat_instrument")))

      ndata <- ndata |>
        dplyr::left_join(form_check, by = dplyr::join_by("form" == "redcap_repeat_instrument")) |>
        dplyr::relocate("form_factor" = "redcap_repeat_instrument.factor", .after = form) |>
        dplyr::mutate(df = purrr::map2(.data$form_factor, .data$df, ~ {
          if (is.na(.x)) {
            .y |>
              dplyr::filter(is.na(.data$redcap_repeat_instrument.factor)) |>
              dplyr::select(-dplyr::starts_with("redcap_repeat_instrument"))
          } else {
            .y |>
              dplyr::filter(.data$redcap_repeat_instrument.factor == .x) |>
              dplyr::mutate(redcap_repeat_instrument = redcap_repeat_instrument.factor) |>
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
            ~.x |>
              dplyr::group_by(.data$record_id) |>
              dplyr::mutate(id = seq_along(.data$record_id),
                            max_id = dplyr::n()) |>
              dplyr::ungroup() |>
              dplyr::pull(max_id) |>
              # 0 when the form has no rows (e.g. a project without records)
              max(0)
          ),
          df = purrr::pmap(list(.data$vars, .data$df, if (longitudinal) .data$events else as.list(rep(NA, dplyr::n()))), function(x, y, z) {

            y <- y |>
              dplyr::select(dplyr::any_of(c("record_id", setdiff(x, basic_redcap_vars))))

            # One row per record: the rows of a record (events, instances of a repeated instrument or event) become numbered columns
            if (dplyr::n_distinct(z) > 1 || any(duplicated(y$record_id))) {

              y <- y |>
                dplyr::group_by(.data$record_id) |>
                dplyr::mutate(id = seq_along(.data$record_id)) |>
                dplyr::ungroup() |>
                tidyr::pivot_wider(names_from = "id", values_from = -c("record_id", "id"))
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
      dplyr::filter(.data$field_name != "record_id") |>
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
    results <- c(results, stringr::str_glue("Final arrangment of the data by {by}. (rd_split)\n"))
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

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Final arrangment of the data by {by}. (rd_split)\n"))
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
