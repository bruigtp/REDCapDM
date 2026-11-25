#' Insert Missing Values Using a Filter
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Sets selected variables to `NA` when a filter condition is satisfied. Useful for managing checkboxes or other fields without explicit gatekeeper questions.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param vars Character vector of variable names to set to `NA`.
#' @param filter A single logical expression (as string). Rows where the filter evaluates to `TRUE` will have the corresponding `vars` set to `NA`. It can use objects created in your session or in your function, columns chosen with `.data[[field]]` (with `field` one of these objects) or be a logical vector of your session with one value per row (e.g. `filter = "eligible"`).
#'
#' @details
#' * Each variable is only updated in rows/events where both the variable and filter are present.
#' * If the variable has a `.factor` version, it is updated too.
#' * For longitudinal projects, `event_form` must be provided for proper event-level filtering.
#' * Only one filter expression is allowed.
#' * Variables and filter columns must exist in both `data` and `dictionary`: their `.factor` and `_mdc` versions and the options of a checkbox (e.g. `cb___1`) are checked with their field. The objects of your session used in the filter don't need to be in them.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The dataset with `NA` inserted where the filter applies.}
#'   \item{dictionary}{The unchanged dictionary.}
#'   \item{event_form}{The `event_form` passed in (if applicable).}
#'   \item{results}{Summary message of the changes applied.}
#' }
#'
#' @examples
#' # Set 'potassium' to NA where age < 65
#' result <- rd_insert_na(
#'   covican,
#'   vars = "potassium",
#'   filter = "age < 65"
#' )
#' table(is.na(result$data$potassium))
#'
#' @family process functions
#' @export
#' @importFrom rlang .data parse_expr eval_tidy

rd_insert_na <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, vars, filter) {

  # Environment where the user called the function: the filters and expressions look for their objects there
  user_env <- rlang::caller_env()

  results <- NULL

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    # browser()
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) || is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = c("field_name", "form_name"))
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_strings(vars, "vars", null_ok = FALSE)
  check_strings(filter, "filter", null_ok = FALSE)

  # Determine if the dataset is longitudinal
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  # Error: For longitudinal data, ensure `event_form` is specified
  if (is.null(event_form) && longitudinal) {
    rd_abort("The data is longitudinal (it has the column `redcap_event_name`), but the {.arg event_form} mapping was not provided. Please specify it.")
  }

  # Validate there is exactly one filter and allow multiple vars
  if (length(filter) != 1) {
    rd_abort("Please provide exactly one filter.", class = "REDCapDM_error_argument")
  } else {
    # Variables of the filter: the names of the expression that aren't functions (e.g. `abs` or `dplyr::between`) nor
    # objects of the session of the user (e.g. a limit saved in a variable)
    # (the errors inside tryCatch() name rd_insert_na() through fn_env)
    fn_env <- environment()
    parsed <- tryCatch({
      rlang::parse_expr(filter[1])
    }, error = function(e) {
      rd_abort("Unable to parse filter expression {.code {filter[1]}}.", class = "REDCapDM_error_argument", parent = e, call = fn_env)
    })
    # (`data$x`, `.data$x` and `.data[[field]]` are always columns of the data)
    vars_filter <- filter_names(parsed, env = user_env, columns = names(data))
    vars_filter <- unique(c(
      vars_filter$columns,
      vars_filter$names[vars_filter$names %in% names(data) | !vapply(vars_filter$names, exists, logical(1), envir = user_env)]
    ))

    # check filter vars exist in the dataset
    missing_in_data <- vars_filter[!vars_filter %in% names(data)]
    if (length(missing_in_data) > 0) {
      rd_abort("Filter variable(s) not found in data: {.val {missing_in_data}}", class = "REDCapDM_error_argument")
    }

    # check filter vars exist in the dictionary (the `.factor` and `_mdc` versions and the options of a checkbox are
    # its field)
    vars_filter <- unique(vapply(vars_filter, data_field, character(1), fields = dic$field_name))
    missing_in_dic <- vars_filter[!vars_filter %in% dic$field_name]
    if (length(missing_in_dic) > 0) {
      rd_abort("Filter variable(s) not found in dictionary: {.val {missing_in_dic}}", class = "REDCapDM_error_argument")
    }

    # Events are only relevant in longitudinal projects (a filter without variables of the data, e.g. a logical vector
    # created in the session, doesn't restrict the events: `events` is NULL). The record identifier is in every row,
    # whatever the form of its field, so it doesn't restrict them either.
    events <- NULL
    vars_events <- setdiff(vars_filter, id_field(data, dic))
    if (longitudinal && length(vars_events) > 0) {
      # Extract corresponding events for filter variables
      event_filter <- tibble::tibble(vars_filter = vars_events) |>
        dplyr::mutate(
          form = purrr::map_chr(.data$vars_filter, ~ dic |>
                                  dplyr::filter(.data$field_name %in% .x) |>
                                  dplyr::pull(.data$form_name)),
          event = purrr::map(.data$form, ~ event_form |>
                               dplyr::filter(.data$form %in% .x) |>
                               dplyr::pull(.data$unique_event_name))
        )

      # Identify common events for filter variables
      events <- Reduce(intersect, event_filter$event)

      # Stop if there are no common events among filter variables
      if (length(events) == 0) {
        rd_abort("The variables in the filter belong to different events.")
      }
    }
    
    # Evaluate the filter expression once to get logical mask for rows
    filter_expr <- parsed

    rows_mask <- tryCatch({
      # Evaluate in the context of `data`; result should be logical vector of length nrow(data)
      # (without the class `labelled` of the variable labels, which some functions don't accept, e.g. dplyr::between())
      data_eval <- without_labelled(data)
      rlang::eval_tidy(filter_expr, data = data_eval, env = rlang::env(user_env, data = data_eval))
    }, error = function(e) {
      rd_abort("Error evaluating filter {.code {filter[1]}}.", class = "REDCapDM_error_argument", parent = e, call = fn_env)
    })

    if (!is.logical(rows_mask) || length(rows_mask) != nrow(data)) {
      rd_abort("Filter {.code {filter[1]}} did not return a logical vector with length equal to {.code nrow(data)}.", class = "REDCapDM_error_argument")
    }
    
    # Warning: no row meets the filter
    if (!any(rows_mask %in% TRUE)) {
      rd_warn("No row meets the filter {.code {filter[1]}}: no value was changed.")
    }

    # Validate that provided vars exist in data and in dictionary
    missing_vars_in_data <- vars[!vars %in% names(data)]
    if (length(missing_vars_in_data) > 0) {
      rd_abort("Variable(s) not found in data: {.val {missing_vars_in_data}}", class = "REDCapDM_error_argument")
    }
    missing_vars_in_dic <- vars[!vars %in% dic$field_name]
    if (length(missing_vars_in_dic) > 0) {
      rd_abort("Variable(s) not found in dictionary: {.val {missing_vars_in_dic}}", class = "REDCapDM_error_argument")
    }

    # Event of each row: the project is longitudinal when the data has the column redcap_event_name
    event_col <- if (longitudinal) "redcap_event_name" else NULL

    # Loop through the variables to transform
    for (j in seq_along(vars)) {
      var_j <- vars[j]

      if (longitudinal) {
        # Identify events for the variable to be transformed
        form_var <- dic |>
          dplyr::filter(.data$field_name == var_j) |>
          dplyr::pull(.data$form_name)

        event_var <- event_form |>
          dplyr::filter(.data$form == form_var) |>
          dplyr::pull(.data$unique_event_name)

        # Ensure the variable's events overlap with filter events
        match_events <- if (is.null(events)) event_var else intersect(events, event_var)

        # Error: no overlapping events between the variable and the filter
        if (length(match_events) == 0) {
          rd_abort("The variable {.var {var_j}} and the filter do not overlap in any events.", class = "REDCapDM_error_argument")
        } else {
          # Warn: variable present in more events than the filter
          if (!all(event_var %in% match_events)) {
            rd_warn("The variable {.var {var_j}} is present in more events than the filter. Only rows in common events ({.val {match_events}}) will be transformed.")
          }
        }
      } else {
        # not longitudinal: no per-event checks required
        match_events <- NULL
      }

      # Compute candidate row ids where the filter is TRUE
      ids <- which(rows_mask)

      # If longitudinal and an event column is found, restrict ids to the overlapping events for this var
      if (longitudinal) {
        ids <- ids[which(data[[event_col]][ids] %in% match_events)]
      }

      # Apply the transformation (set selected rows for var_j to NA), also in its .factor version so that a later rd_factor() doesn't bring the values back
      if (length(ids) > 0) {
        data[ids, var_j] <- NA

        factor_j <- paste0(var_j, ".factor")
        if (factor_j %in% names(data)) {
          data[ids, factor_j] <- NA
        }
      } else {
        # No rows to change for this variable (possible due to event restriction)
        # We do not stop here; just inform via a message (could be silent depending on preference)
        rd_inform("No rows matched for variable {.var {var_j}} after applying filter and event overlap.")
      }
    }

    # Update results with this transformation
    inserted_msg <- stringr::str_glue("Inserting missing values into variable(s): {paste(vars, collapse = ', ')}. (rd_insert_na)\n")
    if (is.null(results)) {
      results <- c(results, inserted_msg)
    } else {
      if (grepl("^[A-Z]", results[1])) {
        results[1] <- paste("1.", results[1])
      }

      last_val_res <- results |>
        stringr::str_extract("^(\n)?\\d+\\.") |>
        na.omit() |>
        dplyr::last() |>
        stringr::str_remove("\\.") |>
        as.numeric()

      results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. {inserted_msg}\n"))
    }

    # Return the updated data, dictionary, event_form, and results (if present)
    list(
      data = data,
      dictionary = dic,
      event_form = event_form,
      results = stringr::str_glue("{results}")
    ) |>
      purrr::compact() # Remove NULL elements from the list
  }
}


#' Names of the variables of a filter
#'
#' The names of an expression, except the functions (`abs(x)`, `dplyr::between(x, 1, 2)`), the objects after `.env$` and
#' the components of other objects (`settings$threshold`). `data$x`, `.data$x`, `.data[["x"]]` and `.data[[field]]` (with
#' `field` an object of the session) are the column `x`.
#'
#' @param expr A parsed expression.
#' @param env Environment where the filter is evaluated (to know the column of `.data[[field]]`).
#' @param columns Names of the columns of the data (to know the column of `data[[2]]`).
#' @return A list with the names of the expression (`names`) and the names that are columns of the data for sure
#'   (`columns`).
#' @noRd
filter_names <- function(expr, env = emptyenv(), columns = character(0)) {
  out <- character(0)
  out_columns <- character(0)
  walk <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (nzchar(name)) out <<- c(out, name)
      return(invisible())
    }
    if (!is.call(e)) {
      return(invisible())
    }
    head <- e[[1]]
    if (identical(head, as.name("::")) || identical(head, as.name(":::"))) {
      return(invisible())
    }
    # The arguments of a formula of purrr or of the selection helpers (e.g. `~ .x > 1`, with `.x`, `.y`, `.` or `..1`)
    # aren't variables
    if (identical(head, as.name("~"))) {
      inner <- filter_names(as.call(c(as.name("list"), as.list(e)[-1])), env, columns)
      out <<- c(out, inner$names[!grepl("^[.]([xy]|[.][0-9]+)?$", inner$names)])
      out_columns <<- c(out_columns, inner$columns)
      return(invisible())
    }
    # The arguments of a function written in the filter (e.g. `function(v) v > 1`) aren't variables
    if (identical(head, as.name("function"))) {
      inner <- filter_names(e[[3]], env, columns)
      out <<- c(out, setdiff(inner$names, names(e[[2]])))
      out_columns <<- c(out_columns, inner$columns)
      return(invisible())
    }
    is_access <- length(e) >= 3 && (identical(head, as.name("$")) || identical(head, as.name("@")) || identical(head, as.name("[[")) || identical(head, as.name("[")))
    if (is_access) {
      object <- if (is.symbol(e[[2]])) as.character(e[[2]]) else ""
      # Objects of the session (`.env$x`, `.env$settings$threshold`)
      if (object == ".env") {
        return(invisible())
      }
      # Columns of the data chosen with `[` (`data[, "x"]`, `data[, 3]`, `data[rows, c("x", "y")]` or `data["x"]`): the
      # same columns as with `$` or `[[` (the index of the rows is walked, as it can use other columns)
      if (object %in% c("data", ".data") && identical(head, as.name("["))) {
        index <- as.list(e)[-(1:2)]
        # (named arguments, e.g. `drop = FALSE`, aren't indices)
        if (!is.null(names(index))) {
          index <- index[names(index) %in% ""]
        }
        empty <- function(x) identical(x, quote(expr = ))
        if (length(index) >= 2) {
          if (!empty(index[[1]])) walk(index[[1]])
          index <- index[[2]]
        } else if (length(index) == 1) {
          index <- index[[1]]
        } else {
          index <- quote(expr = )
        }
        # (all the columns, e.g. `data[rows, ]`)
        if (empty(index)) {
          out_columns <<- c(out_columns, columns)
          return(invisible())
        }
        value <- tryCatch(eval(index, env), error = function(e) NULL)
        if (is.numeric(value) && length(value) > 0 && all(abs(value) %in% seq_along(columns))) {
          value <- columns[value]
        } else if (is.logical(value) && length(value) == length(columns) && !anyNA(value)) {
          value <- columns[value]
        }
        if (is.character(value) && length(value) > 0 && !anyNA(value)) {
          out_columns <<- c(out_columns, value)
        } else {
          walk(index)
        }
        return(invisible())
      }
      # Columns of the data (`data$x`, `.data[["x"]]`, `.data[[field]]`)
      if (object %in% c("data", ".data") && !identical(head, as.name("[")) && !identical(head, as.name("@"))) {
        index <- e[[3]]
        if (identical(head, as.name("$")) || is.character(index)) {
          out_columns <<- c(out_columns, as.character(index))
          return(invisible())
        }
        # (an index that is an object or an expression: its value says the column)
        value <- tryCatch(eval(index, env), error = function(e) NULL)
        if (is.numeric(value) && length(value) == 1 && object == "data" && value %in% seq_along(columns)) {
          value <- columns[value]
        }
        if (is.character(value) && length(value) == 1 && !is.na(value)) {
          out_columns <<- c(out_columns, value)
        } else {
          walk(index)
        }
        return(invisible())
      }
      # Components of other objects (`settings$threshold`, `x@slot`) aren't variables: only the object is
      if (identical(head, as.name("$")) || identical(head, as.name("@"))) {
        walk(e[[2]])
        return(invisible())
      }
      # (a chain that starts with .env, e.g. `.env$settings[["threshold"]]`)
      root <- e[[2]]
      while (is.call(root) && length(root) >= 2 && (identical(root[[1]], as.name("$")) || identical(root[[1]], as.name("@")) || identical(root[[1]], as.name("[[")) || identical(root[[1]], as.name("[")))) {
        root <- root[[2]]
      }
      if (identical(root, as.name(".env"))) {
        walk_args(as.list(e)[-(1:2)])
        return(invisible())
      }
    }
    # (the function of a call isn't a variable)
    if (!is.symbol(head)) walk(head)
    walk_args(as.list(e)[-1])
  }
  # (without the empty arguments, e.g. in `data[, "age"]`)
  walk_args <- function(args) {
    for (k in seq_along(args)) {
      if (!identical(args[[k]], quote(expr = ))) walk(args[[k]])
    }
  }
  walk(expr)
  list(names = unique(out), columns = unique(out_columns))
}

#' Field of the dictionary of a column of the data
#'
#' @param column Name of the column.
#' @param fields Fields of the dictionary.
#' @return The field: the column itself, or the field of its `.factor` or `_mdc` version or of a checkbox option.
#' @noRd
data_field <- function(column, fields) {
  base <- sub("(\\.factor|_mdc)$", "", column)
  if (column %in% fields || !base %in% fields && !sub("___.*$", "", base) %in% fields) {
    return(column)
  }
  if (base %in% fields) base else sub("___.*$", "", base)
}
