#' Delete Variables from REDCap Dataset and Dictionary
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Deletes selected variables from a REDCap dataset and its dictionary, keeping them consistent and preserving variable labels.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param vars Optional. A character vector of variable names to remove from both the dataset and dictionary.
#' @param pattern Optional. A character vector of regular expression patterns. Variables matching these patterns will be removed from the dataset and dictionary.
#' @param forms Optional. A character vector of form names (as in the `form_name` column of the dictionary). All the variables of these forms will be removed from the dataset and dictionary.
#'
#' @details
#' - Ensure that at least one of `vars`, `pattern` or `forms` is specified.
#' - Removes specified variables and their factor versions (e.g., `variable.factor`) from the dataset.
#' - Removes matching variables from the dictionary.
#' - Warns about factor versions of variables matching patterns, recommending use of `rd_factor()` if necessary.
#' - With `forms`, every variable of the form is removed: its factor versions, checkbox options, recalculated versions (`_recalc`) and the form's `_complete` and `_timestamp` fields. The record identifier is always kept. The forms are also removed from `event_form`, and so are the rows of the repeating instruments among them.
#' 
#' @return A list containing:
#' \describe{
#'   \item{data}{The updated dataset with specified variables removed.}
#'   \item{dictionary}{The updated REDCap dictionary.}
#'   \item{event_form}{The event-form mapping (if applicable), without the removed forms.}
#'   \item{results}{A summary message describing the variable removal operation.}
#' }
#'
#' @examples
#' # Delete specific variables by name
#' result <- rd_delete_vars(
#'   project = covican,
#'   vars = c("potassium", "leuk_lymph")
#' )
#'
#' # Delete variables matching patterns (here, the inclusion and exclusion criteria)
#' result <- rd_delete_vars(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   pattern = c("^inc_", "^exc_")
#' )
#'
#' # Delete every variable of a form
#' result <- rd_delete_vars(
#'   project = covican,
#'   forms = "laboratory_findings"
#' )
#'
#' @export

rd_delete_vars <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, vars = NULL, pattern = NULL, forms = NULL) {
  results <- NULL

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)
    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = "field_name")
  
  for (arg in c("vars", "pattern", "forms")) {
    check_strings(get(arg), arg)
  }

  # Ensure one of the arguments is fullfilled
  if (is.null(vars) & is.null(pattern) & is.null(forms)) {
    rd_abort("At least one of the {.arg vars}, {.arg pattern} or {.arg forms} arguments must be provided.", class = "REDCapDM_error_argument")
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

  # If `vars` is specified, iterate through each variable and remove it from the data and dictionary
  if (!is.null(vars)) {
    vars_missing <- setdiff(vars, names(data))

    if (length(vars_missing) > 0) {
      rd_abort(c("The following variables are not present in the dataset: {.var {vars_missing}}.", "i" = "Please remove them from the {.arg vars} argument."), class = "REDCapDM_error_argument")
    }
    
    # Error: the record identifier is needed by every other function
    if ("record_id" %in% vars) {
      rd_abort("{.code record_id} can't be removed: the other functions need the record identifier.")
    }

    # Columns with the Missing Data Codes of the variables (redcap_data(rm_mdc = FALSE))
    mdc_cols <- mdc_columns(names(data), dic$field_name)

    for (i in seq_along(vars)) {
      # Remove the variable from the dataset
      data <- data |>
        dplyr::select(!vars[i])

      # Check if the factor version of the variable exists and remove it if present
      if (paste0(vars[i], ".factor") %in% names(data)) {
        data <- data |>
          dplyr::select(!paste0(vars[i], ".factor"))
      }
      
      # Same for the column with its Missing Data Codes (redcap_data(rm_mdc = FALSE))
      if (paste0(vars[i], "_mdc") %in% mdc_cols) {
        data <- data |>
          dplyr::select(!paste0(vars[i], "_mdc"))
      }

      # Remove the variable from the dictionary
      dic <- dic |>
        dplyr::filter(.data$field_name != vars[i])
    }
  }


  # If `pattern` is specified, use it to identify and remove matching variables
  if (!is.null(pattern)) {
    # Collapse patterns used
    comb_pattern <- paste(pattern, collapse = "|")

    # Create factor versions of the patterns for additional checks
    pattern_factor <- data |>
      dplyr::select(grep(comb_pattern, names(data), value = TRUE)) |>
      names()

    if (length(pattern_factor) > 0) {
      pattern_factor <- ifelse(
        endsWith(pattern_factor, ".factor"),
        pattern_factor,
        paste0(pattern_factor, ".factor")
      )

      vars_eliminated <- data |>
        dplyr::select(!dplyr::matches(comb_pattern))

      # Warn if factor versions of the variables matching the patterns are present in the dataset
      if (any(pattern_factor %in% names(vars_eliminated)) & any(grepl("\\$", pattern))) {
        rd_warn("The dataset contains factor versions of variables matching the specified patterns. To properly remove them, use the {.fn rd_factor} function first.")
      }
    }

    # Variables matching the pattern (never the record identifier nor the REDCap columns of events and repetitions)
    matched <- grep(comb_pattern, names(data), value = TRUE, ignore.case = TRUE)
    matched <- matched[!matched %in% "record_id" & !grepl("^redcap_(event_name|repeat_instrument|repeat_instance)(\\.factor)?$", matched)]

    if (length(matched) == 0) {
      rd_warn("No variable matches the pattern(s) {.val {pattern}}: nothing was removed.")
    }

    # Remove variables matching the pattern from the dataset
    data <- data |>
      dplyr::select(!dplyr::all_of(matched))

    # Remove variables matching the pattern from the dictionary
    dic <- dic |>
      dplyr::filter(!grepl(comb_pattern, .data$field_name) | .data$field_name == "record_id")
  }

  
  # If `forms` is specified, remove every variable of these forms
  if (!is.null(forms)) {
    if (!"form_name" %in% names(dic)) {
      rd_abort("The dictionary doesn't have the {.code form_name} column, needed to use the {.arg forms} argument.")
    }

    forms_missing <- setdiff(forms, dic$form_name)

    if (length(forms_missing) > 0) {
      rd_abort(c("The following forms are not present in the dictionary: {.val {forms_missing}}.", "i" = "Please remove them from the {.arg forms} argument."), class = "REDCapDM_error_argument")
    }

    # The record identifier (first field of the dictionary) is never removed
    id_var <- dic$field_name[1]
    form_fields <- setdiff(dic$field_name[dic$form_name %in% forms], id_var)

    # Data columns of these fields: factor versions, checkbox options and the _complete/_timestamp fields of the forms
    base_names <- sub("\\.factor$", "", names(data))
    form_cols <- names(data)[
      (base_names %in% form_fields | sub("___.*$", "", base_names) %in% form_fields |
         base_names %in% c(paste0(forms, "_complete"), paste0(forms, "_timestamp"))) &
        base_names != id_var
    ]
    
    # Columns with the Missing Data Codes of these fields (redcap_data(rm_mdc = FALSE))
    form_cols <- c(form_cols, intersect(paste0(form_fields, "_mdc"), mdc_columns(names(data), dic$field_name)))

    data <- data |>
      dplyr::select(!dplyr::all_of(form_cols))

    # Rows of the removed repeating instruments are left without data
    if ("redcap_repeat_instrument" %in% names(data)) {
      data <- data |>
        dplyr::filter(!(.data$redcap_repeat_instrument %in% forms))
    }

    dic <- dic |>
      dplyr::filter(!.data$field_name %in% form_fields)

    if (!is.null(event_form) && "form" %in% names(event_form)) {
      event_form <- event_form |>
        dplyr::filter(!.data$form %in% forms)
    }
  }

  # Reapply variable labels to the dataset after modifications
  data <- data |>
    labelled::set_variable_labels(
      .labels = labels |> as.list(), .strict = FALSE
    )

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("Removing selected variables (rd_delete_vars)\n"))
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

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Removing selected variables (rd_delete_vars)\n"))
  }

  # Return the updated dataset, dictionary, event_form, and results
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
