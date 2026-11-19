#' Transform the data dictionary and handle branching logic
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Updates a REDCap data dictionary by converting branching logic and calculation expressions into valid R expressions. This ensures that conditional display rules and calculated fields in the dictionary can be programmatically evaluated with the dataset. Any variables that cannot be converted are reported in the results.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{data}{The original dataset passed to the function.}
#'   \item{dictionary}{The updated data dictionary with modified branching logic and calculations.}
#'   \item{event_form}{The original event-form mapping (if applicable).}
#'   \item{results}{A summary of the transformations, including any variables with unconverted branching logic.}
#' }
#'
#' @details
#' The function performs the following tasks:
#' * Evaluates and transforms branching logic expressions into valid R expressions using `rd_rlogic`.
#' * Evaluates and converts calculation expressions for fields of type `calc`.
#' * Keeps in REDCap syntax, with the codes of the options, the logic that uses as a number a variable already converted into a factor (e.g. `[score] >= 3` or `[a] + [b]` after `rd_factor()`): it can't be written as R logic of the factors. `rd_rlogic()` and `rd_query()` evaluate it with the codes, as REDCap does. The same happens with the logic that uses variables of other events.
#' * Generates a results summary table listing any variables that could not be converted.
#'
#' @examples
#' \dontrun{
#' result <- rd_dictionary(covican)
#' print(result$results)
#' updated_dic <- result$dictionary
#' }
#'
#' @family process functions
#' @export
#' @importFrom stats setNames na.omit


rd_dictionary <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL) {

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
  check_inputs(data, dic)
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)

  # Checking if there was already a transformation of the original variables into factors
  factors <- data |>
    dplyr::select(dplyr::any_of(dic$field_name) & dplyr::where(is.factor)) |>
    colnames()

  # Categories of the factors (yes/no and true/false fields have them fixed, as in the R script exported by REDCap)
  factor_choices <- dic$choices_calculations_or_slider_labels
  if ("field_type" %in% names(dic)) {
    factor_choices[dic$field_type %in% "yesno"] <- "1, Yes | 0, No"
    factor_choices[dic$field_type %in% "truefalse"] <- "1, True | 0, False"
  }
  factors <- intersect(factors, dic$field_name[!factor_choices %in% c(NA, "")])

  # Dictionary with the codes of the options (before writing the labels in the logic), and labels of each factor
  dic_codes <- dic
  factor_labels <- list()

  if (length(factors) > 0) {
    # Select the branching logic for any variables that are being transformed into factors
    cat_factors <- data.frame(field_name = dic$field_name, choices_calculations_or_slider_labels = factor_choices, stringsAsFactors = FALSE) |>
      dplyr::filter(.data$field_name %in% factors)

    # Split the `choices_calculations_or_slider_labels` to separate each option in a multi-option field
    cat_factors <- cat_factors |>
      dplyr::mutate(
        choices_calculations_or_slider_labels = strsplit(.data$choices_calculations_or_slider_labels, "\\|")
      ) |>
      tidyr::unnest("choices_calculations_or_slider_labels")

    # Separate numeric and category labels from the choices string
    # (the code is the text before the first comma, also without a space after it, e.g. `5,Always`; an option without
    # label is left out)
    cat_factors <- cat_factors |>
      tidyr::separate(.data$choices_calculations_or_slider_labels,
        into = c("num", "cat"),
        sep = "\\s*,\\s*",
        extra = "merge",
        fill = "right"
      ) |>
      dplyr::filter(.data$cat != "") |>
      dplyr::mutate(
        num = stringr::str_trim(.data$num),
        cat = stringr::str_trim(.data$cat)
      )

    factor_labels <- split(cat_factors$cat, cat_factors$field_name)

    # Write the comparisons with the codes of the options as comparisons with their labels (e.g. `[sex] = '1'` is
    # `[sex]='Male'`), in one reading of each logic: a label that is the code of another option is not changed again
    options <- data.frame(ref = cat_factors$field_name, code = cat_factors$num, name = cat_factors$field_name, label = cat_factors$cat)
    dic <- dic |>
      dplyr::mutate(
        branching_logic_show_field_only_if = relabel_logic(.data$branching_logic_show_field_only_if, options),
        choices_calculations_or_slider_labels = relabel_logic(.data$choices_calculations_or_slider_labels, options)
      )
  }

  logics <- NULL
  # Generate the object that will contain the warnings
  warnings_env <- new.env(parent = emptyenv())
  warnings_env$count <- 0
  warnings_env$msgs <- character()
  warnings_env$id <- numeric()

  # Starting time
  start_time <- Sys.time()

  # Identify rows in the dictionary with branching logic that needs evaluation
  pos_branch <- which(!dic$branching_logic_show_field_only_if %in% "")

  # Logic with variables of other events ([event][field]) can't be written as R logic of each row: it is kept in REDCap
  # syntax, which rd_rlogic() and rd_query() evaluate
  other_event <- function(logic) {
    events <- gsub("^\\[|\\]\\[$", "", unlist(stringr::str_extract_all(without_texts(logic), "\\[[\\w-]+\\]\\[")))
    !is.null(event_form) && any(events %in% event_form$unique_event_name)
  }
  kept <- NULL

  # Logic that uses a factor as a number (e.g. `[score] >= 3` or `[a] + [b]`) or compares it with a code that isn't one
  # of its labels can't be written as R logic of the data with factors: it is kept in REDCap syntax, with the codes of
  # the options, which rd_rlogic() and rd_query() evaluate with the codes. Comparisons with a label or with an empty
  # text (`[a] = 'Yes'`, `[a] <> ''`) and isblankormissingcode([a]) work with the factor.
  # (also the options of the checkboxes that are factors, e.g. after rd_factor(), written as `[cb(1)]` or `[cb___1]`)
  checkbox_factors <- names(data)[grepl("___", names(data)) & !grepl("\\.factor$", names(data)) & vapply(data, is.factor, logical(1))]
  checkbox_factors <- setdiff(checkbox_factors, names(factor_labels))
  factor_labels <- c(factor_labels, lapply(stats::setNames(checkbox_factors, checkbox_factors), function(col) levels(data[[col]])))
  factor_as_number <- function(logic) {
    if (length(factor_labels) == 0 || is.na(logic)) {
      return(FALSE)
    }
    logic <- checkbox_columns(logic)
    for (field in names(factor_labels)) {
      if (factor_other_use(logic, field, factor_labels[[field]])) {
        return(TRUE)
      }
    }
    FALSE
  }
  kept_codes <- NULL

  # Loop through each row with branching logic
  for (i in pos_branch) {
    if (other_event(dic$branching_logic_show_field_only_if[i])) {
      kept <- c(kept, dic$field_name[i])
      next
    }
    as_number <- factor_as_number(dic$branching_logic_show_field_only_if[i])
    if (as_number) {
      dic$branching_logic_show_field_only_if[i] <- dic_codes$branching_logic_show_field_only_if[i]
    }

    # Attempt to evaluate and convert the branching logic using `rd_rlogic`
    evaluation <- withCallingHandlers(
      {
        try(
          rd_rlogic(
            data       = data,
            dic        = dic,
            event_form = event_form,
            logic      = dic$branching_logic_show_field_only_if[i],
            var        = dic$field_name[i]
          )$rlogic,
          silent = TRUE
        )
      },
      warning = function(w) {
        warnings_env$count <- warnings_env$count + 1
        warnings_env$msgs <- c(warnings_env$msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    # If evaluation succeeds, update the dictionary with the converted logic (or keep the logic with the codes)
    if (!inherits(evaluation, "try-error")) {
      if (as_number) {
        kept_codes <- c(kept_codes, dic$field_name[i])
      } else {
        dic$branching_logic_show_field_only_if[i] <- evaluation
      }
    } else {
      # If evaluation fails, add the variable name to the `logics` object
      logics <- rbind(logics, dic$field_name[i])
    }
  }

  # Warning with almost done
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  if (elapsed > 10) {
    rd_inform("\u23F3 Almost done!")
  }

  # Identify rows in the dictionary with calculations that need evaluation
  pos_calc <- which(dic$field_type == "calc")

  # Loop through each row with calculations
  for (i in pos_calc) {
    if (other_event(dic$choices_calculations_or_slider_labels[i])) {
      kept <- c(kept, dic$field_name[i])
      next
    }
    as_number <- factor_as_number(dic$choices_calculations_or_slider_labels[i])
    if (as_number) {
      dic$choices_calculations_or_slider_labels[i] <- dic_codes$choices_calculations_or_slider_labels[i]
    }

    # Attempt to evaluate and convert the calculations using `rd_rlogic`
    evaluation <- withCallingHandlers(
      {
        try(
          rd_rlogic(
            data       = data,
            dic        = dic,
            event_form = event_form,
            logic      = dic$choices_calculations_or_slider_labels[i],
            var        = dic$field_name[i]
          )$rlogic,
          silent = TRUE
        )
      },
      warning = function(w) {
        warnings_env$count <- warnings_env$count + 1
        warnings_env$msgs <- c(warnings_env$msgs, conditionMessage(w))
        warnings_env$id <- c(warnings_env$id, i)
        invokeRestart("muffleWarning")
      }
    )

    # If evaluation succeeds, update the dictionary with the converted logic (or keep the logic with the codes)
    if (!inherits(evaluation, "try-error")) {
      if (as_number) {
        kept_codes <- c(kept_codes, dic$field_name[i])
      } else {
        dic$choices_calculations_or_slider_labels[i] <- evaluation
      }
    } else {
      # If evaluation fails, add the variable name to the `logics` object
      logics <- rbind(logics, dic$field_name[i])
    }
  }

  # Update results with the this transformation
  if (is.null(results)) {
    results <- c(results, stringr::str_glue("Converting every branching logic in the dictionary into R logic. (rd_dictionary)\n"))
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

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Converting every branching logic in the dictionary into R logic. (rd_dictionary)\n"))
  }

  # If there are variables with unconverted logic, prepare a report
  if (!is.null(logics)) {
    # Make sure there are unique variables to review
    logics <- unique(logics)

    # Create a table of unconverted variables
    tabla <- tibble::tibble("Variables" = logics)

    # Append the table to the `results` for later reporting
    results <- c(results, "\n", knitr::kable(tabla, "pipe", align = c("ccc"), caption = "Variables with unconverted branching logic"))
  }

  # Report the variables whose logic uses other events
  if (!is.null(kept)) {
    tabla <- tibble::tibble("Variables" = unique(kept))
    results <- c(results, "\n", knitr::kable(tabla, "pipe", align = c("ccc"), caption = "Variables whose logic uses other events (kept in REDCap syntax)"))
  }

  # Report the variables whose logic uses factors as numbers
  if (!is.null(kept_codes)) {
    tabla <- tibble::tibble("Variables" = unique(kept_codes))
    results <- c(results, "\n", knitr::kable(tabla, "pipe", align = c("ccc"), caption = "Variables whose logic uses multiple-choice fields as numbers (kept in REDCap syntax, with the codes)"))
  }

  # Generating a global warning if one of the iteractions of the loop generates a warning inside rd_rlogic
  if (warnings_env$count > 0) {
    warnings_env$msgs <- warnings_env$msgs |> unique()

    for (i in warnings_env$msgs) {
      rd_warn("{i}")
    }
  }

  # Return the processed objects
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
