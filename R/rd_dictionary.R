#' Transform the data dictionary and handle branching logic
#'
#' @description
#' `r lifecycle::badge('experimental')`
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
#' * Generates a results summary table listing any variables that could not be converted.
#'
#' @examples
#' \dontrun{
#' result <- rd_dictionary(covican)
#' print(result$results)
#' updated_dic <- result$dictionary
#' }
#'
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
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
  }

  # Checking if there was already a transformation of the original variables into factors
  factors <- data |>
    dplyr::select(dplyr::any_of(dic$field_name) & dplyr::where(is.factor)) |>
    colnames()

  if (length(factors) > 0) {
    # Select the branching logic for any variables that are being transformed into factors
    cat_factors <- dic |>
      dplyr::select("field_name", "choices_calculations_or_slider_labels") |>
      dplyr::filter(.data$field_name %in% factors)

    # Split the `choices_calculations_or_slider_labels` to separate each option in a multi-option field
    cat_factors <- cat_factors |>
      dplyr::mutate(
        choices_calculations_or_slider_labels = strsplit(.data$choices_calculations_or_slider_labels, "\\|")
      ) |>
      tidyr::unnest("choices_calculations_or_slider_labels")

    # Separate numeric and category labels from the choices string
    cat_factors <- cat_factors |>
      tidyr::separate(.data$choices_calculations_or_slider_labels,
        into = c("num", "cat"),
        sep = ", ",
        extra = "merge"
      ) |>
      dplyr::filter(.data$cat != "") |>
      dplyr::mutate(
        num = stringr::str_trim(.data$num),
        cat = stringr::str_trim(.data$cat)
      )

    # Transform each branching logic into R logic
    cat_factors <- cat_factors |>
      dplyr::mutate(
        redcap_v1 = paste0("\\[", .data$field_name, "\\] ?=? ?'?", .data$num, "'?"),
        redcap_v2 = paste0("\\[", .data$field_name, "\\] ?<?>? ?'?", .data$num, "'?"),
        factor_v1 = paste0("[", .data$field_name, "]='", .data$cat, "'"),
        factor_v2 = paste0("[", .data$field_name, "]<>'", .data$cat, "'")
      ) |>
      dplyr::arrange(.data$field_name, dplyr::desc(.data$num))

    # Create a mapping for replacing the branching logic in the dictionary
    replace_v1 <- setNames(cat_factors$factor_v1, cat_factors$redcap_v1)
    replace_v2 <- setNames(cat_factors$factor_v2, cat_factors$redcap_v2)

    # Apply the replacements to the dictionary
    dic <- dic |>
      dplyr::mutate(
        branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace_v1),
        branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace_v2),
        choices_calculations_or_slider_labels = stringr::str_replace_all(.data$choices_calculations_or_slider_labels, replace_v1),
        choices_calculations_or_slider_labels = stringr::str_replace_all(.data$choices_calculations_or_slider_labels, replace_v2)
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

  # Loop through each row with branching logic
  for (i in pos_branch) {
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

    # If evaluation succeeds, update the dictionary with the converted logic
    if (!inherits(evaluation, "try-error")) {
      dic$branching_logic_show_field_only_if[i] <- evaluation
    } else {
      # If evaluation fails, add the variable name to the `logics` object
      logics <- rbind(logics, dic$field_name[i])
    }
  }

  # Warning with almost done
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  if (elapsed > 10) {
    message("\u23F3 Almost done!")
  }

  # Identify rows in the dictionary with calculations that need evaluation
  pos_calc <- which(dic$field_type == "calc")

  # Loop through each row with calculations
  for (i in pos_calc) {
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

    # If evaluation succeeds, update the dictionary with the converted logic
    if (!inherits(evaluation, "try-error")) {
      dic$choices_calculations_or_slider_labels[i] <- evaluation
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

  # Generating a global warning if one of the iteractions of the loop generates a warning inside rd_rlogic
  if (warnings_env$count > 0) {
    warnings_env$msgs <- warnings_env$msgs |> unique()

    for (i in warnings_env$msgs) {
      warning(i, call. = FALSE)
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
