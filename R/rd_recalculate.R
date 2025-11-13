#' Recalculate and Verify Calculated Fields in REDCap Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function processes REDCap project data, recalculates fields defined as calculated fields in the dictionary,
#' and compares the recalculated values with the original ones. It also generates a report of discrepancies and
#' updates the dataset and dictionary with new calculated fields (if applicable).
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#' @param exclude (Optional) A character vector of field names to exclude from recalculation.
#'
#' @return A list containing the following elements:
#'   \item{data}{The updated dataset with recalculated fields (if applicable).}
#'   \item{dictionary}{The updated dictionary with recalculated field entries (if applicable).}
#'   \item{event_form}{The original event-form mapping passed to the function (if applicable).}
#'   \item{results}{A string summarizing the results of the recalculation process.}
#'
#' @details
#' The function:
#' - Identifies calculated fields from the dictionary and evaluates the specified formulas.
#' - Compares recalculated values with the original values.
#' - Adds recalculated fields to the dataset, appending `_recalc` to the original variable names.
#' - Updates the dictionary to reflect the new variables.
#' - Summarizes the number of calculated fields, discrepancies, and untranslated fields in a report.
#'
#'
#' @note
#' - Recalculation is only possible for single-event projects unless `event_form` is specified for longitudinal projects.
#' - If branching logic is incomplete, poorly defined or contains smart-variables, recalculation may fail for some fields.
#'
#' @examples
#'
#' # Example usage with individual arguments
#' results <- rd_recalculate(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   event_form = covican$event_form
#' )
#'
#' # Example usage with a project object, excluding variables from the recalculation
#' results <- covican |>
#'   rd_recalculate(exclude = c("age", "screening_fail_crit"))
#'
#' @export
#' @importFrom rlang :=
#' @importFrom stats na.omit

rd_recalculate <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, exclude = NULL) {
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

  # Extract labels from the data to reapply later
  labels <- labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })


  # Identify if the project is longitudinal or includes repeated instruments
  longitudinal <- "redcap_event_name" %in% names(data)
  repeat_instrument <- any("redcap_repeat_instrument" %in% names(data) & !is.na(data$redcap_repeat_instrument))

  # Check if there are datetime variables stored as characters in the dataset
  vars_date <- dic |>
    dplyr::filter(grepl("^date_|^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name)

  # Warn if datetime variables need transformation
  if (any(purrr::map(data |> dplyr::select(dplyr::all_of(vars_date)), class) == "character")) {
    warning("The dataset contains date fields stored as character class, which may lead to incorrect or inconsistent date transcription results. Use the `rd_dates` function before this one to properly format these date fields.", call. = FALSE)
  }

  # Proceed only if the project is not longitudinal or has event-form mapping, and is not repeated
  if (!(longitudinal & is.null(event_form)) & !repeat_instrument) {
    # Replace `NA` in branching logic fields with empty strings
    if (any(is.na(dic$branching_logic_show_field_only_if))) {
      dic <- dic |>
        dplyr::mutate(branching_logic_show_field_only_if = dplyr::case_when(
          is.na(branching_logic_show_field_only_if) ~ "",
          .default = .data$branching_logic_show_field_only_if
        ))
    }


    # Redefine rounding function to avoid issues in special cases
    # round <- function(x, digits) {
    #   posneg <- sign(x)
    #   z <- abs(x)*10^digits
    #   z <- z + 0.5 + sqrt(.Machine$double.eps)
    #   z <- trunc(z)
    #   z <- z / 10 ^ digits
    #   z*posneg
    # }

    # Process calculated fields: evaluate, transcribe logic, and compare results
    calc <- tibble::tibble(dic) |>
      dplyr::filter(.data$field_type == "calc", !.data$field_name %in% exclude) |>
      dplyr::mutate(
        calc = purrr::map(.data$field_name, function(x) {
          val <- data[, x]
          if (is.numeric(val)) {
            as.numeric(val)
          } else {
            val
          }
        }),
        rlogic = purrr::map2(.data$choices_calculations_or_slider_labels, .data$field_name, function(x, y) {
          rlogic <- try(suppressWarnings(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = x, var = y)), silent = TRUE)
          if (!inherits(rlogic, "try-error")) {
            rlogic
          } else {
            NULL
          }
        }),
        trans = purrr::map_chr(.data$rlogic, function(x) {
          if (!is.null(x)) {
            x$rlogic
          } else {
            NA
          }
        }),
        recalc = purrr::map(.data$rlogic, function(x) {
          if (!is.null(x)) {
            x$eval
          } else {
            NULL
          }
        }),
        is_equal = purrr::map2_lgl(.data$calc, .data$recalc, function(x, y) {
          if (!is.null(y)) {
            if (is.numeric(x) & is.numeric(y)) {
              identical(round(x, 3), round(y, 3))
            } else if (all(is.na(x)) & all(is.na(y))) {
              TRUE
            } else {
              identical(x, y)
            }
          } else {
            NA
          }
        })
      ) |>
      dplyr::select(-"rlogic")


    # Add recalculated fields to the dataset and dictionary
    calc_change <- calc |>
      dplyr::filter(!is.na(.data$trans))

    if (nrow(calc_change) > 0) {
      for (i in seq_len(nrow(calc_change))) {
        name <- stringr::str_glue("{calc_change$field_name[i]}_recalc")

        data <- data |>
          tibble::add_column("{name}" := calc_change$recalc[[i]], .after = as.character(calc_change$field_name[i]))

        add_row <- dic |>
          dplyr::filter(.data$field_name == calc_change$field_name[i]) |>
          dplyr::mutate(
            field_name = stringr::str_glue("{field_name}_recalc"),
            field_label = stringr::str_glue("{field_label} (Recalculate)")
          )

        pos <- which(dic$field_name == calc_change$field_name[i])

        dic <- dic |> tibble::add_row(!!!as.list(add_row), .after = pos)
      }
    }

    # Reapply labels to the modified dataset
    data <- data |>
      labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

    # Update results with this transformation
    if (is.null(results)) {
      results <- c(results, stringr::str_glue("Recalculating calculated fields and saving them as '[field_name]_recalc'. (rd_recalculate)\n"))
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

      results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. Recalculating calculated fields and saving them as '[field_name]_recalc'. (rd_recalculate)\n"))
    }

    # Generate a summary report
    report1 <- calc |>
      dplyr::mutate(n = 1) |>
      dplyr::summarise(
        trans = sum(!is.na(.data$trans)),
        N = sum(.data$n),
        no_trans = .data$N - .data$trans,
        no_equal = sum(!.data$is_equal, na.rm = TRUE),
      ) |>
      dplyr::mutate(
        text1 = stringr::str_glue("{no_trans} ({round(no_trans*100/N, 2)}%)"),
        text2 = stringr::str_glue("{no_equal} ({round(no_equal*100/trans, 2)}%)")
      ) |>
      dplyr::select("Total calculated fields" = "N", "Non-transcribed fields" = "text1", "Recalculated different fields" = "text2")

    results <- c(results, "\n", knitr::kable(report1, "pipe", align = "ccc"))

    # Create a detailed field-level report
    report2 <- calc |>
      dplyr::mutate(trans2 = ifelse(!is.na(.data$trans), "Yes", "No")) |>
      dplyr::arrange(.data$trans2, .data$is_equal) |>
      dplyr::select("field_name", "Transcribed?" = "trans2", "Is equal?" = "is_equal")

    results <- c(results, "\n", knitr::kable(report2, "pipe", align = "ccc"))

    results <- stringr::str_glue("{results}")
  } else {
    # Stop if recalculation is not possible due to missing event-form mapping in longitudinal projects
    stop("\nRecalculation cannot proceed because the project has more than one event, but the event-form correspondence has not been provided. Please specify the event-form mapping for accurate recalculation.\n", call. = FALSE)
  }

  # Return updated datasets and results
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}
