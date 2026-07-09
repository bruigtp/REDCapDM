#' Recalculate and Verify Calculated Fields in REDCap Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Recalculates fields marked as calculated in the REDCap dictionary, compares them with the original values, and reports discrepancies. If any differences are found, new recalculated fields are added to the dataset and dictionary with `_recalc` appended to the names.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param exclude Optional. Character vector of field names to exclude from recalculation.
#'
#' @details
#' * Fields of type `calc` in the dictionary are recalculated.
#' * Recalculated values are compared with the original values.
#' * If differences exist, new fields `[field_name]_recalc` are added to the dataset and dictionary.
#' * Works for single-event projects; for longitudinal projects, `event_form` must be provided.
#' * Fields with incomplete branching logic or smart variables may fail to recalculate.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The dataset with new `_recalc` fields for any differing calculated fields.}
#'   \item{dictionary}{Updated dictionary including the new `_recalc` fields.}
#'   \item{event_form}{The `event_form` passed in (if applicable).}
#'   \item{results}{Summary report of the recalculation process, including tables of discrepancies.}
#' }
#'
#' @examples
#' # Recalculate all calculated fields
#' \dontrun{
#' results <- rd_recalculate(
#'   data = covican$data,
#'   dic = covican$dictionary,
#'   event_form = covican$event_form
#' )
#' }
#'
#' # Recalculate but exclude some variables
#' \dontrun{
#' results <- rd_recalculate(
#'   project = covican,
#'   exclude = c("age", "screening_fail_crit")
#' )
#' }
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
  repeat_instrument <- "redcap_repeat_instrument" %in% names(data) && any(!is.na(data$redcap_repeat_instrument))

  # Check if there are datetime variables stored as characters in the dataset
  vars_date <- dic |>
    dplyr::filter(grepl("^date_|^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name)

  # Warn if datetime variables need transformation
  if (any(purrr::map(data |> dplyr::select(dplyr::all_of(vars_date)), class) == "character")) {
    warning("The dataset contains date fields stored as character class, which may lead to incorrect or inconsistent date transcription results. Use the `rd_dates` function before this one to properly format these date fields.", call. = FALSE)
  }

  # Proceed only if the project is not longitudinal or has event-form mapping, and is not repeated
  if (!(longitudinal & is.null(event_form))) {

    if(repeat_instrument) {
      stop("The dataset contains repeated instruments, which are not supported by this function. ", call. = FALSE)
    }

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
      dplyr::filter(!is.na(.data$trans) & !.data$is_equal)

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
    stop(
      "\nRecalculation cannot proceed because the project has more than one event, but the event-form correspondence has not been provided. Please specify the event-form mapping for accurate recalculation.\n",
      call. = FALSE
    )
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
