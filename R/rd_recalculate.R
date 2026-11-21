#' Recalculate and Verify Calculated Fields in REDCap Data
#'
#' @description
#' `r lifecycle::badge('stable')`
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
#' * Fields of type `calc` in the dictionary are recalculated. As in REDCap, a calculation without a numeric result (e.g. a division by zero) is blank (`NA`).
#' * Recalculated values are compared with the original values, both rounded to three decimals (e.g. 1.0004 and 1 are equal).
#' * If differences exist, new fields `[field_name]_recalc` are added to the dataset and dictionary, with the label of the original field followed by `(Recalculated)`. Running the function again on its output updates these fields instead of duplicating them.
#' * The fields added by this function are recognised because they are calculated fields with the same calculation as the original field and, when the dictionary has the column `field_label`, its label followed by `(Recalculated)` (or by `(Recalculate)`, the label of previous versions of the package, which are also recognised). No other field or column is overwritten: if `[field_name]_recalc` is already one of them, the recalculated values are saved in `[field_name]_recalc_1` (or `_2`, ...) with a warning.
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
#' @family process functions
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
  if (is.null(data) || is.null(dic)) {
    rd_abort("Both {.arg data} and {.arg dic} (data and dictionary) arguments must be provided.")
  }
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = c("field_name", "field_type", "choices_calculations_or_slider_labels", "text_validation_type_or_show_slider_number"))
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_strings(exclude, "exclude")

  # Extract labels from the data to reapply later
  labels <- labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })


  # Identify if the project is longitudinal
  longitudinal <- "redcap_event_name" %in% names(data)

  # Check if there are datetime variables stored as characters in the dataset
  vars_date <- dic |>
    dplyr::filter(grepl("^date_|^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name) |>
    intersect(names(data))

  # Warn if datetime variables need transformation
  if (any(purrr::map(data |> dplyr::select(dplyr::all_of(vars_date)), class) == "character")) {
    rd_warn("The dataset contains date fields stored as character class, which may lead to incorrect or inconsistent date transcription results. Use the {.fn rd_dates} function before this one to properly format these date fields.")
  }

  # Proceed only if the project is not longitudinal or has event-form mapping
  if (!(longitudinal && is.null(event_form))) {

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

    # Warning: excluded fields that are not calculated fields of the dictionary
    if (any(!exclude %in% dic$field_name[dic$field_type == "calc"])) {
      not_calc <- setdiff(exclude, dic$field_name[dic$field_type == "calc"])
      rd_warn("The following variables of {.arg exclude} are not calculated fields of the dictionary: {.var {not_calc}}.")
    }

    # Process calculated fields: evaluate, transcribe logic, and compare results
    calc <- tibble::tibble(dic) |>
      # The `_recalc` fields added by a previous rd_recalculate() are not recalculated again (the fields of the project whose names end in `_recalc` are)
      dplyr::filter(.data$field_type == "calc", !.data$field_name %in% exclude, !recalc_generated(.data$field_name, dic)) |>
      dplyr::mutate(
        calc = purrr::map(.data$field_name, function(x) {
          val <- data[[x]]
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
            # As in REDCap, a calculation without a numeric result (e.g. a division by zero, 3/0 or 0/0) is blank
            value <- x$eval
            if (is.numeric(value)) value[!is.finite(value)] <- NA
            value
          } else {
            NULL
          }
        }),
        n_diff = purrr::map2_int(.data$calc, .data$recalc, count_differences),
        # Equal when no value differs (identical() would also compare attributes such as the variable label)
        is_equal = ifelse(is.na(.data$n_diff), NA, .data$n_diff == 0)
      ) |>
      dplyr::select(-"rlogic")


    # Add recalculated fields to the dataset and dictionary
    calc_change <- calc |>
      dplyr::filter(!is.na(.data$trans) & !.data$is_equal)

    # Fields of the project that already use the name `[field_name]_recalc`: the recalculation gets another name
    renamed <- character(0)

    if (nrow(calc_change) > 0) {
      for (i in seq_len(nrow(calc_change))) {
        field <- as.character(calc_change$field_name[i])
        name <- recalc_name(field, data, dic)

        if (name != paste0(field, "_recalc") && !recalc_generated(name, dic)) {
          renamed <- c(renamed, stats::setNames(name, field))
        }

        # If rd_recalculate() was already applied, the `_recalc` field is updated instead of duplicated
        if (name %in% names(data)) {
          data[[name]] <- calc_change$recalc[[i]]
        } else {
          data <- data |>
            tibble::add_column("{name}" := calc_change$recalc[[i]], .after = field)
        }

        if (!name %in% dic$field_name) {
          add_row <- dic |>
            dplyr::filter(.data$field_name == field)
          add_row$field_name <- rep(name, nrow(add_row))

          if ("field_label" %in% names(add_row)) {
            add_row$field_label <- recalc_labels(add_row$field_label)[, 1]
          }

          pos <- which(dic$field_name == field)

          dic <- dic |> tibble::add_row(!!!as.list(add_row), .after = pos)
        }
      }
    }

    # Warning: the recalculations saved with another name
    if (length(renamed) > 0) {
      existing <- paste0(names(renamed), "_recalc")
      rd_warn(c("The project already has {.var {existing}}, which {?is/are} not overwritten.", "i" = "The recalculated values of {.var {names(renamed)}} are saved in {.var {unname(renamed)}}."))
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
        no_equal = sum(!.data$is_equal, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        # Without calculated fields (or without transcribed ones) there is no percentage
        text1 = stringr::str_glue("{no_trans} ({ifelse(N == 0, '-', paste0(round(no_trans*100/N, 2), '%'))})"),
        text2 = stringr::str_glue("{no_equal} ({ifelse(trans == 0, '-', paste0(round(no_equal*100/trans, 2), '%'))})")
      ) |>
      dplyr::select("Total calculated fields" = "N", "Non-transcribed fields" = "text1", "Fields with different recalculated values" = "text2")

    results <- c(results, "\n", knitr::kable(report1, "pipe", align = "ccc"))

    # Create a detailed field-level report
    report2 <- calc |>
      dplyr::mutate(trans2 = ifelse(!is.na(.data$trans), "Yes", "No")) |>
      dplyr::arrange(.data$trans2, .data$is_equal) |>
      dplyr::select("field_name", "Transcribed?" = "trans2", "Is equal?" = "is_equal", "Different values" = "n_diff")

    results <- c(results, "\n", knitr::kable(report2, "pipe", align = "ccc"))

    results <- stringr::str_glue("{results}")
  } else {
    # Stop if recalculation is not possible due to missing event-form mapping in longitudinal projects
    rd_abort("Recalculation cannot proceed because the project is longitudinal (the data has the column `redcap_event_name`), but the event-form correspondence has not been provided. Please specify the event-form mapping for accurate recalculation.")
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



#' Number of values that differ between a calculated field and its recalculation
#'
#' Uses the same criteria as the `Is equal?` column: numbers are compared rounded to 3 decimals, and a value that is missing in only one of the two versions counts as different.
#'
#' @param x Values of the calculated field in the data.
#' @param y Recalculated values (`NULL` if the field couldn't be transcribed).
#' @return Integer, or `NA` when the field couldn't be transcribed.
#' @noRd
count_differences <- function(x, y) {
  if (is.null(y)) {
    return(NA_integer_)
  }

  if (is.data.frame(x)) {
    x <- x[[1]]
  }

  if (is.numeric(x) && is.numeric(y)) {
    x <- round(x, 3)
    y <- round(y, 3)
  } else {
    x <- as.character(x)
    y <- as.character(y)
  }

  as.integer(sum(xor(is.na(x), is.na(y)) | (!is.na(x) & !is.na(y) & x != y)))
}



#' Labels of the recalculated versions of some fields
#'
#' @param label Labels of the original fields.
#' @return A matrix with the current label (`(Recalculated)`) in the first column and the one of previous versions of the package (`(Recalculate)`) in the second.
#' @noRd
recalc_labels <- function(label) {
  label <- as.character(label)
  cbind(paste0(label, " (Recalculated)"), paste0(label, " (Recalculate)"))
}


#' Fields added by rd_recalculate()
#'
#' A field is a recalculated version added by `rd_recalculate()` when its name is `[field_name]_recalc` (or `[field_name]_recalc_1`, ...) of another calculated field of the dictionary, it is a calculated field with the same calculation and its label is the label of that field followed by `(Recalculated)` (`rd_recalculate()` copies the row of the field and only changes the name and the label). Without the column `field_label` in the dictionary, the type and the calculation decide. The other fields whose names end in `_recalc` are fields of the project.
#'
#' @param names Field names.
#' @param dic Dictionary.
#' @return Logical vector.
#' @noRd
recalc_generated <- function(names, dic) {
  names <- as.character(names)
  base <- sub("_recalc(_[0-9]+)?$", "", names)
  own <- match(names, dic$field_name)
  orig <- match(base, dic$field_name)

  type <- as.character(dic$field_type)
  calc <- as.character(dic$choices_calculations_or_slider_labels)
  calc_own <- calc[own]
  calc_orig <- calc[orig]
  same_calc <- ifelse(is.na(calc_own) | is.na(calc_orig), is.na(calc_own) & is.na(calc_orig), calc_own == calc_orig)

  generated <- base != names & !is.na(own) & !is.na(orig) & type[own] %in% "calc" & type[orig] %in% "calc" & same_calc

  if (!"field_label" %in% names(dic)) {
    return(generated)
  }

  label <- as.character(dic$field_label)
  own_label <- label[own]
  orig_labels <- recalc_labels(label[orig])

  generated & !is.na(own_label) & (own_label == orig_labels[, 1] | own_label == orig_labels[, 2])
}


#' Name of the recalculated version of a field
#'
#' `[field]_recalc`, unless the dictionary or the data already have a field or column with that name: then `[field]_recalc_1`, `[field]_recalc_2`, ... The recalculated version added by a previous `rd_recalculate()` is reused.
#'
#' @param field Name of the calculated field.
#' @param data Data.
#' @param dic Dictionary.
#' @return Character.
#' @noRd
recalc_name <- function(field, data, dic) {
  k <- 0

  repeat {
    name <- if (k == 0) paste0(field, "_recalc") else paste0(field, "_recalc_", k)

    if (recalc_generated(name, dic) || !name %in% c(dic$field_name, names(data))) {
      return(name)
    }

    k <- k + 1
  }
}
