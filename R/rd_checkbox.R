#' Transform Checkbox Variables in a REDCap Project
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function is used to process checkbox variables in a REDCap dataset. By default, it changes their default categories ("Unchecked" and "Checked") to new ones ("No" and "Yes). Optionally, the function can also evaluate the branching logic for checkbox fields and adjust the data and dictionary accordingly.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param checkbox_labels Character vector of length 2 for labels of unchecked/checked values. Default: `c("No", "Yes")`.
#' @param checkbox_names Logical. If `TRUE` (default), checkbox columns are renamed using choice labels. With `FALSE`, the columns keep their names, but their values are already the labels and each option is a field of the dictionary: running the function again on its result stops with an error instead of converting them again.
#' @param na_logic Controls how missing values are set based on branching logic. Must be one of `"none"` (do nothing), `"missing"` (set to `NA` only when the logic evaluation is `NA`), or `"eval"` (set to `NA` when the logic evaluates to `FALSE` or `NA`). Defaults to `"missing"`.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Transformed dataset with checkbox fields as factors and optionally renamed.}
#'   \item{dictionary}{Updated dictionary with checkbox fields expanded and optionally renamed.}
#'   \item{event_form}{The `event_form` passed in (if applicable).}
#'   \item{results}{Summary of transformations and any fields needing review.}
#' }
#'
#' @details
#' * Checkbox columns are expected in REDCap wide format (`field___code`).
#' * Branching logic evaluation requires `event_form` for longitudinal projects.
#' * Names are cleaned and truncated to 60 characters; uniqueness is enforced.
#' * Fields that cannot be evaluated are listed in `results`.
#' * In the branching logic and calculations, the options get their new names and keep their codes `0` and `1`, as in REDCap logic (e.g. `[cb(1)] = '1'` is `[cb_option] = '1'`, and `[cb(1)] + [cb(2)] >= 2` is `[cb_option] + [cb_other] >= 2`). `rd_dictionary()` writes the labels when it translates the logic into R.
#'
#'
#' @examples
#' # Basic usage with a project object
#' res <- rd_checkbox(covican)
#'
#' # With custom labels
#' res <- rd_checkbox(data = covican$data,
#'                    dic = covican$dictionary,
#'                    event_form = covican$event_form,
#'                    checkbox_labels = c("Not present", "Present"))
#'
#' # Keep original checkbox names
#' res <- rd_checkbox(covican, checkbox_names = FALSE)
#'
#' # Longitudinal project with NA logic
#' res <- rd_checkbox(data = covican$data,
#'                    dic = covican$dictionary,
#'                    event_form = covican$event_form,
#'                    na_logic = "eval")
#'
#' @family process functions
#' @export
#' @importFrom stats setNames na.omit

rd_checkbox <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, checkbox_labels = c("No", "Yes"), checkbox_names = TRUE, na_logic = "missing") {
  results <- NULL
  rlogic_eval <- NULL

  # validate na_logic against allowed choices
  na_logic <- check_choice(na_logic, "na_logic", c("none", "missing", "eval"), partial = TRUE)
  
  # Error: one label for unchecked and one for checked
  if (length(checkbox_labels) != 2) {
    rd_abort("{.arg checkbox_labels} must have two labels: the first for unchecked and the second for checked (e.g. c(\"No\", \"Yes\")).")
  }

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
  check_inputs(data, dic, dic_cols = c("field_name", "field_type", "field_label", "choices_calculations_or_slider_labels", "branching_logic_show_field_only_if"))
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_flag(checkbox_names, "checkbox_names")

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

  # Identify checkbox variables in the data (those with '___' in their names)
  var_check <- names(data)[grep("___", names(data))]

  # Remove factor-type checkbox variables from the list
  var_check_factors <- var_check[grep("\\.factor$", var_check)]

  # Identify checkbox variables in the dictionary
  var_check_dic <- dic$field_name[dic$field_type == "checkbox"]

  # Ensure there are checkbox fields in either the data or the dictionary
  # A project without checkboxes is returned unchanged, so that the function can be part of any pipeline
  if (length(var_check) == 0 && length(var_check_dic) == 0) {
    rd_warn("The project has no checkbox fields: the data is returned unchanged.")

    return(
      list(
        data = data,
        dictionary = dic,
        event_form = event_form,
        results = if (!is.null(results)) stringr::str_glue("{results}")
      ) |>
        purrr::compact()
    )
  }
  # Check for missing checkbox fields in either the data or the dictionary
  if (length(var_check) > 0 && length(var_check_dic) == 0) {
    rd_abort("No checkbox fields found in the dictionary.")
  }
  if (length(var_check) == 0 && length(var_check_dic) > 0) {
    rd_abort("No checkbox fields found in the data. If they have already been transformed (e.g. with rd_checkbox() or rd_transform()), there is nothing left to do.")
  }

  # Error: checkboxes already transformed (e.g. by rd_checkbox(checkbox_names = FALSE), which keeps their names): their
  # values are labels, not the codes of REDCap, and converting them again would change them or turn them into NA.
  # - By the structure: in the dictionary of REDCap the options (`cb___1`) are never fields (the field is `cb`), while
  #   rd_checkbox(checkbox_names = FALSE) writes each option as a field of its own (the labels can't tell it, as they can
  #   look like codes, e.g. `checkbox_labels = c("0", "1")`)
  options <- unique(sub("\\.factor$", "", var_check))
  expanded <- options[options %in% dic$field_name & !sub("___.*$", "", options) %in% var_check_dic]
  # - By the values (e.g. transformed data with the original dictionary)
  raw_codes <- c("0", "1", "Unchecked", "Checked", "")
  transformed <- var_check[vapply(var_check, function(v) {
    values <- as.character(data[[v]])
    !all(is.na(values) | values %in% raw_codes)
  }, logical(1))]

  if (length(expanded) > 0 || length(transformed) > 0) {
    shown_expanded <- utils::head(expanded, 10)
    shown <- utils::head(transformed, 10)
    rd_abort(
      c("The checkboxes have already been transformed (e.g. with {.fn rd_checkbox} or {.fn rd_transform}): there is nothing left to do.",
        "i" = if (length(expanded) > 0) "The dictionary already has the option{?s} {.var {shown_expanded}} as field{?s} of {?its/their} own.",
        "i" = if (length(transformed) > 0) "{.var {shown}} {?has/have} values that aren't the codes of REDCap (0/1 or Unchecked/Checked)."),
      class = "REDCapDM_error_data"
    )
  }

  # Checkboxes already converted to factors by rd_factor() (their options have the labels Unchecked/Checked)
  already_factor <- length(var_check_factors) == 0 &&
    any(purrr::map_lgl(var_check, ~ is.factor(data[[.x]]) && "Unchecked" %in% levels(data[[.x]])))

  # Options without their `.factor` version when other options have it: it is created from the option (0/1), so that
  # every option ends with the same labels
  if (length(var_check_factors) > 0) {
    options_num <- var_check[!grepl("\\.factor$", var_check)]
    lacking <- options_num[!paste0(options_num, ".factor") %in% names(data)]
    for (option in lacking) {
      value <- as.character(data[[option]])
      data[[paste0(option, ".factor")]] <- factor(
        ifelse(value %in% c("1", "Checked"), "Checked", ifelse(value %in% c("0", "Unchecked"), "Unchecked", NA)),
        levels = c("Unchecked", "Checked")
      )
      if (option %in% names(labels)) labels[paste0(option, ".factor")] <- labels[[option]]
    }
    var_check <- names(data)[grep("___", names(data))]
    var_check_factors <- var_check[grep("\\.factor$", var_check)]
  }

  # Normalize checkbox codes before evaluating branching logic.
  if (length(var_check_factors) > 0) {
    var_check <- var_check[!grepl("\\.factor$", var_check)]
  } else {
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(var_check),
          ~ {
            value <- as.character(.x)

            dplyr::case_when(
              value %in% c("0", "Unchecked") ~ 0,
              value %in% c("1", "Checked") ~ 1,
              TRUE ~ NA_real_
            )
          }
        )
      )
  }

  # Update results with the transformation
  reason <- if (na_logic == "eval")
    "when the logic isn't satisfied or it's missing"
  else if (na_logic == "missing")
    "when the logic is missing"
  else
    ""

  base <- checkbox_message(checkbox_labels, checkbox_names)

  transf_message <- if (reason == "") base
  else paste0(base, " For checkboxes that have a branching logic, ", reason, " their values will be set to missing.")

  if (is.null(results)) {
    results <- c(results, stringr::str_glue("{transf_message} (rd_checkbox)\n"))
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

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. {transf_message} (rd_checkbox)\n"))
  }

  # Evaluate branching logic for checkbox variables if applicable
  if (any(dic$field_type %in% "checkbox" & !dic$branching_logic_show_field_only_if %in% c(NA, ""))) {
    if (is.null(event_form) && longitudinal) {
      rd_warn("Branching logic evaluation could not be performed because the project is longitudinal (the data has the column `redcap_event_name`) and the event-form correspondence was not specified. Please provide the {.arg event_form} argument to enable branching logic evaluation.")
    } else {

      caption <- "Checkbox variables advisable to be reviewed"
      review <- NULL
      review2 <- NULL

      for (i in seq_along(var_check_dic)) {
        # Identify variables associated with each checkbox option
        vars_data <- names(data)[grep(stringr::str_glue("^{var_check_dic[i]}___"), names(data))]
        vars_data <- vars_data[!grepl("\\.factor$", vars_data)]

        # Retrieve branching logic for the checkbox field
        logic <- dic$branching_logic_show_field_only_if[dic$field_name == var_check_dic[i]]

        # If there is branching logic, attempt to translate and evaluate it
        if (!is.na(logic) && !logic %in% "") {
          rlogic_eval <- NULL

          # Checking if the logic is already in R format
          if (grepl("<>|\\[.*?\\]", logic) && !grepl("==|!=|\\$", logic)) {
            # Translate REDCap logic to R language using rd_rlogic function
            rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = logic, var = var_check_dic[i]), silent = TRUE)

            if (!inherits(rlogic, "try-error")) {
              # Evaluate the logic and apply missing values accordingly
              rlogic_eval <- rlogic$eval
            }
          } else {
            rlogic_eval <- tryCatch(eval(parse(text = logic)), error = function(e) NULL)
          }

          # Logic that can't be evaluated: the options of this checkbox are left as they are
          if (is.null(rlogic_eval) || length(rlogic_eval) != nrow(data)) {
            review2 <- c(review2, var_check_dic[i])
            next
          }

          # Set missing values where logic is not satisfied
          if (na_logic == "eval") {
            for (j in seq_along(vars_data)) {
              data[[vars_data[j]]] <- ifelse(rlogic_eval, as.character(data[[vars_data[j]]]), NA)
            }
          } else if (na_logic == "missing") {
            # Set missing values only where logic evaluation is missing
            for (j in seq_along(vars_data)) {
              data[[vars_data[j]]] <- ifelse(!is.na(rlogic_eval), as.character(data[[vars_data[j]]]), NA)
            }
          }
        } else {
          # If no branching logic, mark variable for review
          review <- c(review, var_check_dic[i])
        }
      }

      # Summarize the results of the branching logic review
      if (!is.null(review)) {
        results1 <- tibble::tibble("Variables without any branching logic" = review)
        results <- c(results, "", knitr::kable(results1, "pipe", align = c("ccc"), caption = caption))
        if (!is.null(review2)) {
          results <- c(results, "\n")
          caption <- NULL
        }
      }

      if (!is.null(review2)) {
        results2 <- tibble::tibble("Variables with a logic that can't be transcribed" = review2)
        results <- c(results, knitr::kable(results2, "pipe", align = c("ccc"), caption = caption))
        rd_warn(c("The branching logic of {cli::qty(length(review2))}the checkbox{?es} {.var {review2}} could not be evaluated: {cli::qty(length(review2))}{?its/their} options were not changed.", "i" = "Check the {.field results} element of the output for details."))
      }

      data <- data
    }
  }

  # Transform checkbox variables into the defined labels
  if (length(var_check_factors) > 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_check_factors),
        ~ factor(.x, levels = c("Unchecked", "Checked"), labels = .env$checkbox_labels)
      ))
  } else {
    data <- data |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_check),
        ~ factor(as.character(.x), levels = c(0, 1), labels = .env$checkbox_labels)
      ))
  }


  # Identify checkbox variables
  var_check <- names(data)[grep("___", names(data))]

  # Trim the checkbox variable names
  names_trim <- unique(gsub("___.*$", "", var_check))

  correspondence <- NULL

  # Update the dictionary with new variable names and labels
  for (i in seq_along(names_trim)) {
    # Find variable names in `var_check` that start with the current name in `names_trim`
    svar_check <- grep(stringr::str_glue("^{names_trim[i]}___"), var_check, value = TRUE)
    svar_check <- svar_check[!grepl("\\.factor$", svar_check)]

    # Extract labels corresponding to the found variables
    label <- labels[svar_check]
    label <- gsub(".*choice=", "", label)
    label <- gsub("\\)", "", label)

    # Data without variable labels: take the option labels from the dictionary choices
    choices <- dic$choices_calculations_or_slider_labels[dic$field_name == names_trim[i]]
    if (length(choices) == 1 && !is.na(choices)) {
      parts <- trimws(strsplit(choices, "\\|")[[1]])
      parts <- parts[grepl(",", parts)]
      dic_label <- stats::setNames(trimws(sub("^[^,]*,\\s*", "", parts)), trimws(sub(",.*$", "", parts)))
      dic_label <- unname(dic_label[checkbox_code(svar_check, names_trim[i], choices)])
      no_label <- is.na(label) | !nzchar(label)
      label[no_label] <- dic_label[no_label]
    }

    # Add rows to dictionary for each checkbox option
    new_row <- dic |>
      dplyr::filter(.data$field_name == names_trim[i])

    # Repeat the `new_row` for each checkbox option and update fields
    new_row <- purrr::map_dfr(seq_len(length(svar_check)), ~new_row) |>
      dplyr::mutate(
        field_name = svar_check,
        field_label = label,
        choices_calculations_or_slider_labels = stringr::str_glue("0, {checkbox_labels[1]} | 1, {checkbox_labels[2]}")
      )

    # Add the new row to the dictionary and remove the original checkbox variable
    dic <- dic |>
      tibble::add_row(new_row, .before = which(dic$field_name == names_trim[i])) |>
      dplyr::filter(!.data$field_name %in% names_trim[i])

    # Create clean variable names for the new checkbox options
    label_name <- purrr::map_chr(label, ~ janitor::make_clean_names(.x))
    label_name <- gsub("^x(\\d)", "\\1", label_name)

    if (checkbox_names) {

      # Generate new variable names by appending the cleaned labels to the original variable names
      out <- stringr::str_glue("{names_trim[i]}_{label_name}")

      # Trim the name if it exceeds 60 characters (to prevent very long names)
      out <- strtrim(out, 60)

      # Save correspondence between the old names and the new names
      x <- cbind(paste0(names_trim[i], "(", checkbox_code(svar_check, names_trim[i], choices), ")"), out)
      correspondence <- rbind(correspondence, x)

      # For each new variable name, check if it already exists in the dataset
      for (j in seq_along(out)) {
        out0 <- out[j]

        # Ensure uniqueness by appending a unique suffix if necessary (`_1`, `_2`, ...: a name that the logic can use, e.g.
        # `[cb_one_1]`), also with the fields of the dictionary
        out[j] <- utils::tail(make.unique(c(unique(c(names(data), dic$field_name)), out[j]), sep = "_"), 1)

        # If the name was changed to ensure uniqueness, issue a warning
        if (out[j] != out0) {
          rd_warn("The transformed checkbox name {.var {out0}} already exists in the dataset. It has been renamed to {.var {out[j]}} to avoid conflicts.")

          # (only the row of this option: two options can have the same clean name, e.g. `A-B` and `A B`)
          correspondence[nrow(correspondence) - length(out) + j, "out"] <- out[j]
        }

        # Update the variable names in the data and dictionary
        names(data) <- dplyr::case_when(names(data) == svar_check[j] ~ out[j],
                                        names(data) == paste0(svar_check[j], ".factor") ~ paste0(out[j], ".factor"),
                                        .default = names(data))

        # Update the labels to match the new variable names
        names(labels) <- dplyr::case_when(names(labels) == svar_check[j] ~ out[j],
                                          names(labels) == paste0(svar_check[j], ".factor") ~ paste0(out[j], ".factor"),
                                          .default = names(labels))

        # Update the dictionary with the new variable name
        dic <- dic |>
          dplyr::mutate(field_name = dplyr::case_when(field_name == svar_check[j] ~ out[j], TRUE ~ field_name))
      }
    } else {
      # Trim the name if it exceeds 60 characters (to prevent very long names)
      out <- strtrim(svar_check, 60)

      # Save correspondence between the old names and the new names
      x <- cbind(paste0(names_trim[i], "(", checkbox_code(svar_check, names_trim[i], choices), ")"), out)
      correspondence <- rbind(correspondence, x)
    }
  }

  # After processing all the checkboxes, transform the branching logic that contains checkboxes
  correspondence <- as.data.frame(correspondence)

  # Filter the dictionary to include only variables that were renamed during checkbox transformation
  cats <- dic |>
    dplyr::select("field_name", "choices_calculations_or_slider_labels") |>
    dplyr::filter(.data$field_name %in% correspondence$out)

  # Split the `choices_calculations_or_slider_labels` into separate options for each checkbox
  cats <- cats |>
    dplyr::mutate(choices_calculations_or_slider_labels = strsplit(.data$choices_calculations_or_slider_labels, "\\|")) |>
    tidyr::unnest("choices_calculations_or_slider_labels")

  # Separate numeric and category parts from the options
  cats <- cats |>
    # (the code is the text before the first comma, also without a space after it, e.g. `5,Always`; an option without
    # label is left out)
    tidyr::separate(.data$choices_calculations_or_slider_labels, c("num", "cat"), "\\s*,\\s*", extra = "merge", fill = "right") |>
    dplyr::filter(.data$cat != "") |>
    dplyr::mutate(num = trimws(.data$num), cat = trimws(.data$cat))

  # Merge the transformed data with the correspondence to link new names with the original variables
  cats <- merge(cats, correspondence, by.x = "field_name", by.y = "out")

  # Apply the new names of the options to the branching logic and calculations of the dictionary: `[cb(1)]` is
  # `[cb_option]`, with the codes 0 and 1 (REDCap logic always compares the codes). rd_dictionary() writes the labels of
  # the options in the logic it translates into R, where the comparison with the factor is clear also with labels that
  # look like codes (e.g. `checkbox_labels = c("1", "0")`)
  # (the texts between quotes are kept, e.g. `[t] = '[cb(1)]'`)
  checkbox_logic <- function(logic) {
    if (nrow(correspondence) == 0) {
      return(logic)
    }
    logic <- as.character(logic)
    for (k in seq_len(nrow(correspondence))) {
      logic <- outside_quotes(logic, function(s) gsub(paste0("[", correspondence$V1[k], "]"), paste0("[", correspondence$out[k], "]"), s, fixed = TRUE))
    }
    logic
  }
  dic <- dic |>
    dplyr::mutate(
      choices_calculations_or_slider_labels = checkbox_logic(.data$choices_calculations_or_slider_labels),
      branching_logic_show_field_only_if = checkbox_logic(.data$branching_logic_show_field_only_if)
    )

  # Returning checkboxes to numeric version & applying the same missing values to their factor version
  if (length(var_check_factors) > 0) {

    correspondence <- correspondence |>
      dplyr::mutate(out_factor = paste0(out, ".factor"))

    data <- data |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(correspondence$out), as.numeric),
        dplyr::across(dplyr::any_of(correspondence$out_factor), ~{

          # (.env: a field of the project can be called `data`)
          var_fac <- .env$data[[dplyr::cur_column()]]
          var_num <- .env$data[[sub("\\.factor$", "", dplyr::cur_column())]]

          dplyr::case_when(!is.na(var_num) ~ var_fac)
          })
      )
  }

  # Apply the labels to the data
  data <- data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)


  # Return the modified data, dictionary, event_form, and results
  list(
    data = data,
    dictionary = dic,
    event_form = event_form,
    results = stringr::str_glue("{results}")
  ) |>
    purrr::compact() # Remove any NULL elements from the output list
}

#' Codes of the checkbox columns
#'
#' REDCap exports each option of a checkbox as the column `field___code`, with
#' the code in lower case and its `-` and `.` replaced by `_`. This returns the
#' code of the dictionary that corresponds to each column (e.g. `A` for
#' `symptoms___a`, `-1` for `symptoms____1`), or the suffix of the column when
#' it is not among the choices.
#'
#' @param columns Checkbox columns of the data (`field___code`).
#' @param field Name of the checkbox field.
#' @param choices Choices of the field in the dictionary.
#' @return Character vector with the code of each column.
#' @noRd
checkbox_code <- function(columns, field, choices) {
  suffix <- substring(columns, nchar(field) + 4)
  codes <- character(0)
  if (length(choices) == 1 && !is.na(choices)) {
    parts <- trimws(strsplit(choices, "\\|")[[1]])
    codes <- trimws(sub(",.*$", "", parts[grepl(",", parts)]))
  }
  code <- codes[match(suffix, gsub("[-.]", "_", tolower(codes)))]
  ifelse(is.na(code), suffix, code)
}


#' Description of the transformation of the checkboxes in the results
#'
#' @param checkbox_labels Labels of the unchecked and checked options.
#' @param checkbox_names Whether the options are renamed with their labels.
#' @return Character: e.g. "Transforming checkboxes: changing their values to No/Yes and changing their names to the
#'   names of its options."
#' @noRd
checkbox_message <- function(checkbox_labels, checkbox_names) {
  paste0(
    "Transforming checkboxes: changing their values to ", checkbox_labels[1], "/", checkbox_labels[2],
    if (checkbox_names) " and changing their names to the names of its options", "."
  )
}
