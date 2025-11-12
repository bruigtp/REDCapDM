#' Transform Checkbox Variables in a REDCap Project
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function is used to convert checkbox variables in a REDCap dataset from their default categories (e.g., "Checked" and "Unchecked") to numeric values (0 and 1), and optionally, relabel and rename them according to user-defined options. It also evaluates branching logic for checkbox fields and adjusts the data and dictionary accordingly.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping, typically the output of the `redcap_data` function. If provided, it overrides individual `data`, `dic`, and `event_form` arguments.
#' @param data A `data.frame` or `tibble` representing the REDCap dataset containing the checkbox variables.
#' @param dic A `data.frame` representing the REDCap dictionary with metadata, including field names, field types, and branching logic.
#' @param event_form A `data.frame` or `list` mapping event names to forms for longitudinal projects. Optional; defaults to `NULL` if not applicable.
#' @param checkbox_labels A character vector of length 2 specifying the labels to be used for the checkbox options. Defaults to `c("No", "Yes")`.
#' @param checkbox_na Logical indicating whether to assign `NA` to checkbox fields when the branching logic condition is not satisfied. Defaults to `FALSE`.
#' @param checkbox_names Logical indicating whether to rename the checkbox variables in the dataset and dictionary according to their label options. Defaults to `TRUE`.
#'
#' @return A list containing the following elements:
#'   \item{data}{The transformed dataset with checkbox variables updated.}
#'   \item{dictionary}{The updated dictionary reflecting any changes made to the checkbox fields, including renamed variables.}
#'   \item{event_form}{The event-form mapping (if provided).}
#'   \item{results}{A summary of the transformation process, including any issues with branching logic or fields that need review.}
#'
#' @details
#' This function is primarily used to process checkbox fields in a REDCap project. It performs the following:
#'   - Converts checkbox variables in the dataset from text labels ("Checked" and "Unchecked") to numeric values (0 and 1), and then applies the specified labels.
#'   - Optionally renames the checkbox variables based on their labels (e.g., transforming variable names like `varname___1` to `varname_Yes`).
#'   - Optionally modifies the branching logic in the REDCap dictionary to reflect renamed checkbox options.
#'
#' @note
#' - If `event_form` is not provided for a longitudinal project, the function may not be able to evaluate branching logic correctly.
#'
#' @examples
#' # Example with a project object containing data and dictionary
#' results <- rd_checkbox(project = covican)
#'
#' # Example with custom labels for the checkboxes
#' results <- rd_checkbox(
#'  data = covican$data,
#'  dic = covican$dictionary,
#'  checkbox_labels = c("No", "Yes")
#' )
#'
#' # Example without renaming checkbox fields
#' results <- rd_checkbox(covican, checkbox_names = FALSE)
#'
#' @export
#' @importFrom stats setNames na.omit

rd_checkbox <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, checkbox_labels = c("No", "Yes"), checkbox_na = FALSE, checkbox_names = TRUE) {
  results <- NULL
  rlogic_eval <- NULL

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

  # Identify checkbox variables in the data (those with '___' in their names)
  var_check <- names(data)[grep("___", names(data))]

  # Remove factor-type checkbox variables from the list
  var_check_factors <- var_check[grep(".factor$", var_check)]

  # Identify checkbox variables in the dictionary
  var_check_dic <- dic$field_name[dic$field_type == "checkbox"]

  # Ensure there are checkbox fields in either the data or the dictionary
  if (length(var_check) == 0 & length(var_check_dic) == 0) {
    stop("No checkbox fields found in either the data or the dictionary.")
  }

  # Check for missing checkbox fields in either the data or the dictionary
  if (length(var_check) > 0 & length(var_check_dic) == 0) {
    stop("No checkbox fields found in the dictionary.")
  }
  if (length(var_check) == 0 & length(var_check_dic) > 0) {
    stop("No checkbox fields found in the data.")
  }

  # Remove factor-type checkbox variables from the data
  if (length(var_check_factors) > 0) {
    data <- data |>
      dplyr::select(-tidyselect::all_of(var_check_factors))

    var_check <- var_check[!grepl(".factor$", var_check)]
  } else {
    if (any(purrr::map_lgl(var_check, ~ "Unchecked" %in% levels(data[[.x]])))) {
      # Transform checkbox variables into binary values (0 or 1)
      data <- data |>
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(var_check),
          ~ dplyr::case_when(
            .x == "Unchecked" ~ 0,
            .x == "Checked" ~ 1,
            TRUE ~ NA
          )
        ))
    }
  }

  # Update results with the this transformation
  transf_message <- if (!repeat_instrument) {
    reason <- if (checkbox_na)
      "when the logic isn't satisfied or it's missing"
    else
      "when the logic is missing"

    stringr::str_glue(
      "Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. ",
      "For checkboxes that have a branching logic, {reason} their values will be set to missing."
    )
  } else {
    stringr::str_glue(
      "Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options."
    )
  }

  if (is.null(results)) {
    results <- c(results, stringr::str_glue("1. {transf_message} (rd_checkbox)\n"))
  } else {
    last_val_res <- results |>
      stringr::str_extract("^(\n)?\\d+\\.") |>
      na.omit() |>
      dplyr::last() |>
      stringr::str_remove("\\.") |>
      as.numeric()

    results <- c(results, stringr::str_glue("\n\n{last_val_res + 1}. {transf_message} (rd_checkbox)\n"))
  }

  # Evaluate branching logic for checkbox variables if applicable
  if (any(dic$field_type == "checkbox" & dic$branching_logic_show_field_only_if != "")) {
    if (is.null(event_form) & longitudinal) {
      warning("Branching logic evaluation could not be performed because the project contains multiple events and the event-form correspondence was not specified. Please provide the `event_form` argument to enable branching logic evaluation.", call. = FALSE)
    } else {
      # Handle projects with repeated instruments where branching logic can't be evaluated
      if (repeat_instrument) {
        warning("The project contains repeated instruments, and this function cannot accurately evaluate the branching logic of checkboxes in such cases.", call. = FALSE)
      }

      # warning(stringr::str_glue("There are {sum(dic$field_type == 'checkbox' & dic$branching_logic_show_field_only_if != '')} checkboxes with branching logic, please specify `checkbox_na` to determine the behaviour of this function for these cases.\n For more information `?rd_checkbox`."), call. = FALSE)


      caption <- "Checkbox variables advisable to be reviewed"
      review <- NULL
      review2 <- NULL

      for (i in seq_along(var_check_dic)) {
        # Identify variables associated with each checkbox option
        vars_data <- names(data)[grep(stringr::str_glue("{var_check_dic[i]}___"), names(data))]

        # Retrieve branching logic for the checkbox field
        logic <- dic$branching_logic_show_field_only_if[dic$field_name == var_check_dic[i]]

        # If there is branching logic, attempt to translate and evaluate it
        if (!is.na(logic) & !logic %in% "") {
          # Checking if the logic is already in R format
          if (grepl("<>|\\[.*?\\]", logic) & !grepl("==|!=|\\$", logic)) {
            # Translate REDCap logic to R language using rd_rlogic function
            rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = logic, var = var_check_dic[i]), silent = TRUE)

            if (!inherits(rlogic, "try-error")) {
              # Evaluate the logic and apply missing values accordingly
              rlogic_eval <- rlogic$eval
            } else {
              # Track variables that can't be evaluated due to logic issues
              review2 <- c(review2, var_check_dic[i])
            }
          } else {
            rlogic_eval <- eval(parse(text = logic))
          }

          # Set missing values where logic is not satisfied
          if (checkbox_na) {
            for (j in seq_along(vars_data)) {
              data[, vars_data[j]] <- ifelse(rlogic_eval, as.character(data[, vars_data[j]]), NA)
            }
          } else {
            # Set missing values only where logic evaluation is missing
            for (j in seq_along(vars_data)) {
              data[, vars_data[j]] <- ifelse(!is.na(rlogic_eval), as.character(data[, vars_data[j]]), NA)
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
      }

      data <- data
    }
  }

  # Transform checkbox variables into "No"/"Yes" labels
  data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(var_check),
      ~ factor(.x, levels = 0:1, labels = checkbox_labels)
    ))

  # Identify checkbox variables
  var_check <- names(data)[grep("___", names(data))]

  # Trim the checkbox variable names
  names_trim <- unique(gsub("___.*$", "", var_check))

  correspondence <- NULL

  # Update the dictionary with new variable names and labels
  for (i in seq_along(names_trim)) {
    # Find variable names in `var_check` that start with the current name in `names_trim`
    svar_check <- grep(stringr::str_glue("^{names_trim[i]}___"), var_check, value = TRUE)

    # Extract labels corresponding to the found variables
    label <- labels[svar_check]
    label <- gsub(".*choice=", "", label)
    label <- gsub("\\)", "", label)

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
      x <- cbind(gsub("___(.+)", "\\(\\1\\)", svar_check), out)
      correspondence <- rbind(correspondence, x)

      # For each new variable name, check if it already exists in the dataset
      for (j in seq_along(out)) {
        out0 <- out[j]

        # Ensure uniqueness by appending a unique suffix if necessary
        out[j] <- utils::tail(make.unique(c(names(data), out[j])), 1)

        # If the name was changed to ensure uniqueness, issue a warning
        if (out[j] != out0) {
          warning(
            stringr::str_glue(
              "The transformed checkbox name '{out0}' already exists in the dataset. It has been renamed to '{out[j]}' to avoid conflicts."
            )
          )
        }

        # Update the variable names in the data and dictionary
        names(data) <- dplyr::case_when(names(data) == svar_check[j] ~ out[j], TRUE ~ names(data))

        # Update the labels to match the new variable names
        names(labels) <- dplyr::case_when(names(labels) == svar_check[j] ~ out[j], TRUE ~ names(labels))

        # Update the dictionary with the new variable name
        dic <- dic |>
          dplyr::mutate(field_name = dplyr::case_when(field_name == svar_check[j] ~ out[j], TRUE ~ field_name))
      }
    } else {
      # Trim the name if it exceeds 60 characters (to prevent very long names)
      out <- strtrim(svar_check, 60)

      # Save correspondence between the old names and the new names
      x <- cbind(gsub("___(.+)", "\\(\\1\\)", svar_check), out)
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
    tidyr::separate(.data$choices_calculations_or_slider_labels, c("num", "cat"), ", ", extra = "merge") |>
    dplyr::filter(.data$cat != "") |>
    dplyr::mutate(num = trimws(.data$num), cat = trimws(.data$cat))

  # Merge the transformed data with the correspondence to link new names with the original variables
  cats <- merge(cats, correspondence, by.x = "field_name", by.y = "out")

  # Prepare the branching logic expressions for the transformed checkbox variables
  cats <- cats |>
    dplyr::mutate(
      factor = paste0("[", .data$field_name, "]='", .data$cat, "'"),
      V1 = stringi::stri_replace_all_fixed(cats$V1, c("(", ")"), c("\\(", "\\)"), vectorize_all = FALSE),
      redcap = paste0("\\[", .data$V1, "\\] ?=? ?'?", .data$num, "'?"),
      redcap2 = paste0("\\[", .data$V1, "\\] ?<?>? ?'?", .data$num, "'?")
    ) |>
    dplyr::select(-"V1") |>
    dplyr::arrange(dplyr::desc(.data$redcap))

  # Create the final mappings for replacing the branching logic in REDCap with the new factor logic
  replace <- setNames(cats$factor, cats$redcap)
  replace2 <- setNames(cats$factor, cats$redcap2)

  # Apply the new branching logic to the dictionary by replacing the old logic with the new ones
  dic <- dic |>
    dplyr::mutate(
      choices_calculations_or_slider_labels = stringr::str_replace_all(.data$choices_calculations_or_slider_labels, replace),
      choices_calculations_or_slider_labels = stringr::str_replace_all(.data$choices_calculations_or_slider_labels, replace2),
      branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace),
      branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace2)
    )

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
