#' Translate REDCap Logic to R Logic
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Converts a REDCap logic expression into R-compatible logic. Processes one logic expression (`logic`) for one target variable (`var`) at a time. Supports common REDCap operators (`and`, `or`, `=`, `<`, `>`, etc.) and handles event-specific logic in longitudinal projects. Logic involving smart variables or repeated instruments may require manual review.
#'
#' @param project A list containing the REDCap data, dictionary, and event mapping (expected `redcap_data()` output). Overrides `data`, `dic`, and `event_form`.
#' @param data A `data.frame` or `tibble` with the REDCap dataset.
#' @param dic A `data.frame` with the REDCap dictionary.
#' @param event_form Only applicable for longitudinal projects (presence of events). Event-to-form mapping for longitudinal projects.
#' @param logic A single REDCap logic string (e.g., `"if([exc_1]='1' or [inc_1]='0', 1, 0)"`).
#' @param var A single string specifying the target variable the logic applies to.
#'
#' @details
#' * Translates REDCap operators and functions into R equivalents:
#'   - `=` → `==`, `<>` → `!=`, `and` → `&`, `or` → `|`.
#'   - Converts functions like `if()`, `rounddown()`, `datediff()`, `sum()` to R equivalents.
#' * Handles date transformations and empty strings (`''`) → `NA`.
#' * Adjusts logic for longitudinal data using `event_form` if provided.
#' * Evaluates the translated R logic against the dataset and returns the results.
#' * Logic with repeated instruments, smart variables, or multiple events per variable may require manual inspection.
#'
#' @return A list with:
#' \describe{
#'   \item{rlogic}{The translated R-compatible logic as a string.}
#'   \item{eval}{The evaluation of the translated logic on the provided dataset, filtered by event if applicable.}
#' }
#'
#' @examples
#' # Translate a single REDCap logic expression for one variable
#' covican |>
#'   rd_rlogic(
#'     logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0', 1, 0)",
#'     var = "screening_fail_crit"
#'   )
#'
#' @export

rd_rlogic <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, logic, var) {

  # Handle potential overwriting when both `project` and other arguments are provided
  if (!is.null(project)) {
    env_vars <- check_proj(project, data, dic, event_form)

    list2env(env_vars, envir = environment())
  }

  # Ensure both `data` and `dic` are provided; stop if either is missing
  if (is.null(data) | is.null(dic)) {
    stop("Both `data` and `dic` (data and dictionary) arguments must be provided.")
  }

  # Check if the project is longitudinal (more than one event present in the data)
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  # Check for repeated instruments
  repeat_instrument <- "redcap_repeat_instrument" %in% names(data) && any(!is.na(data$redcap_repeat_instrument))

  # Error: data is longitudinal, but event_form isn't provided
  if (is.null(event_form) & longitudinal) {
    stop("There is more than one event in the data, but the event-form correspondence hasn't been specified.")
  }

  # If user accidentally passes multiple logic expressions or multiple vars
  if (length(logic) > 1) {
    warning("`logic` contains more than one expression; rd_rlogic processes only one logic at a time. Using the first element.", call. = FALSE)
    logic <- logic[[1]]
  }
  if (length(var) > 1) {
    warning("`var` contains more than one variable name; rd_rlogic processes only one variable at a time. Using the first element.", call. = FALSE)
    var <- var[[1]]
  }

  rlogic <- logic # Initialize REDCap logic to be converted

  # Process checkboxes and other specific cases in REDCap logic
  if (grepl("\\)\\]", rlogic)) {
    num_vars <- stringr::str_count(rlogic, "]")
    for (i in 1:num_vars) {
      rlogic <- gsub("\\[(.+)\\((\\d+)\\)\\]", "[\\1___\\2]", rlogic)
    }
  }

  # Modify event-name and current-instance occurrences
  rlogic <- gsub("\\[event\\-name\\]\\[", "[", rlogic)
  rlogic <- gsub("\\]\\[current-instance\\]", "]", rlogic)

  # Update REDCap specific variable names
  rlogic <- gsub("\\[event\\-name\\]", "[redcap_event_name]", rlogic)
  rlogic <- gsub("\\[user\\-dag\\-name\\]", "[redcap_data_access_group]", rlogic)
  rlogic <- gsub("\\[record\\-dag\\-name\\]", "[redcap_data_access_group]", rlogic)

  # Get the variables evaluated in the REDCap logic
  rlogic_var <- unlist(stringr::str_extract_all(rlogic, "\\[[\\w,\\-]+\\]"))
  rlogic_var <- gsub("^\\[|\\]$", "", rlogic_var)

  # Check if the variables are present in the data or events
  if (longitudinal) {
    check_lgl <- rlogic_var %in% names(data) | rlogic_var %in% data$redcap_event_name
  } else {
    check_lgl <- rlogic_var %in% names(data)
  }

  # Error: one of the variables is in a repeated instrument
  if (repeat_instrument) {
    rep_forms <- unique(na.omit(data$redcap_repeat_instrument))
    bad <- dic$field_name %in% rlogic_var & dic$form_name %in% rep_forms
    if (any(bad)) {
      vars <- paste0(dic$field_name[bad], " (form:", dic$form_name[bad], ")", collapse = ", ")
      stop(sprintf(
        "This function cannot translate logic involving variables that belong to repeated instruments. Review the following variables manually: %s",
        vars
      ), call. = FALSE)
    }
  }

  # Error: any variable in logic is not found in the data
  if (any(!check_lgl)) {
    stop("REDCap logic contains variables that are not found in the database (possibly a smart variable).")
  }

  # Proceed to transcribe REDCap logic to R logic
  rlogic <- gsub('"', "'", rlogic) # Replace double quotes with single quotes for R compatibility

  # Verify if any of the variables in the logic is now a factor in the dataset (example: [copd] + [hemato_neo], if we do this it generates a warning)
  vars_calc <- rlogic |>
    stringr::str_extract_all("(?<=\\[)[^\\]]+(?=\\]\\s*[+\\-*/])") |>
    purrr::pluck(1)

  factors <- data |>
    dplyr::select(dplyr::where(is.factor)) |>
    names()

  vars_calc <- intersect(vars_calc, factors)

  if (length(vars_calc) > 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(vars_calc),
        ~ {
          field <- dplyr::cur_column()

          choices <- dic$choices_calculations_or_slider_labels[dic$field_name == field]

          parts <- strsplit(choices, "\\|")[[1]]
          parts <- parts[grepl(",", parts, fixed = FALSE)]

          nums <- trimws(sub(",.*$", "", parts))
          labs <- trimws(sub("^[^,]*,\\s*", "", parts))

          mapping <- setNames(as.numeric(nums), labs)

          as.numeric(mapping[as.character(data[[field]])])
        }
      ))
  }

  # Convert REDCap functions to equivalent R functions
  rlogic <- gsub("if\\s?\\(", "ifelse(", rlogic)
  rlogic <- gsub("rounddown(.*),0\\)", "floor\\1)", rlogic)
  rlogic <- gsub("rounddown(.*),1\\)", "round\\1, 1)", rlogic)
  rlogic <- gsub("rounddown(.*),2\\)", "round\\1, 2)", rlogic)
  rlogic <- gsub("rounddown(.*),3\\)", "round\\1, 3)", rlogic)
  rlogic <- gsub("rounddown(.*)\\)", "floor\\1)", rlogic)
  rlogic <- gsub("datediff\\s?", "lubridate::time_length(lubridate::interval", rlogic)
  rlogic <- gsub("sum\\((.*?)\\)", "rowSums(cbind(\\1))", rlogic)
  rlogic <- gsub("year\\((.*?)\\)", "lubridate::year(\\1)", rlogic)

  # Handle date formats in logic
  if (grepl("'dmy'", rlogic)) {
    rlogic <- gsub("'(\\d\\d-\\d\\d-\\d\\d\\d\\d)'", "lubridate::dmy('\\1')", rlogic)
  } else if (grepl("'mdy'", rlogic)) {
    rlogic <- gsub("'(\\d\\d-\\d\\d-\\d\\d\\d\\d)'", "lubridate::mdy('\\1')", rlogic)
  } else if (grepl("'ymd'", rlogic)) {
    rlogic <- gsub("'(\\d\\d\\d\\d-\\d\\d-\\d\\d)'", "lubridate::ymd('\\1')", rlogic)
  } else if (grepl("'ydm'", rlogic)) {
    rlogic <- gsub("'(\\d\\d\\d\\d-\\d\\d-\\d\\d)'", "lubridate::ydm('\\1')", rlogic)
  } else if (grepl("'myd'", rlogic)) {
    rlogic <- gsub("'(\\d\\d-\\d\\d\\d\\d-\\d\\d)'", "lubridate::myd('\\1')", rlogic)
  } else if (grepl("'dym'", rlogic)) {
    rlogic <- gsub("'(\\d\\d-\\d\\d\\d\\d-\\d\\d)'", "lubridate::dym('\\1')", rlogic)
  }

  # Remove 'true' value from the logic as it's not used in R
  rlogic <- gsub("\\,\\s?true", "", rlogic)

  # Replace the date format specifications for 'dmy', 'mdy', etc., with 'year', 'day', 'month' as needed
  rlogic <- gsub("\\,\\s?'y'\\,\\s?'dmy'", "), 'year'", rlogic)
  rlogic <- gsub("\\,\\s?'d'\\,\\s?'dmy'", "), 'day'", rlogic)
  rlogic <- gsub("\\,\\s?'m'\\,\\s?'dmy'", "), 'month'", rlogic)

  # Modify the closing parentheses to correctly align with 'year', 'day', 'month' for R
  rlogic <- gsub("\\,\\s?'y'\\)", "), 'year')", rlogic)
  rlogic <- gsub("\\,\\s?'d'\\)", "), 'day')", rlogic)
  rlogic <- gsub("\\,\\s?'m'\\)", "), 'month')", rlogic)

  # Change variables specification. If [][] we get the event with the first claudator. If not, the event will be the same as the one of the calculated variable.

  # Vector with all the [][] if found:
  var_event <- unlist(stringr::str_extract_all(rlogic, "\\[[\\w,\\-]+\\]\\[[\\w,\\-]+\\]"))

  # If there are variables in the format [][] (event + variable):
  if (length(var_event) > 0) {
    # Separate the event and variable names
    list_var_event <- purrr::map(var_event, function(x) {
      x <- unlist(stringr::str_split(x, "\\]\\["))
      x <- gsub("\\[", "", x)
      x <- gsub("\\]", "", x)
    })

    # Check if the same variable is evaluated for different events (this logic can't be transcribed)
    n_events <- data.frame(do.call(rbind, list_var_event))
    names(n_events) <- c("events", "vars")
    n_events <- n_events |>
      dplyr::group_by(.data$vars) |>
      dplyr::summarise(n = length(unique(.data$events)))

    if (any(n_events$n > 1)) {
      stop("The logic cannot be transcribed because the same variable is specified for different events.")
    }

    # Apply the previously defined function to get the value in the corresponding event and fill it for all the rows of data
    for (i in seq_along(list_var_event)) {
      data <- fill_data(list_var_event[[i]][1], list_var_event[[i]][2], data)
    }

    # Update rlogic by removing the event-specific variable format ([][] -> [])
    rlogic <- gsub("\\[\\w+\\]\\[", "[", rlogic)
  }

  # Change variable specification from [] to data$

  # Change first [.] = '' for is.na(data$.) and [.] <>'' for !is.na(data$.)
  rlogic <- gsub("\\[(\\w+)\\]\\s?<>\\s?''", "!is.na(data$\\1)", rlogic)
  rlogic <- gsub("\\[(\\w+)\\]\\s?=\\s?''", "is.na(data$\\1)", rlogic)
  rlogic <- gsub("\\[(\\w+)\\]", "data$\\1", rlogic)

  # Optional: Inside the interval function, there might be date variables. We would need to wrap them with as.Date().
  # The code is commented out because it's not always needed, but you can enable it if needed.
  # if(grepl("interval\\(", rlogic)) {
  #   interval_str <- unlist(str_match_all(rlogic, "interval\\(.*?\\)"))
  #   interval_str2 <- map(interval_str, ~gsub("(data\\$\\w+)", "as.Date(\\1)", .x))
  #   for(i in 1:length(interval_str2)) {
  #     rlogic <- stringi::stri_replace_all_fixed(rlogic, interval_str[[i]], interval_str2[[i]])
  #   }
  # }

  # Change the REDCap operators into R operators
  rlogic <- gsub("=", "==", rlogic)
  rlogic <- gsub("<==", "<=", rlogic)
  rlogic <- gsub(">==", ">=", rlogic)
  rlogic <- gsub("<>", "!=", rlogic)
  rlogic <- gsub(" and ", " & ", rlogic)
  rlogic <- gsub(" or ", " | ", rlogic)

  # Remove '' (empty strings) after comparison operators like <, >, <=, >=
  rlogic <- gsub("\\s?<\\s?'([\\d\\.]+)'", " < \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?>\\s?'([\\d\\.]+)'", " > \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?<=\\s?'([\\d\\.]+)'", " <= \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?>=\\s?'([\\d\\.]+)'", " >= \\1", rlogic, perl = TRUE)

  # Transform '' (empty strings) into missing values (NA)
  rlogic <- gsub("''", "NA", rlogic)

  # After transcribing the logic, we need to evaluate it based on the corresponding event of the variable. This is only necessary if the data contains more than one event:
  if (!is.null(event_form)) {
    # Get the form where the variable is found through the dictionary
    form_var <- dic |>
      dplyr::filter(.data$field_name == var) |>
      dplyr::pull(.data$form_name)

    # Get the event(s) through the event-form mapping
    event_var <- event_form |>
      dplyr::filter(.data$form == form_var) |>
      dplyr::pull(.data$unique_event_name)
  }

  # Redefine the rounding function to match the one in REDCap for rounding .5 decimals similarly (e.g., 2.5 ~ 3)
  # round <- function(x, digits) {
  #   posneg <- sign(x)
  #   z <- abs(x)*10^digits
  #   z <- z + 0.5 + sqrt(.Machine$double.eps)
  #   z <- trunc(z)
  #   z <- z/10^digits
  #   z*posneg
  # }

  # Check for date fields in the logic that are still in character class
  date_class <- dic |>
    dplyr::filter(.data$field_name %in% rlogic_var) |>
    dplyr::filter(grepl("^date_|^datetime_", .data$text_validation_type_or_show_slider_number)) |>
    dplyr::pull(.data$field_name)

  if (any(purrr::map(data |> dplyr::select(dplyr::all_of(date_class)), class) == "character")) {
    warning("The dataset contains date fields stored as character class, which may lead to incorrect evaluation results. To format these date fields, use the `rd_dates` function before proceeding.", call. = FALSE)

    # Evaluate the logic in the R format
    rlogic_eval <- suppressWarnings(try(eval(parse(text = rlogic)), silent = TRUE))
  } else {
    # Evaluate the logic in the R format
    rlogic_eval <- try(eval(parse(text = rlogic)), silent = TRUE)
  }


  # If there's an error or the result is empty, stop the process
  if (inherits(rlogic_eval, "try-error") | length(rlogic_eval) == 0) {
    stop("The logic could not be evaluated after translation.")
  } else {
    # If there's more than one event, return the evaluation only for the specified event(s):
    if (!is.null(event_form)) {
      return(
        list(
          rlogic = rlogic,
          eval = data |>
            dplyr::mutate(
              calc = rlogic_eval,
              calc = ifelse(!.data$redcap_event_name %in% event_var, NA, .data$calc)
            ) |>
            dplyr::pull(.data$calc)
        )
      )
    } else {
      # If there is only one event, return the result directly
      return(
        list(
          rlogic = rlogic,
          eval = rlogic_eval
        )
      )
    }
  }
}
