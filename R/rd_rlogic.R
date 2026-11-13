#' Translate REDCap Logic to R Logic
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Converts a REDCap logic expression into R-compatible logic. Processes one logic expression (`logic`) for one target variable (`var`) at a time. Supports common REDCap operators (`and`, `or`, `=`, `<`, `>`, etc.) and handles event-specific logic in longitudinal projects and repeated instruments. Logic involving smart variables may require manual review.
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
#'   - Converts the functions `if()`, `datediff()`, `round()`, `rounddown()`, `roundup()`, `sum()`, `mean()`, `median()`, `min()`, `max()`, `mod()`, `exponential()`, `isnumber()`, `contains()` and `length()` to R equivalents.
#'   - As in REDCap, `sum()`, `mean()`, `median()`, `min()` and `max()` are computed for each row and ignore blank values.
#' * Handles date transformations and empty strings (`''`) → `NA`.
#' * Adjusts logic for longitudinal data using `event_form` if provided.
#' * Evaluates the translated R logic against the dataset and returns the results.
#' * With repeated instruments and repeating events, the logic is evaluated in the rows where `var` is collected. As in REDCap, in a repeating instrument the variables of the other forms take the value of the base row of the same record and event. Logic that uses variables of another repeating instrument can't be translated.
#' * As in REDCap, `datediff()` returns the absolute difference unless its last argument is `true`, its units are `"y"`, `"M"` (months), `"d"`, `"h"`, `"m"` (minutes) and `"s"`, and `"today"` is the current date.
#' * The same variable can be used in several events (e.g. `[visit_2_arm_1][weight] - [visit_1_arm_1][weight]`).
#' * Logic with smart variables or other REDCap functions may require manual inspection.
#'
#' @return A list with:
#' \describe{
#'   \item{rlogic}{The translated R-compatible logic as a string.}
#'   \item{eval}{The evaluation of the translated logic on the provided dataset, filtered by event if applicable.}
#'   \item{eval_data}{A `data.frame` with the record identifier, the event and repetition columns present in the dataset (`redcap_event_name`, `redcap_repeat_instrument`, `redcap_repeat_instance`) and `eval`, to identify the record of each evaluated value.}
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
#' @importFrom lubridate time_length interval
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
  
  # Error: invalid arguments
  check_inputs(data, dic, dic_cols = c("field_name", "text_validation_type_or_show_slider_number"))
  check_strings(logic, "logic", null_ok = FALSE)
  check_strings(var, "var", null_ok = FALSE)

  # Check if the project is longitudinal (more than one event present in the data)
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  # Repeating instruments of the project (event/form combinations)
  rep_pairs <- repeat_info(data)$pairs

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

  # As in REDCap, datediff() returns the absolute difference unless its last argument is `true`
  rlogic <- datediff_absolute(rlogic)

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

  # Repeated instruments: the logic is evaluated in the rows where `var` is collected
  if (nrow(rep_pairs) > 0) {
    target_form <- var_form(var, dic)
    target_events <- if (!is.null(event_form) && !is.na(target_form)) {
      event_form$unique_event_name[event_form$form == target_form]
    } else {
      unique(event_values(data))
    }
    logic_fields <- intersect(rlogic_var, names(data))
    logic_forms <- vapply(logic_fields, var_form, character(1), dic = dic)

    # Error: a variable of another repeating instrument (its instance can't be linked to the rows of `var`)
    other_rep <- !is.na(logic_forms) & logic_forms != ifelse(is.na(target_form), "", target_form) &
      logic_forms %in% rep_pairs$form[rep_pairs$event %in% target_events]
    if (any(other_rep)) {
      stop(sprintf(
        "This function cannot translate logic that uses variables of another repeated instrument, because their instance can't be determined. Review the following variables manually: %s",
        paste0(logic_fields[other_rep], " (form:", logic_forms[other_rep], ")", collapse = ", ")
      ), call. = FALSE)
    }

    # `var` belongs to a repeating instrument: as in REDCap, the variables of the other forms take the value of the base row of the same record and event
    if (!is.na(target_form) && any(rep_pairs$form == target_form & rep_pairs$event %in% target_events)) {
      data <- fill_from_base(data, logic_fields[is.na(logic_forms) | logic_forms != target_form])
    }
  }

  # Error: any variable in logic is not found in the data
  if (any(!check_lgl)) {
    stop(sprintf("REDCap logic contains variables that are not found in the database (possibly a smart variable or an event that doesn't exist): %s", paste0("[", unique(rlogic_var[!check_lgl]), "]", collapse = ", ")), call. = FALSE)
  }

  # Proceed to transcribe REDCap logic to R logic
  rlogic <- gsub('"', "'", rlogic) # Replace double quotes with single quotes for R compatibility

  # Variables of the logic that are factors in the dataset (e.g. after rd_factor()) are evaluated with their original
  # codes, as in REDCap: [dm] = '1' or [copd] + [hemato_neo] give the same result before and after rd_factor().
  # They are left as factors if the logic compares them with their labels (e.g. [dm] = 'Yes', as rd_dictionary() writes it)
  factors <- data |>
    dplyr::select(dplyr::where(is.factor)) |>
    names()

  for (field in intersect(rlogic_var, factors)) {
    mapping <- factor_code_mapping(field, dic)

    if (length(mapping) == 0) {
      next
    }

    label_pattern <- paste0("\\[", field, "\\]\\s*(=|<>|!=)\\s*'(", paste(escape_regex(names(mapping)), collapse = "|"), ")'")
    codes <- unname(mapping[as.character(data[[field]])])

    # Leave the variable as it is if the logic uses its labels or if any of its values isn't one of them
    if (grepl(label_pattern, rlogic, perl = TRUE) || any(!is.na(data[[field]]) & is.na(codes))) {
      next
    }

    data[[field]] <- if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", mapping))) as.numeric(codes) else codes
  }

  # Convert REDCap functions to equivalent R functions
  rlogic <- gsub("if\\s?\\(", "ifelse(", rlogic)
  rlogic <- gsub("datediff\\s?", "lubridate::time_length(lubridate::interval", rlogic)
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

  # Remove the date format argument of datediff() ('dmy', 'mdy', 'ymd'...): the dates in the logic were converted above
  rlogic <- gsub("(\\,\\s?'[yMdhms]')\\,\\s?'(dmy|mdy|ymd|ydm|myd|dym)'", "\\1", rlogic)

  # Units of datediff(): as in REDCap, 'M' are months and 'm' minutes
  datediff_units <- c(y = "year", M = "month", d = "day", h = "hour", m = "minute", s = "second")
  for (u in names(datediff_units)) {
    rlogic <- gsub(paste0("\\,\\s?'", u, "'\\)"), paste0("), '", datediff_units[[u]], "')"), rlogic)
  }

  # 'today' in datediff() is the current date
  rlogic <- gsub("'today'", "Sys.Date()", rlogic)

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

    # Variables used in several events (e.g. the change between two visits)
    multi_event_vars <- n_events$vars[n_events$n > 1]

    # Apply the previously defined function to get the value in the corresponding event and fill it for all the rows of data
    for (i in seq_along(list_var_event)) {
      event_i <- list_var_event[[i]][1]
      var_i <- list_var_event[[i]][2]

      if (var_i %in% multi_event_vars) {
        # One column per event: `<variable>__<event>`
        event_col_i <- paste0(var_i, "__", event_i)
        data[[event_col_i]] <- fill_data(event_i, var_i, data)[[var_i]]
        rlogic <- gsub(paste0("[", event_i, "][", var_i, "]"), paste0("[", event_col_i, "]"), rlogic, fixed = TRUE)
      } else {
        data <- fill_data(event_i, var_i, data)
      }
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
  
  # REDCap functions (after the operators, so that the `=` of their translation is not changed)
  rlogic <- translate_functions(rlogic)

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
  if (inherits(rlogic_eval, "try-error") | (length(rlogic_eval) == 0 & nrow(data) > 0)) {
    stop(paste0(
      "The logic could not be evaluated after translation. R logic: ", rlogic,
      if (inherits(rlogic_eval, "try-error")) paste0(" | R error: ", trimws(gsub("\\s+", " ", sub("^Error in .*? : ", "", as.character(rlogic_eval)))))
    ), call. = FALSE)
  } else {
    # If there's more than one event, return the evaluation only for the specified event(s):
    if (!is.null(event_form)) {
      rlogic_eval <- data |>
        dplyr::mutate(
          calc = rlogic_eval,
          calc = ifelse(!.data$redcap_event_name %in% event_var, NA, .data$calc)
        ) |>
        dplyr::pull(.data$calc)
    }
    
    # With repeated instruments, only the rows where `var` is collected are evaluated
    if (nrow(rep_pairs) > 0 && length(rlogic_eval) %in% c(1, nrow(data))) {
      rlogic_eval <- rlogic_eval[rep_len(seq_along(rlogic_eval), nrow(data))]
      rlogic_eval[!repeat_home_rows(data, dic, var, rep_pairs)] <- NA
    }

    return(
      list(
        rlogic = rlogic,
        eval = rlogic_eval,
        eval_data = rlogic_eval_data(data, rlogic_eval)
      )
    )
  }
}



#' Evaluation of a logic next to the record it belongs to
#'
#' @param data REDCap dataset the logic was evaluated on.
#' @param eval Evaluation of the logic (one value per row of `data`).
#' @return A `data.frame` with the record identifier, the event and repetition columns present in `data` and `eval`. `NULL` if `eval` doesn't have one value per row.
#' @noRd
rlogic_eval_data <- function(data, eval) {
  if (!length(eval) %in% c(1, nrow(data))) {
    return(NULL)
  }

  # Record identifier: record_id or, if it has another name, the first column
  id_col <- if ("record_id" %in% names(data)) "record_id" else names(data)[1]
  key_cols <- intersect(c(id_col, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance"), names(data))

  eval_data <- as.data.frame(data)[, key_cols, drop = FALSE]
  eval_data$eval <- eval
  rownames(eval_data) <- NULL

  eval_data
}


#' Codes of the labels of a factor variable
#'
#' @param field Variable name as it appears in the dataset.
#' @param dic REDCap dictionary.
#' @return Named character vector: the codes, named by their labels. Empty if
#'   they can't be determined.
#' @noRd
factor_code_mapping <- function(field, dic) {
  # Checkbox options: REDCap exports them as Unchecked/Checked
  if (grepl("___", field)) {
    return(c(Unchecked = "0", Checked = "1"))
  }

  type <- dic$field_type[match(field, dic$field_name)]

  if (identical(type, "yesno")) {
    return(c(No = "0", Yes = "1"))
  }
  if (identical(type, "truefalse")) {
    return(c(False = "0", True = "1"))
  }

  choices <- dic$choices_calculations_or_slider_labels[match(field, dic$field_name)]

  if (length(choices) == 0 || is.na(choices) || !nzchar(choices)) {
    return(character(0))
  }

  parts <- trimws(strsplit(choices, "\\|")[[1]])
  parts <- parts[grepl(",", parts)]

  stats::setNames(trimws(sub(",.*$", "", parts)), trimws(sub("^[^,]*,\\s*", "", parts)))
}

#' Escape the special characters of a regular expression
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}



#' Fill variables of the base rows into the rows of the repeating instruments
#'
#' In a repeating instrument, REDCap takes the value of the fields of the other
#' (non-repeating) forms from the base row of the same record and event.
#'
#' @param data REDCap dataset.
#' @param fields Variables to fill.
#' @return `data` with `fields` filled in the rows of the repeating instruments.
#' @noRd
fill_from_base <- function(data, fields) {
  instrument <- repeat_instrument_values(data)
  rows <- which(!is.na(instrument))

  if (length(fields) == 0 || length(rows) == 0) {
    return(data)
  }

  id <- if ("record_id" %in% names(data)) data$record_id else data[[1]]
  instance <- if ("redcap_repeat_instance" %in% names(data)) data$redcap_repeat_instance else rep(NA, nrow(data))
  key <- paste(id, event_values(data), sep = "\r")
  base <- which(is.na(instrument) & is.na(instance))
  source <- base[match(key[rows], key[base])]

  for (f in fields) {
    data[[f]][rows] <- data[[f]][source]
  }

  data
}

#' Absolute value of datediff() unless it is signed
#'
#' REDCap's `datediff()` returns the absolute difference unless its last
#' argument (`returnSignedValue`) is `true`, so the unsigned calls are wrapped in
#' `abs()`.
#'
#' @param logic REDCap logic.
#' @return The logic with the unsigned `datediff()` calls wrapped in `abs()`.
#' @noRd
datediff_absolute <- function(logic) {
  out <- ""
  rest <- logic

  repeat {
    start <- regexpr("datediff\\s*\\(", rest, perl = TRUE)
    if (start == -1) {
      break
    }

    chars <- strsplit(rest, "")[[1]]
    open <- start + attr(start, "match.length") - 1

    # Closing parenthesis of the call and its arguments (commas at the first level)
    depth <- 0
    close <- NA
    commas <- integer(0)
    for (k in open:length(chars)) {
      if (chars[k] == "(") {
        depth <- depth + 1
      } else if (chars[k] == ")") {
        depth <- depth - 1
        if (depth == 0) {
          close <- k
          break
        }
      } else if (chars[k] == "," && depth == 1) {
        commas <- c(commas, k)
      }
    }

    # Unbalanced parentheses: leave the rest as it is
    if (is.na(close)) {
      break
    }

    bounds <- c(open, commas, close)
    args <- trimws(substring(rest, bounds[-length(bounds)] + 1, bounds[-1] - 1))
    signed <- tolower(gsub("['\"]", "", args[length(args)])) == "true"

    call <- substr(rest, start, close)
    out <- paste0(out, substr(rest, 1, start - 1), if (signed) call else paste0("abs(", call, ")"))
    rest <- substr(rest, close + 1, nchar(rest))
  }

  paste0(out, rest)
}


#' Replace the calls to a REDCap function with their R translation
#'
#' Finds every call to `fun` (with nested parentheses and quoted text in its arguments) and replaces it with the code that
#' `translate` builds from its arguments.
#'
#' @param x A logic.
#' @param fun Name of the REDCap function.
#' @param translate A function that receives the arguments of a call (character vector) and returns its R code.
#' @return The logic with the calls translated.
#' @noRd
replace_function <- function(x, fun, translate) {
  pattern <- paste0("(?<![\\w.$])", fun, "\\s*\\(")
  out <- ""
  rest <- x

  repeat {
    start <- regexpr(pattern, rest, perl = TRUE)

    if (start == -1) {
      break
    }

    chars <- strsplit(rest, "")[[1]]
    open <- start + attr(start, "match.length") - 1

    # Closing parenthesis of the call and its arguments (commas at the first level, outside quotes)
    depth <- 0
    quote <- ""
    close <- NA
    commas <- integer(0)
    for (k in open:length(chars)) {
      if (nzchar(quote)) {
        if (chars[k] == quote) quote <- ""
      } else if (chars[k] %in% c("'", "\"")) {
        quote <- chars[k]
      } else if (chars[k] == "(") {
        depth <- depth + 1
      } else if (chars[k] == ")") {
        depth <- depth - 1
        if (depth == 0) {
          close <- k
          break
        }
      } else if (chars[k] == "," && depth == 1) {
        commas <- c(commas, k)
      }
    }

    # Unbalanced parentheses: leave the rest as it is
    if (is.na(close)) {
      break
    }

    bounds <- c(open, commas, close)
    args <- trimws(substring(rest, bounds[-length(bounds)] + 1, bounds[-1] - 1))

    # The arguments can contain other calls to the same function
    args <- vapply(args, replace_function, character(1), fun = fun, translate = translate, USE.NAMES = FALSE)

    out <- paste0(out, substr(rest, 1, start - 1), translate(args))
    rest <- substr(rest, close + 1, nchar(rest))
  }

  paste0(out, rest)
}

#' Translate the REDCap functions to R
#'
#' As in REDCap, `sum()`, `mean()`, `median()`, `min()` and `max()` ignore blank values and are computed for each row
#' (blank when all the values are blank); `rounddown()` and `roundup()` keep the requested decimals.
#'
#' @param x A logic.
#' @return The logic with the functions translated.
#' @noRd
translate_functions <- function(x) {
  values <- function(args) paste0("cbind(", paste(args, collapse = ", "), ")")
  digits <- function(args) if (length(args) > 1) args[2] else "0"
  rounding <- function(r_fun) function(args) {
    if (digits(args) %in% c("0", "'0'", "\"0\"")) {
      paste0(r_fun, "(", args[1], ")")
    } else {
      paste0(r_fun, "((", args[1], ") * 10^", digits(args), ") / 10^", digits(args))
    }
  }

  x <- replace_function(x, "rounddown", rounding("floor"))
  x <- replace_function(x, "roundup", rounding("ceiling"))
  x <- replace_function(x, "sum", function(args) {
    paste0("ifelse(rowSums(!is.na(", values(args), ")) == 0, NA, rowSums(", values(args), ", na.rm = TRUE))")
  })
  x <- replace_function(x, "mean", function(args) {
    paste0("ifelse(rowSums(!is.na(", values(args), ")) == 0, NA, rowMeans(", values(args), ", na.rm = TRUE))")
  })
  x <- replace_function(x, "median", function(args) paste0("apply(", values(args), ", 1, stats::median, na.rm = TRUE)"))
  x <- replace_function(x, "min", function(args) paste0("pmin(", paste(args, collapse = ", "), ", na.rm = TRUE)"))
  x <- replace_function(x, "max", function(args) paste0("pmax(", paste(args, collapse = ", "), ", na.rm = TRUE)"))
  x <- replace_function(x, "mod", function(args) paste0("((", args[1], ") %% (", args[2], "))"))
  x <- replace_function(x, "exponential", function(args) paste0("exp(", args[1], ")"))
  x <- replace_function(x, "isnumber", function(args) paste0("!is.na(suppressWarnings(as.numeric(", args[1], ")))"))
  x <- replace_function(x, "contains", function(args) paste0("grepl(tolower(", args[2], "), tolower(", args[1], "), fixed = TRUE)"))
  x <- replace_function(x, "length", function(args) paste0("ifelse(is.na(", args[1], "), 0, nchar(as.character(", args[1], ")))"))

  x
}
