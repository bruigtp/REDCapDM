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
#'   - Converts the functions `if()`, `datediff()`, `round()`, `rounddown()`, `roundup()`, `sum()`, `mean()`, `median()`, `min()`, `max()`, `mod()`, `exponential()`, `isnumber()`, `isblankormissingcode()`, `contains()`, `not_contain()`, `starts_with()`, `ends_with()`, `left()`, `right()`, `mid()`, `find()`, `replace_text()`, `concat()`, `concat_ws()`, `upper()`, `lower()`, `trim()`, `length()`, `stdev()`, `isinteger()`, `year()`, `month()` and `day()` to R equivalents.
#'   - As in REDCap, `sum()`, `mean()`, `median()`, `min()` and `max()` are computed for each row and ignore blank values.
#' * Handles date transformations and empty strings (`''`) → `NA`.
#' * Adjusts logic for longitudinal data using `event_form` if provided.
#' * Evaluates the translated R logic against the dataset and returns the results.
#' * With repeated instruments and repeating events, the logic is evaluated in the rows where `var` is collected. As in REDCap, in a repeating instrument the variables of the other forms take the value of the base row of the same record and event. Logic that uses variables of another repeating instrument can't be translated.
#' * As in REDCap, `datediff()` returns the absolute difference unless its last argument is `true`, its units are `"y"` (years of 365.2425 days), `"M"` (months of 30.44 days), `"d"`, `"h"`, `"m"` (minutes) and `"s"`, `"today"` is the current date and `"now"` the current date and time. With two time fields (e.g. `HH:MM`), it is the difference between the two times of the same day, and its date format (e.g. `'hhmm'`) is not used.
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
#' @family process functions
#' @export

rd_rlogic <- function(project = NULL, data = NULL, dic = NULL, event_form = NULL, logic, var) {

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
  check_inputs(data, dic, dic_cols = c("field_name", "text_validation_type_or_show_slider_number"))
  check_event_form(event_form)
  data <- plain_data(data)
  dic <- plain_dic(dic)
  check_strings(logic, "logic", null_ok = FALSE)
  check_strings(var, "var", null_ok = FALSE)

  # Check if the project is longitudinal (the data has events)
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  # Repeating instruments of the project (event/form combinations)
  rep_pairs <- repeat_info(data)$pairs

  # Record identifier (record_id or else the first field of the dictionary, whatever its position in the data)
  id_col <- id_field(data, dic)

  # Error: data is longitudinal, but event_form isn't provided
  if (is.null(event_form) && longitudinal) {
    rd_abort("The data is longitudinal (it has the column `redcap_event_name`), but the event-form correspondence hasn't been specified.")
  }

  # If user accidentally passes multiple logic expressions or multiple vars
  if (length(logic) > 1) {
    rd_warn("{.arg logic} contains more than one expression; rd_rlogic processes only one logic at a time. Using the first element.")
    logic <- logic[[1]]
  }
  if (length(var) > 1) {
    rd_warn("{.arg var} contains more than one variable name; rd_rlogic processes only one variable at a time. Using the first element.")
    var <- var[[1]]
  }

  # Error: in longitudinal projects, the form of `var` gives the events where the logic is evaluated
  if (!is.null(event_form) && !var %in% dic$field_name) {
    rd_abort(
      c("{.arg var} must be a field of the dictionary: {.val {var}} is not.", "i" = "In longitudinal projects, its form gives the events where the logic is evaluated."),
      class = "REDCapDM_error_argument"
    )
  }

  rlogic <- logic # Initialize REDCap logic to be converted

  # A logic written in several lines (also with Windows line breaks) or with tabs is read as one line
  rlogic <- outside_quotes(rlogic, function(s) gsub("[ \\t]*[\\r\\n][\\s]*|\\t", " ", s, perl = TRUE))

  # The texts between quotes with characters of operators, functions or fields (e.g. 'a=b', 'if(x)' or '[age]') are texts:
  # they are set aside during the translation and put back at the end
  masked <- mask_texts(rlogic)
  rlogic <- masked$logic

  # A function can be written with spaces before its parenthesis (e.g. `if  (` or `year ([date])`): they are removed, so
  # that all the functions are read in the same way (not `and (` or `or (`, which are operators; the texts with a
  # parenthesis were set aside above)
  rlogic <- gsub("(?<![\\w.$\\[])((?!(?i:and|or)\\s*\\()[A-Za-z_][\\w.]*)\\s+\\(", "\\1(", rlogic, perl = TRUE)

  # As in REDCap, datediff() returns the absolute difference unless its last argument is `true`
  rlogic <- datediff_absolute(rlogic)

  # datediff() of two time fields (HH:MM): the difference between the times of the same day (its date format, e.g.
  # 'hhmm', is irrelevant)
  rlogic <- datediff_times(rlogic, dic)

  # Checkboxes: [field(code)] is the column field___code of the data
  rlogic <- checkbox_columns(rlogic)

  # Modify event-name and current-instance occurrences
  rlogic <- gsub("\\[event\\-name\\]\\[", "[", rlogic)

  # Update REDCap specific variable names
  rlogic <- gsub("\\[event\\-name\\]", "[redcap_event_name]", rlogic)
  rlogic <- gsub("\\[user\\-dag\\-name\\]", "[redcap_data_access_group]", rlogic)
  rlogic <- gsub("\\[record\\-dag\\-name\\]", "[redcap_data_access_group]", rlogic)

  # Keep categorical Missing Data Codes distinguishable from ordinary values.
  rlogic <- replace_function(
    rlogic,
    "isblankormissingcode",
    function(args) {
      if (length(args) != 1L) {
        rd_abort(
          "isblankormissingcode() requires one argument.",
          class = "REDCapDM_error_logic"
        )
      }

      argument <- trimws(args[1])
      pattern <- "^((?:\\[[\\w-]+\\])?)\\[(\\w+)\\](\\[current-instance\\])?$"

      hit <- regmatches(
        argument,
        regexec(pattern, argument, perl = TRUE)
      )[[1]]

      if (length(hit) == 0L) {
        fields <- unique(gsub(
          "^\\[|\\]$",
          "",
          unlist(stringr::str_extract_all(argument, "\\[\\w+\\]"))
        ))

        companions <- paste0(fields, "_mdc")
        companions <- companions[
          companions %in% names(data) &
            !companions %in% dic$field_name
        ]

        if (any(vapply(companions, function(f) {
          any(
            !is.na(data[[f]]) &
              !is.na(data[[sub("_mdc$", "", f)]])
          )
        }, logical(1)))) {
          rd_abort(
            "Compound isblankormissingcode() expressions with preserved Missing Data Codes cannot be evaluated safely. Use a direct field reference or import with rm_mdc = TRUE.",
            class = "REDCapDM_error_logic"
          )
        }

        return(paste0(
          "isblankormissingcode(", argument, ")"
        ))
      }

      companion <- paste0(hit[3], "_mdc")

      if (!companion %in% names(data) ||
          companion %in% dic$field_name) {
        return(paste0(
          "isblankormissingcode(", argument, ")"
        ))
      }

      companion_ref <- paste0(
        hit[2], "[", companion, "]", hit[4]
      )

      paste0(
        "(", argument, " = '' or ",
        companion_ref, " <> '')"
      )
    }
  )

  # Keep instance references in the logic, but not in the field list.
  logic_for_fields <- gsub(
    "][current-instance]",
    "]",
    rlogic,
    fixed = TRUE
  )

  rlogic_var <- unlist(stringr::str_extract_all(
    logic_for_fields,
    "\\[[\\w,\\-]+\\]"
  ))

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
      other_rep_fields <- paste0(logic_fields[other_rep], " (form: ", logic_forms[other_rep], ")")
      rd_abort(
        c("This function cannot translate logic that uses variables of another repeated instrument, because their instance can't be determined.", "i" = "Review the following variables manually: {other_rep_fields}"),
        class = "REDCapDM_error_logic"
      )
    }

    # `var` belongs to a repeating instrument: as in REDCap, the variables of the other forms take the value of the base row of the same record and event
    if (!is.na(target_form) && any(rep_pairs$form == target_form & rep_pairs$event %in% target_events)) {
      data <- fill_from_base(data, logic_fields[is.na(logic_forms) | logic_forms != target_form], id_col)
    }
  }

  # Error: any variable in logic is not found in the data
  if (any(!check_lgl)) {
    unknown_vars <- paste0("[", unique(rlogic_var[!check_lgl]), "]")
    rd_abort("REDCap logic contains variables that are not found in the database (possibly a smart variable or an event that doesn't exist): {.code {unknown_vars}}", class = "REDCapDM_error_logic")
  }

  # As in REDCap, an empty text is a blank value: the empty texts of the variables of the logic (e.g. in data modified
  # after redcap_data(), which reads them as NA) are evaluated as NA, so that `[text] = ''`, `'' = [text]` and
  # `isblankormissingcode([text])` give the same result (the identifier of the record, the event and the repetition are kept)
  key_cols <- c(id_col, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
  for (field in setdiff(intersect(rlogic_var, names(data)), key_cols)) {
    if (is.character(data[[field]]) || is.factor(data[[field]])) {
      data[[field]][data[[field]] %in% ""] <- NA
    }
  }

  # Proceed to transcribe REDCap logic to R logic
  # Texts between double quotes are written between single quotes, as the rest of the translation expects (except the
  # texts with an apostrophe, e.g. "O'Brien")
  rlogic <- single_quotes(rlogic)

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

    # Include the extra category created for retained Missing Data Codes.
    mdc_labels <- character(0)
    mdc_col <- paste0(field, "_mdc")

    if (mdc_col %in% names(data) &&
        !mdc_col %in% dic$field_name) {
      mdc_value <- as.character(data[[mdc_col]])

      mdc_rows <- !is.na(mdc_value) &
        nzchar(trimws(mdc_value)) &
        !is.na(data[[field]])

      mdc_labels <- unique(
        as.character(data[[field]])[mdc_rows]
      )
      mdc_labels <- setdiff(mdc_labels, names(mapping))

      if (length(mdc_labels) > 0L) {
        numeric_codes <- suppressWarnings(
          as.numeric(unname(mapping))
        )
        numeric_codes <- numeric_codes[!is.na(numeric_codes)]

        extra_code <- if (length(numeric_codes) > 0L) {
          max(numeric_codes) + 1
        } else {
          1
        }

        mapping <- c(
          mapping,
          stats::setNames(
            rep(as.character(extra_code), length(mdc_labels)),
            mdc_labels
          )
        )
      }
    }

    # (labels between single or double quotes, e.g. [name] = "O'Brien")
    # (and labels with both quotes or backslashes, written with escaped characters, e.g. [name] = 'O\'Brien "A"')
    labels <- names(mapping)
    # (a label that is also a code, e.g. code 1 with label `2`, is read as a code, as in REDCap logic)
    only_labels <- labels[!labels %in% mapping]
    label_pattern <- paste0(
      "\\[", field,
      "\\](?:\\[current-instance\\])?\\s*(=|<>|!=)\\s*",
      "((['\"])(",
      paste(escape_regex(only_labels), collapse = "|"),
      ")\\3|",
      paste(escape_regex(r_text(only_labels)), collapse = "|"),
      ")"
    )
    codes <- unmapped <- unname(mapping[as.character(data[[field]])])
    unmasked <- unmask_texts(rlogic, masked$texts)
    uses_labels <- length(only_labels) > 0 && grepl(label_pattern, unmasked, perl = TRUE)

    # A logic that also uses the variable as a number (e.g. `[score] = '2' or [score] + 1 >= 4`) is evaluated with the
    # codes: its comparisons are with codes, as in REDCap (a label that isn't a code is written as its code)
    if (uses_labels && factor_other_use(unmasked, field, labels)) {

      # Several MDC labels share one additional numeric category.
      if (length(mdc_labels) > 1L) {
        comparisons <- logic_comparisons(unmasked)

        label_use <- comparisons$field %in% field &
          comparisons$complete &
          !is.na(comparisons$value) &
          grepl("^['\"]", comparisons$value)

        if (any(
          text_value(comparisons$value[label_use]) %in% mdc_labels
        )) {
          rd_abort(
            "The logic mixes labels of different Missing Data Codes with numeric operations on {.var {field}}. Use its _mdc column to distinguish the missing codes.",
            class = "REDCapDM_error_logic"
          )
        }
      }

      not_codes <- labels[!labels %in% mapping]
      if (length(not_codes) > 0) {
        unmasked <- relabel_logic(unmasked, data.frame(ref = field, code = not_codes, name = field, label = unname(mapping[not_codes])))
        masked <- mask_texts(unmasked)
        rlogic <- masked$logic
      }
      uses_labels <- FALSE
    }

    # Leave the variable as it is if the logic uses its labels or if any of its values isn't one of them
    if (uses_labels || any(!is.na(data[[field]]) & is.na(codes))) {
      next
    }

    if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", mapping))) {
      value <- restore_choice_codes(codes)

      data[[field]] <- if (is.numeric(value) || is.logical(value)) {
        as.numeric(value)
      } else {
        value
      }
    } else {
      data[[field]] <- codes
    }
    
  }

  # Convert REDCap functions to equivalent R functions
  # (only the functions: not a field whose name contains them, e.g. `[motif(1)]` or `[datediff]`)
  rlogic <- gsub("(?<![\\w.$\\[])if\\s?\\(", "ifelse(", rlogic, perl = TRUE)
  rlogic <- gsub("(?<![\\w.$\\[])datediff\\s?(?=\\()", "(lubridate::time_length(lubridate::interval", rlogic, perl = TRUE)

  # Arguments of datediff(): its dates are read with its own date format (e.g. 'dmy'; 'ymd' for the dates with time when it
  # has none), 'today' is the current date and 'now' the current date and time (in UTC, as the dates of rd_dates()), its
  # format and `true` or `false` (see datediff_absolute()) are removed, and its unit is translated. Only the arguments of
  # each datediff() are changed: e.g. `concat('today', ':', [text])` keeps the text 'today'
  rlogic <- datediff_arguments(rlogic)

  # Comparisons of a date field with 'today', 'now' or a date with time (e.g. `[visit] < 'today'`)
  rlogic <- date_comparisons(rlogic, dic)

  # Resolve explicit event/field/current-instance references.
  instance_refs <- stringr::str_match_all(
    rlogic,
    paste0(
      "\\[([A-Za-z0-9_]+)\\]",
      "\\[([A-Za-z0-9_]+)\\]",
      "\\[current-instance\\]"
    )
  )[[1]]

  if (nrow(instance_refs) > 0L) {
    if (!"redcap_repeat_instance" %in% names(data)) {
      rd_abort(
        "[current-instance] requires redcap_repeat_instance.",
        class = "REDCapDM_error_logic"
      )
    }

    instance_refs <- instance_refs[
      !duplicated(instance_refs[, 1]),
      ,
      drop = FALSE
    ]

    instance <- as.character(data$redcap_repeat_instance)
    instance[instance %in% ""] <- NA_character_

    # Encode identifiers as integer positions to avoid separator collisions.
    record_key <- match(
      data[[id_col]],
      unique(data[[id_col]])
    )
    key <- paste(record_key, instance, sep = "\r")

    for (k in seq_len(nrow(instance_refs))) {
      event_i <- instance_refs[k, 2]
      field_i <- instance_refs[k, 3]

      source <- which(
        as.character(data$redcap_event_name) %in% event_i &
          repeat_home_rows(data, dic, field_i, rep_pairs) &
          !is.na(instance)
      )

      if (length(source) == 0L) {
        rd_abort(
          "No repeating observations were found for {.var {field_i}} in {.val {event_i}}.",
          class = "REDCapDM_error_logic"
        )
      }

      if (anyDuplicated(key[source]) > 0L) {
        rd_abort(
          "Several source observations share the same record and instance for {.var {field_i}} in {.val {event_i}}.",
          class = "REDCapDM_error_logic"
        )
      }

      index <- source[match(key, key[source])]
      index[is.na(instance)] <- NA_integer_

      auxiliary <- utils::tail(
        make.unique(
          c(names(data), paste0("rd_current_instance_", k)),
          sep = "_"
        ),
        1L
      )

      data[[auxiliary]] <- data[[field_i]][index]

      rlogic <- gsub(
        instance_refs[k, 1],
        paste0("[", auxiliary, "]"),
        rlogic,
        fixed = TRUE
      )
    }
  }

  # References without an explicit event use the current row.
  rlogic <- gsub(
    "][current-instance]",
    "]",
    rlogic,
    fixed = TRUE
  )

  # Vector with all the [][] if found:
  var_event <- unique(unlist(stringr::str_extract_all(rlogic, "\\[[\\w,\\-]+\\]\\[[\\w,\\-]+\\]")))

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

    # Variables used in several events (e.g. the change between two visits) or also without event (e.g.
    # `[base_arm_1][value] + [value]`, where `[value]` is the value of the row): their values in each event are kept in
    # auxiliary columns, so that the variable keeps its own values
    without_event <- gsub("\\[[\\w,\\-]+\\]\\[[\\w,\\-]+\\]", "", rlogic, perl = TRUE)
    alone_vars <- n_events$vars[vapply(n_events$vars, function(v) grepl(paste0("[", v, "]"), without_event, fixed = TRUE), logical(1))]
    multi_event_vars <- union(n_events$vars[n_events$n > 1], alone_vars)

    # Apply the previously defined function to get the value in the corresponding event and fill it for all the rows of data
    for (i in seq_along(list_var_event)) {
      event_i <- list_var_event[[i]][1]
      var_i <- list_var_event[[i]][2]

      # An unindexed reference cannot select a repeating source instance.
      source_form <- var_form(var_i, dic)

      if (!is.na(source_form) &&
          any(rep_pairs$event %in% event_i &
              rep_pairs$form %in% source_form)) {
        rd_abort(
          c(
            "The reference [{event_i}][{var_i}] points to a repeating instrument without specifying its source instance.",
            "i" = "Use an explicit instance reference, such as [current-instance], only when the source and target instances correspond."
          ),
          class = "REDCapDM_error_logic"
        )
      }

      if (var_i %in% multi_event_vars) {
        # One column per event: `<variable>__<event>` (with another name if the data already has it, e.g. a field called
        # `value__base_arm_1`)
        event_col_i <- utils::tail(make.unique(c(unique(names(data)), paste0(var_i, "__", event_i)), sep = "_"), 1)
        data[[event_col_i]] <- fill_data(event_i, var_i, data, id_col)[[var_i]]
        rlogic <- gsub(paste0("[", event_i, "][", var_i, "]"), paste0("[", event_col_i, "]"), rlogic, fixed = TRUE)
      } else {
        data <- fill_data(event_i, var_i, data, id_col)
      }
    }

    # Update rlogic by removing the event-specific variable format ([][] -> [])
    rlogic <- gsub("\\[\\w+\\]\\[", "[", rlogic)
  }

  # Change variable specification from [] to data$

  # Fields whose name isn't a valid name in R (e.g. `if`, `in`, `TRUE` or `_x`) are written between backticks
  # (e.g. data$`if`)
  fields <- unique(gsub("^\\[|\\]$", "", unlist(regmatches(rlogic, gregexpr("\\[\\w+\\]", rlogic)))))
  for (field in fields[make.names(fields) != fields]) {
    rlogic <- gsub(paste0("[", field, "]"), paste0("[`", field, "`]"), rlogic, fixed = TRUE)
  }

  # Change first [.] = '' for is.na(data$.) and [.] <>'' for !is.na(data$.)
  rlogic <- gsub("\\[(\\w+|`\\w+`)\\]\\s?(<>|!=)\\s?''", "!is.na(data$\\1)", rlogic)
  rlogic <- gsub("\\[(\\w+|`\\w+`)\\]\\s?=\\s?''", "is.na(data$\\1)", rlogic)
  rlogic <- gsub("\\[(\\w+|`\\w+`)\\]", "data$\\1", rlogic)

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
  rlogic <- gsub("!==", "!=", rlogic)
  rlogic <- gsub("<>", "!=", rlogic)

  # A comparison with a negative number written without spaces (e.g. [a]<-3) is not an assignment in R
  rlogic <- gsub("<-", "< -", rlogic, fixed = TRUE)

  # As in REDCap, a blank value is different from any value: [x] <> '1' is TRUE when x is blank
  rlogic <- logical_operators(rlogic)
  
  # REDCap functions (after the operators, so that the `=` of their translation is not changed)
  rlogic <- translate_functions(rlogic)

  # Remove the quotes of the numbers (also negative) after comparison operators like <, >, <=, >=, so that they are compared as numbers
  rlogic <- gsub("\\s?<\\s?'(-?[\\d\\.]+)'", " < \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?>\\s?'(-?[\\d\\.]+)'", " > \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?<=\\s?'(-?[\\d\\.]+)'", " <= \\1", rlogic, perl = TRUE)
  rlogic <- gsub("\\s?>=\\s?'(-?[\\d\\.]+)'", " >= \\1", rlogic, perl = TRUE)

  # Transform '' (empty strings) into missing values (NA)
  rlogic <- gsub("''", "NA", rlogic)

  # The texts set aside at the beginning
  rlogic <- unmask_texts(rlogic, masked$texts)

  # An operation compared with an empty text (e.g. `trim([text]) = ''`) is compared with a blank value
  rlogic <- blank_comparisons(rlogic)
  rlogic <- not_equal_blank(rlogic)

  # A condition of if() that doesn't use any variable (e.g. `if(1 < 2, [a], 0)`) gives one value per row, as in REDCap
  rlogic <- recycle_constant_conditions(rlogic)

  # An if() whose values are dates (e.g. `year(if([flag] = 1, [a], [b]))`) gives dates: ifelse() gives numbers
  rlogic <- date_conditionals(rlogic, data)

  # After transcribing the logic, we need to evaluate it based on the corresponding event of the variable. This is only necessary if the data has events:
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
    rd_warn("The dataset contains date fields stored as character class, which may lead to incorrect evaluation results. To format these date fields, use the {.fn rd_dates} function before proceeding.")

    # Evaluate the logic in the R format
    rlogic_eval <- suppressWarnings(try(eval(parse(text = rlogic)), silent = TRUE))
  } else {
    # Evaluate the logic in the R format
    rlogic_eval <- try(eval(parse(text = rlogic)), silent = TRUE)
  }


  # If there's an error, the result is empty or it doesn't have one value per row, stop the process
  if (inherits(rlogic_eval, "try-error") || (length(rlogic_eval) == 0 && nrow(data) > 0) || !length(rlogic_eval) %in% c(0, 1, nrow(data))) {
    logic_error <- if (inherits(rlogic_eval, "try-error")) {
      paste("R error:", trimws(gsub("\\s+", " ", sub("^Error in .*? : ", "", as.character(rlogic_eval)))))
    } else if (length(rlogic_eval) > 0) {
      paste0("It gives ", length(rlogic_eval), " values for the ", nrow(data), " rows of the data.")
    }
    rd_abort(
      c("The logic could not be evaluated after translation.", "i" = "R logic: {.code {rlogic}}", if (!is.null(logic_error)) c("x" = "{logic_error}")),
      class = "REDCapDM_error_logic"
    )
  } else {
    # Data without rows: no values (also for a logic without variables, e.g. `1` or `upper('abc')`, which gives one value)
    if (nrow(data) == 0) {
      rlogic_eval <- if (is.null(rlogic_eval)) logical(0) else rlogic_eval[0]
    }

    # If the data has events, return the evaluation only for the specified event(s):
    if (!is.null(event_form)) {
      if (inherits(rlogic_eval, c("Date", "POSIXct"))) {
        # (dates keep their class and time zone: ifelse() would turn them into numbers)
        rlogic_eval <- rlogic_eval[rep_len(seq_along(rlogic_eval), nrow(data))]
        rlogic_eval[!data$redcap_event_name %in% event_var] <- NA
      } else {
        rlogic_eval <- data |>
          dplyr::mutate(
            calc = .env$rlogic_eval,
            calc = ifelse(!.data$redcap_event_name %in% .env$event_var, NA, .data$calc)
          ) |>
          dplyr::pull(.data$calc)
      }
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
        eval_data = rlogic_eval_data(data, rlogic_eval, id_col)
      )
    )
  }
}



#' Evaluation of a logic next to the record it belongs to
#'
#' @param data REDCap dataset the logic was evaluated on.
#' @param eval Evaluation of the logic (one value per row of `data`).
#' @param id_col Name of the column of the record identifier (see `id_field()`).
#' @return A `data.frame` with the record identifier, the event and repetition columns present in `data` and `eval`. `NULL` if `eval` doesn't have one value per row.
#' @noRd
rlogic_eval_data <- function(data, eval, id_col = id_field(data)) {
  if (!length(eval) %in% c(1, nrow(data))) {
    return(NULL)
  }

  key_cols <- intersect(c(id_col, "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance"), names(data))

  eval_data <- as.data.frame(data)[, key_cols, drop = FALSE]
  # (a single value is repeated in all the rows; data without rows has no values)
  eval_data$eval <- if (nrow(eval_data) == 0) eval[0] else eval
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
  # Checkbox options: REDCap exports them as Unchecked/Checked (after rd_checkbox(), the dictionary has their labels)
  options_dic <- dic$choices_calculations_or_slider_labels[match(field, dic$field_name)]
  if (grepl("___", field) && (length(options_dic) == 0 || is.na(options_dic) || !nzchar(options_dic))) {
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
#' @param id_col Name of the column of the record identifier (see `id_field()`).
#' @return `data` with `fields` filled in the rows of the repeating instruments.
#' @noRd
fill_from_base <- function(data, fields, id_col = id_field(data)) {
  instrument <- repeat_instrument_values(data)
  rows <- which(!is.na(instrument))

  if (length(fields) == 0 || length(rows) == 0) {
    return(data)
  }

  id <- data[[id_col]]
  instance <- if ("redcap_repeat_instance" %in% names(data)) data$redcap_repeat_instance else rep(NA, nrow(data))
  key <- paste(id, event_values(data), sep = "\r")
  base <- which(is.na(instrument) & is.na(instance))
  source <- base[match(key[rows], key[base])]

  for (f in fields) {
    data[[f]][rows] <- data[[f]][source]
  }

  data
}

#' Columns of the checkbox options in REDCap logic
#'
#' REDCap refers to a checkbox option as `[field(code)]` and exports it as the
#' column `field___code`, with the code in lower case and its `-` and `.`
#' replaced by `_` (e.g. `[symptoms(A)]` is `symptoms___a` and `[symptoms(-1)]`
#' is `symptoms____1`).
#'
#' @param logic REDCap logic.
#' @return The logic with each `[field(code)]` replaced by `[field___code]`.
#' @noRd
checkbox_columns <- function(logic) {
  pattern <- "\\[([A-Za-z0-9_]+)\\(([A-Za-z0-9_.-]+)\\)\\]"
  matches <- gregexpr(pattern, logic)
  regmatches(logic, matches) <- lapply(regmatches(logic, matches), function(x) {
    field <- sub(pattern, "\\1", x)
    code <- gsub("[-.]", "_", tolower(sub(pattern, "\\2", x)))
    paste0("[", field, "___", code, "]")
  })
  logic
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

#' Arguments of datediff()
#'
#' Each `datediff()` (already written as `(lubridate::time_length(lubridate::interval(`) has two dates, its unit (`'y'`,
#' years of 365.2425 days; `'M'`, months of 30.44 days; `'d'`; `'h'`; `'m'`, minutes; or `'s'`) and, optionally, its date
#' format (e.g. `'dmy'`) and `true` or `false` (signed difference, see datediff_absolute()). The dates written in the
#' logic are read with the format of their datediff() (the dates with time with `'ymd'` when it has none), `'today'` is
#' the current date and `'now'` the current date and time. Only the arguments of datediff() are changed: the same texts
#' in other functions (e.g. `concat('today', ':', [text])` or `if([sex] = '1', 'f', 'm')`) are kept.
#'
#' @param x A logic.
#' @return The logic with the arguments of datediff() translated.
#' @noRd
datediff_arguments <- function(x) {
  units <- c(y = "'day') / 365.2425", M = "'day') / 30.44", d = "'day')", h = "'hour')", m = "'minute')", s = "'second')")
  formats <- c(
    dmy = "\\d\\d-\\d\\d-\\d\\d\\d\\d", mdy = "\\d\\d-\\d\\d-\\d\\d\\d\\d", ymd = "\\d\\d\\d\\d-\\d\\d-\\d\\d",
    ydm = "\\d\\d\\d\\d-\\d\\d-\\d\\d", myd = "\\d\\d-\\d\\d\\d\\d-\\d\\d", dym = "\\d\\d-\\d\\d\\d\\d-\\d\\d"
  )
  marker <- "(lubridate::time_length(lubridate::interval("
  out <- ""
  rest <- x

  # Dates of an argument (not the ones already read, e.g. of another datediff() inside it; a date between parentheses,
  # e.g. `('01-02-2024')`, is read too)
  read <- "(?<!lubridate::[a-z]{3}\\()(?<!lubridate::parse_date_time\\()"
  dates <- function(arg, format) {
    arg <- gsub("'today'", "Sys.Date()", arg, fixed = TRUE)
    arg <- gsub("'now'", "lubridate::with_tz(Sys.time(), 'UTC')", arg, fixed = TRUE)
    if (!is.na(format)) {
      arg <- gsub(paste0(read, "'(", formats[[format]], ")'"), paste0("lubridate::", format, "('\\1')"), arg, perl = TRUE)
    }
    time_format <- if (is.na(format)) "ymd" else format
    gsub(
      paste0(read, "'(\\d{1,4}-\\d{1,2}-\\d{1,4} \\d{1,2}:\\d{2}(:\\d{2})?)'"),
      paste0("lubridate::parse_date_time('\\1', c('", time_format, " HMS', '", time_format, " HM'))"),
      arg,
      perl = TRUE
    )
  }

  repeat {
    start <- regexpr(marker, rest, fixed = TRUE)
    if (is.na(start) || start == -1) {
      break
    }

    # Closing parenthesis of the call and its arguments (commas at the first level)
    chars <- strsplit(rest, "")[[1]]
    open <- start + nchar(marker) - 1
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
    if (is.na(close)) {
      break
    }

    # (the arguments can have other datediff())
    bounds <- c(open, commas, close)
    args <- vapply(seq_len(length(bounds) - 1), function(i) datediff_arguments(substr(rest, bounds[i] + 1, bounds[i + 1] - 1)), character(1))
    values <- trimws(args)

    # Date format and `true` or `false` after the unit
    if (length(args) >= 3) {
      extra <- values[-(1:3)]
      is_format <- grepl("^'(dmy|mdy|ymd|ydm|myd|dym)'$", extra)
      format <- if (any(is_format)) gsub("'", "", extra[is_format][1]) else NA_character_
      args[1:2] <- vapply(args[1:2], dates, character(1), format = format, USE.NAMES = FALSE)
      keep <- !(is_format | tolower(gsub("^['\"]|['\"]$", "", extra)) %in% c("true", "false"))
      args <- c(args[1:3], args[-(1:3)][keep])
      values <- c(values[1:3], extra[keep])
    }

    unit <- if (length(args) == 3 && grepl("^'[yMdhms]'$", values[3])) gsub("'", "", values[3]) else NA_character_
    call <- if (!is.na(unit)) {
      paste0(substr(rest, start, open), args[1], ",", args[2], "), ", units[[unit]], ")")
    } else {
      paste0(substr(rest, start, open), paste(args, collapse = ","), ")")
    }
    out <- paste0(out, substr(rest, 1, start - 1), call)
    rest <- substr(rest, close + 1, nchar(rest))
  }

  paste0(out, rest)
}

#' Comparisons of a date field with the current date or a date with time
#'
#' A date or datetime field compared with `'today'`, `'now'` or a date with time (e.g. `'2024-01-05 10:30'`, read as
#' year-month-day and in UTC, as the dates of rd_dates()) is compared with that date. The same texts compared with other
#' fields (e.g. `[text] = 'today'`) are kept.
#'
#' @param x A logic.
#' @param dic The dictionary.
#' @return The logic with these dates translated.
#' @noRd
date_comparisons <- function(x, dic) {
  validation <- dic$text_validation_type_or_show_slider_number
  date_fields <- dic$field_name[!is.na(validation) & grepl("^(date|datetime)_", validation)]
  if (length(date_fields) == 0 || length(x) != 1 || is.na(x)) {
    return(x)
  }

  literal <- "'(today|now|\\d{1,4}-\\d{1,2}-\\d{1,4} \\d{1,2}:\\d{2}(?::\\d{2})?)'"
  field <- "(?:\\[[\\w-]+\\])?\\[(\\w+)\\]"
  operator <- "\\s*(?:<=|>=|<>|!=|=|<|>)\\s*"
  date_value <- function(text) {
    if (text == "today") {
      "Sys.Date()"
    } else if (text == "now") {
      "lubridate::with_tz(Sys.time(), 'UTC')"
    } else {
      paste0("lubridate::parse_date_time('", text, "', c('ymd HMS', 'ymd HM'))")
    }
  }

  # (the field before or after the operator)
  for (pattern in c(paste0(field, operator, literal), paste0(literal, operator, field))) {
    m <- gregexpr(pattern, x, perl = TRUE)
    found <- regmatches(x, m)[[1]]
    if (length(found) == 0) {
      next
    }
    parts <- regmatches(found, regexec(pattern, found, perl = TRUE))
    field_first <- grepl("^(\\[[\\w-]+\\])?\\[", found, perl = TRUE)
    changed <- vapply(seq_along(found), function(i) {
      name <- if (field_first[i]) parts[[i]][2] else parts[[i]][3]
      text <- if (field_first[i]) parts[[i]][3] else parts[[i]][2]
      if (!name %in% date_fields) {
        return(found[i])
      }
      quoted <- regexpr(literal, found[i], perl = TRUE)
      paste0(substr(found[i], 1, quoted - 1), date_value(text), substring(found[i], quoted + attr(quoted, "match.length")))
    }, character(1))
    regmatches(x, m) <- list(changed)
  }

  x
}

#' Comparisons of an operation with an empty text
#'
#' As in REDCap, `trim([text]) = ''` is `TRUE` when the result is blank: `NA` or an empty text. The empty texts of the
#' logic are `NA` in its translation (`[field] = ''` is `is.na(data$field)`), so a comparison of an operation with them
#' (`== NA` or `!= NA`) would always be `NA`: it is written as a check of blank values.
#'
#' @param x A logic translated to R.
#' @return The logic.
#' @noRd
blank_comparisons <- function(x) {
  if (is.na(x) || !grepl("(==|!=)\\s*NA\\b|\\bNA\\s*(==|!=)", x, perl = TRUE)) {
    return(x)
  }
  parsed <- tryCatch(parse(text = x, keep.source = FALSE), error = function(e) NULL)
  if (length(parsed) != 1) {
    return(x)
  }
  is_na <- function(e) identical(e, NA)
  rewrite <- function(e) {
    if (!is.call(e)) {
      return(e)
    }
    for (k in seq_along(e)[-1]) {
      if (!identical(e[[k]], quote(expr = )) && !is.null(e[[k]])) e[[k]] <- rewrite(e[[k]])
    }
    if (length(e) == 3 && (identical(e[[1]], as.name("==")) || identical(e[[1]], as.name("!="))) && (is_na(e[[2]]) || is_na(e[[3]]))) {
      operation <- if (is_na(e[[3]])) e[[2]] else e[[3]]
      blank <- bquote((function(v) is.na(v) | as.character(v) %in% "")(.(operation)))
      e <- if (identical(e[[1]], as.name("=="))) blank else bquote(!.(blank))
    }
    e
  }
  one_line(rewrite(parsed[[1]]))
}

#' Different values, also when one of them is blank
#'
#' As in REDCap, a blank value is different from any value: `data$x != 1` is written `!(data$x %in% 1)`. When the value
#' is the start of an operation (e.g. `data$x != 0 + 1`), the whole operation is compared (`!(data$x %in% (0 + 1))`), or
#' the comparison is left as it is if the operation uses variables of the data (its value can change in each row) or the
#' variable is the end of an operation (e.g. `1 + data$x != 2`).
#'
#' @param x A logic translated to R.
#' @return The logic.
#' @noRd
not_equal_blank <- function(x) {
  parsed <- tryCatch(
    parse(text = x, keep.source = FALSE),
    error = function(e) NULL
  )

  if (length(parsed) != 1L) {
    return(x)
  }

  changed <- FALSE

  rewrite <- function(e) {
    if (!is.call(e) ||
        identical(e[[1]], as.name("function"))) {
      return(e)
    }

    for (k in seq_along(e)[-1L]) {
      if (!identical(e[[k]], quote(expr = )) &&
          !is.null(e[[k]])) {
        e[[k]] <- rewrite(e[[k]])
      }
    }

    if (length(e) == 3L &&
        identical(e[[1]], as.name("!="))) {
      changed <<- TRUE

      left <- e[[2]]
      right <- e[[3]]

      return(bquote(
        (function(a, b) {
          blank_a <- is.na(a) | as.character(a) %in% ""
          blank_b <- is.na(b) | as.character(b) %in% ""

          out <- a != b
          blank <- blank_a | blank_b
          out[blank] <- xor(blank_a, blank_b)[blank]

          out
        })(.(left), .(right))
      ))
    }

    e
  }

  result <- rewrite(parsed[[1L]])

  if (!changed) {
    return(x)
  }

  one_line(result)
}

#' Translate the datediff() of two time fields
#'
#' As in REDCap, `datediff([start], [end], 'h', ...)` of two time fields (validation `time`, `time_hh_mm_ss` or
#' `time_mm_ss`) is the difference between the two times of the same day, in the units given (`end` minus `start`). The
#' date format and the other arguments are not used. The other calls to datediff() are left as they are.
#'
#' @param logic REDCap logic (after datediff_absolute()).
#' @param dic The dictionary.
#' @return The logic with these calls translated into R.
#' @noRd
datediff_times <- function(logic, dic) {
  formats <- c(time = "%H:%M", time_hh_mm_ss = "%H:%M:%S", time_mm_ss = "%M:%S")
  units <- c(y = "31556952", M = "2630016", d = "86400", h = "3600", m = "60", s = "1")
  validation <- if ("text_validation_type_or_show_slider_number" %in% names(dic)) dic$text_validation_type_or_show_slider_number else character(0)
  time_fields <- dic$field_name[validation %in% names(formats)]
  if (length(time_fields) == 0 || is.na(logic) || !grepl("datediff", logic, fixed = TRUE)) {
    return(logic)
  }

  # Time of a field in seconds (the field can be written with its event, e.g. `[visit_1][start]`)
  seconds <- function(arg) {
    field <- sub("^(\\[[A-Za-z0-9_-]+\\])?\\[([A-Za-z0-9_]+)\\]$", "\\2", arg)
    if (!grepl("^(\\[[A-Za-z0-9_-]+\\])?\\[[A-Za-z0-9_]+\\]$", arg) || !field %in% time_fields) {
      return(NA_character_)
    }
    format <- formats[[validation[match(field, dic$field_name)]]]
    paste0("as.numeric(as.difftime(as.character(", arg, "), '", format, "', 'secs'))")
  }

  out <- ""
  rest <- logic
  repeat {
    start <- regexpr("(?<![\\w.$\\[])datediff\\s*\\(", rest, perl = TRUE)
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
    if (is.na(close)) {
      break
    }

    bounds <- c(open, commas, close)
    args <- trimws(substring(rest, bounds[-length(bounds)] + 1, bounds[-1] - 1))
    unit <- if (length(args) >= 3) gsub("^['\"]|['\"]$", "", args[3]) else NA_character_
    call <- substr(rest, start, close)
    if (length(args) >= 3 && unit %in% names(units)) {
      from <- seconds(args[1])
      to <- seconds(args[2])
      if (!is.na(from) && !is.na(to)) {
        call <- paste0("((", to, " - ", from, ") / ", units[[unit]], ")")
      }
    }
    out <- paste0(out, substr(rest, 1, start - 1), call)
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

    # A name inside a quoted text (e.g. `[side] = 'left (L)'`) is not a call: keep the text as it is
    quote <- ""
    for (k in seq_len(start - 1)) {
      if (nzchar(quote)) {
        if (chars[k] == quote) quote <- ""
      } else if (chars[k] %in% c("'", "\"")) {
        quote <- chars[k]
      }
    }
    if (nzchar(quote)) {
      end_quote <- which(chars == quote & seq_along(chars) >= start)[1]
      if (is.na(end_quote)) {
        break
      }
      out <- paste0(out, substr(rest, 1, end_quote))
      rest <- substr(rest, end_quote + 1, nchar(rest))
      next
    }
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

#' Dates of the conditions of `ifelse()` and of `min()` and `max()`
#'
#' `ifelse()` turns dates into numbers (e.g. `year(if([flag] = 1, [a], [b]))` gave 1970), and `pmin()` and `pmax()` (the
#' translation of `min()` and `max()`) give the class of their first value (e.g. `min('', [a])` gave a number). The
#' `ifelse()` whose two values are dates of the same class (`Date`, or dates with time of the same time zone), or a date
#' and a missing value, and the `pmin()` and `pmax()` whose values are dates and missing values but the first one isn't a
#' date, are converted back into dates: `as.Date(ifelse(...), origin = "1970-01-01")`. The other ones are kept as they
#' are.
#'
#' @param x A logic translated to R.
#' @param data The data where the logic is evaluated.
#' @return The logic.
#' @noRd
date_conditionals <- function(x, data) {
  if (length(x) != 1 || is.na(x) || !grepl("(ifelse|pmin|pmax)\\(", x)) {
    return(x)
  }
  parsed <- tryCatch(parse(text = x, keep.source = FALSE), error = function(e) NULL)
  if (length(parsed) != 1) {
    return(x)
  }

  date_calls <- c(paste0("lubridate::", c("ymd", "dmy", "mdy", "ydm", "myd", "dym")), "Sys.Date", "as.Date")
  time_calls <- c("lubridate::parse_date_time", "lubridate::force_tz")

  # Class of the values of an expression: "Date", "tz:<time zone>" for dates with time, "NA" for a missing value or ""
  date_kind <- function(e) {
    if (identical(e, NA)) {
      return("NA")
    }
    if (!is.call(e)) {
      return("")
    }
    head <- paste(deparse(e[[1]]), collapse = "")
    if (head == "$" && identical(e[[2]], as.name("data"))) {
      v <- data[[as.character(e[[3]])]]
      tz <- attr(v, "tzone")
      return(if (inherits(v, "Date")) "Date" else if (inherits(v, "POSIXct")) paste0("tz:", if (length(tz) > 0) tz[1] else "") else "")
    }
    if (head == "(") {
      return(date_kind(e[[2]]))
    }
    if (head %in% date_calls) {
      return("Date")
    }
    if (head %in% time_calls) {
      return("tz:UTC")
    }
    if (head == "as.POSIXct" && !is.null(e[["tz"]])) {
      return(paste0("tz:", e[["tz"]]))
    }
    # (the minimum and maximum of dates whose first value is a date, e.g. `pmin(data$a, data$b, na.rm = TRUE)`)
    if (head %in% c("pmin", "pmax")) {
      kinds <- value_kinds(e)
      return(if (length(kinds) == 1 && kinds != "" && date_kind(e[[2]]) == kinds) kinds else "")
    }
    # (a date plus or minus a number is a date)
    if (head %in% c("+", "-") && length(e) == 3) {
      first <- date_kind(e[[2]])
      second <- date_kind(e[[3]])
      if (!first %in% c("", "NA") && second == "") {
        return(first)
      }
      if (head == "+" && first == "" && !second %in% c("", "NA")) {
        return(second)
      }
    }
    ""
  }

  # Classes of the values of pmin() and pmax() (without `na.rm`) and of ifelse() (without its condition), without the
  # missing values
  value_kinds <- function(e) {
    args <- as.list(e)[-1]
    if (identical(e[[1]], as.name("ifelse"))) {
      args <- args[-1]
    } else if (!is.null(names(args))) {
      args <- args[!names(args) %in% "na.rm"]
    }
    setdiff(unique(vapply(args, date_kind, character(1))), "NA")
  }

  changed <- FALSE
  rewrite <- function(e) {
    if (!is.call(e)) {
      return(e)
    }
    for (k in seq_along(e)[-1]) {
      if (!identical(e[[k]], quote(expr = )) && !is.null(e[[k]])) e[[k]] <- rewrite(e[[k]])
    }
    to_dates <- FALSE
    if (identical(e[[1]], as.name("ifelse")) && length(e) == 4) {
      kinds <- value_kinds(e)
      to_dates <- length(kinds) == 1 && kinds != ""
    } else if ((identical(e[[1]], as.name("pmin")) || identical(e[[1]], as.name("pmax"))) && length(e) > 2) {
      kinds <- value_kinds(e)
      to_dates <- length(kinds) == 1 && kinds != "" && date_kind(e[[2]]) != kinds
    }
    if (to_dates) {
      changed <<- TRUE
      e <- if (kinds == "Date") {
        bquote(as.Date(.(e), origin = "1970-01-01"))
      } else {
        bquote(as.POSIXct(.(e), origin = "1970-01-01", tz = .(sub("^tz:", "", kinds))))
      }
    }
    e
  }
  new <- rewrite(parsed[[1]])

  if (!changed) {
    return(x)
  }
  one_line(new)
}

#' Write an R expression in one line
#'
#' `deparse()` writes the expressions between braces in several lines (e.g. the functions of the translation of
#' `roundup()` or `concat_ws()`): they are joined with `;`, so that the logic can be read again. The numbers are written
#' with all the digits needed to read the same number (e.g. 1.0000000000000002 or 1125899906842624.5).
#'
#' @param e An R expression.
#' @return A string.
#' @noRd
one_line <- function(e) {
  blocks <- character(0)
  # (placeholders that aren't in the expression, e.g. in its texts)
  base <- "REDCapDMblock"
  written <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
  while (grepl(base, written, fixed = TRUE)) {
    base <- paste0(base, "x")
  }
  hide <- function(e) {
    # The numbers are written with the digits that R needs to read the same number (deparse() writes 15, e.g.
    # 1.0000000000000002 would be 1)
    if (is.double(e) && length(e) == 1 && is.null(attributes(e)) && is.finite(e)) {
      written_number <- sprintf("%.15g", e)
      for (digits in 16:17) {
        if (identical(as.numeric(written_number), e)) break
        written_number <- sprintf(paste0("%.", digits, "g"), e)
      }
      key <- paste0(base, length(blocks) + 1, "x")
      blocks[[key]] <<- written_number
      return(as.name(key))
    }
    if (!is.call(e)) {
      return(e)
    }
    if (identical(e[[1]], as.name("{"))) {
      key <- paste0(base, length(blocks) + 1, "x")
      blocks[[key]] <<- paste0("{ ", paste(vapply(as.list(e)[-1], one_line, character(1)), collapse = "; "), " }")
      return(as.name(key))
    }
    for (k in seq_along(e)) {
      if (!identical(e[[k]], quote(expr = )) && !is.null(e[[k]])) e[[k]] <- hide(e[[k]])
    }
    e
  }
  text <- paste(deparse(hide(e), width.cutoff = 500L), collapse = " ")
  for (key in rev(names(blocks))) {
    text <- sub(key, blocks[[key]], text, fixed = TRUE)
  }
  text
}

#' Repeat for each row the conditions of `ifelse()` that don't use any variable
#'
#' `ifelse()` gives as many values as its condition: with a condition without variables (e.g. `if(1 < 2, [a], 0)` in
#' REDCap) it would give a single value, the first one of `[a]`. The condition is repeated for each row of `data` (the
#' data where the logic is evaluated), so that each row gets its own value. The other conditions are kept as they are.
#'
#' @param x A logic translated to R.
#' @return The logic with the constant conditions repeated for each row.
#' @noRd
recycle_constant_conditions <- function(x) {
  quoted <- function(chars) {
    inside <- logical(length(chars))
    quote <- ""
    escaped <- FALSE
    for (k in seq_along(chars)) {
      if (nzchar(quote)) {
        inside[k] <- TRUE
        # (a quote after a backslash is part of the text, e.g. 'O\'Brien')
        if (escaped) {
          escaped <- FALSE
        } else if (chars[k] == "\\") {
          escaped <- TRUE
        } else if (chars[k] == quote) {
          quote <- ""
        }
      } else if (chars[k] %in% c("'", "\"")) {
        quote <- chars[k]
        inside[k] <- TRUE
      }
    }
    inside
  }

  starts <- gregexpr("(?<![\\w.$])ifelse\\s*\\(", x, perl = TRUE)[[1]]
  if (starts[1] == -1) {
    return(x)
  }
  lengths <- attr(starts, "match.length")
  chars <- strsplit(x, "")[[1]]

  # From the last call to the first, so that the positions of the previous calls don't change
  for (i in rev(seq_along(starts))) {
    inside <- quoted(chars)
    if (inside[starts[i]]) next
    open <- starts[i] + lengths[i] - 1

    # End of the condition: the first comma outside parentheses and quotes
    depth <- 0
    comma <- NA
    for (k in open:length(chars)) {
      if (inside[k]) next
      if (chars[k] == "(") {
        depth <- depth + 1
      } else if (chars[k] == ")") {
        depth <- depth - 1
        if (depth == 0) break
      } else if (chars[k] == "," && depth == 1) {
        comma <- k
        break
      }
    }
    if (is.na(comma) || comma == open + 1) next

    condition <- paste(chars[(open + 1):(comma - 1)], collapse = "")
    if (grepl("data\\$|data\\[\\[", without_texts(condition))) next

    chars <- c(chars[seq_len(open)], strsplit(paste0("rep_len(", trimws(condition), ", nrow(data))"), "")[[1]], chars[comma:length(chars)])
  }

  paste(chars, collapse = "")
}

#' Label of an option written as a text of a REDCap logic
#'
#' Between single quotes, or between double quotes when it has an apostrophe (e.g. `"O'Brien"`). A label with both quotes
#' or with backslashes is written between single quotes with these characters escaped, as in R (e.g. `'O\'Brien "A"'`
#' or `'C:\\Ward'`).
#'
#' @param x Labels.
#' @return The labels between quotes.
#' @noRd
logic_label <- function(x) {
  plain <- !grepl("\\", x, fixed = TRUE)
  double <- plain & grepl("'", x, fixed = TRUE) & !grepl("\"", x, fixed = TRUE)
  ifelse(double, paste0("\"", x, "\""), r_text(x))
}

#' Comparisons of a logic with a field
#'
#' Finds, in one reading of the logic, the texts between quotes and the comparisons `[field] = value`, `[field] <> value`
#' and `[field] != value`, with the value between quotes or a whole number (e.g. `1`, `1.0` or `10`, never the start of
#' another number). A comparison is complete when its value isn't the start of an operation and its field isn't the end
#' of one (e.g. `[a] = 0 + 1` or `1 + [a] = 2` aren't comparisons of `[a]` with a value).
#'
#' @param logic A logic.
#' @return A list with the match data of gregexpr() (`matches`) and, for each match, the field (`field`, `NA` for a
#'   text), the operator (`op`), the value as it is written (`value`) and whether it is complete (`complete`).
#' @noRd
logic_comparisons <- function(logic) {
  quoted <- sub("^\\(\\?s\\)", "", quoted_pattern())
  number <- "-?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?![\\w.])"
  comparison <- paste0(
    "\\[([^\\[\\]]+)\\](?:\\[current-instance\\])?",
    "\\s*(=|<>|!=)\\s*(",
    quoted, "|", number, ")"
  )
  matches <- gregexpr(paste0("(?s)(?:", quoted, ")|", comparison), logic, perl = TRUE)
  found <- regmatches(logic, matches)[[1]]
  parts <- regmatches(found, regexec(paste0("(?s)^", comparison, "$"), found, perl = TRUE))
  starts <- as.integer(matches[[1]])
  ends <- starts + attr(matches[[1]], "match.length") - 1
  operation <- "[-+*/^%]"
  complete <- if (length(found) == 0) logical(0) else {
    !grepl(paste0(operation, "\\s*$"), substr(rep(logic, length(starts)), 1, starts - 1)) &
      !grepl(paste0("^\\s*", operation), substring(logic, ends + 1))
  }
  list(
    matches = matches,
    found = found,
    complete = complete,
    field = vapply(parts, function(p) if (length(p) > 0) p[2] else NA_character_, character(1)),
    op = vapply(parts, function(p) if (length(p) > 0) p[3] else NA_character_, character(1)),
    value = vapply(parts, function(p) if (length(p) > 0) p[4] else NA_character_, character(1))
  )
}

#' Write the comparisons of a logic with the codes of options as comparisons with their labels
#'
#' Each comparison of an option with one of its codes (between quotes, e.g. `'1'`, or as a number, e.g. `1` or `1.0`)
#' becomes `[name]='label'` or `[name]<>'label'`. The logic is read once, so a label that is the code of another option is
#' not changed again (e.g. code 1 with label `2`), and the texts between quotes (e.g. `'[cb(1)]'`) are kept.
#'
#' @param logic Character vector with the logic.
#' @param options Data frame with `ref` (the field as written in the logic, e.g. `sex` or `cb(1)`), `code`, `name` (the
#'   field in the new logic) and `label`.
#' @return The logic with the comparisons changed.
#' @noRd
relabel_logic <- function(logic, options) {
  if (nrow(options) == 0) {
    return(logic)
  }
  # (as text, also a column of the dictionary without any value, read as logical)
  vapply(as.character(logic), function(l) {
    if (is.na(l) || !grepl("[", l, fixed = TRUE)) {
      return(l)
    }
    found <- logic_comparisons(l)
    new <- found$found
    for (k in which(found$field %in% options$ref & found$complete)) {
      rows <- options[options$ref == found$field[k], ]
      value <- found$value[k]
      hit <- if (grepl("^['\"]", value)) {
        match(text_value(value), rows$code)
      } else {
        which(suppressWarnings(as.numeric(rows$code)) == as.numeric(value))[1]
      }
      if (!is.na(hit)) {
          suffix <- if (startsWith(
          found$found[k],
          paste0("[", found$field[k], "][current-instance]")
        )) {
          "[current-instance]"
        } else {
          ""
        }

        new[k] <- paste0(
          "[", rows$name[hit], "]",
          suffix,
          if (found$op[k] == "=") "=" else "<>",
          logic_label(rows$label[hit])
        )
      }
    }
    regmatches(l, found$matches) <- list(new)
    l
  }, character(1), USE.NAMES = FALSE)
}

#' Whether a logic uses a factor in another way than comparing it with its labels
#'
#' Comparisons with a label or with an empty text (`[a] = 'Yes'`, `[a] <> ''`) and `isblankormissingcode([a])` work with
#' the factor. Any other use (e.g. `[a] >= 3`, `[a] + 1`, or a comparison with a code that isn't a label) needs its codes.
#'
#' @param logic A logic.
#' @param field The field.
#' @param labels Labels of the field.
#' @return `TRUE` or `FALSE`.
#' @noRd
factor_other_use <- function(logic, field, labels) {
  field_pattern <- paste0("\\[", field, "\\]")
  if (is.na(logic) || !grepl(field_pattern, without_texts(logic))) {
    return(FALSE)
  }
  found <- logic_comparisons(logic)
  with_label <- found$field %in% field & found$complete & grepl("^['\"]", found$value) & text_value(ifelse(is.na(found$value), "''", found$value)) %in% c(labels, "")
  rest <- logic
  regmatches(rest, found$matches) <- list(ifelse(with_label, "''", found$found))
  rest <- gsub(paste0("isblankormissingcode\\(\\s*", field_pattern, "\\s*\\)"), "''", rest, perl = TRUE)
  grepl(field_pattern, without_texts(rest))
}

#' A value written as an R text between single quotes
#'
#' @param x Values.
#' @return The values between single quotes, with their backslashes and single quotes escaped.
#' @noRd
r_text <- function(x) {
  paste0("'", gsub("'", "\\'", gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE), "'")
}

#' Value of a text between quotes of a logic
#'
#' A backslash before a backslash or a quote escapes it (e.g. `'O\'Brien'`); before any other character it is a
#' backslash (e.g. `'C:\Ward'`).
#'
#' @param x Texts with their quotes.
#' @return The values of the texts.
#' @noRd
text_value <- function(x) {
  gsub("\\\\([\\\\'\"])", "\\1", substr(x, 2, nchar(x) - 1), perl = TRUE)
}

#' Regular expression of the texts between quotes of a logic
#'
#' Between single or double quotes. A quote after a backslash is part of the text (e.g. `'O\'Brien "A"'`).
#'
#' @return The regular expression (Perl).
#' @noRd
quoted_pattern <- function() {
  "(?s)'(?:[^'\\\\]|\\\\.)*'|\"(?:[^\"\\\\]|\\\\.)*\""
}

#' Regular expression of the texts of R code
#'
#' The texts between quotes and the raw strings of R (e.g. `r"(a" x > b)"`, `R'[a]'` or `r"--{a}--"`, whose text ends
#' with the same bracket, dashes and quote), for the expressions of rd_query().
#'
#' @return A regular expression (perl = TRUE).
#' @noRd
r_texts_pattern <- function() {
  paste0("(?s)[rR]([\"'])(-*)(?:\\((?:.*?)\\)|\\[(?:.*?)\\]|\\{(?:.*?)\\})\\2\\1|", sub("^\\(\\?s\\)", "", quoted_pattern()))
}

#' A logic without the texts between quotes
#'
#' To look for fields or R code in a logic (e.g. `[age]` in `data$status == '[age]'` is a text).
#'
#' @param x Character vector with the logic.
#' @return The logic with each text between quotes replaced by `''`.
#' @noRd
without_texts <- function(x) {
  gsub(quoted_pattern(), "''", x, perl = TRUE)
}

#' Write between single quotes the texts between double quotes
#'
#' Except the texts with an apostrophe (e.g. `"O'Brien"`), which keep their double quotes.
#'
#' @param x A logic.
#' @return The logic.
#' @noRd
single_quotes <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return(x)
  }
  quotes <- gregexpr(quoted_pattern(), x, perl = TRUE)
  found <- regmatches(x, quotes)[[1]]
  double <- startsWith(found, "\"") & !grepl("'", found, fixed = TRUE)
  found[double] <- paste0("'", substr(found[double], 2, nchar(found[double]) - 1), "'")
  regmatches(x, quotes) <- list(found)
  x
}

#' Set aside the texts between quotes that the translation could change
#'
#' The texts with characters of operators, functions or fields (`=`, `<`, `>`, `(`, `)`, `[`, `]`) are replaced by
#' placeholders, so that e.g. `[x] = 'a=b'` compares with the text `a=b`. The other texts (codes, numbers, dates, units of
#' datediff()...) are left, because the translation uses them.
#'
#' @param x A logic.
#' @return A list with the logic with the placeholders (`logic`) and the texts (`texts`, named by their placeholder).
#' @noRd
mask_texts <- function(x) {
  texts <- character(0)
  if (length(x) != 1 || is.na(x)) {
    return(list(logic = x, texts = texts))
  }
  quotes <- gregexpr(quoted_pattern(), x, perl = TRUE)
  found <- regmatches(x, quotes)[[1]]
  # (also the texts with backslashes, i.e. with escaped characters, e.g. 'O\'Brien "A"': they are put back as R texts)
  escaped <- grepl("\\", found, fixed = TRUE)
  # (and the texts with two apostrophes, e.g. "L''Z": the empty texts '' are translated into NA)
  special <- grepl("[=<>()]|\\[|\\]", found) | escaped | (nchar(found) > 2 & grepl("''", found, fixed = TRUE))
  if (!any(special)) {
    return(list(logic = x, texts = texts))
  }
  # Placeholders that can't be texts of the logic: their name isn't in it
  base <- "REDCapDMtext"
  while (grepl(base, x, fixed = TRUE)) {
    base <- paste0(base, "x")
  }
  holders <- paste0("'", base, seq_along(found), "x'")
  found_texts <- found
  found_texts[escaped] <- r_text(text_value(found[escaped]))
  texts <- stats::setNames(found_texts[special], holders[special])
  found[special] <- holders[special]
  regmatches(x, quotes) <- list(found)
  list(logic = x, texts = texts)
}

#' Put back the texts set aside by mask_texts()
#'
#' @param x A logic with placeholders.
#' @param texts The texts, named by their placeholder.
#' @return The logic with the texts.
#' @noRd
unmask_texts <- function(x, texts) {
  for (holder in names(texts)) {
    x <- gsub(holder, texts[[holder]], x, fixed = TRUE)
  }
  x
}

#' Apply a function to the parts of a logic that are not texts between quotes
#'
#' @param x Character vector with the logic.
#' @param f A function that receives and returns the parts of the logic outside the quotes (character vector).
#' @param pattern Regular expression of the texts (by default, the texts between quotes of REDCap logic).
#' @return The logic with those parts changed and the texts between quotes as they were.
#' @noRd
outside_quotes <- function(x, f, pattern = quoted_pattern()) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return(s)
    }
    quotes <- gregexpr(pattern, s, perl = TRUE)
    texts <- regmatches(s, quotes)[[1]]
    rest <- f(regmatches(s, quotes, invert = TRUE)[[1]])
    paste0(rest, c(texts, ""), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Translate the logical operators `and` and `or` of REDCap logic into `&` and `|`
#'
#' As whole words in upper or lower case, and not inside the texts between quotes (e.g. `[x] = 'yes or no'`).
#' Only the word is replaced, so the spaces stay as they are.
#'
#' @param x Character vector with the logic.
#' @return The logic with the operators translated.
#' @noRd
logical_operators <- function(x) {
  outside_quotes(x, function(s) {
    s <- gsub("(?i)(?<![\\w.$])and(?![\\w.$])", "&", s, perl = TRUE)
    gsub("(?i)(?<![\\w.$])or(?![\\w.$])", "|", s, perl = TRUE)
  })
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
  # rounddown() and roundup() with decimals: the result is the largest (rounddown) or smallest (roundup) number with
  # these decimals, read as R reads the numbers written with them (e.g. "0.29" or "29e-2", as the data), that isn't
  # larger or smaller than the value. So a value that already has these decimals is kept (e.g. roundup(1.1, 2) is 1.1,
  # although 1.1 * 100 is 110.00000000000001, or rounddown(1e-24, 24) is 1e-24), and the result is never on the wrong
  # side of the value. The value is scaled in two steps, so that 10^digits doesn't overflow (e.g. 309 decimals). A number
  # too large to have decimals is kept (e.g. 1e308 with one decimal), a result beyond the largest number is Inf (e.g.
  # roundup(12, -324)), the decimals are whole numbers (as in REDCap) and without digits (e.g. NA) the result is NA
  # (without the functions translated below, e.g. length() or ifelse())
  rounding <- function(r_fun) function(args) {
    if (digits(args) %in% c("0", "'0'", "\"0\"")) {
      paste0(r_fun, "(", args[1], ")")
    } else {
      steps <- if (r_fun == "floor") {
        "k <- k + (dec(k + 1) <= v); k <- k - (dec(k) > v); k <- k - (dec(k) > v)"
      } else {
        "k <- k - (dec(k - 1) >= v); k <- k + (dec(k) < v); k <- k + (dec(k) < v)"
      }
      paste0(
        "(function(v, d) { d <- trunc(d); z <- v * 10^(d %/% 2) * 10^(d - d %/% 2); ",
        "dec <- function(k) suppressWarnings(as.numeric(sprintf(\"%.0fe%.0f\", k, -d))); k <- floor(z); ", steps, "; ",
        "r <- dec(k); v0 <- rep_len(v, NROW(r)); keep <- !is.na(d) & (!is.finite(v) | !is.finite(z) | abs(z) >= 2^52); ",
        "r[keep] <- v0[keep]; r })(", args[1], ", ", digits(args), ")"
      )
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
  # As in REDCap, the remainder of mod() has the sign of the dividend (mod(-7, 3) is -1). It is exact, also with large
  # numbers (e.g. mod(2^54, 3) is 1): the divisor multiplied by a power of 2 is subtracted while the remainder is
  # larger than it (subtractions without rounding errors). The power is taken one smaller and then doubled, and is
  # multiplied in three parts, so that it is never larger than the largest number (e.g. mod(1.797693e308, 1) or with a
  # divisor such as 5e-324)
  # (without the functions translated below, e.g. length() or ifelse())
  x <- replace_function(x, "mod", function(args) {
    paste0(
      "(function(a, b) { n <- if (min(lengths(list(a, b))) == 0) 0 else max(lengths(list(a, b))); ",
      "a <- rep_len(as.numeric(a), n); b <- rep_len(as.numeric(b), n); r <- abs(a); m <- abs(b); ",
      "while (any(is.finite(r) & m > 0 & r >= m, na.rm = TRUE)) { i <- which(is.finite(r) & m > 0 & r >= m); ",
      "k <- pmax(floor(log2(r[i]) - log2(m[i])) - 1, 0); s <- m[i] * 2^(k %/% 3) * 2^(k %/% 3) * 2^(k - 2 * (k %/% 3)); s <- s / 2^(s > r[i]); ",
      "s <- s * 2^(2 * s <= r[i]); s <- s * 2^(2 * s <= r[i]); r[i] <- r[i] - s }; r[(is.finite(a) & m == 0) | is.infinite(a)] <- NaN; ",
      "r[is.na(m)] <- NA; sign(a) * r })(", args[1], ", ", args[2], ")"
    )
  })
  x <- replace_function(x, "stdev", function(args) paste0("apply(", values(args), ", 1, stats::sd, na.rm = TRUE)"))
  x <- replace_function(x, "exponential", function(args) paste0("exp(", args[1], ")"))
  # (as text: the number of a factor is its internal code, not its value)
  x <- replace_function(x, "isnumber", function(args) paste0("!is.na(suppressWarnings(as.numeric(as.character(", args[1], "))))"))

  # Texts of the arguments: an empty text '' is "" (the '' left in the logic become NA below). With an argument that
  # isn't a fixed text or number (e.g. another field), the search or replacement is made record by record (with the
  # values of each record, also with data without rows)
  empty <- function(arg) if (identical(arg, "''")) "\"\"" else arg
  fixed_value <- function(arg) grepl("^('[^']*'|\"[^\"]*\"|-?[0-9.]+)$", arg)
  by_record <- function(fun, args) {
    paste0(
      "(function(f, ...) { a <- list(...); n <- if (min(lengths(a)) == 0) 0 else max(lengths(a)); ",
      "unlist(do.call(mapply, c(list(f), lapply(a, rep_len, n), USE.NAMES = FALSE))) })(", fun, ", ", paste(args, collapse = ", "), ")"
    )
  }
  # (as in REDCap, a blank value is an empty text, also in the texts to search, the replacements and the separators;
  # written with double quotes: the '' left in the logic become NA below)
  text <- function(arg) {
    arg <- empty(arg)
    if (grepl("^('[^']*'|\"[^\"]*\")$", arg)) arg else paste0("ifelse(is.na(", arg, "), \"\", as.character(", arg, "))")
  }
  lower_text <- function(arg) if (fixed_value(empty(arg))) paste0("tolower(", empty(arg), ")") else paste0("tolower(", text(arg), ")")
  x <- replace_function(x, "isinteger", function(args) {
    paste0("(function(v) !is.na(v) & v == trunc(v))(suppressWarnings(as.numeric(as.character(", args[1], "))))")
  })
  x <- replace_function(x, "contains", function(args) {
    if (fixed_value(empty(args[2]))) {
      paste0("grepl(", lower_text(args[2]), ", ", lower_text(args[1]), ", fixed = TRUE)")
    } else {
      paste0("as.logical(", by_record("function(t, p) grepl(p, t, fixed = TRUE)", c(lower_text(args[1]), lower_text(args[2]))), ")")
    }
  })
  x <- replace_function(x, "length", function(args) paste0("ifelse(is.na(", args[1], "), 0, nchar(as.character(", args[1], ")))"))

  # Missing values: redcap_data() turns the Missing Data Codes into NA
  # (the result of a function can be an empty text, e.g. trim([text]) of a field with spaces: it is blank too)
  x <- replace_function(x, "isblankormissingcode", function(args) {
    if (grepl("^data\\$(\\w+|`\\w+`)$", args[1])) {
      paste0("is.na(", args[1], ")")
    } else {
      paste0("(function(v) is.na(v) | as.character(v) %in% \"\")(", args[1], ")")
    }
  })

  # Text functions (as in REDCap, the comparisons of text don't distinguish upper and lower case, and a blank value is an empty text)
  x <- replace_function(x, "not_contain", function(args) {
    if (fixed_value(empty(args[2]))) {
      paste0("!grepl(", lower_text(args[2]), ", ", lower_text(args[1]), ", fixed = TRUE)")
    } else {
      paste0("!as.logical(", by_record("function(t, p) grepl(p, t, fixed = TRUE)", c(lower_text(args[1]), lower_text(args[2]))), ")")
    }
  })
  x <- replace_function(x, "starts_with", function(args) paste0("startsWith(tolower(", text(args[1]), "), ", lower_text(args[2]), ")"))
  x <- replace_function(x, "ends_with", function(args) paste0("endsWith(tolower(", text(args[1]), "), ", lower_text(args[2]), ")"))
  x <- replace_function(x, "left", function(args) paste0("substr(", text(args[1]), ", 1, ", args[2], ")"))
  x <- replace_function(x, "right", function(args) paste0("substring(", text(args[1]), ", pmax(nchar(", text(args[1]), ") - (", args[2], ") + 1, 1))"))
  x <- replace_function(x, "mid", function(args) paste0("substr(", text(args[1]), ", ", args[2], ", (", args[2], ") + (", args[3], ") - 1)"))
  x <- replace_function(x, "find", function(args) {
    if (fixed_value(empty(args[1]))) {
      paste0("pmax(regexpr(", lower_text(args[1]), ", tolower(", text(args[2]), "), fixed = TRUE), 0)")
    } else {
      paste0("pmax(as.numeric(", by_record("function(n, h) regexpr(n, h, fixed = TRUE)", c(lower_text(args[1]), paste0("tolower(", text(args[2]), ")"))), "), 0)")
    }
  })
  # (an empty text to search leaves the text as it is)
  x <- replace_function(x, "replace_text", function(args) {
    args[2:3] <- vapply(args[2:3], empty, character(1), USE.NAMES = FALSE)
    if (identical(args[2], "\"\"")) {
      text(args[1])
    } else if (fixed_value(args[2]) && fixed_value(args[3])) {
      paste0("gsub(", args[2], ", ", args[3], ", ", text(args[1]), ", fixed = TRUE)")
    } else {
      paste0("as.character(", by_record("function(t, s, r) if (nzchar(s)) gsub(s, r, t, fixed = TRUE) else t", vapply(args[1:3], text, character(1))), ")")
    }
  })
  # (with the separator of each record)
  x <- replace_function(x, "concat_ws", function(args) {
    parts <- paste0("as.character(", args[-1], ")")
    paste0("as.character(", by_record("function(s, ...) { v <- c(...); paste(v[!is.na(v) & nzchar(v)], collapse = s) }", c(text(args[1]), parts)), ")")
  })
  x <- replace_function(x, "concat", function(args) paste0("paste0(", paste(vapply(args, text, character(1)), collapse = ", "), ")"))
  x <- replace_function(x, "upper", function(args) paste0("toupper(", text(args[1]), ")"))
  x <- replace_function(x, "lower", function(args) paste0("tolower(", text(args[1]), ")"))
  x <- replace_function(x, "trim", function(args) paste0("trimws(", text(args[1]), ")"))

  # Parts of a date
  x <- replace_function(x, "year", function(args) paste0("lubridate::year(", args[1], ")"))
  x <- replace_function(x, "month", function(args) paste0("lubridate::month(", args[1], ")"))
  x <- replace_function(x, "day", function(args) paste0("lubridate::day(", args[1], ")"))

  x
}
