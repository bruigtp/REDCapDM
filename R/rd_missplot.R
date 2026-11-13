#' Plot Missing Data Overview (Barplot, Longitudinal Heatmap or Record Map)
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Generates a visual summary of missing data for variables in a REDCap dataset or list structure. With `by = "vars"` (default), cross-sectional (single-event) studies get a barplot showing the proportion or count of missing values per variable, and longitudinal (multi-event) studies get a heatmap illustrating missingness across events and variables. With `by = "record"`, the plot shows one row per record and one column per variable, with the missing values painted.
#'
#' @param data A `data frame` or a list containing a `data` element retrieved from REDCap.
#' @param sort_by_missingness Logical. Variables are ordered from highest to lowest missingness. Default `TRUE`.
#' @param max_vars Integer. If specified, only the top `max_vars` variables are plotted (by missingness, or by original column order if `sort_by_missingness = FALSE`). Default `NULL` (show all variables).
#' @param show_percentages Logical. Displays missingness as percentages. Only used with `by = "vars"`. Default `TRUE`.
#' @param use_field_labels Logical. Uses field labels from the dictionary for axis labels instead of variable names. Default `TRUE`.
#' @param drop_complete Logical. Variables with 0 missing values are excluded from the plot. Default `TRUE`.
#' @param max_label_length Integer. Maximum number of characters of the axis labels. Use `NULL` to keep them complete. Default `40`.
#' @param by Character. `"vars"` (default) summarises the missing values of each variable; `"record"` shows them record by record.
#' @param events Character vector. Only for longitudinal projects: events to plot, by their unique name (e.g. `"baseline_arm_1"`) or the name shown in the plot (e.g. `"Baseline"`). Use `"all"` to plot every event. Default `NULL`: every event with `by = "vars"` and the first event with `by = "record"`.
#' @param forms Character vector. Only plot the variables of these forms (instrument names as in the `form_name` column of the dictionary). Needs the dictionary. Default `NULL` (every form).
#' @return A `ggplot2` object representing a missingness barplot, a longitudinal missingness heatmap or a record-level missingness map.
#' 
#' @details
#' The function automatically detects whether the input dataset contains longitudinal event information. If `redcap_event_name` is detected in the data columns, a missingness heatmap grouped by event is produced.
#' When the event-form mapping is available (`event_form` element of the list), each variable is only evaluated in the events where its form is collected; the other cells of the heatmap are left blank.
#'
#' With `by = "record"`, longitudinal projects get one panel per event, each with the variables collected in that event. By default only the first event is plotted (with a warning); use `events` to choose others. Records without data in an event are shown in grey in that event's panel. Every row is labelled with its record identifier.
#'
#' @examples
#' \dontrun{
#' # Missing values per variable and event
#' rd_missplot(covican)
#'
#' # Missing values record by record in the baseline laboratory findings
#' rd_missplot(covican, by = "record", events = "baseline_visit_arm_1",
#'             forms = "laboratory_findings")
#' }
#'
#' @export

rd_missplot <- function(data, sort_by_missingness = TRUE, max_vars = NULL, show_percentages = TRUE, use_field_labels = TRUE, drop_complete = TRUE, max_label_length = 40, by = c("vars", "record"), events = NULL, forms = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The `ggplot2` package is required for `rd_missplot()`. Install it with `install.packages('ggplot2')`.", call. = FALSE)
  }

  by <- match.arg(by)

  # Extract data frame, dictionary and event-form mapping if nested inside a list (e.g., data_def)
  dictionary_df <- NULL
  event_form_df <- NULL

  if (is.list(data) && !is.data.frame(data)) {
    if ("data" %in% names(data)) {
      df <- data$data
    } else {
      stop("The list provided does not contain a 'data' element.")
    }
    if ("dictionary" %in% names(data)) {
      dictionary_df <- data$dictionary
    }
    if ("event_form" %in% names(data)) {
      event_form_df <- data$event_form
    }
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    stop("The 'data' argument must be a data frame or a list containing a 'data' element.")
  }

  # Detect if the study is longitudinal
  event_col <- "redcap_event_name"
  is_longitudinal <- event_col %in% colnames(df)

  # Check for factor version of event name for clean labels
  if (is_longitudinal && "redcap_event_name.factor" %in% colnames(df)) {
    event_vector_display <- as.character(df[["redcap_event_name.factor"]])
  } else if (is_longitudinal) {
    # Clean craw event names if factor version is absent
    event_vector_display <- df[[event_col]] |>
      stringr::str_remove("_arm_[0-9]+$") |>
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
  }

    # Identifier of the records (REDCap's record ID is the first field of the project)
  id_col <- if ("record_id" %in% colnames(df)) "record_id" else colnames(df)[1]
  all_records <- unique(as.character(df[[id_col]]))

  # Events to plot
  if (!is_longitudinal) {
    if (!is.null(events)) {
      warning("`events` is ignored: the dataset has no events.", call. = FALSE)
    }
  } else {
    raw_events <- as.character(df[[event_col]])

    # Chronological order: the one of event_form, then the order of appearance in the data
    events_in_data <- unique(raw_events[!is.na(raw_events) & nzchar(raw_events)])
    event_levels <- unique(c(intersect(as.character(event_form_df$unique_event_name), events_in_data), events_in_data))
    shown_name <- stats::setNames(event_vector_display, raw_events)[event_levels]

    if (is.null(events)) {
      selected_events <- if (by == "record") event_levels[1] else event_levels

      if (by == "record" && length(event_levels) > 1) {
        warning(stringr::str_glue(
          "Only the first event ({shown_name[1]}) is plotted. ",
          "Use `events` to choose other events or `events = \"all\"` to plot all of them."
        ), call. = FALSE)
      }
    } else if (length(events) == 1 && tolower(events) == "all") {
      selected_events <- event_levels
    } else {
      # Events can be given by their unique name or by the name shown in the plot
      events_raw <- ifelse(events %in% event_levels, events, event_levels[match(events, shown_name)])

      if (anyNA(events_raw)) {
        stop(sprintf(
          "Event(s) not found in the data: %s. Available events: %s.",
          paste0("'", events[is.na(events_raw)], "'", collapse = ", "),
          paste0("'", event_levels, "'", collapse = ", ")
        ), call. = FALSE)
      }

      selected_events <- event_levels[event_levels %in% events_raw]
    }

    keep <- raw_events %in% selected_events
    df <- df[keep, , drop = FALSE]
    event_vector_display <- event_vector_display[keep]
  }

  # System columns and form completion fields to always exclude
  redcap_system_cols <- c(
    "record_id", "redcap_event_name", "redcap_event_name.factor",
    "redcap_repeat_instrument", "redcap_repeat_instance",
    "redcap_data_access_group", "redcap_survey_identifier"
  )

  # Identify all columns to drop (.factor, system variables, and _complete fields)
  factor_cols <- grep("\\.factor$", colnames(df), value = TRUE)
  complete_status_cols <- grep("_complete$", colnames(df), value = TRUE)

  cols_to_exclude <- unique(c(redcap_system_cols, factor_cols, complete_status_cols))

  # Axis label of every column: its dictionary label (when requested and available) or its name, shortened to `max_label_length` characters
  axis_labels <- colnames(df)

  if (use_field_labels && !is.null(dictionary_df) &&
      all(c("field_name", "field_label") %in% colnames(dictionary_df))) {
    dic_labels <- as.character(dictionary_df$field_label[match(axis_labels, dictionary_df$field_name)])
    has_label <- !is.na(dic_labels) & nzchar(dic_labels)
    axis_labels[has_label] <- dic_labels[has_label]
  }

  if (!is.null(max_label_length)) {
    axis_labels <- stringr::str_trunc(axis_labels, max_label_length)
  }

  # Variables sharing a label would be merged in the plot: add their name
  shared <- axis_labels %in% axis_labels[duplicated(axis_labels)]
  axis_labels[shared] <- paste0(axis_labels[shared], " (", colnames(df)[shared], ")")

  label_lookup <- stats::setNames(axis_labels, colnames(df))

  get_label_mapping <- function(df_cols, dict) {
    unname(label_lookup[df_cols])
  }

    # Form of each variable (checkbox options are matched by the field name)
  has_forms <- !is.null(dictionary_df) && all(c("field_name", "form_name") %in% names(dictionary_df))

  get_var_form <- function(vars) {
    vapply(vars, function(v) {
      form <- dictionary_df$form_name[match(c(v, sub("___.*$", "", v)), dictionary_df$field_name)]
      form <- form[!is.na(form)]
      if (length(form) > 0) as.character(form[1]) else NA_character_
    }, character(1))
  }

  # Clean dataframe by removing system variables
  df_cleaned <- df |>
    dplyr::select(-dplyr::any_of(cols_to_exclude))

  # Variables of the selected forms
  if (!is.null(forms)) {
    if (!has_forms) {
      stop("`forms` needs the dictionary: provide a list with a `dictionary` element that includes `field_name` and `form_name`.", call. = FALSE)
    }

    unknown_forms <- setdiff(forms, dictionary_df$form_name)
    if (length(unknown_forms) > 0) {
       stop(sprintf("Form(s) not found in the dictionary: %s.", paste0("'", unknown_forms, "'", collapse = ", ")), call. = FALSE)
    }

    df_cleaned <- df_cleaned[, get_var_form(colnames(df_cleaned)) %in% forms, drop = FALSE]
  }

    ## Record-level map: one row per record, one column per variable ##

  if (by == "record") {

    df_cleaned <- df_cleaned[, setdiff(colnames(df_cleaned), id_col), drop = FALSE]

    if (ncol(df_cleaned) == 0) {
      stop("There are no variables to plot.", call. = FALSE)
    }

    # Row of the plot: the record, and its repetition if the row belongs to a repeating instrument or event
    row_id <- as.character(df[[id_col]])

    if ("redcap_repeat_instance" %in% colnames(df)) {
      instance <- as.character(df$redcap_repeat_instance)
      instrument <- if ("redcap_repeat_instrument" %in% colnames(df)) as.character(df$redcap_repeat_instrument) else rep(NA_character_, nrow(df))
      repetition <- ifelse(!is.na(instrument) & nzchar(instrument), paste0(instrument, "-", instance), instance)
      row_id <- ifelse(is.na(instance) | !nzchar(instance), row_id, paste0(row_id, " (", repetition, ")"))
    }

    plot_vars <- colnames(df_cleaned)
    n_rows <- nrow(df_cleaned)

    missing_records <- data.frame(
      row_id = rep(row_id, times = length(plot_vars)),
      raw_event = rep(if (is_longitudinal) as.character(df[[event_col]]) else "", times = length(plot_vars)),
      event = rep(if (is_longitudinal) event_vector_display else "", times = length(plot_vars)),
      variable = rep(plot_vars, each = n_rows),
      missing = unlist(lapply(df_cleaned, is.na), use.names = FALSE),
      stringsAsFactors = FALSE
    )

    # Leave out the events where a variable is not collected (its form is not designated in that event)
    if (is_longitudinal && has_forms && all(c("unique_event_name", "form") %in% names(event_form_df))) {
      collected_keys <- paste(event_form_df$unique_event_name, event_form_df$form, sep = "\r")
      row_form <- get_var_form(plot_vars)[missing_records$variable]
      missing_records <- missing_records[
        is.na(row_form) | paste(missing_records$raw_event, row_form, sep = "\r") %in% collected_keys, ]
    }

    total_missing <- tapply(missing_records$missing, factor(missing_records$variable, levels = plot_vars), sum)
    total_missing <- total_missing[!is.na(total_missing)]

    # Drop 100% complete variables if requested
    if (drop_complete) {
      complete_vars <- names(total_missing)[total_missing == 0]
      if (length(complete_vars) > 0) {
        message(paste0("Note: ", length(complete_vars), " fully complete variable(s) (0% missing) were hidden from the plot. Set drop_complete = FALSE to display them."))
        total_missing <- total_missing[total_missing > 0]
      }
    }

    if (length(total_missing) == 0) {
      stop("There are no variables to plot: none of the selected variables has missing values.", call. = FALSE)
    }

    # Variable order (columns)
    var_order <- if (sort_by_missingness) names(sort(total_missing, decreasing = TRUE)) else names(total_missing)

    if (!is.null(max_vars) && is.numeric(max_vars)) {
      var_order <- utils::head(var_order, max_vars)
    }

    missing_records <- missing_records[missing_records$variable %in% var_order, ]

    # Every record in every event/variable: records without data in an event are shown in grey
    record_rows <- unique(c(row_id, all_records))
    cells <- unique(missing_records[, c("event", "variable")])
    full_grid <- merge(data.frame(row_id = record_rows, stringsAsFactors = FALSE), cells, by = NULL)
    missing_records <- merge(full_grid, missing_records[, c("row_id", "event", "variable", "missing")],
                             by = c("row_id", "event", "variable"), all.x = TRUE)

    missing_records$status <- factor(
      ifelse(is.na(missing_records$missing), "Event not recorded",
             ifelse(missing_records$missing, "Missing", "Observed")),
      levels = c("Missing", "Observed", "Event not recorded")
    )

    # Records in natural order (e.g. 100-6 before 100-13), the first one on top
    row_order <- record_rows[stringi::stri_order(record_rows, opts_collator = stringi::stri_opts_collator(numeric = TRUE))]

    missing_records$display_label <- factor(get_label_mapping(missing_records$variable, dictionary_df),
                                            levels = get_label_mapping(var_order, dictionary_df))
    missing_records$row_id <- factor(missing_records$row_id, levels = rev(row_order))

    # Panels in chronological order
    if (is_longitudinal) {
      missing_records$event <- factor(missing_records$event, levels = unique(shown_name[selected_events]))
    }

    n_records <- length(all_records)

    # Smaller identifiers when there are many rows, so that they don't overlap (7 with 50 rows or fewer, 2 at least)
    id_size <- max(2, min(7, 350 / length(record_rows)))
    not_recorded <- any(missing_records$status == "Event not recorded")

    p <- ggplot2::ggplot(
      missing_records,
      ggplot2::aes(x = .data$display_label, y = .data$row_id, fill = .data$status)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(
        values = c("Missing" = "#2c3e50", "Observed" = "white", "Event not recorded" = "grey85"),
        name = NULL
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = "grey60"))) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Missing Data per Record",
        x = "Variables",
        y = paste0("Records (n = ", n_records, ")"),
        caption = if (not_recorded) "Grey cells: the record has no data in that event." else NULL
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = ggplot2::element_text(size = id_size),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey60"),
        legend.position = "bottom"
      )

    # Longitudinal projects: one panel per event, each with its own variables
    if (is_longitudinal) {
      p <- p + ggplot2::facet_grid(~event, scales = "free_x", space = "free_x")
    }

    return(p)
  }

  ## Cross-Sectional Study (Single Event): Barplot ##

  if (!is_longitudinal) {

    # Calculate missingness per variable
    missing_summary <- df_cleaned |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          ~ sum(is.na(.x))
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "missing_count"
      ) |>
      dplyr::mutate(
        total_obs = nrow(df),
        missing_pct = (.data$missing_count / .data$total_obs) * 100,
        display_label = get_label_mapping(.data$variable, dictionary_df)
      )

    # Drop 100% complete variables if requested
    if (drop_complete) {
      complete_count <- sum(missing_summary$missing_count == 0)
      if (complete_count > 0) {
        message(paste0("Note: ", complete_count, " fully complete variable(s) (0% missing) were hidden from the plot. Set drop_complete = FALSE to display them."))
        missing_summary <- missing_summary |>
          dplyr::filter(.data$missing_count > 0)
      }
    }

    # Apply ordering logic
    if (sort_by_missingness) {
      missing_summary <- missing_summary |>
        dplyr::arrange(dplyr::desc(.data$missing_count))
    } else {
      orig_levels <- get_label_mapping(colnames(df), dictionary_df)
      missing_summary <- missing_summary |>
        dplyr::mutate(display_label = factor(.data$display_label, levels = orig_levels))
    }

    # Filter top N variables if 'max_vars' is defined
    if (!is.null(max_vars) && is.numeric(max_vars)) {
      missing_summary <- missing_summary |>
        dplyr::slice_head(n = max_vars)
    }

    # Fix factor levels for correct ggplot axis display (bottom-to-top)
    if (sort_by_missingness) {
      missing_summary <- missing_summary |>
        dplyr::mutate(display_label = stats::reorder(.data$display_label, .data$missing_count))
    } else {
      missing_summary <- missing_summary |>
        dplyr::mutate(display_label = factor(.data$display_label, levels = rev(levels(.data$display_label))))
    }

    # Set y-axis target (percentage vs raw count)
    y_var <- if (show_percentages) "missing_pct" else "missing_count"
    y_label <- if (show_percentages) "Missing Values (%)" else "Missing Values (Count)"

    # Build barplot
    p <- ggplot2::ggplot(
      missing_summary,
      ggplot2::aes(x = .data$display_label, y = .data[[y_var]])
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "#3498db", alpha = 0.85) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Missing Data Overview per Variable",
        x = "Variables",
        y = y_label
      ) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
      )

    return(p)

    ## Longitudinal Study (Multi-Event): Heatmap ##

  } else {

    # Attach the cleaned display event vector to dataset
    df_cleaned$clean_event_name <- event_vector_display

    # Extract true chronological event order (First appearance)
    if ("unique_event_name" %in% colnames(event_form_df)) {
      raw_event_order <- unique(event_form_df$unique_event_name)

      # Map the raw events from event_form to the formatted names in the display
      event_map <- stats::setNames(event_vector_display, df[[event_col]])
      event_order <- unique(event_map[raw_event_order])

      # Remove NAs if any event in event_form had no recorded data
      event_order <- event_order[!is.na(event_order) & nzchar(event_order)]

    } else {
      # If event_form does not exist, use the order of occurrence in the clean dataset
      event_order <- unique(df_cleaned$clean_event_name)
    }

    # Remove NAs or empty values
    event_order <- event_order[!is.na(event_order) & nzchar(event_order)]

    # Calculate missingness grouped by clean_event_name
    missing_longitudinal <- df_cleaned |>
      dplyr::group_by(.data$clean_event_name) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          ~ sum(is.na(.x))
        ),
        event_total_obs = dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = -c("clean_event_name", "event_total_obs"),
        names_to = "variable",
        values_to = "missing_count"
      ) |>
      dplyr::mutate(
        missing_pct = (.data$missing_count / .data$event_total_obs) * 100,
        display_label = get_label_mapping(.data$variable, dictionary_df)
      )
    
    # Leave out the events where a variable is not collected (its form is not designated in that event), instead of showing them as 100% missing
    if (has_forms && all(c("unique_event_name", "form") %in% names(event_form_df))) {

      # Forms collected in each event, using the event names shown in the plot
      event_names <- unique(data.frame(raw = as.character(df[[event_col]]), shown = event_vector_display))
      collected <- merge(event_form_df[, c("unique_event_name", "form")], event_names,
                         by.x = "unique_event_name", by.y = "raw")
      collected_keys <- paste(collected$shown, collected$form, sep = "\r")

      # Form of each variable (checkbox options are matched by the field name)
      var_form <- get_var_form(unique(missing_longitudinal$variable))

      row_form <- var_form[missing_longitudinal$variable]
      missing_longitudinal <- missing_longitudinal[
        is.na(row_form) | paste(missing_longitudinal$clean_event_name, row_form, sep = "\r") %in% collected_keys, ]
    }

    # Drop 100% complete variables if requested across all events
    if (drop_complete) {
      complete_vars <- missing_longitudinal |>
        dplyr::group_by(.data$display_label) |>
        dplyr::summarise(total_missing = sum(.data$missing_count), .groups = "drop") |>
        dplyr::filter(.data$total_missing == 0) |>
        dplyr::pull(.data$display_label)

      if (length(complete_vars) > 0) {
        message(paste0("Note: ", length(complete_vars), " fully complete variable(s) (0% missing across all events) were hidden from the heatmap. Set drop_complete = FALSE to display them."))
        missing_longitudinal <- missing_longitudinal |>
          dplyr::filter(!.data$display_label %in% complete_vars)
      }
    }

    # Determine variable order for columns
    if (sort_by_missingness) {
      var_order_df <- missing_longitudinal |>
        dplyr::group_by(.data$display_label) |>
        dplyr::summarise(total_missing = sum(.data$missing_count), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$total_missing))

      var_order <- var_order_df$display_label
    } else {
      raw_cols <- setdiff(colnames(df_cleaned), "clean_event_name")
      var_order <- get_label_mapping(raw_cols, dictionary_df)
      if (drop_complete) {
        var_order <- setdiff(var_order, complete_vars)
      }
    }

    # Filter top N variables if 'max_vars' is specified
    if (!is.null(max_vars) && is.numeric(max_vars)) {
      var_order <- head(var_order, max_vars)

      missing_longitudinal <- missing_longitudinal |>
        dplyr::filter(.data$display_label %in% var_order)
    }

    # Assign Factors with fixed levels
    missing_longitudinal <- missing_longitudinal |>
      dplyr::mutate(
        display_label = factor(.data$display_label, levels = var_order),
        clean_event_name = factor(.data$clean_event_name, levels = rev(event_order))
      )

    fill_var <- if (show_percentages) "missing_pct" else "missing_count"
    fill_label <- if (show_percentages) "Missing %" else "Missing Count"
    
    # Explain the blank cells (variables not collected in some events)
    blank_cells <- nrow(missing_longitudinal) < dplyr::n_distinct(missing_longitudinal$clean_event_name) * dplyr::n_distinct(missing_longitudinal$display_label)
    caption <- if (blank_cells) "Blank cells: the variable is not collected in that event." else NULL

    # Build heatmap: X = Variables (Columns), Y = REDCap Event (Rows)
    p <- ggplot2::ggplot(
      missing_longitudinal,
      ggplot2::aes(
        x = .data$display_label,
        y = .data$clean_event_name,
        fill = .data[[fill_var]]
      )
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.2) +
      ggplot2::scale_fill_viridis_c(option = "magma", direction = -1, name = fill_label) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Longitudinal Missing Data Heatmap",
        x = "Variables",
        y = "REDCap Event",
        caption = caption
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = ggplot2::element_text(size = 9),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        panel.grid = ggplot2::element_blank()
      )

    return(p)
  }
}
