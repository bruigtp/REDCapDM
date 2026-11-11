#' Plot Missing Data Overview (Barplot or Longitudinal Heatmap)
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Generates a visual summary of missing data for variables in a REDCap dataset or list structure. For cross-sectional (single-event) studies, it displays a barplot showing the proportion or count of missing values per variable. For longitudinal (multi-event) studies, it generates a heatmap illustrating missingness across events and variables.
#'
#' @param data A `data frame` or a list containing a `data` element retrieved from REDCap.
#' @param sort_by_missingness Logical. Variables are ordered from highest to lowest missingness. Default `TRUE`.
#' @param max_vars Integer. If specified, only the top `max_vars` variables are plotted (by missingness, or by original column order if `sort_by_missingness = FALSE`). Default `NULL` (show all variables).
#' @param show_percentages Logical. Displays missingness as percentages. Default `TRUE`.
#' @param use_field_labels Logical. Uses field labels from the dictionary for axis labels instead of variable names. Default `TRUE`.
#' @param drop_complete Logical. Variables with 0 missing values are excluded from the plot. Default `TRUE`.
#' @param max_label_length Integer. Maximum number of characters of the axis labels. Use `NULL` to keep them complete. Default `40`.
#' @return A `ggplot2` object representing either a missingness barplot or a longitudinal missingness heatmap.
#'
#' @details
#' The function automatically detects whether the input dataset contains longitudinal event information. If `redcap_event_name` is detected in the data columns, a missingness heatmap grouped by event is produced.
#' When the event-form mapping is available (`event_form` element of the list), each variable is only evaluated in the events where its form is collected; the other cells of the heatmap are left blank.
#'
#' @export

rd_missplot <- function(data, sort_by_missingness = TRUE, max_vars = NULL, show_percentages = TRUE, use_field_labels = TRUE, drop_complete = TRUE, max_label_length = 40) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The `ggplot2` package is required for `rd_missplot()`. Install it with `install.packages('ggplot2')`.", call. = FALSE)
  }

  # Extract data frame and dictionary if nested inside a list (e.g., data_def)
  dictionary_df <- NULL

  if (is.list(data) && !is.data.frame(data)) {
    if ("data" %in% names(data)) {
      df <- data$data
    } else {
      stop("The list provided does not contain a 'data' element.")
    }
    if ("dictionary" %in% names(data)) {
      dictionary_df <- data$dictionary
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

  # Clean dataframe by removing system variables
  df_cleaned <- df |>
    dplyr::select(-dplyr::any_of(cols_to_exclude))

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
    if (is.list(data) && "event_form" %in% names(data) &&
        "unique_event_name" %in% colnames(data$event_form)) {
      raw_event_order <- unique(data$event_form$unique_event_name)

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
    event_form_df <- if (is.list(data) && !is.data.frame(data)) data$event_form else NULL

    if (!is.null(dictionary_df) &&
        all(c("field_name", "form_name") %in% names(dictionary_df)) &&
        all(c("unique_event_name", "form") %in% names(event_form_df))) {

      # Forms collected in each event, using the event names shown in the plot
      event_names <- unique(data.frame(raw = as.character(df[[event_col]]), shown = event_vector_display))
      collected <- merge(event_form_df[, c("unique_event_name", "form")], event_names,
                         by.x = "unique_event_name", by.y = "raw")
      collected_keys <- paste(collected$shown, collected$form, sep = "\r")

      # Form of each variable (checkbox options are matched by the field name)
      var_form <- vapply(unique(missing_longitudinal$variable), function(v) {
        form <- dictionary_df$form_name[match(c(v, sub("___.*$", "", v)), dictionary_df$field_name)]
        form <- form[!is.na(form)]
        if (length(form) > 0) as.character(form[1]) else NA_character_
      }, character(1))

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
