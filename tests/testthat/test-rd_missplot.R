make_missplot_data <- function() {
  df <- data.frame(
    record_id = 1:6,
    age = c(20, NA, 22, NA, NA, 25),           # 3 missing
    weight = c(70, 71, NA, NA, NA, NA),        # 4 missing
    height = c(160, 161, 162, 163, 164, 165),  # 0 missing -> fully complete
    sex = c("1", "2", "1", "2", "1", "2"),     # 0 missing -> fully complete
    stringsAsFactors = FALSE
  )

  # weight's field_label is deliberately NA: regression fixture for the
  # nzchar(NA) crash that used to happen in get_label_mapping().
  dic <- data.frame(
    field_name = c("record_id", "age", "weight", "height", "sex"),
    field_label = c("Record ID", "Age (years)", NA, "Height (cm)", "Sex"),
    stringsAsFactors = FALSE
  )

  list(data = df, dic = dic)
}

make_longitudinal_missplot_data <- function() {
  # Note the raw event order of appearance in the data is visit_2, visit_1 --
  # the opposite of the order declared in event_form -- so that a test can
  # tell whether event_form's order is actually being honored.
  df <- data.frame(
    record_id = c(1, 1, 2, 2),
    redcap_event_name = c("visit_2_arm_1", "visit_1_arm_1", "visit_2_arm_1", "visit_1_arm_1"),
    age = c(NA, 30, 31, NA),
    weight = c(70, NA, NA, 72),
    stringsAsFactors = FALSE
  )

  dic <- data.frame(
    field_name = c("record_id", "age", "weight"),
    field_label = c("Record ID", "Age (years)", NA),
    stringsAsFactors = FALSE
  )

  event_form <- data.frame(
    unique_event_name = c("visit_1_arm_1", "visit_2_arm_1"),
    form = "form1",
    stringsAsFactors = FALSE
  )

  list(data = df, dictionary = dic, event_form = event_form)
}

test_that("rd_missplot errors when data is neither a data frame nor a list with a 'data' element", {
  expect_error(rd_missplot(1:5), "data frame")
})

test_that("rd_missplot errors when the list provided lacks a 'data' element", {
  expect_error(rd_missplot(list(foo = 1)), "`data` element", class = "REDCapDM_error_argument")
})

test_that("rd_missplot does not crash on a NA field_label and falls back to the variable name", {
  fx <- make_missplot_data()

  p <- suppressMessages(rd_missplot(list(data = fx$data, dictionary = fx$dic), drop_complete = FALSE))

  expect_s3_class(p, "ggplot")
  expect_true("weight" %in% as.character(p$data$display_label))
})

test_that("drop_complete = TRUE messages and filters out fully complete variables", {
  fx <- make_missplot_data()

  expect_message(
    p <- rd_missplot(list(data = fx$data, dictionary = fx$dic), use_field_labels = FALSE),
    "fully complete"
  )

  vars_shown <- as.character(p$data$variable)
  expect_false("height" %in% vars_shown)
  expect_false("sex" %in% vars_shown)
  expect_true("age" %in% vars_shown)
  expect_true("weight" %in% vars_shown)
})

test_that("max_vars limits the number of variables plotted to the ones with the most missingness", {
  fx <- make_missplot_data()

  p <- suppressMessages(
    rd_missplot(list(data = fx$data, dictionary = fx$dic), use_field_labels = FALSE, max_vars = 1)
  )

  expect_equal(nrow(p$data), 1)
  expect_equal(as.character(p$data$variable), "weight")
})

test_that("longitudinal heatmap orders events using event_form, not first appearance in the data", {
  fx <- make_longitudinal_missplot_data()

  p <- suppressMessages(rd_missplot(fx, use_field_labels = FALSE))

  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$clean_event_name), c("Visit 2", "Visit 1"))
})

test_that("longitudinal heatmap with sort_by_missingness = FALSE and drop_complete = TRUE does not error", {
  fx <- make_longitudinal_missplot_data()

  p <- suppressMessages(
    rd_missplot(fx, sort_by_missingness = FALSE, use_field_labels = FALSE)
  )

  expect_s3_class(p, "ggplot")
})


# ---- Repeated instruments ----

test_that("rd_missplot only counts the rows where each variable is collected", {
  fx <- make_repeat_classic()
  p <- suppressMessages(rd_missplot(fx, use_field_labels = FALSE, drop_complete = FALSE))

  res <- as.data.frame(p$data)
  expect_equal(res$missing_pct[res$variable == "age"], 50)
  expect_equal(res$total_obs[res$variable == "age"], 2)
  expect_equal(res$total_obs[res$variable == "ae_term"], 3)
})

test_that("rd_missplot heatmap uses the instances of a form repeating in only some events", {
  fx <- make_repeat_longitudinal()
  p <- suppressMessages(rd_missplot(fx, use_field_labels = FALSE, drop_complete = FALSE))

  res <- as.data.frame(p$data)
  hb_treatment <- res[res$variable == "hb" & res$clean_event_name == "Treatment", ]
  expect_equal(hb_treatment$event_total_obs, 2)
  expect_equal(hb_treatment$missing_pct, 50)
})

test_that("rd_missplot warns when there are repeated instruments but no dictionary", {
  fx <- make_repeat_classic()
  expect_warning(
    suppressMessages(rd_missplot(fx$data, drop_complete = FALSE)),
    "no dictionary was provided"
  )
})

# ---- Axis labels and event-form mapping ----

make_long_label_data <- function() {
  df <- data.frame(record_id = 1:4, a = c(NA, 1, 2, 3), b = c(NA, NA, 1, 2), c = c(NA, 1, 1, 1))
  dic <- data.frame(
    field_name = c("record_id", "a", "b", "c"),
    field_label = c("ID",
                    "A very long label that describes the first variable in detail",
                    "A very long label that describes the second variable in detail",
                    "Short"),
    stringsAsFactors = FALSE
  )
  list(data = df, dictionary = dic)
}

test_that("rd_missplot shortens long labels and keeps them distinct", {
  p <- suppressMessages(rd_missplot(make_long_label_data(), max_label_length = 20, drop_complete = FALSE))
  labels <- as.character(p$data$display_label)

  expect_true("Short" %in% labels)
  # both long labels are cut to the same 20 characters, so the variable name is added
  expect_setequal(labels[labels != "Short"], c("A very long label... (a)", "A very long label... (b)"))
})

test_that("rd_missplot keeps complete labels with max_label_length = NULL", {
  p <- suppressMessages(rd_missplot(make_long_label_data(), max_label_length = NULL, drop_complete = FALSE))
  expect_true("A very long label that describes the first variable in detail" %in% as.character(p$data$display_label))
})

test_that("rd_missplot heatmap leaves out the events where a variable is not collected", {
  p <- suppressMessages(rd_missplot(covican, use_field_labels = FALSE, drop_complete = FALSE))
  res <- as.data.frame(p$data)

  # copd (comorbidities) is only collected at baseline
  expect_equal(as.character(res$clean_event_name[res$variable == "copd"]), "Baseline visit")
  # potassium (laboratory_findings) is collected at both events
  expect_equal(sum(res$variable == "potassium"), 2)
  expect_match(p$labels$caption, "not collected")
})

test_that("rd_missplot heatmap uses every event when there is no event-form mapping", {
  p <- suppressMessages(rd_missplot(covican[c("data", "dictionary")], use_field_labels = FALSE, drop_complete = FALSE))
  res <- as.data.frame(p$data)

  expect_equal(sum(res$variable == "copd"), 2)
  expect_null(p$labels$caption)
})

# ---- by = "record", events and forms ----

test_that("by = 'record' paints the missing cells of each record", {
  fx <- make_missplot_data()
  p <- suppressMessages(rd_missplot(list(data = fx$data, dictionary = fx$dic), by = "record", use_field_labels = FALSE))
  res <- as.data.frame(p$data)

  expect_s3_class(p, "ggplot")
  expect_setequal(unique(as.character(res$variable)), c("age", "weight"))
  expect_equal(nrow(res), 6 * 2)
  expect_equal(sort(as.character(res$row_id[res$variable == "age" & res$status == "Missing"])), c("2", "4", "5"))
  expect_equal(sum(res$status == "Missing"), 3 + 4)
  # record_id is the identifier, not a variable
  expect_false("record_id" %in% res$variable)
  # first record on top
  expect_equal(levels(res$row_id), as.character(6:1))
  expect_equal(p$labels$y, "Records (n = 6)")
})

test_that("by = 'record' keeps drop_complete, max_vars and sort_by_missingness", {
  fx <- make_missplot_data()
  input <- list(data = fx$data, dictionary = fx$dic)

  expect_message(rd_missplot(input, by = "record"), "fully complete")

  p <- suppressMessages(rd_missplot(input, by = "record", use_field_labels = FALSE, drop_complete = FALSE))
  expect_equal(levels(p$data$display_label), c("weight", "age", "height", "sex"))

  p <- suppressMessages(rd_missplot(input, by = "record", use_field_labels = FALSE, drop_complete = FALSE, sort_by_missingness = FALSE))
  expect_equal(levels(p$data$display_label), c("age", "weight", "height", "sex"))

  p <- suppressMessages(rd_missplot(input, by = "record", use_field_labels = FALSE, max_vars = 1))
  expect_equal(unique(as.character(p$data$variable)), "weight")
})

test_that("by = 'record' orders the records naturally", {
  df <- data.frame(record_id = c("10-2", "9-1", "10-10", "10-1"), a = c(NA, 1, NA, 2))
  p <- rd_missplot(df, by = "record")

  expect_equal(rev(levels(p$data$row_id)), c("9-1", "10-1", "10-2", "10-10"))
})

test_that("by = 'record' uses the first column as identifier when there is no record_id", {
  df <- data.frame(patient = c("A", "B"), a = c(NA, 1), b = c(2, NA))
  p <- rd_missplot(df, by = "record")

  expect_setequal(as.character(p$data$row_id), c("A", "B"))
  expect_false("patient" %in% p$data$variable)
})

test_that("by = 'record' plots the first event by default, with a warning", {
  expect_warning(
    p <- suppressMessages(rd_missplot(covican, by = "record")),
    "Only the first event \\(Baseline visit\\)"
  )

  expect_equal(levels(p$data$event), "Baseline visit")
  expect_equal(length(unique(p$data$row_id)), 190)
})

test_that("by = 'record' plots the chosen events in chronological order, by unique or shown name", {
  p <- suppressMessages(rd_missplot(covican, by = "record", use_field_labels = FALSE,
                                    events = c("Follow up visit day 14+/-5d", "baseline_visit_arm_1")))
  res <- as.data.frame(p$data)

  expect_equal(levels(res$event), c("Baseline visit", "Follow up visit day 14+/-5d"))
  # copd (comorbidities) is only collected at baseline
  expect_equal(unique(as.character(res$event[res$variable == "copd"])), "Baseline visit")
  # 38 records have no follow-up visit: shown in grey in that panel
  follow_up <- res[res$event == "Follow up visit day 14+/-5d" & res$variable == "potassium", ]
  expect_equal(sum(follow_up$status == "Event not recorded"), 38)
  expect_equal(sum(follow_up$status == "Missing"), sum(is.na(covican$data$potassium[covican$data$redcap_event_name == "follow_up_visit_da_arm_1"])))
  expect_match(p$labels$caption, "no data in that event")

  p_all <- suppressMessages(rd_missplot(covican, by = "record", events = "all"))
  expect_equal(levels(p_all$data$event), levels(res$event))
})

test_that("by = 'record' labels every row with its record identifier", {
  p <- suppressWarnings(suppressMessages(rd_missplot(covican, by = "record")))
  expect_false(inherits(p$theme$axis.text.y, "element_blank"))
  expect_setequal(levels(p$data$row_id), unique(covican$data$record_id))

  small <- covican
  small$data <- covican$data[covican$data$record_id %in% unique(covican$data$record_id)[1:10], ]
  p <- suppressMessages(rd_missplot(small, by = "record", events = "all"))
  expect_false(inherits(p$theme$axis.text.y, "element_blank"))
    expect_equal(p$labels$y, "Records (n = 10)")
  expect_equal(p$theme$axis.text.y$size, 7)
})

test_that("by = 'record' makes the identifiers smaller when there are many records", {
  p <- suppressWarnings(suppressMessages(rd_missplot(covican, by = "record")))
  expect_lt(p$theme$axis.text.y$size, 7)
  expect_gte(p$theme$axis.text.y$size, 2)
})

test_that("forms restricts the variables plotted, in both modes", {
  p <- suppressMessages(rd_missplot(covican, by = "record", events = "all", forms = "laboratory_findings",
                                    use_field_labels = FALSE, drop_complete = FALSE))
  forms_shown <- covican$dictionary$form_name[match(unique(as.character(p$data$variable)), covican$dictionary$field_name)]
  expect_equal(unique(forms_shown), "laboratory_findings")

  p <- suppressMessages(rd_missplot(covican, forms = c("demographics", "comorbidities"), use_field_labels = FALSE, drop_complete = FALSE))
  forms_shown <- covican$dictionary$form_name[match(unique(as.character(p$data$variable)), covican$dictionary$field_name)]
  expect_setequal(unique(forms_shown), c("demographics", "comorbidities"))
})

test_that("events filters the heatmap of by = 'vars'", {
  p <- suppressMessages(rd_missplot(covican, events = "follow_up_visit_da_arm_1", use_field_labels = FALSE))

  expect_equal(as.character(unique(p$data$clean_event_name)), "Follow up visit day 14+/-5d")
  expect_false("copd" %in% p$data$variable)
})

test_that("rd_missplot validates by, events and forms", {
  fx <- make_missplot_data()

  expect_error(rd_missplot(covican, by = "patient"), "should be one of")
  expect_error(rd_missplot(covican, events = c("baseline_visit_arm_1", "visit_99")), "Event\\(s\\) not found in the data: \"visit_99\"", class = "REDCapDM_error_argument")
  expect_error(rd_missplot(covican, forms = "labs"), "Form\\(s\\) not found in the dictionary: \"labs\"", class = "REDCapDM_error_argument")
  expect_error(rd_missplot(fx$data, forms = "demo"), "`forms` needs the dictionary")
  expect_warning(suppressMessages(rd_missplot(fx$data, events = "baseline")), "`events` is ignored")
  expect_error(suppressMessages(rd_missplot(list(data = fx$data[, c("record_id", "height")]), by = "record")), "There are no variables to plot")
})

test_that("by = 'vars' gives the same plot as before when events and forms are not used", {
  p_default <- suppressMessages(rd_missplot(covican))
  p_all <- suppressMessages(rd_missplot(covican, by = "vars", events = "all"))

  expect_equal(as.data.frame(p_default$data), as.data.frame(p_all$data))
})


test_that("rd_missplot doesn't plot the `_mdc` columns of redcap_data(rm_mdc = FALSE)", {
  mdc <- covican
  mdc$data <- dplyr::mutate(covican$data, potassium_mdc = ifelse(is.na(potassium), "UNK", NA), .after = "potassium")

  p <- suppressMessages(rd_missplot(mdc, use_field_labels = FALSE, drop_complete = FALSE))
  expect_false("potassium_mdc" %in% p$data$variable)
  expect_true("potassium" %in% p$data$variable)
})


test_that("by = 'record' tells apart the cells not collected in a row of a repeated instrument", {
  fx <- make_repeat_classic()
  p <- suppressMessages(rd_missplot(fx, by = "record", use_field_labels = FALSE, drop_complete = FALSE))
  res <- as.data.frame(p$data)

  status <- function(row, var) as.character(res$status[res$row_id == row & res$variable == var])
  expect_equal(status("1", "age"), "Observed")
  expect_equal(status("2", "age"), "Missing")
  expect_equal(status("1 (ae-2)", "ae_term"), "Missing")
  # age is not asked in the instances, nor ae_term in the base rows
  expect_equal(status("1 (ae-1)", "age"), "Not collected in this row")
  expect_equal(status("1", "ae_term"), "Not collected in this row")
  expect_match(p$labels$caption, "not collected in that row")
  expect_equal(p$labels$y, "Records (n = 2)")
})

test_that("by = 'record' leaves out the rows without any of the plotted variables", {
  fx <- make_repeat_classic()
  p <- suppressMessages(rd_missplot(fx, by = "record", forms = "ae", use_field_labels = FALSE, drop_complete = FALSE))

  expect_setequal(levels(droplevels(p$data$row_id)), c("1 (ae-1)", "1 (ae-2)", "2 (ae-1)"))
  expect_false("Not collected in this row" %in% p$data$status)
})


test_that("rd_missplot explains why it can't plot", {
  fx <- make_repeat_classic()
  expect_error(rd_missplot(fx, max_vars = 0), "`max_vars` must be a positive number")

  fx$data <- fx$data[0, ]
  expect_error(rd_missplot(fx), "the data has 0 rows")
})
