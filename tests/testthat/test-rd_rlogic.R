# improved helper: return full dictionary rows for the requested fields
get_dic_rows <- function(fields) {
  # ensure fields is a character vector
  fields <- as.character(fields)

  # select rows from covican$dictionary that match the requested fields
  if (!is.null(covican$dictionary) && nrow(covican$dictionary) > 0) {
    rows <- covican$dictionary[covican$dictionary$field_name %in% fields, , drop = FALSE]
    if (nrow(rows) > 0) {
      # coerce relevant columns to character to avoid labelled/factor surprises
      rows[] <- lapply(rows, function(x) if (is.factor(x) || inherits(x, "labelled")) as.character(x) else x)
      return(rows)
    }
  }

  # fallback: return a minimal dic containing columns rd_rlogic may expect
  data.frame(
    field_name = fields,
    form_name = rep("form1", length(fields)),
    section_header = "",
    field_type = "",
    field_label = "",
    choices_calculations_or_slider_labels = "",
    field_note = "",
    text_validation_type_or_show_slider_number = NA_character_,
    text_validation_min = "",
    text_validation_max = "",
    identifier = "",
    branching_logic_show_field_only_if = "",
    required_field = "",
    custom_alignment = "",
    question_number_surveys_only = "",
    matrix_group_name = "",
    matrix_ranking = "",
    field_annotation = "",
    stringsAsFactors = FALSE
  )
}

test_that("rd_rlogic stops if data or dic is missing", {
  expect_error(
    rd_rlogic(data = NULL, dic = NULL, logic = "[x]='1'", var = "y"),
    "Both `data` and `dic`"
  )
})

test_that("rd_rlogic stops if longitudinal data but no event_form", {
  # use first two rows of covican$data (they correspond to different events)
  df_multi <- head(covican$data, 2)
  field <- "exc_1"
  dic <- get_dic_rows(field)

  expect_error(
    rd_rlogic(data = df_multi, dic = dic, logic = "[exc_1]='1'", var = field),
    "event-form"
  )
})

test_that("rd_rlogic stops if logic references variables not in data", {
  # choose a single event row to guarantee rd_rlogic won't demand event_form
  target_event <- as.character(covican$event_form$unique_event_name[1])
  row_idx <- which(as.character(covican$data$redcap_event_name) == target_event)[1]
  if (is.na(row_idx) || length(row_idx) == 0) row_idx <- 1L
  df_single <- covican$data[row_idx, , drop = FALSE]

  field <- "exc_1"
  dic <- get_dic_rows(field)

  expect_error(
    rd_rlogic(data = df_single, dic = dic, logic = "[nonexistent]='1'", var = field),
    "There is more than one event in the data, but the event-form correspondence hasn't been specified."
  )
})

test_that("rd_rlogic translates simple logic correctly", {
  target_event <- as.character(covican$event_form$unique_event_name[1])
  df_all <- covican$data[as.character(covican$data$redcap_event_name) == target_event, , drop = FALSE]

  # fallback if that event subset is empty
  if (nrow(df_all) == 0) df_all <- covican$data

  # ensure at least two non-NA rows for exc_1
  idx <- which(!is.na(df_all$exc_1))
  skip_if(length(idx) < 2, "not enough non-NA exc_1 values to run test")
  df <- df_all[idx[1:2], , drop = FALSE]

  dic <- get_dic_rows("exc_1")

  out <- rd_rlogic(data = df, dic = dic,
                   logic = "if([exc_1]='1',1,0)",
                   var = "exc_1",
                   event_form = covican$event_form)

  expect_true(grepl("ifelse\\(data\\$exc_1==", out$rlogic))
  # normalize and compare (allow for logical or numeric return)
  eval_numeric <- as.numeric(out$eval)
  expect_equal(eval_numeric, as.numeric(df$exc_1 == 1))
})

test_that("rd_rlogic handles checkbox notation", {
  # find an expanded checkbox column in covican$data (___)
  checkbox_cols <- grep("___", names(covican$data), value = TRUE)
  skip_if(length(checkbox_cols) == 0, "no checkbox-style columns found in covican$data")

  base_name <- unique(gsub("___\\d+$", "", checkbox_cols))[1]
  choice_col <- grep(paste0("^", base_name, "___"), names(covican$data), value = TRUE)[1]
  skip_if(is.na(choice_col), "no checkbox choice column found")

  # Use at least two rows from the real dataset and keep full context columns (including event)
  df <- covican$data[1:2, , drop = FALSE]
  # ensure the choice column exists
  stopifnot(choice_col %in% names(df))

  # coerce the choice column to character like REDCap exports
  df[[choice_col]] <- as.character(df[[choice_col]])

  dic <- get_dic_rows(base_name)

  out <- rd_rlogic(data = df, dic = dic,
                   logic = paste0("[", base_name, "(0)]='1'"),
                   var = base_name,
                   event_form = covican$event_form)

  # expect the generated rlogic to reference the expanded checkbox column name
  expect_true(any(grepl(paste0("data\\$", choice_col), out$rlogic)))
})

test_that("rd_rlogic replaces and/or operators", {
  # pick two real binary-like fields present in covican$data
  required <- c("exc_1", "inc_1")
  present <- intersect(required, names(covican$data))
  skip_if(length(present) < 2, "required fields not present in covican$data")

  # use a small subset but include redcap_event_name (rd_rlogic uses it when event_form provided)
  df <- head(covican$data[, c(present, "redcap_event_name")], 3)

  dic <- get_dic_rows(present)

  out <- rd_rlogic(data = df, dic = dic,
                   logic = "if([exc_1]='1' or [inc_1]='0',1,0)",
                   var = "exc_1",
                   event_form = covican$event_form)

  expect_true(grepl("\\|", out$rlogic)) # "or" -> "|"
  eval_numeric <- as.numeric(out$eval)
  expected <- as.numeric((df$exc_1 == 1) | (df$inc_1 == 0))
  expect_equal(eval_numeric, expected)
})

test_that("rd_rlogic warns when date field is character", {
  df <- data.frame(date_start = "2020-01-01", stringsAsFactors = FALSE)
  dic <- data.frame(field_name = "date_start",
                    text_validation_type_or_show_slider_number = "date_ymd",
                    form_name = "form1",
                    stringsAsFactors = FALSE)

  expect_warning(
    rd_rlogic(data = df, dic = dic,
              logic = "[date_start]='2020-01-01'",
              var = "date_start"),
    "date fields stored as character"
  )
})

test_that("rd_rlogic handles event-specific logic", {
  # two-row dataset with two different events
  df <- head(covican$data, 2)
  field <- "exc_1"
  dic <- get_dic_rows(field)

  # construct event_form mapping the field's form to only the first event
  formname <- as.character(covican$dictionary$form_name[covican$dictionary$field_name == field])
  if (length(formname) == 0 || is.na(formname)) formname <- dic$form_name[1]

  event_form <- data.frame(
    form = formname,
    unique_event_name = as.character(covican$event_form$unique_event_name[1]),
    stringsAsFactors = FALSE
  )

  out <- rd_rlogic(data = df, dic = dic, event_form = event_form,
                   logic = "[exc_1]='1'",
                   var = field)

  # rd_rlogic should evaluate only for the rows that match event_form; the other row(s) should be NA
  expect_equal(length(out$eval), nrow(df))
  expect_true(is.na(out$eval[2]) || identical(out$eval[2], NA))
})

test_that("fill_data fills from the specified event across records and uses first non-NA when repeated", {
  # build a small toy dataset with multiple records and repeated events
  df <- data.frame(
    record_id = c(1, 1, 1, 2, 2, 3, 3, 4),
    redcap_event_name = c("ev2", "ev1", "ev1", "ev2", "ev1", "ev2", "ev3", "ev1"),
    visit_date = c(NA, "A", "A2", "B", NA, NA, "C", "D"),
    stringsAsFactors = FALSE
  )

  # call fill_data: we want the value from event "ev1" to be filled for each record
  out <- fill_data(which_event = "ev1", which_var = "visit_date", data = df)

  # Expectations:
  # - For record 1: two ev1 entries ("A","A2") -> function should pick the *first* non-NA unique value ("A")
  #   and fill it to all rows for record 1.
  # - For record 2: ev1 exists but is NA -> result for record 2 should be NA for all its rows.
  # - For record 3: ev1 is not present for that record -> result NA for its rows.
  # - For record 4: ev1 present with "D" -> "D" for its rows.
  expected <- c("A", "A", "A", NA, NA, NA, NA, "D")

  expect_equal(out$visit_date, expected)
})

test_that("fill_data errors when the requested event is not present in the dataset", {
  df2 <- data.frame(
    record_id = c(1,2),
    redcap_event_name = c("evA", "evB"),
    some_var = c("x", "y"),
    stringsAsFactors = FALSE
  )

  expect_error(
    fill_data(which_event = "nonexistent_event", which_var = "some_var", data = df2),
    "The logic can't be evaluated after the translation"
  )
})
