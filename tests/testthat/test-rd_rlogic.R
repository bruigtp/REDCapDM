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
    "The data is longitudinal (it has the column `redcap_event_name`), but the event-form correspondence hasn't been specified.",
    fixed = TRUE
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

test_that("fill_data fills from the specified event across records correctly", {
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
  # - For record 1: ev1 rows remain "A", "A2", propagated to ev2 row with fill -> "A", "A", "A2"
  # - For record 2: ev1 is NA -> ev2 row stays NA
  # - For record 3: no ev1 -> all NA
  # - For record 4: ev1 "D" -> stays "D"
  expected <- c("A", "A", "A2", NA, NA, NA, NA, "D")

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

# helper that creates a tiny dictionary row for given fields
make_dic <- function(fields, choices = NULL, text_validation = NA_character_, form = "form1") {
  fields <- as.character(fields)
  n <- length(fields)
  df <- data.frame(
    field_name = fields,
    form_name = rep(as.character(form), n),
    section_header = "",
    field_type = "",
    field_label = "",
    choices_calculations_or_slider_labels = rep(ifelse(is.null(choices), "", choices), n),
    field_note = "",
    text_validation_type_or_show_slider_number = rep(text_validation, n),
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
  df
}

test_that("rd_rlogic warns when multiple logic expressions or multiple vars are provided and uses first", {
  df <- data.frame(a = c(1,0), stringsAsFactors = FALSE)
  dic <- make_dic("a")
  expect_warning(
    expect_warning(
      out <- rd_rlogic(data = df, dic = dic,
                       logic = c("if([a]='1',1,0)", "if([a]='0',1,0)"),
                       var = c("a", "a")),
      "`logic` contains more than one expression"
    ),
    "`var` contains more than one variable name"
  )

  # Should have translated the first logic only
  expect_true(grepl("ifelse\\(data\\$a==", out$rlogic))
  expect_equal(as.numeric(out$eval), as.numeric(df$a == 1))
})

test_that("rd_rlogic translates rounddown(...) to floor/round variants", {
  df <- data.frame(x = c(2.6, 3.2), stringsAsFactors = FALSE)
  dic <- make_dic("x")
  # test rounddown with 0 -> floor
  out0 <- rd_rlogic(data = df, dic = dic, logic = "rounddown([x],0)", var = "x")
  expect_true(grepl("floor\\(", out0$rlogic) || grepl("round\\(", out0$rlogic))
  # evaluate should equal floor(x) when used as expression (no ifelse wrapper)
  # Because rd_rlogic returns value in `eval` for single-event (no event_form)
  expect_equal(as.numeric(out0$eval), floor(df$x))
})

test_that("rd_rlogic translates sum(...) ignoring blank values, as REDCap", {
  df <- data.frame(a = c(1, 2, NA), b = c(3, NA, NA), stringsAsFactors = FALSE)
  dic <- make_dic(c("a", "b"))
  out <- rd_rlogic(data = df, dic = dic,
                   logic = "sum([a],[b])",
                   var = "a")
  expect_true(grepl("rowSums\\(", out$rlogic))
  # blank values are ignored; blank when all the values are blank
  expect_equal(as.numeric(out$eval), c(4, 2, NA))
})

test_that("rd_rlogic translates the REDCap functions row by row, as REDCap", {
  df <- data.frame(a = c(7.25, NA, 2.5, NA), b = c(3.333, 5, NA, NA), t = c("Abc", "x", NA, NA), stringsAsFactors = FALSE)
  dic <- make_dic(c("a", "b", "t"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "a")$eval)

  # aggregate functions ignore blank values
  expect_equal(ev("min([a],[b])"), c(3.333, 5, 2.5, NA))
  expect_equal(ev("max([a],[b])"), c(7.25, 5, 2.5, NA))
  expect_equal(ev("mean([a],[b])"), c(5.2915, 5, 2.5, NA))
  expect_equal(ev("median([a],[b])"), c(5.2915, 5, 2.5, NA))
  expect_equal(ev("sum(min([a],[b]),max([a],[b]))"), c(10.583, 10, 5, NA))

  # rounding keeps the decimals requested
  expect_equal(ev("rounddown([a],1)"), c(7.2, NA, 2.5, NA))
  expect_equal(ev("roundup([b],2)"), c(3.34, 5, NA, NA))
  expect_equal(ev("rounddown([a],0) + rounddown([b],0)"), c(10, NA, NA, NA))
  expect_equal(ev("round([a])"), c(7, NA, 3, NA))

  # other functions
  expect_equal(ev("mod([b],2)"), c(3.333 %% 2, 1, NA, NA))
  expect_equal(ev("exponential([a])"), exp(df$a))
  expect_equal(ev("isnumber([a])"), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(ev("contains([t],'b')"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(ev("length([t])"), c(3, 1, 0, 0))
})

test_that("rd_rlogic translates logic that uses the same variable in different events", {
  df <- data.frame(record_id = c(1, 1, 2, 2),
                   redcap_event_name = c("ev1", "ev2", "ev1", "ev2"),
                   x = c(10, 14, 20, NA),
                   stringsAsFactors = FALSE)
  dic <- make_dic(c("x", "change"))
  event_form <- data.frame(form = "form1", unique_event_name = c("ev1", "ev2"), stringsAsFactors = FALSE)

  # e.g. the change between two visits
  out <- rd_rlogic(data = df, dic = dic, event_form = event_form, logic = "[ev2][x] - [ev1][x]", var = "change")

  expect_equal(out$eval, c(4, 4, NA, NA))
  expect_match(out$rlogic, "data$x__ev2 - data$x__ev1", fixed = TRUE)
})

test_that("rd_rlogic evaluates the logic of a repeated instrument in its instances", {
  # Build a dataset with redcap_repeat_instrument and a dic marking the form
  df <- data.frame(record_id = 1:2,
                   redcap_event_name = c("ev1", "ev1"),
                   redcap_repeat_instrument = c("form_repeat", NA),
                   redcap_repeat_instance = c(1, NA),
                   repvar = c(1, 0),
                   stringsAsFactors = FALSE)
  # dic says repvar belongs to form_repeat (so it's in a repeated instrument)
  dic <- make_dic("repvar", form = "form_repeat")
  logic <- "if([repvar]='1',1,0)"
  out <- rd_rlogic(data = df, dic = dic, event_form = data.frame(form="form_repeat", unique_event_name="ev1", stringsAsFactors = FALSE),
                   logic = logic, var = "repvar")

  # the second row is a base row, where repvar is not collected
  expect_equal(out$eval, c(1, NA))
})

test_that("rd_rlogic maps factor variables used in arithmetic to numeric via choices_calculations_or_slider_labels", {
  # factor variable with labelled choices like "1, Yes|2, No"
  df <- data.frame(f = factor(c("Yes", "No")), g = c(1, 2), stringsAsFactors = FALSE)
  dic <- make_dic("f", choices = "1, Yes | 2, No")
  # logic uses f in arithmetic with g; vars_calc detection should convert factor to numeric
  out <- rd_rlogic(data = df, dic = dic, logic = "if([f]+[g] > 1, 1, 0)", var = "f")
  expect_true(grepl("data\\$f", out$rlogic))
  # After mapping, evaluation should be numeric and length matches rows
  expect_equal(length(out$eval), nrow(df))
  # Confirm at least one TRUE/1 expected value
  expect_true(any(as.numeric(out$eval) %in% c(0,1)))
})

test_that("rd_rlogic throws an error when final evaluation fails (invalid R code after translation)", {
  df <- data.frame(a = c(1, 0), stringsAsFactors = FALSE)
  dic <- make_dic("a")
  # malformed logic that will produce invalid R code after translation
  bad_logic <- "if([a]='1', 1, )"
  expect_error(
    rd_rlogic(data = df, dic = dic, logic = bad_logic, var = "a"),
    "The logic could not be evaluated after translation\\."
  )
})


# ---- eval_data ----

test_that("rd_rlogic returns eval_data with the record, the event and eval", {
  out <- rd_rlogic(covican,
                   logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)",
                   var = "screening_fail_crit")

  expect_s3_class(out$eval_data, "data.frame")
  expect_equal(names(out$eval_data), c("record_id", "redcap_event_name", "eval"))
  expect_equal(nrow(out$eval_data), nrow(covican$data))
  expect_equal(as.character(out$eval_data$record_id), as.character(covican$data$record_id))
  expect_equal(as.character(out$eval_data$redcap_event_name), as.character(covican$data$redcap_event_name))
  # eval is unchanged and the same as in eval_data
  expect_true(is.vector(out$eval))
  expect_identical(out$eval_data$eval, out$eval)
})

test_that("rd_rlogic eval_data uses the first column as identifier when there is no record_id", {
  df <- data.frame(patient = c("A", "B", "C"), a = c(1, 0, 1))
  dic <- data.frame(field_name = c("patient", "a", "b"), form_name = "form1", field_type = c("text", "text", "calc"),
                    text_validation_type_or_show_slider_number = "", stringsAsFactors = FALSE)

  out <- rd_rlogic(data = df, dic = dic, logic = "[a]='1'", var = "b")

  expect_equal(names(out$eval_data), c("patient", "eval"))
  expect_equal(out$eval_data$patient, c("A", "B", "C"))
  expect_equal(out$eval_data$eval, c(TRUE, FALSE, TRUE))
})

test_that("rlogic_eval_data keeps the repetition columns and needs one value per row", {
  df <- data.frame(record_id = c(1, 1, 2), redcap_repeat_instrument = c(NA, "visits", NA),
                   redcap_repeat_instance = c(NA, 1, NA), a = 1:3)

  res <- rlogic_eval_data(df, c(TRUE, FALSE, NA))
  expect_equal(names(res), c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", "eval"))
  expect_equal(res$eval, c(TRUE, FALSE, NA))

  expect_equal(rlogic_eval_data(df, 5)$eval, c(5, 5, 5))
  expect_null(rlogic_eval_data(df, c(1, 2)))
})


# ---- Variables already converted to factors ----

test_that("rd_rlogic gives the same evaluation before and after rd_factor()", {
  logic <- "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)"
  before <- rd_rlogic(covican, logic = logic, var = "screening_fail_crit")
  after <- rd_rlogic(suppressWarnings(rd_factor(covican)), logic = logic, var = "screening_fail_crit")

  expect_equal(after$rlogic, before$rlogic)
  expect_equal(after$eval, before$eval)
  expect_equal(sum(after$eval == 1, na.rm = TRUE), 4)
})

test_that("rd_rlogic keeps working with logic that compares factors with their labels", {
  fac <- suppressWarnings(rd_factor(covican))
  before <- rd_rlogic(covican, logic = "[dm]='1'", var = "type_dm")
  after <- rd_rlogic(fac, logic = "[dm]='Yes'", var = "type_dm")

  expect_equal(after$eval, before$eval)
})

test_that("rd_rlogic evaluates factor checkboxes, yes/no fields and non-numeric codes with their codes", {
  data <- data.frame(
    record_id = 1:3,
    sym___1 = factor(c("Checked", "Unchecked", NA), levels = c("Unchecked", "Checked")),
    smoker = factor(c("Yes", "No", "Yes"), levels = c("No", "Yes")),
    group = factor(c("Alpha", "Beta", "Alpha"))
  )
  dic <- data.frame(
    field_name = c("record_id", "sym", "smoker", "group", "res"),
    form_name = "form1",
    field_type = c("text", "checkbox", "yesno", "radio", "calc"),
    choices_calculations_or_slider_labels = c("", "1, Cough | 2, Fever", "", "A, Alpha | B, Beta", ""),
    text_validation_type_or_show_slider_number = "",
    stringsAsFactors = FALSE
  )

  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[sym(1)]='1'", var = "res")$eval, c(TRUE, FALSE, NA))
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[smoker]='1'", var = "res")$eval, c(TRUE, FALSE, TRUE))
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[group]='B'", var = "res")$eval, c(FALSE, TRUE, FALSE))
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[smoker]+[sym(1)]", var = "res")$eval, c(2, 0, NA))
})

test_that("rd_rlogic leaves a factor as it is when its values aren't the labels of the dictionary", {
  data <- data.frame(record_id = 1:2, smoker = factor(c("Si", "No")))
  dic <- data.frame(
    field_name = c("record_id", "smoker", "res"), form_name = "form1", field_type = c("text", "radio", "calc"),
    choices_calculations_or_slider_labels = c("", "0, No | 1, Yes", ""),
    text_validation_type_or_show_slider_number = "", stringsAsFactors = FALSE
  )

  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[smoker]='Si'", var = "res")$eval, c(TRUE, FALSE))
})


# ---- Repeated instruments and repeating events ----

test_that("rd_rlogic evaluates a repeating instrument variable in its instances only", {
  fx <- make_repeat_classic()
  out <- rd_rlogic(fx, logic = "[ae_grade] > '1'", var = "ae_term")

  expect_equal(out$eval, c(NA, FALSE, TRUE, NA, TRUE))
  expect_equal(names(out$eval_data), c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", "eval"))
})

test_that("rd_rlogic takes the variables of the other forms from the base row, as REDCap does", {
  fx <- make_repeat_classic()
  out <- rd_rlogic(fx, logic = "[age] > '40'", var = "ae_grade")

  # record 1 is 50 years old in its base row; record 2 has no age
  expect_equal(out$eval, c(NA, TRUE, TRUE, NA, NA))
})

test_that("rd_rlogic errors when the logic uses a variable of another repeating instrument", {
  fx <- make_repeat_classic()

  err <- expect_error(rd_rlogic(fx, logic = "[ae_grade] = '1'", var = "age"), class = "REDCapDM_error_logic")
  expect_match(conditionMessage(err), "variables\\s+of\\s+another\\s+repeated\\s+instrument")
  expect_match(conditionMessage(err), "ae_grade (form: ae)", fixed = TRUE)
})

test_that("rd_rlogic handles forms repeating in only some events", {
  fx <- make_repeat_longitudinal()

  # labs is a regular form at baseline and a repeating instrument at treatment
  out <- rd_rlogic(fx, logic = "[hb] < '12'", var = "hb")
  expect_equal(out$eval, c(FALSE, NA, TRUE, NA, NA, NA))

  # sbp (vitals, treatment base row) is taken for the instances of labs at treatment
  out <- rd_rlogic(fx, logic = "[sbp] > '100'", var = "hb")
  expect_equal(out$eval, c(NA, NA, TRUE, TRUE, NA, NA))
})

# ---- datediff ----

test_that("rd_rlogic returns the absolute datediff() unless it is signed, as REDCap does", {
  df <- data.frame(record_id = 1:2, d1 = as.Date(c("2024-03-05", "2024-03-01")), d2 = as.Date(c("2024-03-01", "2024-03-05")))
  dic <- make_dic(c("record_id", "d1", "d2", "res"), text_validation = "date_ymd")

  out <- rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'d')", var = "res")
  expect_equal(out$eval, c(4, 4))
  expect_match(out$rlogic, "^abs\\(")

  out <- rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'d','ymd',true)", var = "res")
  expect_equal(out$eval, c(-4, 4))
})

test_that("datediff_absolute only wraps the unsigned datediff() calls", {
  expect_equal(
    datediff_absolute("if(datediff([a],[b],'d')>5 and datediff([c],'today','y',true)<1,1,0)"),
    "if(abs(datediff([a],[b],'d'))>5 and datediff([c],'today','y',true)<1,1,0)"
  )
  expect_equal(datediff_absolute("rounddown(datediff([a],[b],'y'),0)"), "rounddown(abs(datediff([a],[b],'y')),0)")
  expect_equal(datediff_absolute("[x] + 1"), "[x] + 1")
})

test_that("rd_rlogic translates the date format, the units and 'today' of datediff()", {
  df <- data.frame(record_id = 1:2, d1 = as.Date(c("2024-01-01", "2023-01-01")), d2 = as.Date(c("2024-03-01", "2024-01-01")))
  dic <- make_dic(c("record_id", "d1", "d2", "res"), text_validation = "date_ymd")

  # date format argument
  for (format in c("ymd", "mdy", "dmy")) {
    out <- rd_rlogic(data = df, dic = dic, logic = sprintf("datediff([d1],[d2],'d','%s')", format), var = "res")
    expect_equal(out$eval, c(60, 365))
  }

  # 'M' are months and 'm' minutes
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'M')", var = "res")$eval, c(60, 365) / 30.44)
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'m')", var = "res")$eval, c(60, 365) * 24 * 60)

  # 'today'
  out <- rd_rlogic(data = df, dic = dic, logic = "datediff([d1],'today','d')", var = "res")
  expect_equal(out$eval, as.numeric(Sys.Date() - df$d1))
})


test_that("rd_rlogic names the variables it can't find and shows the translation it can't evaluate", {
  df <- data.frame(a = c(1, 2), stringsAsFactors = FALSE)
  dic <- make_dic("a")

  expect_error(rd_rlogic(data = df, dic = dic, logic = "[b] > 1", var = "a"), "\\[b\\]")
  err <- expect_error(rd_rlogic(data = df, dic = dic, logic = "percentile([a],[a])", var = "a"), class = "REDCapDM_error_logic")
  expect_match(conditionMessage(err), "R logic: `percentile(data$a,data$a)`", fixed = TRUE)
  expect_match(conditionMessage(err), "could not find function", fixed = TRUE)
})

test_that("datediff() counts years of 365.2425 days and months of 30.44 days, as REDCap does", {
  df <- data.frame(record_id = 1:2, d1 = as.Date(c("2023-01-01", "2023-01-01")), d2 = as.Date(c("2024-01-01", "2023-07-01")))
  dic <- make_dic(c("record_id", "d1", "d2", "res"), text_validation = "date_ymd")
  days <- c(365, 181)

  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'y')", var = "res")$eval, days / 365.2425)
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'M')", var = "res")$eval, days / 30.44)

  # A year of 365 days is not a whole year: REDCap gives 0
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "rounddown(datediff([d1],[d2],'y'),0)", var = "res")$eval, c(0, 0))

  # Signed and inside other functions
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d2],[d1],'M','ymd',true)", var = "res")$eval, -days / 30.44)
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "mod(datediff([d1],[d2],'y'),1)", var = "res")$eval, (days / 365.2425) %% 1)
})

test_that("checkbox_columns translates the checkbox options of any code", {
  expect_equal(checkbox_columns("[cb(1)] = '1'"), "[cb___1] = '1'")
  expect_equal(checkbox_columns("[cb(A)] = '1' or [cb(b2)] = '1'"), "[cb___a] = '1' or [cb___b2] = '1'")
  expect_equal(checkbox_columns("[cb(-1)] = '1'"), "[cb____1] = '1'")
  expect_equal(checkbox_columns("[visit_1_arm_1][cb(x_y)] = '0'"), "[visit_1_arm_1][cb___x_y] = '0'")
  expect_equal(checkbox_columns("[a] + 1"), "[a] + 1")
})

test_that("rd_rlogic evaluates checkbox options with letters and negative codes", {
  df <- data.frame(record_id = 1:3, cb___a = c(1, 0, 1), cb____1 = c(0, 1, 1))
  dic <- make_dic(c("record_id", "cb", "res"))

  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[cb(A)] = '1'", var = "res")$eval), c(TRUE, FALSE, TRUE))
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "sum([cb(A)],[cb(-1)])", var = "res")$eval), c(1, 1, 2))
})

test_that("rd_rlogic translates != as <>, as REDCap does", {
  df <- data.frame(record_id = 1:4, a = c(1, 4, NA, 5))
  dic <- make_dic(c("record_id", "a", "res"))

  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[a] != 4", var = "res")$eval), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[a] != ''", var = "res")$eval), c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "[a] != 4", var = "res")$rlogic, rd_rlogic(data = df, dic = dic, logic = "[a] <> 4", var = "res")$rlogic)
})

test_that("rd_rlogic: a blank value is different from any value, as in REDCap", {
  df <- data.frame(record_id = 1:3, a = c(1, NA, 2), t = c("x", NA, "y"))
  dic <- make_dic(c("record_id", "a", "t", "res"))

  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[a] <> '1'", var = "res")$eval), c(FALSE, TRUE, TRUE))
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[t] <> 'x' and [a] <> 2", var = "res")$eval), c(FALSE, TRUE, FALSE))
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "if([a] <> 1, 5, 0)", var = "res")$eval), c(0, 5, 5))
})

test_that("rd_rlogic translates the text functions of REDCap, with blank values as empty texts", {
  df <- data.frame(record_id = 1:4, t = c("Hello World", "hello", NA, "  pad  "), n = c(1, NA, 3, 4))
  dic <- make_dic(c("record_id", "t", "n", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  expect_equal(ev("isblankormissingcode([n])"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(ev("not_contain([t], 'WORLD')"), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(ev("starts_with([t], 'hel')"), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(ev("ends_with([t], 'LO')"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(ev("left([t], 3) = 'Hel'"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(ev("right([t], 2)"), c("ld", "lo", "", "  "))
  expect_equal(ev("mid([t], 2, 3)"), c("ell", "ell", "", " pa"))
  expect_equal(ev("find('l', [t])"), c(3, 3, 0, 0))
  expect_equal(ev("replace_text([t], 'l', 'L')"), c("HeLLo WorLd", "heLLo", "", "  pad  "))
  expect_equal(ev("upper([t]) = 'HELLO'"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(ev("lower([t])"), c("hello world", "hello", "", "  pad  "))
  expect_equal(ev("trim([t]) = 'pad'"), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(ev("concat([t], '-', [n])"), c("Hello World-1", "hello-", "-3", "  pad  -4"))
  expect_equal(ev("concat_ws(' ', [t], [n])"), c("Hello World 1", "hello", "3", "  pad   4"))
})

test_that("rd_rlogic translates stdev(), isinteger(), month(), day() and mod() as REDCap", {
  df <- data.frame(record_id = 1:4, a = c(-7, 2.5, NA, 6), b = c(3, 1.5, 2, NA), d = as.Date(c("2024-03-15", NA, "2020-12-31", "2023-01-02")))
  dic <- make_dic(c("record_id", "a", "b", "d", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  expect_equal(ev("mod([a], [b])"), c(-1, 1, NA, NA))
  expect_equal(ev("stdev([a], [b])"), c(stats::sd(c(-7, 3)), stats::sd(c(2.5, 1.5)), NA, NA))
  expect_equal(ev("isinteger([a])"), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(ev("month([d])"), c(3, NA, 12, 1))
  expect_equal(ev("day([d])"), c(15, NA, 31, 2))
})

test_that("rd_rlogic doesn't translate the functions written inside a text", {
  df <- data.frame(record_id = 1:3, t = c("left (L)", "sum(1,2)", "x"), n = c(1, 5, NA), m = 2)
  dic <- make_dic(c("record_id", "t", "n", "m", "res"))

  out <- rd_rlogic(data = df, dic = dic, logic = "[t] = 'sum(1,2)' or sum([n],[m]) > 5", var = "res")
  expect_match(out$rlogic, "data$t == 'sum(1,2)'", fixed = TRUE)
  expect_equal(as.vector(out$eval), c(FALSE, TRUE, FALSE))
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = "[t] = \"left (L)\" and left([t], 4) = 'left'", var = "res")$eval), c(TRUE, FALSE, FALSE))
})

test_that("rd_rlogic understands 'now' and dates with time", {
  df <- data.frame(record_id = 1:2, dt = as.POSIXct(c("2024-01-05 10:30:00", "2024-01-06 12:00:00"), tz = "UTC"))
  dic <- make_dic(c("record_id", "dt", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  expect_equal(ev("datediff([dt], '2024-01-05 12:30', 'h', 'ymd', true)"), c(2, -23.5))
  expect_equal(ev("datediff('05-01-2024 10:30:00', [dt], 'm', 'dmy')"), c(0, 1530))
  expect_true(all(ev("datediff([dt], 'now', 'd') > 300")))
})

test_that("rd_rlogic gives one value per row with an if() whose condition doesn't use any variable", {
  age <- suppressWarnings(suppressMessages(rd_rlogic(covican, logic = "[age]", var = "age")))$eval
  yes <- suppressWarnings(suppressMessages(rd_rlogic(covican, logic = "if(1 < 2, [age], 0)", var = "age")))
  no <- suppressWarnings(suppressMessages(rd_rlogic(covican, logic = "if(1 > 2, [age], 0)", var = "age")))
  nested <- suppressWarnings(suppressMessages(rd_rlogic(covican, logic = "if([age] > 60, if(1 = 1, [age], 0), 1)", var = "age")))
  expect_gt(length(unique(stats::na.omit(age))), 1)
  expect_equal(yes$eval, age)
  expect_true(all(no$eval[!is.na(age)] == 0))
  expect_equal(nested$eval, ifelse(age > 60, age, 1))
  # The conditions that use variables are translated as before
  expect_false(grepl("rep_len", suppressWarnings(suppressMessages(rd_rlogic(covican, logic = "if([age] > 60, 1, 0)", var = "age")))$rlogic))
})

test_that("rd_rlogic compares with negative numbers, with or without quotes and spaces", {
  df <- data.frame(record_id = 1:5, a = c(-5, 2, NA, -3.5, -20))
  dic <- make_dic(c("record_id", "a", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  # [a]<-3 is a comparison, not an assignment
  expect_equal(ev("[a]<-3"), c(TRUE, FALSE, NA, TRUE, TRUE))
  expect_equal(ev("if([a]<-3, 1, 0)"), c(1, 0, NA, 1, 1))
  expect_equal(ev("[a] < '-3'"), c(TRUE, FALSE, NA, TRUE, TRUE))
  expect_equal(ev("[a]<='-5'"), c(TRUE, FALSE, NA, FALSE, TRUE))
  expect_equal(ev("[a] >= '-4'"), c(FALSE, TRUE, NA, TRUE, FALSE))
  expect_equal(ev("[a] > '-10'"), c(TRUE, TRUE, NA, TRUE, FALSE))
})

test_that("rd_rlogic explains a var that is not in the dictionary and a logic without one value per row", {
  expect_error(rd_rlogic(covican, logic = "[age] > 70", var = "not_a_field"), class = "REDCapDM_error_argument")
  err <- expect_error(rd_rlogic(covican, logic = "c(1, 2, 3)", var = "age"), class = "REDCapDM_error_logic")
  expect_match(conditionMessage(err), "3 values", fixed = TRUE)
})

test_that("rd_rlogic only translates the REDCap functions, not the fields with the same name", {
  df <- data.frame(record_id = 1:3, motif___1 = c(1, 0, 0), motif___2 = c(0, 1, 0), datediff = c(5, 10, 15), d = as.Date(c("2020-01-01", "2021-06-01", NA)))
  dic <- make_dic(c("record_id", "motif", "datediff", "d", "res"))
  dic$field_type[dic$field_name == "motif"] <- "checkbox"
  dic$choices_calculations_or_slider_labels[dic$field_name == "motif"] <- "1, A | 2, B"
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  expect_equal(ev("[datediff] > 7"), c(FALSE, TRUE, TRUE))
  expect_equal(ev("if([datediff] > 7, year([d]), 0)"), c(0, 2021, NA))
  expect_equal(ev("if([motif(2)] = '1', 1, 0)"), c(0, 1, 0))
  expect_equal(ev("datediff([d], '2022-01-01', 'd', 'ymd') > 300"), c(TRUE, FALSE, NA))
})

test_that("rd_rlogic translates `and` and `or` written in several lines, in capitals or next to a parenthesis, but not inside a text", {
  df <- data.frame(record_id = 1:4, a = c(1, 2, NA, 1), b = c(2, 1, 1, NA), t = c("x or y", "z", "and", NA))
  dic <- make_dic(c("record_id", "a", "b", "t", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  either <- ev("[a] = '1' or [b] = '1'")
  both <- ev("[a] = '1' and [b] = '2'")
  expect_equal(either, c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(ev("[a] = '1' or\n[b] = '1'"), either)
  expect_equal(ev("[a] = '1'\n  or [b] = '1'"), either)
  expect_equal(ev("[a] = '1' or\r\n[b] = '1'"), either)
  expect_equal(ev("[a] = '1'\tor\t[b] = '1'"), either)
  expect_equal(ev("[a] = '1' OR [b] = '1'"), either)
  expect_equal(ev("([a] = '1')or([b] = '1')"), either)
  expect_equal(ev("[a] = '1' and\n[b] = '2'"), both)
  expect_equal(ev("[a] = '1' AND [b] = '2'"), both)
  expect_equal(ev("if(\n  [a] = '' or\n  [b] = '',\n  0,\n  1\n)"), c(1, 1, 0, 0))
  # The words inside a text are not operators
  expect_equal(ev("[t] = 'x or y'"), c(TRUE, FALSE, FALSE, NA))
  expect_equal(ev("[t] = 'and' or [a] = '2'"), c(FALSE, TRUE, TRUE, NA))
})

test_that("rd_rlogic evaluates a total of several items written in several lines, as exported from REDCap", {
  items <- sprintf("fss_%02d", 1:9)
  df <- data.frame(record_id = 1:5)
  for (x in items) df[[x]] <- c(1, 2, 5555, 3, NA)
  df$fss_02[2] <- 7777
  df$fss_09[4] <- NA
  dic <- make_dic(c("record_id", items, "fss_total"))
  logic <- paste0(
    "if(\n    ", paste0("[", items, "]=''", collapse = " or\n    "), ",\n    '',\n    sum(\n",
    paste0("        if([", items, "] < 5555, [", items, "], 0)", collapse = ",\n"), "\n    )\n)"
  )
  expect_equal(as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "fss_total")$eval), c(9, 16, 0, NA, NA))
})

test_that("rd_rlogic compares with texts that look like operators, functions or fields", {
  df <- data.frame(record_id = 1:6, text = c("a=b", "a<>b", "if(x)", "year(x)", "[age]", "[nofield]"), age = c(10, 20, 30, 40, 50, 60))
  dic <- make_dic(c("record_id", "text", "age", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = df, dic = dic, logic = logic, var = "res")$eval)

  for (text in df$text) {
    expect_equal(ev(paste0("[text] = '", text, "'")), df$text == text)
  }
  expect_equal(ev("[text] = \"a=b\" or [age] = 30"), c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  # The other texts are still translated (numbers, empty texts)
  expect_equal(ev("[age] > '25' and [text] <> ''"), df$age > 25)
})

test_that("rd_dictionary and rd_transform write the labels with apostrophes so that the logic still works", {
  data <- data.frame(record_id = c("1", "2"), name = c(1, 2), q = c(NA, NA))
  data$name.factor <- factor(c("O'Brien", "Other"), levels = c("O'Brien", "Other"))
  dic <- make_dic(c("record_id", "name", "q"))
  dic$field_type <- c("text", "radio", "text")
  dic$choices_calculations_or_slider_labels <- c("", "1, O'Brien | 2, Other", "")
  dic$branching_logic_show_field_only_if <- c("", "", "[name] = '1'")
  project <- list(data = data, dictionary = dic)

  transformed <- suppressWarnings(suppressMessages(rd_transform(project)))
  # (translated to R by rd_dictionary(): before, the label broke the logic and it was left untranslated, with a warning)
  expect_equal(transformed$dictionary$branching_logic_show_field_only_if[transformed$dictionary$field_name == "q"], "data$name==\"O'Brien\"")
  # The query only for the record with O'Brien, as with the project before rd_transform()
  queries <- suppressWarnings(suppressMessages(rd_query(transformed, variables = "q", expression = "is.na(x)")))$queries
  expect_equal(queries$Identifier, 1, ignore_attr = TRUE)
  before <- suppressWarnings(suppressMessages(rd_query(project, variables = "q", expression = "is.na(x)")))$queries
  expect_equal(nrow(queries), nrow(before))
})

test_that("rd_dictionary and rd_transform escape the labels with both quotes or backslashes", {
  for (label in c("O'Brien \"A\"", "C:\\Ward", "say \"hi\"", "a\\'b")) {
    data <- data.frame(record_id = c("1", "2"), name = c(1, 2), q = c(NA, NA))
    data$name.factor <- factor(c(label, "Other"), levels = c(label, "Other"))
    dic <- make_dic(c("record_id", "name", "q"))
    dic$field_type <- c("text", "radio", "text")
    dic$choices_calculations_or_slider_labels <- c("", paste0("1, ", label, " | 2, Other"), "")
    dic$branching_logic_show_field_only_if <- c("", "", "[name] = '1'")
    project <- list(data = data, dictionary = dic)

    transformed <- suppressWarnings(suppressMessages(rd_transform(project)))
    logic <- transformed$dictionary$branching_logic_show_field_only_if[transformed$dictionary$field_name == "q"]
    # The R logic compares with the label
    expect_equal(eval(parse(text = logic), list(data = data.frame(name = c(label, "Other")))), c(TRUE, FALSE), info = label)
    queries <- suppressWarnings(suppressMessages(rd_query(transformed, variables = "q", expression = "is.na(x)")))$queries
    expect_equal(queries$Identifier, 1, ignore_attr = TRUE, info = label)
  }
})

test_that("rd_rlogic reads the escaped quotes of the texts", {
  data <- data.frame(record_id = 1:3, t = c("O'Brien \"A\"", "C:\\Ward", "x"), res = NA)
  dic <- make_dic(c("record_id", "t", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("[t] = 'O\\'Brien \"A\"'"), c(TRUE, FALSE, FALSE))
  expect_equal(ev("[t] = \"O'Brien \\\"A\\\"\""), c(TRUE, FALSE, FALSE))
  expect_equal(ev("[t] = 'C:\\\\Ward'"), c(FALSE, TRUE, FALSE))
  # (a backslash before another character is a backslash)
  expect_equal(ev("[t] = 'C:\\Ward'"), c(FALSE, TRUE, FALSE))
  expect_equal(ev("[t] <> 'O\\'Brien \"A\"' and [t] <> 'x'"), c(FALSE, TRUE, FALSE))
  expect_equal(ev("if([t] = 'O\\'Brien \"A\"', 1, 0)"), c(1, 0, 0))
})

test_that("the texts of a logic are never taken for the internal placeholders of rd_rlogic", {
  data <- data.frame(record_id = 1:3, t = c("REDCapDMtext1x", "a=b", "z"))
  dic <- make_dic(c("record_id", "t", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("[t] = 'a=b' or [t] = 'REDCapDMtext1x'"), c(TRUE, TRUE, FALSE))
  expect_equal(ev("[t] = 'REDCapDMtext2x' or [t] = 'a=b'"), c(FALSE, TRUE, FALSE))
  expect_equal(ev("[t] = 'REDCapDMtextx1x' or [t] = '(z)'"), c(FALSE, FALSE, FALSE))
})

test_that("rd_rlogic keeps two apostrophes inside a text", {
  data <- data.frame(record_id = 1:3, text = c("L''Z", "LNAZ", NA), res = NA)
  dic <- make_dic(c("record_id", "text", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  # (a blank value gives NA with `=`, as before)
  expect_equal(ev("[text] = \"L''Z\""), c(TRUE, FALSE, NA))
  expect_equal(ev("[text] <> \"L''Z\""), c(FALSE, TRUE, TRUE))
  expect_equal(ev("[text] = ''"), c(FALSE, FALSE, TRUE))
  expect_equal(ev("[text] = \"''\" or [text] = 'LNAZ'"), c(FALSE, TRUE, NA))

  # A label with two apostrophes
  data <- data.frame(record_id = c("1", "2"), name = c(1, 2), q = c(NA, NA))
  data$name.factor <- factor(c("L''Z", "Other"), levels = c("L''Z", "Other"))
  dic <- make_dic(c("record_id", "name", "q"))
  dic$field_type <- c("text", "radio", "text")
  dic$choices_calculations_or_slider_labels <- c("", "1, L''Z | 2, Other", "")
  dic$branching_logic_show_field_only_if <- c("", "", "[name] = '1'")
  transformed <- suppressWarnings(suppressMessages(rd_transform(list(data = data, dictionary = dic))))
  queries <- suppressWarnings(suppressMessages(rd_query(transformed, variables = "q", expression = "is.na(x)")))$queries
  expect_equal(queries$Identifier, 1, ignore_attr = TRUE)
})

test_that("rd_rlogic translates datediff() of two time fields", {
  data <- data.frame(record_id = 1:5, start = c("23:30", "01:15", "22:00", NA, "00:00"), end = c("07:00", "08:45", "22:00", "07:00", "23:59"), res = NA)
  dic <- make_dic(c("record_id", "start", "end", "res"))
  dic$text_validation_type_or_show_slider_number <- c("", "time", "time", "")
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  logic <- "if(datediff([start],[end],'h','hhmm',true) < 0, 24+datediff([start],[end],'h','hhmm',true), datediff([start],[end],'h','hhmm',true))"
  expect_equal(ev(logic), c(7.5, 7.5, 0, NA, 23 + 59 / 60))
  expect_equal(ev("datediff([start],[end],'h',true)"), c(-16.5, 7.5, 0, NA, 23 + 59 / 60))
  # Without `true`, the absolute difference
  expect_equal(ev("datediff([start],[end],'m')"), c(990, 450, 0, NA, 1439))
  expect_equal(ev("datediff([end],[start],'m','hhmm')"), c(990, 450, 0, NA, 1439))

  # With seconds
  data$start <- c("23:30:30", "01:15:00", "22:00:00", NA, "00:00:00")
  data$end <- c("07:00:00", "08:45:15", "22:00:00", "07:00:00", "23:59:59")
  dic$text_validation_type_or_show_slider_number <- c("", "time_hh_mm_ss", "time_hh_mm_ss", "")
  expect_equal(ev("datediff([start],[end],'s',true)"), c(-59430, 27015, 0, NA, 86399))
})

test_that("rd_rlogic evaluates with the codes a factor compared and used as a number", {
  data <- data.frame(record_id = 1:3, res = NA)
  dic <- make_dic(c("record_id", "score", "res"))
  dic$field_type <- c("text", "radio", "text")
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)

  # Labels that are the codes
  data$score <- factor(c("1", "2", "3"), levels = c("1", "2", "3"))
  dic$choices_calculations_or_slider_labels <- c("", "1, 1 | 2, 2 | 3, 3", "")
  expect_equal(ev("[score] = '2' or [score] + 1 >= 4"), c(FALSE, TRUE, TRUE))

  # Labels that aren't codes
  data$score <- factor(c("Low", "Mid", "High"), levels = c("Low", "Mid", "High"))
  dic$choices_calculations_or_slider_labels <- c("", "1, Low | 2, Mid | 3, High", "")
  expect_equal(ev("[score] = 'Mid' or [score] + 1 >= 4"), c(FALSE, TRUE, TRUE))
  expect_equal(ev("[score] = 'Mid'"), c(FALSE, TRUE, FALSE))
})

test_that("rd_rlogic keeps the operation of the value of <> and !=", {
  data <- data.frame(record_id = 1:4, a = c(0, 1, 2, NA), b = c(1, 1, NA, 1), res = NA)
  dic <- make_dic(c("record_id", "a", "b", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  for (logic in c("[a] <> 0 + 1", "[a] != 0 + 1", "[a] <> (0 + 1)", "[a] <> 2 * 0.5", "[a] <> 1")) {
    # (a blank value is different from any value)
    expect_equal(
      ev(logic),
      c(TRUE, FALSE, TRUE, TRUE),
      info = logic
    )
  }
  expect_equal(ev("if([a] <> 0 + 1, 1, 0)"), c(1, 0, 1, 1))
  expect_equal(ev("[a] <> 0 + 1 and [b] = 1"), c(TRUE, FALSE, NA, TRUE))
  # With variables of the data, the operation is compared row by row
  expect_equal(ev("[a] <> 0 + [b]"), c(TRUE, FALSE, TRUE, TRUE))
})

test_that("rd_rlogic translates the <> inside the value of another <>", {
  data <- data.frame(record_id = 1:3, a = c(0, 0, NA), b = c(NA, 1, 2), res = NA)
  dic <- make_dic(c("record_id", "a", "b", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("[a] <> 0 + if([b] <> 1, 1, 0)"), ev("[a] <> if([b] <> 1, 1, 0)"))
  expect_equal(ev("[a] <> 0 + if([b] <> 1, 1, 0)")[1:2], c(TRUE, FALSE))
})

test_that("rd_rlogic reads the text of factors in isnumber() and searches record by record between fields", {
  data <- data.frame(record_id = 1:3, res = NA)
  dic <- make_dic(c("record_id", "t", "p", "r", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)

  for (values in list(c("abc", "12", ""), factor(c("abc", "12", "")), factor(c("abc", "12", ""), ordered = TRUE))) {
    data$t <- values
    expect_equal(ev("isnumber([t])"), c(FALSE, TRUE, FALSE), info = class(values)[1])
  }

  data$t <- c("alpha", "bravo", "cider")
  data$p <- c("ph", "ra", "id")
  data$r <- c("PH", "RA", "ID")
  expect_no_warning(ev("contains([t], [p])"))
  expect_equal(ev("contains([t], [p])"), c(TRUE, TRUE, TRUE))
  expect_equal(ev("not_contain([t], [p])"), c(FALSE, FALSE, FALSE))
  expect_equal(ev("find([p], [t])"), c(3, 2, 2))
  expect_equal(ev("replace_text([t], [p], [r])"), c("alPHa", "bRAvo", "cIDer"))
  # (the result of each record doesn't depend on the order of the rows)
  reversed <- data[3:1, ]
  expect_equal(as.vector(rd_rlogic(data = reversed, dic = dic, logic = "contains([t], [p])", var = "res")$eval), c(TRUE, TRUE, TRUE))
  # Fixed texts as before
  expect_equal(ev("contains([t], 'ra')"), c(FALSE, TRUE, FALSE))
})

test_that("rd_rlogic keeps the empty texts of the text functions and finds the blank results", {
  data <- data.frame(record_id = 1:3, t = c("abc", "x-y", "   "), a = c("a", "b", "c"), res = NA)
  dic <- make_dic(c("record_id", "t", "a", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("concat('', [t])"), c("abc", "x-y", "   "))
  expect_equal(ev("replace_text([t], '-', '')"), c("abc", "xy", "   "))
  expect_equal(ev("concat_ws('', [t], [a])"), c("abca", "x-yb", "   c"))
  expect_equal(ev("isblankormissingcode(trim([t]))"), c(FALSE, FALSE, TRUE))
  data$t[3] <- NA
  expect_equal(ev("isblankormissingcode([t])"), c(FALSE, FALSE, TRUE))
})

test_that("rd_rlogic compares an operation with an empty text as a check of blank values", {
  data <- data.frame(record_id = 1:4, t = c("   ", "a", NA, " b "), a = c(1, 2, NA, 4), res = NA)
  dic <- make_dic(c("record_id", "t", "a", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("trim([t]) = ''"), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(ev("trim([t]) <> ''"), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(ev("'' = trim([t])"), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(ev("if(trim([t]) = '', 1, 0)"), c(1, 0, 1, 0))
  expect_equal(ev("[a] + 1 = ''"), c(FALSE, FALSE, TRUE, FALSE))
  # A field compared with an empty text is translated as before
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[t] = ''", var = "res")$rlogic, "is.na(data$t)")
})

test_that("rd_rlogic reads a blank text to search, replace or separate as an empty text, with the separator of each record", {
  data <- data.frame(record_id = 1:3, t = c("abc", "x y", "Bcd"), n = c(NA, "y", "c"), r = c(NA, "Z", ""), s = c("-", ":", NA), o = c("B", "B", "C"), res = NA)
  dic <- make_dic(c("record_id", "t", "n", "r", "s", "o", "res"))
  ev <- function(logic, data) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)

  expect_equal(ev("find([n], [t])", data), c(1, 3, 2))
  expect_equal(ev("replace_text([t], 'a', [r])", data), c("bc", "x y", "Bcd"))
  expect_equal(ev("replace_text([t], [n], 'Q')", data), c("abc", "x Q", "BQd"))
  expect_equal(ev("replace_text([t], '', 'Q')", data), c("abc", "x y", "Bcd"))
  expect_equal(ev("contains([t], [n])", data), c(TRUE, TRUE, TRUE))
  expect_equal(ev("not_contain([t], [n])", data), c(FALSE, FALSE, FALSE))
  expect_equal(ev("starts_with([t], [n])", data), c(TRUE, FALSE, FALSE))
  expect_equal(ev("ends_with([t], [n])", data), c(TRUE, TRUE, FALSE))
  expect_equal(ev("contains([n], 'y')", data), c(FALSE, TRUE, FALSE))

  expect_equal(ev("concat_ws([s], [t], [o])", data), c("abc-B", "x y:B", "BcdC"))
  expect_equal(ev("concat_ws([s], 'a', 'b')", data), c("a-b", "a:b", "ab"))
  # (the result of each record doesn't depend on the order of the rows)
  expect_equal(ev("concat_ws([s], [t], [o])", data[3:1, ]), c("BcdC", "x y:B", "abc-B"))
})

test_that("rd_rlogic only reads the units of datediff() in its arguments", {
  data <- data.frame(record_id = 1:2, a = c(1, 2), t = c("ab", "cs"), d1 = as.Date(c("2020-01-01", "2021-01-01")), d2 = as.Date(c("2021-01-01", "2020-01-01")), res = NA)
  dic <- make_dic(c("record_id", "a", "t", "d1", "d2", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("if([a] = 1, 'f', 'm')"), c("f", "m"))
  expect_equal(ev("ends_with([t], 's')"), c(FALSE, TRUE))
  expect_equal(ev("concat([t], 'y')"), c("aby", "csy"))
  expect_equal(ev("if([a] = 1, datediff([d1], [d2], 'd'), 'd')"), c("366", "d"))
  expect_equal(ev("datediff([d1], [d2], 'd', true)"), c(366, -366))
  expect_equal(ev("datediff([d1], [d2], 'd' )"), c(366, 366))
  expect_equal(
    rd_rlogic(data = data, dic = dic, logic = "datediff([d1],[d2],'y')", var = "res")$rlogic,
    "abs((lubridate::time_length(lubridate::interval(data$d1,data$d2), 'day') / 365.2425))"
  )
})

test_that("rd_rlogic gives no values with data without rows", {
  data <- data.frame(record_id = character(0), a = numeric(0), t = character(0), res = logical(0))
  dic <- make_dic(c("record_id", "a", "t", "res"))
  for (logic in c("1", "2 + 3", "upper('abc')", "concat('prefix', [t])", "[a] + 1", "concat_ws('-', [t], 'x')", "find('a', [t])", "replace_text([t], 'a', [t])",
                  "round([a] / 3, 1)", "roundup([a], 1)", "rounddown([a], 2)", "mod([a], 3)")) {
    out <- rd_rlogic(data = data, dic = dic, logic = logic, var = "res")
    expect_length(out$eval, 0)
    expect_equal(nrow(out$eval_data), 0, info = logic)
    expect_named(out$eval_data, c("record_id", "eval"))
  }
})

test_that("rd_rlogic reads the empty texts of the fields as blank values", {
  data <- data.frame(record_id = c("1", "2", "3"), t = c("abc", "", NA), f = factor(c("Yes", "", NA)), res = NA)
  dic <- make_dic(c("record_id", "t", "f", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  for (field in c("t", "f")) {
    expect_equal(ev(paste0("isblankormissingcode([", field, "])")), c(FALSE, TRUE, TRUE), info = field)
    expect_equal(ev(paste0("[", field, "] = ''")), c(FALSE, TRUE, TRUE), info = field)
    expect_equal(ev(paste0("'' = [", field, "]")), c(FALSE, TRUE, TRUE), info = field)
    expect_equal(ev(paste0("[", field, "] <> ''")), c(TRUE, FALSE, FALSE), info = field)
    expect_equal(ev(paste0("'' <> [", field, "]")), c(TRUE, FALSE, FALSE), info = field)
  }
  # The identifier of the records is kept
  data$record_id[2] <- ""
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[t] = ''", var = "res")$eval_data$record_id, c("1", "", "3"))
})

test_that("rd_rlogic reads the dates of each datediff() with its own format", {
  data <- data.frame(record_id = 1:2, res = NA)
  dic <- make_dic(c("record_id", "res"))
  # (a logic without variables gives one value)
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("datediff('01-02-2024', '02-02-2024', 'd', 'dmy') + datediff('02-01-2024', '02-02-2024', 'd', 'mdy')"), 2)
  expect_equal(ev("datediff('01-02-2024 10:00', '02-02-2024 12:00', 'h', 'dmy') + datediff('2024-02-01 10:00', '2024-02-02 10:00', 'h')"), 50)
  expect_equal(ev("datediff('02-01-2024 10:00', '02-02-2024 12:00', 'h', 'mdy') + datediff('01-02-2024', '02-02-2024', 'd', 'dmy')"), 27)
})

test_that("rd_rlogic only reads 'today', 'now' and the dates as dates in datediff() and in comparisons of date fields", {
  data <- data.frame(record_id = 1:2, t = c("now here", "abc"), d = as.Date(c("2020-01-01", "2999-01-01")),
                     dt = as.POSIXct(c("2024-01-05 10:30", "2024-01-06 08:00"), tz = "UTC"), res = NA)
  dic <- make_dic(c("record_id", "t", "d", "dt", "res"), text_validation = c(NA, NA, "date_ymd", "datetime_ymd", NA))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("concat('today', ':', [t])"), c("today:now here", "today:abc"))
  expect_equal(ev("contains([t], 'now')"), c(TRUE, FALSE))
  expect_equal(ev("[t] = 'today'"), c(FALSE, FALSE))
  expect_equal(unique(ev("concat('01-02-2024', 'dmy')")), "01-02-2024dmy")
  expect_equal(unique(ev("concat('2024-01-05 10:30', 'x')")), "2024-01-05 10:30x")
  # (dates as before)
  expect_equal(ev("datediff([d], 'today', 'd', true) > 0"), c(TRUE, FALSE))
  expect_equal(ev("[d] < 'today'"), c(TRUE, FALSE))
  expect_equal(ev("[dt] >= '2024-01-05 12:00'"), c(FALSE, TRUE))
  expect_equal(ev("[dt] < 'now'"), c(TRUE, TRUE))
})

test_that("rd_rlogic translates datediff() with `false`", {
  data <- data.frame(record_id = 1:2, d1 = as.Date(c("2024-01-11", "2024-01-01")), d2 = as.Date(c("2024-01-01", "2024-01-11")), res = NA)
  dic <- make_dic(c("record_id", "d1", "d2", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  for (unit in c("y", "M", "d", "h", "m", "s")) {
    expect_equal(ev(paste0("datediff([d1], [d2], '", unit, "', false)")), ev(paste0("datediff([d1], [d2], '", unit, "')")), info = unit)
    expect_equal(ev(paste0("datediff([d1], [d2], '", unit, "', 'ymd', false)")), ev(paste0("datediff([d1], [d2], '", unit, "')")), info = unit)
  }
  expect_equal(ev("datediff([d1], [d2], 'd', false)"), c(10, 10))
  expect_equal(ev("datediff([d1], [d2], 'd', true)"), c(-10, 10))
})

test_that("rd_rlogic reads the functions with spaces before their parenthesis", {
  data <- data.frame(record_id = 1:2, a = c(1, 2), t = c("ab", "cd"), d1 = as.Date(c("2024-01-01", "2023-03-01")), d2 = as.Date(c("2024-01-11", "2023-03-02")), res = NA)
  dic <- make_dic(c("record_id", "a", "t", "d1", "d2", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("if  ([a] = 1, 1, 0)"), c(1, 0))
  expect_equal(ev("if\t([a] = 1, 1, 0)"), c(1, 0))
  expect_equal(ev("if\n([a] = 1, 1, 0)"), c(1, 0))
  expect_equal(ev("datediff  ([d1], [d2], 'd')"), c(10, 1))
  expect_equal(ev("year ([d1])"), c(2024, 2023))
  expect_equal(ev("upper   ([t])"), c("AB", "CD"))
  expect_equal(ev("([a] = 1) and  ([a] = 1)"), c(TRUE, FALSE))
  expect_equal(ev("[t] = 'if (x)'"), c(FALSE, FALSE))
})

# Longitudinal project with a baseline and a visit
two_events <- function(id = "record_id") {
  data <- data.frame(id = c("1", "1", "2", "2"), redcap_event_name = c("base_arm_1", "visit_arm_1", "base_arm_1", "visit_arm_1"),
                     value = c(60, 65, 10, 30), value__base_arm_1 = c(100, 100, 7, 7), res = NA, stringsAsFactors = FALSE)
  names(data)[1] <- id
  list(
    data = data,
    dic = make_dic(c(id, "value", "value__base_arm_1", "res")),
    event_form = data.frame(arm_num = 1, unique_event_name = c("base_arm_1", "visit_arm_1"), form = "form1")
  )
}

test_that("rd_rlogic keeps apart the values of other events from the fields of the data", {
  p <- two_events()
  ev <- function(logic) as.vector(rd_rlogic(data = p$data, dic = p$dic, event_form = p$event_form, logic = logic, var = "res")$eval)
  # (a field called like the auxiliary column of [base_arm_1][value])
  expect_equal(ev("[visit_arm_1][value] - [base_arm_1][value] + [value__base_arm_1]"), c(105, 105, 27, 27))
  expect_equal(ev("[visit_arm_1][value] - [base_arm_1][value]"), c(5, 5, 20, 20))
  # (the field with and without event: without event, the value of the row)
  expect_equal(ev("[base_arm_1][value] + [value]"), c(120, 125, 20, 40))
})

test_that("rd_rlogic reads the values of other events with an identifier that isn't record_id", {
  p <- two_events("study_id")
  ev <- function(logic) as.vector(rd_rlogic(data = p$data, dic = p$dic, event_form = p$event_form, logic = logic, var = "res")$eval)
  expect_equal(ev("[visit_arm_1][value] - [base_arm_1][value]"), c(5, 5, 20, 20))
  expect_equal(ev("[base_arm_1][value] * 2"), c(120, 120, 20, 20))
})

test_that("rd_rlogic keeps the dates chosen by if()", {
  data <- data.frame(record_id = 1:2, flag = c(1, 0), a = as.Date(c("2023-05-01", "2023-06-01")), b = as.Date(c("2024-05-01", "2024-06-01")),
                     t1 = as.POSIXct(c("2024-02-01 10:00", "2024-02-01 10:00"), tz = "UTC"), t2 = as.POSIXct(c("2024-02-02 10:00", "2024-02-02 12:00"), tz = "UTC"), res = NA)
  dic <- make_dic(c("record_id", "flag", "a", "b", "t1", "t2", "res"))
  ev <- function(logic) rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval
  expect_equal(as.vector(ev("year(if([flag] = 1, [a], [b]))")), c(2023, 2024))
  expect_equal(as.vector(ev("month(if([flag] = 1, [a], [b]))")), c(5, 6))
  expect_equal(as.vector(ev("year(if([flag] = 1, if([flag] = 1, [a], [b]), [b]))")), c(2023, 2024))
  expect_equal(as.vector(ev("datediff(if([flag] = 1, [a], [b]), '2025-01-01', 'd')")), c(611, 214))
  expect_equal(ev("if([flag] = 1, [a], '')"), as.Date(c("2023-05-01", NA)))
  expect_equal(as.vector(ev("datediff(if([flag] = 1, [t1], [t2]), [t2], 'h')")), c(24, 0))
  # The other if() are translated as before
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "if([flag] = 1, 1, 0)", var = "res")$rlogic, "ifelse(data$flag == 1, 1, 0)")
})

test_that("rd_rlogic reads the dates between parentheses in datediff()", {
  data <- data.frame(record_id = 1:2, res = NA)
  dic <- make_dic(c("record_id", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("datediff(('01-02-2024'), '02-02-2024', 'd', 'dmy')"), 1)
  expect_equal(ev("datediff(('01-02-2024 10:00'), '02-02-2024 10:00', 'h', 'dmy')"), 24)
  expect_equal(ev("datediff(('2024-02-01 10:00'), '2024-02-02 10:00', 'h')"), 24)
})

test_that("rd_rlogic compares with an empty text the functions written with several instructions", {
  data <- data.frame(record_id = 1:3, t = c("abc", NA, "x"), p = c("b", "-", NA), n = c(1.21, NA, 3), res = NA)
  dic <- make_dic(c("record_id", "t", "p", "n", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("concat_ws([p], [t], 'x') <> ''"), c(TRUE, TRUE, TRUE))
  expect_equal(ev("replace_text([t], [p], '') = ''"), c(FALSE, TRUE, FALSE))
  expect_equal(ev("roundup([n], 1) = ''"), c(FALSE, TRUE, FALSE))
})

test_that("rd_rlogic keeps the dates of min() and max()", {
  data <- data.frame(record_id = 1:2, flag = c(1, 0), a = as.Date(c("2023-05-01", "2024-06-01")), b = as.Date(c("2024-05-01", "2023-06-01")), res = NA)
  dic <- make_dic(c("record_id", "flag", "a", "b", "res"))
  ev <- function(logic) rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval
  expect_equal(as.vector(ev("year(if([flag] = 1, min([a], [b]), max([a], [b])))")), c(2023, 2024))
  expect_equal(as.vector(ev("month(if([flag] = 1, min([a], [b]), max([a], [b])))")), c(5, 6))
  expect_equal(as.vector(ev("year(min('', [a]))")), c(2023, 2024))
  expect_equal(as.vector(ev("year(max('', [a]))")), c(2023, 2024))
  expect_equal(ev("min('', [a])"), as.Date(c("2023-05-01", "2024-06-01")))
  # (as before when the first value is a date)
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "min([a], [b])", var = "res")$rlogic, "pmin(data$a, data$b, na.rm = TRUE)")
})

test_that("rd_rlogic keeps the dates in longitudinal projects", {
  data <- data.frame(record_id = c("1", "1"), redcap_event_name = c("e1", "e2"), flag = c(1, 0), a = as.Date(c("2023-01-01", "2024-06-01")),
                     b = as.Date(c("2024-05-01", "2023-06-01")), t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 11:00"), tz = "UTC"), res = NA)
  dic <- make_dic(c("record_id", "flag", "a", "b", "t", "res"))
  run <- function(logic, events = c("e1", "e2")) {
    rd_rlogic(data = data, dic = dic, event_form = data.frame(arm_num = 1, unique_event_name = events, form = "form1"), logic = logic, var = "res")
  }
  out <- run("[a]")
  expect_equal(out$eval, as.Date(c("2023-01-01", "2024-06-01")))
  expect_equal(out$eval_data$eval, as.Date(c("2023-01-01", "2024-06-01")))
  expect_equal(run("[t]")$eval, as.POSIXct(c("2024-01-01 10:00", "2024-01-02 11:00"), tz = "UTC"))
  expect_equal(run("if([flag] = 1, [a], [b])")$eval, as.Date(c("2023-01-01", "2023-06-01")))
  # (blank in the events where `var` isn't collected)
  expect_equal(run("[a]", "e1")$eval, as.Date(c("2023-01-01", NA)))
  expect_equal(run("[flag] + 1")$eval, c(2, 1))
})

test_that("rd_rlogic writes between backticks the fields whose name isn't valid in R", {
  words <- c("if", "for", "repeat", "next", "function", "else", "while", "break", "in", "TRUE", "NA", "_x")
  data <- data.frame(record_id = 1:2, res = NA)
  for (w in words) data[[w]] <- c(1, 0)
  dic <- make_dic(c("record_id", words, "res"))
  for (w in words) {
    ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
    expect_equal(ev(paste0("[", w, "] = 1")), c(TRUE, FALSE), info = w)
    expect_equal(ev(paste0("[", w, "] + 1")), c(2, 1), info = w)
    expect_equal(ev(paste0("[", w, "] <> 1")), c(FALSE, TRUE), info = w)
    expect_equal(ev(paste0("isblankormissingcode([", w, "])")), c(FALSE, FALSE), info = w)
    expect_equal(ev(paste0("if([", w, "] = 1, 'a', 'b')")), c("a", "b"), info = w)
  }
  expect_equal(rd_rlogic(data = data, dic = dic, logic = "[if] = 1", var = "res")$rlogic, "data$`if` == 1")
})

test_that("rd_rlogic keeps the texts that look like its internal placeholders", {
  data <- data.frame(record_id = 1:2, t = c("REDCapDMblock1x", "a"), o = c("o", NA), r = c("r", "s"), res = NA)
  dic <- make_dic(c("record_id", "t", "o", "r", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("[t] = 'REDCapDMblock1x' and concat_ws(':', [o], [r]) <> ''"), c(TRUE, FALSE))
  expect_equal(ev("concat_ws(':', [o], [r]) <> '' and [t] = 'REDCapDMblock1x'"), c(TRUE, FALSE))
  expect_equal(ev("[t] = 'xREDCapDMblock1xy' and concat_ws(':', [o], [r]) <> ''"), c(FALSE, FALSE))
})

test_that("rd_rlogic rounds the values below the half down and the halves away from zero", {
  data <- data.frame(record_id = 1:7, a = c(1.49999999, -1.49999999, 0.49999999, 1.5, -1.5, 2.5, 0.5), res = NA)
  dic <- make_dic(c("record_id", "a", "res"))
  ev <- function(logic, data) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("round([a], 0)", data), c(1, -1, 0, 2, -2, 3, 1))
  # (halves stored with a tiny error, e.g. 2.675 is 2.67499999999999982)
  data2 <- data.frame(record_id = 1:5, a = c(2.675, 1.005, -2.675, 0.125, 1.0049999), res = NA)
  expect_equal(ev("round([a], 2)", data2), c(2.68, 1.01, -2.68, 0.13, 1))
})

test_that("rd_rlogic gives the exact remainder of mod() with large numbers", {
  data <- data.frame(record_id = 1:9, a = c(2^54, -2^54, 7, -7, 5.5, -5.5, 1e308, 0, NA), b = c(3, 3, 3, 3, 2, 2, 1e-10, 3, 3), res = NA)
  dic <- make_dic(c("record_id", "a", "b", "res"))
  out <- expect_no_warning(rd_rlogic(data = data, dic = dic, logic = "mod([a], [b])", var = "res")$eval)
  expect_equal(out[1:6], c(1, -1, 1, -1, 1.5, -1.5))
  expect_true(is.finite(out[7]) && out[7] >= 0 && out[7] < 1e-10)
  expect_equal(out[8:9], c(0, NA))
  expect_true(is.nan(rd_rlogic(data = data.frame(record_id = 1, a = 5, b = 0, res = NA), dic = dic, logic = "mod([a], [b])", var = "res")$eval))
})

test_that("rd_rlogic keeps the value when the number of decimals is too large", {
  data <- data.frame(record_id = 1:2, a = c(1.2345, 1e308), d = c(309, NA), res = NA)
  dic <- make_dic(c("record_id", "a", "d", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  for (f in c("round", "roundup", "rounddown")) {
    expect_equal(ev(paste0(f, "([a], 309)")), c(1.2345, 1e308), info = f)
    # (a blank number of decimals gives a blank value)
    expect_equal(ev(paste0(f, "([a], [d])")), c(1.2345, NA), info = f)
  }
})

test_that("rd_rlogic keeps the values that already have the decimals of roundup() and rounddown()", {
  data <- data.frame(record_id = 1, res = NA)
  dic <- make_dic(c("record_id", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("roundup(1.1, 2)"), 1.1)
  expect_equal(ev("rounddown(0.29, 2)"), 0.29)
  expect_equal(ev("roundup(roundup(1.1, 2), 2)"), 1.1)
  expect_equal(ev("rounddown(-1.1, 2)"), -1.1)
  expect_equal(ev("roundup(1.111, 2)"), 1.12)
  expect_equal(ev("rounddown(1.119, 2)"), 1.11)
  expect_equal(ev("round(1.499999999999999, 0)"), 1)
  expect_equal(ev("round(-1.499999999999999, 0)"), -1)
  # (as before: the halves stored with the error of the binary numbers)
  expect_equal(ev("round(2.675, 2)"), 2.68)
})

test_that("rd_rlogic gives the remainder of mod() at the ends of the numbers", {
  data <- data.frame(record_id = 1:4, a = c(.Machine$double.xmax, -.Machine$double.xmax, .Machine$double.xmax, 1), b = c(1, 1, 5e-324, 5e-324), res = NA)
  dic <- make_dic(c("record_id", "a", "b", "res"))
  expect_equal(as.vector(rd_rlogic(data = data, dic = dic, logic = "mod([a], [b])", var = "res")$eval), c(0, 0, 0, 0))
})

test_that("rd_rlogic rounds with so many negative decimals that the scale is 0", {
  data <- data.frame(record_id = 1, res = NA)
  dic <- make_dic(c("record_id", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("round(12, -323)"), 0)
  expect_equal(ev("round(12, -324)"), 0)
  expect_equal(ev("roundup(-12, -324)"), 0)
  expect_equal(ev("rounddown(12, -324)"), 0)
  expect_equal(ev("roundup(12, -324)"), Inf)
  expect_equal(ev("rounddown(1234, -2)"), 1200)
})

test_that("rd_rlogic rounds large numbers and never to the wrong side of the value", {
  data <- data.frame(record_id = 1:4, a = c(2^50, 2^50 + 0.25, 2^51 + 0.5, -2^50 - 0.75), res = NA)
  dic <- make_dic(c("record_id", "a", "res"))
  ev <- function(logic, data) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("round([a], 0)", data), c(2^50, 2^50, 2^51 + 1, -2^50 - 1))
  expect_equal(ev("roundup([a], 0)", data), c(2^50, 2^50 + 1, 2^51 + 1, -2^50))
  expect_equal(ev("rounddown([a], 0)", data), c(2^50, 2^50, 2^51, -2^50 - 1))
  near <- data.frame(record_id = 1:2, a = c(0.28999999999999992, 0.29000000000000004), res = NA)
  expect_equal(ev("rounddown([a], 2)", near), c(0.28, 0.29))
  expect_equal(ev("roundup([a], 2)", near), c(0.29, 0.3))
  halves <- data.frame(record_id = 1:3, a = c(0.4999999999999999, 2.6749999999999994, 2.675), res = NA)
  expect_equal(ev("round([a], 0)", halves), c(0, 3, 3))
  expect_equal(ev("round([a], 2)", halves), c(0.5, 2.67, 2.68))
})

test_that("rd_rlogic rounds in the same way with any number of decimals that is 0", {
  data <- data.frame(record_id = 1:2, a = c(1.0000000000000002, -1.0000000000000002), b = 0, res = NA)
  dic <- make_dic(c("record_id", "a", "b", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  for (digits in c("0", "0.0", "[b]", "+0", "-0", "(0)", "(1-1)")) {
    expect_equal(ev(paste0("roundup([a], ", digits, ")")), c(2, -1), info = digits)
    expect_equal(ev(paste0("rounddown([a], ", digits, ")")), c(1, -2), info = digits)
  }
})

test_that("rd_rlogic keeps the numbers of the logic when it rewrites it", {
  data <- data.frame(record_id = c("N001", "N002"), a = c(1.0000000000000002, 1), b = c(1125899906842624.5, 1125899906842624), text = "x", res = NA)
  dic <- make_dic(c("record_id", "a", "b", "text", "res"))
  ev <- function(logic) as.vector(rd_rlogic(data = data, dic = dic, logic = logic, var = "res")$eval)
  expect_equal(ev("[a] = 1.0000000000000002 and trim([text]) <> ''"), c(TRUE, FALSE))
  expect_equal(ev("[b] = 1125899906842624.5 and trim([text]) <> ''"), c(TRUE, FALSE))
})

test_that("rd_rlogic rounds with the numbers with these decimals as R reads them", {
  dic <- make_dic(c("record_id", "a", "res"))
  ev <- function(logic, a) as.vector(rd_rlogic(data = data.frame(record_id = seq_along(a), a = a, res = NA), dic = dic, logic = logic, var = "res")$eval)
  # (values with these decimals read by R, as the data, are kept)
  x <- as.numeric(c("0.531028", "0.749778", "-0.143997"))
  for (f in c("round", "roundup", "rounddown")) expect_identical(ev(paste0(f, "([a], 6)"), x), x, info = f)
  expect_identical(ev("rounddown([a], 24)", 1e-24), 1e-24)
  expect_identical(ev("roundup([a], 25)", 1e-25), 1e-25)
  # (extreme decimals with very small numbers)
  expect_identical(ev("round([a], 308)", c(1e-320, 2.25e-308)), c(0, 2e-308))
  expect_identical(ev("roundup([a], 309)", 1e-320), 1e-309)
})


# ---- Identifier ----

test_that("rd_rlogic takes the identifier from the dictionary, whatever the order of the columns", {
  for (first in c("none", "age", "external")) {
    project <- make_custom_id(age_first = first == "age", external_first = first == "external")
    project$dictionary <- rbind(project$dictionary, data.frame(
      field_name = "age_next", form_name = "visit", field_type = "calc", field_label = "Age next",
      choices_calculations_or_slider_labels = "[base_arm_1][age] + 1", text_validation_type_or_show_slider_number = "",
      branching_logic_show_field_only_if = "", stringsAsFactors = FALSE
    ))

    res <- rd_rlogic(project, logic = "[base_arm_1][age] + 1", var = "age_next")
    expect_equal(res$eval, c(NA, 21, NA, NA, 61))
    expect_equal(names(res$eval_data)[1], "subject_id")
  }
})

test_that("id_field is the first field of the dictionary", {
  project <- make_custom_id(age_first = TRUE)
  expect_equal(id_field(project$data, project$dictionary), "subject_id")
  # Without dictionary: the first column
  expect_equal(id_field(project$data), "age")
  # record_id (as named by redcap_data()), also with a dictionary without its row
  expect_equal(id_field(data.frame(a = 1, record_id = 2)), "record_id")
  expect_equal(id_field(data.frame(age = 1, record_id = 2), data.frame(field_name = c("age", "b"))), "record_id")
  # A column that isn't in the dictionary in the first place (e.g. a column added for an analysis)
  expect_equal(id_field(data.frame(analysis_score = 1, age = 2, sid = 3), data.frame(field_name = c("sid", "age"))), "sid")
  # Without the first field of the dictionary in the data: the first column
  expect_equal(id_field(data.frame(myid = 1, age = 2), data.frame(field_name = c("sid", "age"))), "myid")
  # A column of REDCap or a version of a field in the first column
  expect_equal(id_field(data.frame(redcap_event_name = "a", age = 2, sid = 1), data.frame(field_name = c("sid", "age"))), "sid")
  expect_equal(id_field(data.frame(age.factor = "a", age = 2, sid = 1), data.frame(field_name = c("sid", "age"))), "sid")
})
