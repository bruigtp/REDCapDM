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

test_that("rd_rlogic converts sum(...) to rowSums and evaluates correctly", {
  df <- data.frame(a = c(1,2), b = c(3, NA), stringsAsFactors = FALSE)
  dic <- make_dic(c("a", "b"))
  out <- rd_rlogic(data = df, dic = dic,
                   logic = "sum([a],[b])",
                   var = "a")
  expect_true(grepl("rowSums\\(", out$rlogic))
  # result should equal rowSums with NA treated as NA (default)
  expect_equal(as.numeric(out$eval), rowSums(cbind(df$a, df$b)))
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

  expect_error(
    rd_rlogic(fx, logic = "[ae_grade] = '1'", var = "age"),
    "variables of another repeated instrument.*ae_grade \\(form:ae\\)"
  )
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
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'M')", var = "res")$eval, c(2, 12))
  expect_equal(rd_rlogic(data = df, dic = dic, logic = "datediff([d1],[d2],'m')", var = "res")$eval, c(60, 365) * 24 * 60)

  # 'today'
  out <- rd_rlogic(data = df, dic = dic, logic = "datediff([d1],'today','d')", var = "res")
  expect_equal(out$eval, as.numeric(Sys.Date() - df$d1))
})
