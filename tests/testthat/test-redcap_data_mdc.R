# Same helper as test-redcap_data.R, duplicated locally so this file is self-contained.
write_redcap_r_export_mdc <- function(data, path) {
  con <- file(path, "w")
  writeLines("## REDCap R export", con)
  writeLines("## begin data", con)
  writeLines("data <- ", con)
  dput(data, con)
  close(con)
}

# A minimal "realistic" project: a radio field and a validated text field that
# each have one record using a built-in REDCap Missing Data Code instead of a
# normal value, plus a checkbox with one option ("NI") nobody ever selected.
make_mdc_fixture <- function() {
  sample_data <- data.frame(
    record_id = 1:3,
    sex = c("1", "2", "UNK"),
    lab_value = c("5.2", "ASKU", "7.1"),
    chk___1 = c(1, 0, 1),
    chk___NI = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  # .factor columns as REDCap's own export script would generate them: NA
  # whenever the raw value isn't one of the declared factor levels (which is
  # exactly what happens when the raw value is a Missing Data Code string).
  sample_data$sex.factor <- factor(c("Male", "Female", NA), levels = c("Male", "Female"))
  sample_data$chk___NI.factor <- factor(rep("Unchecked", 3), levels = c("Unchecked", "Checked"))

  dic_rows <- data.frame(
    field_name = c("record_id", "sex", "lab_value", "chk"),
    form_name = "form1",
    field_type = c("text", "radio", "text", "checkbox"),
    text_validation_type_or_show_slider_number = c(NA, NA, "number", NA),
    field_label = c("Record ID", "Sex", "<b>Lab</b> value&nbsp;(mmol/L)", "Checklist"),
    stringsAsFactors = FALSE
  )

  list(data = sample_data, dic = dic_rows)
}

run_redcap_data_mdc <- function(rm_mdc, fx = make_mdc_fixture()) {
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export_mdc(fx$data, tmp_r)

  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(fx$dic, tmp_dic, row.names = FALSE, quote = FALSE)

  redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = rm_mdc)
}

test_that("rm_mdc = TRUE replaces Missing Data Codes with NA in text/categorical fields", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  expect_true(is.na(out$data$sex[3]))
  expect_true(is.na(out$data$lab_value[2]))
})

test_that("rm_mdc = TRUE preserves the factor class and levels of .factor columns", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  expect_true(is.factor(out$data$sex.factor))
  expect_equal(levels(out$data$sex.factor), c("Male", "Female"))
  expect_true(is.na(out$data$sex.factor[3]))
})

test_that("rm_mdc = TRUE drops checkbox columns named after a Missing Data Code", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  expect_false("chk___NI" %in% names(out$data))
  expect_false("chk___NI.factor" %in% names(out$data))
  # the real checkbox option must survive
  expect_true("chk___1" %in% names(out$data))
})

test_that("rm_mdc = TRUE warns when the dataset actually contains Missing Data Codes", {
  expect_warning(
    run_redcap_data_mdc(rm_mdc = TRUE),
    "Missing Data Codes"
  )
})

test_that("rm_mdc = TRUE does NOT warn when the dataset has no Missing Data Codes at all", {
  # Deliberately has no checkbox field at all: the checkbox-MDC columns are
  # dropped purely by NAME match (any ___<code> column, regardless of its
  # values), so a fixture that still has a chk___NI column -- even one that
  # was never selected -- would always trigger the warning on its own.
  clean_data <- data.frame(
    record_id = 1:3,
    sex = c("1", "2", "1"),
    lab_value = c("5.2", "6.0", "7.1"),
    stringsAsFactors = FALSE
  )
  clean_data$sex.factor <- factor(c("Male", "Female", "Male"), levels = c("Male", "Female"))

  dic_rows <- data.frame(
    field_name = c("record_id", "sex", "lab_value"),
    form_name = "form1",
    field_type = c("text", "radio", "text"),
    text_validation_type_or_show_slider_number = c(NA, NA, "number"),
    field_label = c("Record ID", "Sex", "Lab value"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    run_redcap_data_mdc(rm_mdc = TRUE, fx = list(data = clean_data, dic = dic_rows)),
    regexp = NA
  )
})

test_that("rm_mdc = TRUE does not crash when the dictionary has no validation column at all", {
  fx <- make_mdc_fixture()
  fx$dic$text_validation_type_or_show_slider_number <- NULL

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE, fx = fx))

  # lab_value is no longer recognized as a validated text field without that
  # column, so it is left untouched -- but the call must not error out.
  expect_equal(out$data$lab_value[2], "ASKU")
})

test_that("redcap_data() does not crash when the dictionary has no field_label column at all", {
  fx <- make_mdc_fixture()
  fx$dic$field_label <- NULL

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE, fx = fx))

  expect_false("field_label" %in% names(out$dictionary))
})

test_that("rm_mdc = FALSE keeps the Missing Data Code label in the .factor column", {
  out <- run_redcap_data_mdc(rm_mdc = FALSE)

  expect_equal(as.character(out$data$sex.factor[3]), "UNK")
})

test_that("rm_mdc = FALSE assigns a placeholder numeric code (not a real category code) to the raw column", {
  out <- run_redcap_data_mdc(rm_mdc = FALSE)

  # sex's real codes are 1/2, so the placeholder for the MDC should be >= 3,
  # never collide with an existing, dictionary-defined category code.
  expect_true(as.numeric(out$data$sex[3]) >= 3)
})

test_that("rm_mdc = FALSE still wipes Missing Data Codes to NA in validated text fields (no preservation)", {
  # This documents a likely inconsistency: categorical fields keep the MDC
  # readable via .factor, but validated text fields get no equivalent
  # treatment under rm_mdc = FALSE -- they're wiped exactly like rm_mdc = TRUE.
  out <- run_redcap_data_mdc(rm_mdc = FALSE)

  expect_true(is.na(out$data$lab_value[2]))
})

test_that("rm_mdc = FALSE drops a checkbox Missing Data Code column only when nobody ever selected it", {
  out <- run_redcap_data_mdc(rm_mdc = FALSE)

  # chk___NI is 0 for every record in the fixture -> must be dropped.
  expect_false("chk___NI" %in% names(out$data))
})

test_that("rm_mdc = FALSE keeps a checkbox Missing Data Code column when at least one record selected it", {
  fx <- make_mdc_fixture()
  fx$data$chk___NI <- c(1, 0, 0)  # record 1 *did* select "Not Indicated"

  out <- run_redcap_data_mdc(rm_mdc = FALSE, fx = fx)

  expect_true("chk___NI" %in% names(out$data))
})

test_that("HTML tags and entities are cleaned from dictionary field_label", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  lbl <- out$dictionary$field_label[out$dictionary$field_name == "lab_value"]
  expect_equal(lbl, "Lab value (mmol/L)")
})
