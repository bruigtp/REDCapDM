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
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE))

  expect_equal(as.character(out$data$sex.factor[3]), "UNK")
})

test_that("rm_mdc = FALSE assigns a placeholder numeric code (not a real category code) to the raw column", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE))

  # sex's real codes are 1/2, so the placeholder for the MDC should be >= 3,
  # never collide with an existing, dictionary-defined category code.
  expect_true(as.numeric(out$data$sex[3]) >= 3)
})

test_that("rm_mdc = FALSE keeps the Missing Data Codes of validated text fields in a `_mdc` column", {
  expect_warning(
    out <- run_redcap_data_mdc(rm_mdc = FALSE),
    "Missing Data Codes found in validated fields \\(`lab_value`\\)\\. They are set to NA and kept in the new column `lab_value_mdc`\\."
  )

  # the field keeps its type, with NA where there was a code
  expect_equal(out$data$lab_value, c(5.2, NA, 7.1), ignore_attr = TRUE)
  expect_equal(out$data$lab_value_mdc, c(NA, "ASKU", NA), ignore_attr = TRUE)
  # right after the field, with its label
  expect_equal(which(names(out$data) == "lab_value_mdc"), which(names(out$data) == "lab_value") + 1)
  expect_equal(attr(out$data$lab_value_mdc, "label"), "Lab value (mmol/L) (Missing Data Code)")
  # not added to the dictionary
  expect_false("lab_value_mdc" %in% out$dictionary$field_name)
})

test_that("rm_mdc = FALSE doesn't add `_mdc` columns when there are no codes", {
  fx <- make_mdc_fixture()
  fx$data$lab_value <- c("5.2", "6.0", "7.1")

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE, fx = fx))
  expect_false(any(grepl("_mdc$", names(out$data))))
  expect_true(is.numeric(out$data$lab_value))
})

test_that("rm_mdc = FALSE doesn't overwrite an existing `_mdc` column", {
  fx <- make_mdc_fixture()
  fx$data$lab_value_mdc <- c("a", "b", "c")

  expect_warning(
    out <- run_redcap_data_mdc(rm_mdc = FALSE, fx = fx),
    "already has a `lab_value_mdc` column"
  )
  expect_equal(out$data$lab_value_mdc, c("a", "b", "c"), ignore_attr = TRUE)
  expect_true(is.na(out$data$lab_value[2]))
})

test_that("rm_mdc = FALSE drops a checkbox Missing Data Code column only when nobody ever selected it", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE))

  # chk___NI is 0 for every record in the fixture -> must be dropped.
  expect_false("chk___NI" %in% names(out$data))
})

test_that("rm_mdc = FALSE keeps a checkbox Missing Data Code column when at least one record selected it", {
  fx <- make_mdc_fixture()
  fx$data$chk___NI <- c(1, 0, 0)  # record 1 *did* select "Not Indicated"

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE, fx = fx))

  expect_true("chk___NI" %in% names(out$data))
})

test_that("HTML tags and entities are cleaned from dictionary field_label", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  lbl <- out$dictionary$field_label[out$dictionary$field_name == "lab_value"]
  expect_equal(lbl, "Lab value (mmol/L)")
})



# ---- Column types ----

test_that("redcap_data keeps numbers and multiple-choice codes numeric after removing Missing Data Codes", {
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE))

  expect_equal(out$data$lab_value, c(5.2, NA, 7.1), ignore_attr = TRUE)
  expect_equal(out$data$sex, c(1L, 2L, NA), ignore_attr = TRUE)

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = FALSE))
  expect_true(is.numeric(out$data$lab_value))
  expect_true(is.numeric(out$data$sex))
  expect_true(out$data$sex[3] >= 3)
})

test_that("redcap_data doesn't turn numeric columns into text when there are no Missing Data Codes", {
  fx <- make_mdc_fixture()
  fx$data <- data.frame(record_id = 1:3, sex = c(1L, 2L, 1L), lab_value = c(78, 102, 9.5),
                        chk___1 = c(1, 0, 1), chk___NI = c(0, 0, 0))
  fx$data$sex.factor <- factor(c("Male", "Female", "Male"), levels = c("Male", "Female"))
  fx$data$chk___NI.factor <- factor(rep("Unchecked", 3), levels = c("Unchecked", "Checked"))

  for (rm_mdc in c(TRUE, FALSE)) {
    # the empty checkbox option named after a code (chk___NI) is removed with a warning
    out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = rm_mdc, fx = fx))
    expect_true(is.numeric(out$data$lab_value))
    expect_true(is.numeric(out$data$sex))
    # a text comparison would give 2 ("78" > "100" and "9.5" > "100")
    expect_equal(sum(out$data$lab_value > 100), 1)
  }
})

test_that("redcap_data keeps as text the validated fields that aren't numbers", {
  fx <- make_mdc_fixture()
  fx$data$phone <- c("0612345678", "UNK", "0698765432")
  fx$dic <- rbind(fx$dic, data.frame(field_name = "phone", form_name = "form1", field_type = "text",
                                     text_validation_type_or_show_slider_number = "phone",
                                     field_label = "Phone", stringsAsFactors = FALSE))

  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE, fx = fx))
  expect_equal(out$data$phone, c("0612345678", NA, "0698765432"), ignore_attr = TRUE)
})

test_that("rm_mdc = TRUE works with a field called `missing_codes`", {
  fx <- make_mdc_fixture()
  names(fx$data)[names(fx$data) == "lab_value"] <- "missing_codes"
  fx$dic$field_name[fx$dic$field_name == "lab_value"] <- "missing_codes"
  out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = TRUE, fx = fx))
  expect_equal(out$data$missing_codes, c(5.2, NA, 7.1), ignore_attr = TRUE)
  expect_true(is.na(out$data$sex[3]))
})

# Fixture with codes of the project: -999 in a numeric field, -888 and UNK in a radio field and a checkbox column for -999
make_custom_mdc_fixture <- function() {
  fx <- make_mdc_fixture()
  fx$data$score <- c(10, -999, 12)
  fx$data$sex <- c("1", "-888", "UNK")
  fx$data$sex.factor <- factor(c("Male", NA, NA), levels = c("Male", "Female"))
  fx$data$chk____999 <- c(0, 0, 0)
  fx$data$chk____999.factor <- factor(rep("Unchecked", 3), levels = c("Unchecked", "Checked"))
  fx$dic <- rbind(fx$dic, data.frame(field_name = "score", form_name = "form1", field_type = "text", text_validation_type_or_show_slider_number = "integer", field_label = "Score"))
  fx
}
run_custom_mdc <- function(...) {
  fx <- make_custom_mdc_fixture()
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export_mdc(fx$data, tmp_r)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(fx$dic, tmp_dic, row.names = FALSE, quote = FALSE)
  suppressWarnings(redcap_data(data_path = tmp_r, dic_path = tmp_dic, ...))
}

test_that("redcap_data removes the Missing Data Codes given in `mdc` besides the default ones, also numeric codes in numeric fields", {
  # Without `mdc`, only the default codes
  out <- run_custom_mdc(rm_mdc = TRUE)
  expect_equal(out$data$score, c(10, -999, 12), ignore_attr = TRUE)
  expect_equal(out$data$sex, c(1, -888, NA), ignore_attr = TRUE)
  expect_true("chk____999" %in% names(out$data))

  out <- run_custom_mdc(rm_mdc = TRUE, mdc = c("-999", "-888"))
  expect_equal(out$data$score, c(10, NA, 12), ignore_attr = TRUE)
  expect_equal(out$data$sex, c(1, NA, NA), ignore_attr = TRUE)
  expect_equal(as.character(out$data$sex.factor), c("Male", NA, NA))
  expect_false(any(c("chk____999", "chk____999.factor") %in% names(out$data)))

  # Numeric codes
  expect_equal(run_custom_mdc(rm_mdc = TRUE, mdc = c(-999, -888))$data$score, c(10, NA, 12), ignore_attr = TRUE)
})

test_that("redcap_data keeps the Missing Data Codes given in `mdc` with rm_mdc = FALSE, with their labels in the .factor version", {
  out <- run_custom_mdc(rm_mdc = FALSE, mdc = c(-999, "-888" = "Not applicable"))
  expect_equal(out$data$score, c(10, NA, 12), ignore_attr = TRUE)
  expect_equal(out$data$score_mdc, c(NA, "-999", NA), ignore_attr = TRUE)
  # The label given in `mdc`, or else the code
  expect_equal(as.character(out$data$sex.factor), c("Male", "Not applicable", "UNK"))
})

test_that("redcap_data explains a wrong `mdc`", {
  for (wrong in list(NA, "", c("-999", NA), character(0), TRUE, list("-999"))) {
    expect_error(redcap_data(data_path = "project.r", dic_path = "dictionary.csv", mdc = wrong), class = "REDCapDM_error_argument")
  }
})

test_that("the Missing Data Codes of a project are read as the API returns them", {
  expect_equal(parse_mdc_codes("-999, Not applicable | -888, Unknown"), c("-999", "-888"))
  expect_equal(parse_mdc_codes("NI, No information\r\nUNK, Unknown, not known\n"), c("NI", "UNK"))
  expect_equal(parse_mdc_codes(NA), character(0))
  expect_equal(parse_mdc_codes(NULL), character(0))
  expect_equal(parse_mdc_codes(""), character(0))
  expect_equal(mdc_table(c(1e5, -999))$code, c("100000", "-999"))
  expect_equal(mdc_table(c("-999" = "Not applicable", "-888"))$label, c("Not applicable", NA))
})

test_that("rm_mdc = FALSE never gives the Missing Data Codes the code of an option that nobody chose", {
  fx <- make_mdc_fixture()
  fx$data$sex <- c("1", "1", "UNK")
  fx$data$sex.factor <- factor(c("Male", "Male", NA), levels = c("Male", "Female"))
  fx$data$smoker <- c("0", "NI", "0")
  fx$data$smoker.factor <- factor(c("No", NA, "No"), levels = c("Yes", "No"))
  fx$dic$choices_calculations_or_slider_labels <- c(NA, "1, Male | 2, Female", NA, "1, One | NI, NI")
  fx$dic <- rbind(fx$dic, data.frame(field_name = "smoker", form_name = "form1", field_type = "yesno", text_validation_type_or_show_slider_number = NA,
                                     field_label = "Smoker", choices_calculations_or_slider_labels = NA))
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export_mdc(fx$data, tmp_r)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(fx$dic, tmp_dic, row.names = FALSE)
  out <- suppressWarnings(redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = FALSE))

  # 2 is "Female" and 1 is "Yes", although nobody chose them
  expect_equal(out$data$sex, c(1, 1, 3), ignore_attr = TRUE)
  expect_equal(out$data$smoker, c(0, 2, 0), ignore_attr = TRUE)
  expect_equal(as.character(out$data$sex.factor), c("Male", "Male", "UNK"))
  expect_equal(as.character(out$data$smoker.factor), c("No", "NI", "No"))
})

test_that("rm_mdc = FALSE and `mdc` work in a project without records with multiple-choice fields", {
  fx <- make_mdc_fixture()
  fx$data <- fx$data[0, ]
  for (rm in c(TRUE, FALSE)) {
    out <- suppressWarnings(run_redcap_data_mdc(rm_mdc = rm, fx = fx))
    expect_equal(nrow(out$data), 0)
    expect_true("sex.factor" %in% names(out$data))
  }
})

test_that("redcap_data recognizes the numeric Missing Data Codes written as text in number fields", {
  fx <- make_mdc_fixture()
  fx$data <- rbind(fx$data, fx$data[1:2, ])
  fx$data$record_id <- 1:5
  fx$data$lab_value <- c("5.2", "ASKU", "-999.0", "-9.99e2", "7.1")
  run <- function(rm_mdc) {
    tmp_r <- tempfile(fileext = ".R")
    write_redcap_r_export_mdc(fx$data, tmp_r)
    tmp_dic <- tempfile(fileext = ".csv")
    write.csv(fx$dic, tmp_dic, row.names = FALSE, quote = FALSE)
    suppressMessages(redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = rm_mdc, mdc = -999))
  }
  expect_warning(out <- run(TRUE), "have been removed", class = "REDCapDM_warning")
  expect_equal(out$data$lab_value, c(5.2, NA, NA, NA, 7.1), ignore_attr = TRUE)

  expect_warning(out <- run(FALSE), "lab_value_mdc", class = "REDCapDM_warning")
  expect_equal(out$data$lab_value, c(5.2, NA, NA, NA, 7.1), ignore_attr = TRUE)
  expect_equal(out$data$lab_value_mdc, c(NA, "ASKU", "-999", "-999", NA), ignore_attr = TRUE)
})
