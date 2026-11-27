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
  fx$data$sex <- c("1", "2", "1")
  fx$data$sex.factor <- factor(
    c("Male", "Female", "Male"),
    levels = c("Male", "Female")
  )

  out <- suppressWarnings(
    run_redcap_data_mdc(rm_mdc = FALSE, fx = fx)
  )

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
  # (no warning: the codes of the project were given in `mdc`)
  expect_no_warning(out <- run(TRUE))
  expect_equal(out$data$lab_value, c(5.2, NA, NA, NA, 7.1), ignore_attr = TRUE)

  expect_warning(out <- run(FALSE), "lab_value_mdc", class = "REDCapDM_warning")
  expect_equal(out$data$lab_value, c(5.2, NA, NA, NA, 7.1), ignore_attr = TRUE)
  expect_equal(out$data$lab_value_mdc, c(NA, "ASKU", "-999", "-999", NA), ignore_attr = TRUE)
})

test_that("redcap_data only warns that the Missing Data Codes were removed when the data has them and `mdc` is not given", {
  warnings_of <- function(fx, ...) {
    tmp_r <- tempfile(fileext = ".R")
    write_redcap_r_export_mdc(fx$data, tmp_r)
    tmp_dic <- tempfile(fileext = ".csv")
    write.csv(fx$dic, tmp_dic, row.names = FALSE, quote = FALSE)
    w <- character(0)
    withCallingHandlers(
      suppressMessages(redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = TRUE, ...)),
      warning = function(x) {
        w <<- c(w, conditionMessage(x))
        invokeRestart("muffleWarning")
      }
    )
    any(grepl("have been removed", w))
  }
  fx <- make_mdc_fixture()
  # Codes in the data and no `mdc`
  expect_true(warnings_of(fx))
  # With the codes of the project in `mdc`
  expect_false(warnings_of(fx, mdc = c("-999")))
  # Without codes in the data (the empty checkbox column of a code is not a code)
  fx$data$sex <- c("1", "2", "1")
  fx$data$sex.factor <- factor(c("Male", "Female", "Male"), levels = c("Male", "Female"))
  fx$data$lab_value <- c("5.2", "6.1", "7.1")
  expect_false(warnings_of(fx))
  # A checked option of a code in a checkbox
  fx$data$chk___NI <- c(0, 1, 0)
  expect_true(warnings_of(fx))
})


# ---- The text "NA" is a value of a text field or the Missing Data Code NA ----

na_literal_dic <- function() {
  dic <- audit_dic(c("record_id", "notes", "n", "s"), c("text", "notes", "text", "radio"), choices = c("", "", "", "1, A | 2, B"))
  dic$text_validation_type_or_show_slider_number[3] <- "integer"
  dic
}
na_literal_rows <- function() {
  data.frame(record_id = c("1", "2", "3"), notes = c("NA", "hello", NA), n = c("5", "NA", "7"), s = c("1", "NA", "2"))
}
expect_na_literal <- function(out, rm_mdc) {
  expect_identical(as.vector(out$data$notes), c("NA", "hello", NA))
  expect_equal(as.vector(out$data$n), c(5, NA, 7))
  if (rm_mdc) {
    expect_false(any(c("n_mdc", "s_mdc") %in% names(out$data)))
    expect_equal(as.vector(out$data$s), c(1, NA, 2))
  } else {
    expect_identical(as.vector(out$data$n_mdc), c(NA, "NA", NA))
    expect_identical(as.vector(out$data$s_mdc), c(NA, "NA", NA))
    expect_identical(as.character(out$data$s.factor), c("A", "NA", "B"))
  }
}

test_that("redcap_data keeps the text 'NA' of the exported files (read.csv() reads it as a missing value)", {
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  write.csv(na_literal_rows(), file.path(folder, "P_DATA.csv"), row.names = FALSE, na = "")
  write.csv(na_literal_dic(), file.path(folder, "DIC.csv"), row.names = FALSE)
  writeLines(c("## REDCap R export", "## begin data", "data=read.csv('P_DATA.csv',stringsAsFactors=FALSE)",
               "data$s.factor=factor(data$s,levels=c('1','2'),labels=c('A','B'))"), file.path(folder, "P_R.r"))
  for (rm in c(TRUE, FALSE)) {
    out <- suppressWarnings(redcap_data(data_path = file.path(folder, "P_R.r"), dic_path = file.path(folder, "DIC.csv"), rm_mdc = rm))
    expect_na_literal(out, rm)
  }
})

test_that("redcap_data asks the API for the text 'NA' as a value (only the empty values are missing values)", {
  skip_if_not_installed("REDCapR", "1.2.0")
  skip_if_not_installed("mockery")
  dic <- na_literal_dic()
  na_args <- list()
  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", na = c("", "NA"), ...) {
    na_args[[raw_or_label]] <<- na
    rows <- na_literal_rows()
    # (as readr::read_csv() with `na`)
    rows[] <- lapply(rows, function(x) replace(x, x %in% na, NA))
    if (raw_or_label == "label") {
      names(rows) <- c("Record ID", "notes", "n", "s")
      rows$s <- c("A", "NA", "B")[match(rows$s, c("1", "NA", "2"))]
    }
    list(data = rows)
  }
  f <- redcap_data
  mockery::stub(f, "REDCapR::redcap_read", fake_read)
  mockery::stub(f, "REDCapR::redcap_metadata_read", function(...) list(data = dic))
  mockery::stub(f, "REDCapR::redcap_project_info_read", function(...) list(data = data.frame(missing_data_codes = NA)))
  for (rm in c(TRUE, FALSE)) {
    out <- suppressWarnings(suppressMessages(f(uri = "https://api.example", token = "tok", rm_mdc = rm)))
    expect_identical(na_args, list(label = "", raw = ""))
    expect_na_literal(out, rm)
  }
})

test_that("redcap_data keeps the texts 'NA' of the API also in text fields that look like numbers or logical values", {
  skip_if_not_installed("REDCapR", "1.2.0")
  skip_if_not_installed("mockery")
  dic <- audit_dic(c("record_id", "notes"), c("text", "notes"))
  read <- function(ids, notes, sid = NULL, ...) {
    fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", na = c("", "NA"), ...) {
      rows <- data.frame(record_id = ids, notes = notes)
      if (!is.null(sid)) rows$redcap_survey_identifier <- sid
      rows[] <- lapply(rows, function(x) replace(x, x %in% na, NA))
      if (raw_or_label == "label") names(rows)[1] <- "Record ID"
      list(data = rows)
    }
    f <- redcap_data
    mockery::stub(f, "REDCapR::redcap_read", fake_read)
    mockery::stub(f, "REDCapR::redcap_metadata_read", function(...) list(data = dic))
    mockery::stub(f, "REDCapR::redcap_project_info_read", function(...) list(data = data.frame(missing_data_codes = NA)))
    suppressWarnings(suppressMessages(f(uri = "https://api.example", token = "tok", ...)))$data
  }
  for (notes in list(c("NA", "1", "2"), rep("NA", 3), c("NA", "TRUE", "FALSE"))) {
    expect_identical(as.vector(read(c("1", "2", "3"), notes)$notes), notes)
  }
  expect_identical(as.vector(read(c("NA", "1", "2"), c("a", "b", "c"))$record_id), c("NA", "1", "2"))

  # The identifier of the survey is a text, as the identifier of the record
  out <- read(c("1", "2", "3"), c("a", "b", "c"), c("1.50", "+12", "9007199254740993"), survey_fields = TRUE)
  expect_identical(as.vector(out$redcap_survey_identifier), c("1.50", "+12", "9007199254740993"))
  out <- read(c("1", "2"), c("a", "b"), c("9007199254740992", "9007199254740993"), survey_fields = TRUE)
  expect_identical(as.vector(out$redcap_survey_identifier), c("9007199254740992", "9007199254740993"))
})

test_that("redcap_data keeps the text of the identifier of the survey of the exported files", {
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  write.csv(data.frame(record_id = c("1", "2", "3"), redcap_survey_identifier = c("1.50", "+12", "9007199254740993"), notes = c("a", "NA", "c")),
            file.path(folder, "P_DATA.csv"), row.names = FALSE, na = "")
  write.csv(audit_dic(c("record_id", "notes"), c("text", "notes")), file.path(folder, "DIC.csv"), row.names = FALSE)
  writeLines(c("## REDCap R export", "## begin data", "data=read.csv('P_DATA.csv',stringsAsFactors=FALSE)"), file.path(folder, "P_R.r"))
  out <- suppressWarnings(redcap_data(data_path = file.path(folder, "P_R.r"), dic_path = file.path(folder, "DIC.csv")))
  expect_identical(as.vector(out$data$redcap_survey_identifier), c("1.50", "+12", "9007199254740993"))
  expect_identical(as.vector(out$data$notes), c("a", "NA", "c"))
})

test_that("redcap_data gives each field of the API the label of its field_name when the order of the fields changes between the requests", {
  skip_if_not_installed("REDCapR", "1.2.0")
  skip_if_not_installed("mockery")
  dic <- audit_dic(c("record_id", "age", "weight", "symptoms"), c("text", "text", "text", "checkbox"), choices = c("", "", "", "1, Fever | 2, Cough"))
  dic$field_label <- c("Record ID", "Age", "Weight", "Symptoms")
  read <- function(label_order) {
    fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
      rows <- data.frame(record_id = c("1", "2"), age = c("54", "61"), weight = c("70", "80"), symptoms___1 = c("1", "0"), symptoms___2 = c("0", "1"))
      if (raw_or_label == "label") {
        labels <- c(record_id = "Record ID", age = "Age", weight = "Weight", symptoms___1 = "Symptoms (choice=Fever)", symptoms___2 = "Symptoms (choice=Cough)")
        rows$symptoms___1 <- ifelse(rows$symptoms___1 == "1", "Checked", "Unchecked")
        rows$symptoms___2 <- ifelse(rows$symptoms___2 == "1", "Checked", "Unchecked")
        rows <- rows[label_order]
        names(rows) <- labels[label_order]
      }
      list(data = rows)
    }
    f <- redcap_data
    mockery::stub(f, "REDCapR::redcap_read", fake_read)
    mockery::stub(f, "REDCapR::redcap_metadata_read", function(...) list(data = dic))
    mockery::stub(f, "REDCapR::redcap_project_info_read", function(...) list(data = data.frame(missing_data_codes = NA)))
    out <- suppressWarnings(suppressMessages(f(uri = "https://api.example", token = "tok")))$data
    vapply(c("age", "weight", "symptoms___1", "symptoms___2"), function(v) attr(out[[v]], "label"), character(1))
  }
  expected <- c(age = "Age", weight = "Weight", symptoms___1 = "Symptoms (choice=Fever)", symptoms___2 = "Symptoms (choice=Cough)")
  expect_equal(read(c("record_id", "age", "weight", "symptoms___1", "symptoms___2")), expected)
  # The fields, or the options of a checkbox, in another order in the request with labels
  expect_equal(read(c("record_id", "weight", "age", "symptoms___1", "symptoms___2")), expected)
  expect_equal(read(c("record_id", "age", "weight", "symptoms___2", "symptoms___1")), expected)
})
