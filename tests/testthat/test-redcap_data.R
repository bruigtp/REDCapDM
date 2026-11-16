# Helper that writes a minimal exported R file (same approach as in your example)
write_redcap_r_export <- function(data, path) {
  con <- file(path, "w")
  # put two leading lines (function expects to skip 2 lines)
  writeLines("## REDCap R export", con)
  writeLines("## begin data", con)
  writeLines("data <- ", con)
  dput(data, con)
  close(con)
}

test_that("Exported-files path: successful import returns data, dictionary and event_form", {
  # Sample data
  sample_data <- as.data.frame(matrix(1:9, nrow = 3))
  names(sample_data)[1] <- "id"  # will be renamed to record_id by the function
  attr(sample_data[[2]], "label") <- "Test label"

  # Prepare small dictionary file (first row contains header names)
  # The function reads csv with header=FALSE and uses first row as names
  dic_rows <- data.frame(
    field_name = c("id"),
    form_name = c("demographics"),
    field_type = c("text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Event form mapping (valid — must NOT contain `event_name` column)
  event_form <- data.frame(form = "demographics", other = "x", stringsAsFactors = FALSE)

  # Write files
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE, quote = FALSE)

  tmp_event <- tempfile(fileext = ".csv")
  write.csv(event_form, tmp_event, row.names = FALSE, quote = FALSE)

  expect_warning(out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_event), "not in the dictionary")

  expect_true(is.list(out))
  expect_true("data" %in% names(out))
  expect_true("dictionary" %in% names(out))
  expect_true("event_form" %in% names(out))

  # data first column must be renamed to record_id by function
  expect_equal(names(out$data)[1], "record_id")
})

test_that("Unsupported dictionary extension errors", {
  tmp_r <- tempfile(fileext = ".R")
  sample_data <- as.data.frame(matrix(1:4, nrow = 2))
  write_redcap_r_export(sample_data, tmp_r)

  tmp_bad_dic <- tempfile(fileext = ".txt")
  writeLines("garbage", tmp_bad_dic)

  expect_error(
    redcap_data(data_path = tmp_r, dic_path = tmp_bad_dic),
    "Unsupported dictionary format"
  )
})

test_that("Event file containing 'event_name' column triggers informative error", {
  tmp_r <- tempfile(fileext = ".R")
  sample_data <- as.data.frame(matrix(1:4, nrow = 2))
  write_redcap_r_export(sample_data, tmp_r)

  # Good dictionary
  dic_rows <- data.frame(
    field_name = c("id"),
    form_name = c("demographics"),
    field_type = c("text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE, quote = FALSE)

  # Bad event file — has `event_name` column (simulates wrong file)
  bad_event <- data.frame(event_name = "e1", stringsAsFactors = FALSE)
  tmp_bad_event <- tempfile(fileext = ".csv")
  write.csv(bad_event, tmp_bad_event, row.names = FALSE, quote = FALSE)

  expect_error(
    redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_bad_event),
    "Invalid file provided in `event_path`"
  )
})

test_that("Longitudinal project without event_path issues a warning", {
  # create exported file with a redcap_event_name column to trigger the longitudinal branch
  sample_data <- data.frame(A = 1:2, redcap_event_name = c("ev1", "ev2"), stringsAsFactors = FALSE)
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  # dictionary
  dic_rows <- data.frame(
    field_name = c("id"),
    form_name = c("demographics"),
    field_type = c("text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE, quote = FALSE)

  expect_warning(
    res <- redcap_data(data_path = tmp_r, dic_path = tmp_dic),
    "The project is longitudinal. Consider providing `event_path`"
  )

  # result should still be returned and contain dictionary
  expect_true(is.list(res))
  expect_true("dictionary" %in% names(res))
})

test_that("Descriptive fields are removed from dictionary", {
  sample_data <- data.frame(id = 1:2)
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  # Create dictionary where one field is descriptive
  dic_rows <- data.frame(
    field_name = c("id"),
    form_name = c("demographics"),
    field_type = c("text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE, quote = FALSE)

  out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic)
  expect_false("descfield" %in% out$dictionary$field_name)
})


test_that("API branch: SSL error in labels fetch is converted to helpful stop message", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  # Make a stub that raises an error with SSL phrase
  fake_ssl_error <- function(...) stop("SSL peer certificate: something went wrong", call. = FALSE)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_ssl_error)

  expect_error(
    redcap_data(uri = "https://api.example", token = "tok"),
    "Unable to establish a secure connection"
  )
})

test_that("API branch: if labels retrieval returns zero rows a friendly error is thrown", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  empty_labels <- data.frame()  # zero-row data.frame
  fake_read_empty_labels <- function(redcap_uri = NULL, token = NULL, verbose = FALSE, raw_or_label = "label", raw_or_label_headers = NULL, export_data_access_groups = NULL, export_survey_fields = NULL, fields = NULL) {
    return(list(data = empty_labels))
  }
  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read_empty_labels)

  expect_error(
    redcap_data(uri = "https://api.example", token = "tok"),
    "Data retrieval is currently unavailable"
  )
})

test_that("API branch: when event_path is provided it warns and attempts to read it", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  # Reuse fake functions that return minimal data (labels + raw) and metadata
  labels_df <- data.frame(record_id = 1L, stringsAsFactors = FALSE, check.names = FALSE)
  data_api <- data.frame(record_id = "1", stringsAsFactors = FALSE, check.names = FALSE)
  # Add a form_name column — redcap_data filters on form_name later
  dic_api <- data.frame(
    field_name = "record_id",
    field_type = "text",
    form_name = "form1",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  fake_redcap_read2 <- function(redcap_uri = NULL, token = NULL, verbose = FALSE, raw_or_label = "raw", raw_or_label_headers = NULL, export_data_access_groups = NULL, export_survey_fields = NULL, fields = NULL) {
    if (identical(raw_or_label, "label")) return(list(data = labels_df))
    if (identical(raw_or_label, "raw")) return(list(data = data_api))
    stop("unexpected")
  }
  fake_redcap_metadata_read2 <- function(...) list(data = dic_api)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_redcap_read2)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_redcap_metadata_read2)

  # create a valid event CSV (not the events export)
  event_form <- data.frame(form = "form1", stringsAsFactors = FALSE)
  tmp_event <- tempfile(fileext = ".csv")
  write.csv(event_form, tmp_event, row.names = FALSE)

  expect_warning(
    out <- redcap_data(uri = "https://api.example", token = "tok", event_path = tmp_event),
    "argument is not required when using an API connection"
  )

  expect_true("event_form" %in% names(out))
})


test_that("Unsupported data_path extension errors", {
  tmp_bad <- tempfile(fileext = ".txt")
  writeLines("nonsense", tmp_bad)

  expect_error(
    redcap_data(data_path = tmp_bad, dic_path = tmp_bad),
    "Unsupported file format"
  )
})

test_that("Unsupported event mapping extension errors", {
  sample_data <- data.frame(id = 1:2)
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  dic_rows <- data.frame(field_name = "id", form_name = "f1", field_type = "text")
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  tmp_bad_event <- tempfile(fileext = ".json")
  writeLines("{}", tmp_bad_event)

  expect_error(
    redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_bad_event),
    "Unsupported event mapping format"
  )
})

test_that("Dictionary first field renamed to record_id if not already", {
  sample_data <- data.frame(A = 1:2)
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  dic_rows <- data.frame(field_name = "not_record_id", form_name = "f1", field_type = "text")
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic)
  expect_equal(out$dictionary$field_name[1], "record_id")
})

test_that("Empty strings are converted to NA in character columns", {
  sample_data <- data.frame(id = 1:2, textcol = c("a", ""))
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  dic_rows <- data.frame(field_name = "id", form_name = "f1", field_type = "text")
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  expect_warning(out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic), "not in the dictionary")
  expect_true(is.na(out$data$textcol[2]))
})

test_that("Variables not linked to event_form are removed with warning", {
  sample_data <- data.frame(id = 1:2, extra = c("a", "b"))
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  dic_rows <- data.frame(
    field_name = c("id", "extra"),
    form_name = c("f1", "unlinked"),
    field_type = "text"
  )
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  event_form <- data.frame(form = "f1")
  tmp_event <- tempfile(fileext = ".csv")
  write.csv(event_form, tmp_event, row.names = FALSE)

  expect_warning(
    out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_event),
    "removed since it is not linked"
  )
  expect_false("extra" %in% names(out$data))
})

test_that("API branch: filter_field missing record_id triggers helpful error", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  fake_error <- function(...) {
    stop("REDCap's PHP code is likely trying to process too much text in one bite", call. = FALSE)
  }
  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_error)

  expect_error(
    redcap_data(uri = "https://api.example", token = "tok", filter_field = "fieldX"),
    "record_id"
  )
})

test_that("API branch: observational data retrieval with zero rows errors", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  labels_df <- data.frame(record_id = 1)
  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    if (raw_or_label == "raw") return(list(data = data.frame())) # zero rows
    stop("unexpected")
  }
  fake_meta <- function(...) list(data = data.frame(field_name = "record_id", field_type = "text"))

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_meta)

  expect_error(
    redcap_data(uri = "https://api.example", token = "tok"),
    "Observational data retrieval is currently unavailable"
  )
})

test_that("API branch: descriptive fields are filtered from dictionary", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  labels_df <- data.frame(record_id = 1)
  data_api <- data.frame(record_id = 1)
  dic_api <- data.frame(field_name = "junk", field_type = "descriptive", form_name = "f1")

  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    if (raw_or_label == "raw") return(list(data = data_api))
    stop("unexpected")
  }
  fake_meta <- function(...) list(data = dic_api)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_meta)

  out <- redcap_data(uri = "https://api.example", token = "tok")
  expect_false("junk" %in% out$dictionary$field_name)
})

test_that("API branch: checkbox fields get factor versions", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  labels_df <- data.frame(record_id = 1)
  data_api <- data.frame(record_id = 1, var___1 = "1")
  dic_api <- data.frame(field_name = "var", field_type = "checkbox", form_name = "f1")

  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    if (raw_or_label == "raw") return(list(data = data_api))
    stop("unexpected")
  }
  fake_meta <- function(...) list(data = dic_api)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_meta)

  expect_warning(out <- redcap_data(uri = "https://api.example", token = "tok"), "not in the dictionary")
  expect_true("var___1.factor" %in% names(out$data))
})

test_that("API branch: radio/dropdown fields produce factor versions", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  labels_df <- data.frame(record_id = 1)
  data_api <- data.frame(record_id = 1, radio1 = "1")
  dic_api <- data.frame(
    field_name = "radio1",
    field_type = "radio",
    form_name = "f1",
    choices_calculations_or_slider_labels = "1, Yes | 2, No",
    stringsAsFactors = FALSE
  )

  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    if (raw_or_label == "raw") return(list(data = data_api))
    stop("unexpected")
  }
  fake_meta <- function(...) list(data = dic_api)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_meta)

  expect_warning(out <- redcap_data(uri = "https://api.example", token = "tok"), "not in the dictionary")
  expect_true("record_id.factor" %in% names(out$data))
  expect_true(is.factor(out$data$record_id.factor))
})


test_that("API branch: longitudinal without event_path fetches event_form via API", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  labels_df <- data.frame(record_id = 1, redcap_event_name = "ev1")
  data_api <- labels_df
  dic_api <- data.frame(field_name = "record_id", field_type = "text", form_name = "f1")
  event_api <- data.frame(form = "f1")

  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    if (raw_or_label == "raw") return(list(data = data_api))
    stop("unexpected")
  }
  fake_meta <- function(...) list(data = dic_api)
  fake_event <- function(...) list(data = event_api)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", fake_meta)
  mockery::stub(redcap_data, "REDCapR::redcap_event_instruments", fake_event)

  out <- redcap_data(uri = "https://api.example", token = "tok")
  expect_true("event_form" %in% names(out))
})

test_that("Supplying both file and API arguments errors", {
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(data.frame(id = 1), tmp_r)

  dic_rows <- data.frame(field_name = "id", form_name = "f1", field_type = "text")
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  expect_error(
    redcap_data(data_path = tmp_r, dic_path = tmp_dic, uri = "u", token = "t"),
    "Too many arguments"
  )
})

test_that("Supplying only uri or only token errors", {
  expect_error(
    redcap_data(uri = "https://api.example"),
    "Both `uri` and `token` are required"
  )

  expect_error(
    redcap_data(token = "tok"),
    "Both `uri` and `token` are required"
  )
})


test_that("Event file exported by REDCap without a final newline is read without warnings", {
  sample_data <- data.frame(id = 1:2, x = c(1, 2))
  dic_rows <- data.frame(field_name = c("id", "x"), form_name = "demographics", field_type = "text")

  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE, quote = FALSE)

  # Same layout as the instrument-event mapping downloaded from REDCap: no final newline
  tmp_event <- tempfile(fileext = ".csv")
  cat("arm_num,unique_event_name,form\n1,baseline_arm_1,demographics", file = tmp_event)

  expect_no_warning(
    out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_event)
  )
  expect_equal(out$event_form$unique_event_name, "baseline_arm_1")
  expect_equal(out$event_form$form, "demographics")
})


test_that("redcap_data explains the mistakes in its arguments and files", {
  sample_data <- data.frame(id = 1:2, x = c(1, 2))
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = c("id", "x"), form_name = "f1", field_type = "text"), tmp_dic, row.names = FALSE)

  expect_error(redcap_data(), "No data to read")
  expect_error(redcap_data(data_path = "no_such_file.r", dic_path = tmp_dic), "File not found: `data_path`")
  expect_error(redcap_data(data_path = tmp_r, dic_path = "no_such_dic.csv"), "File not found: `dic_path`")
  expect_error(redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = "yes"), "`rm_mdc` must be TRUE or FALSE")

  # The event file of another project
  tmp_event <- tempfile(fileext = ".csv")
  write.csv(data.frame(arm_num = 1, unique_event_name = "visit_arm_1", form = "other_form"), tmp_event, row.names = FALSE)
  expect_error(redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_event), "not the instrument-event mapping of this project")
})

test_that("redcap_data stops when the csv file read by the R script is not next to it", {
  dir <- tempfile()
  dir.create(dir)
  r_file <- file.path(dir, "PROJ_R.r")
  writeLines(c("#Clear existing data and graphics", "rm(list=ls())", "data=read.csv('PROJ_DATA.csv')"), r_file)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = "record_id", form_name = "f1", field_type = "text"), tmp_dic, row.names = FALSE)

  expect_error(redcap_data(data_path = r_file, dic_path = tmp_dic), "PROJ_DATA.csv")
})

test_that("redcap_data warns about a project without records and keeps working with rm_mdc = FALSE", {
  sample_data <- data.frame(id = character(0), x = character(0), stringsAsFactors = FALSE)
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = c("id", "x"), form_name = "f1", field_type = c("text", "radio"),
                       select_choices_or_calculations = c("", "1, A | 2, B")), tmp_dic, row.names = FALSE)

  expect_warning(out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic), "no records yet")
  expect_equal(nrow(out$data), 0)
  expect_warning(redcap_data(data_path = tmp_r, dic_path = tmp_dic, rm_mdc = FALSE), "no records yet")
})


test_that("redcap_data reads the R script of REDCap without Hmisc and without closing the plots", {
  dir <- tempfile()
  dir.create(dir)
  write.csv(data.frame(record_id = 1:2, sex = c(1, 2)), file.path(dir, "PROJ_DATA.csv"), row.names = FALSE)
  writeLines(c(
    "#Clear existing data and graphics", "rm(list=ls())", "graphics.off()", "#Load Hmisc library", "library(Hmisc)",
    "#Read Data", "data=read.csv('PROJ_DATA.csv')", "#Setting Labels", "", "label(data$record_id)=\"Record ID\"",
    "label(data$sex)=\"Sex\"", "#Setting Units", "units(data$sex)=\"code\"", "",
    "#Setting Factors(will create new variable for factors)", "data$sex.factor = factor(data$sex,levels=c(\"1\",\"2\"))",
    "levels(data$sex.factor)=c(\"Man\",\"Woman\")"
  ), file.path(dir, "PROJ_R.r"))
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = c("record_id", "sex"), form_name = "f1", field_type = c("text", "radio"),
                       select_choices_or_calculations = c("", "1, Man | 2, Woman")), tmp_dic, row.names = FALSE)

  attached_before <- "package:Hmisc" %in% search()
  grDevices::pdf(NULL)
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)

  out <- redcap_data(data_path = file.path(dir, "PROJ_R.r"), dic_path = tmp_dic)

  expect_equal(attr(out$data$sex, "label"), "Sex")
  expect_equal(levels(out$data$sex.factor), c("Man", "Woman"))
  expect_equal("package:Hmisc" %in% search(), attached_before)
  expect_equal(grDevices::dev.cur(), device)
})

test_that("redcap_data ignores the empty rows of the dictionary and keeps the first row of a repeated field", {
  sample_data <- data.frame(record_id = 1:2, x = c(1, 2))
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = c("record_id", "x", "", "x"), form_name = c("f1", "f1", "", "f2"), field_type = c("text", "text", "", "text")), tmp_dic, row.names = FALSE)

  expect_warning(out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic), "more than one row")
  expect_equal(out$dictionary$field_name, c("record_id", "x"))
  expect_equal(out$dictionary$form_name, c("f1", "f1"))
})

test_that("redcap_data explains that the R script can't be run", {
  dir <- tempfile()
  dir.create(dir)
  write.csv(data.frame(record_id = 1:2), file.path(dir, "PROJ_DATA.csv"), row.names = FALSE)
  writeLines(c("#Clear existing data and graphics", "rm(list=ls())", "data=read.csv('PROJ_DATA.csv')", "label(data$record_id) = = \"ID\""), file.path(dir, "PROJ_R.r"))
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(field_name = "record_id", form_name = "f1", field_type = "text"), tmp_dic, row.names = FALSE)

  wd <- getwd()
  expect_error(redcap_data(data_path = file.path(dir, "PROJ_R.r"), dic_path = tmp_dic), class = "REDCapDM_error_file")
  expect_equal(getwd(), wd)
})

test_that("redcap_data keeps the text of the text fields that look like numbers", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("subj,zip,code,weight,f1_complete", "S-1,00123,1.50,70.5,2", "S-2,12000,1e5,80,2"), file.path(dir, "PROJ_DATA.csv"))
  writeLines(c(
    "#Clear existing data and graphics", "rm(list=ls())", "data=read.csv('PROJ_DATA.csv')",
    "label(data$subj)=\"Subject\"", "label(data$zip)=\"Postal code\"", "label(data$code)=\"Code\"", "label(data$weight)=\"Weight\""
  ), file.path(dir, "PROJ_R.r"))
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    field_name = c("subj", "zip", "code", "weight"), form_name = "f1", field_type = "text",
    text_validation_type_or_show_slider_number = c("", "zipcode", "", "number")
  ), tmp_dic, row.names = FALSE)

  out <- redcap_data(data_path = file.path(dir, "PROJ_R.r"), dic_path = tmp_dic)

  expect_equal(out$data$zip, c("00123", "12000"), ignore_attr = TRUE)
  expect_equal(out$data$code, c("1.50", "1e5"), ignore_attr = TRUE)
  expect_equal(attr(out$data$zip, "label"), "Postal code")
  # Numeric fields are not changed
  expect_equal(out$data$weight, c(70.5, 80), ignore_attr = TRUE)
  # The label of the record identifier is kept when it has another name
  expect_equal(attr(out$data$record_id, "label"), "Subject")
})

test_that("API branch: labels, yes/no and form status factors and forms without event are as in the exported files", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("mockery")
  data_api <- data.frame(subj = 1:2, redcap_event_name = "visit_arm_1", name = c("Ann", "Bob"), smoker = c(1, 0), f1_complete = c(2, 0), extra = c("a", "b"), f2_complete = 0)
  labels_df <- data.frame(
    "Subject" = 1:2, "Event Name" = "Visit", "Name" = c("Ann", "Bob"), "Smoker" = c("Yes", "No"), "Complete?" = c("Complete", "Incomplete"),
    "Extra" = c("a", "b"), "Complete?...7" = "Incomplete", check.names = FALSE
  )
  dic_api <- data.frame(
    field_name = c("subj", "name", "smoker", "extra"), form_name = c("f1", "f1", "f1", "f2"), field_type = c("text", "text", "yesno", "text"),
    field_label = c("Subject", "Name", "Smoker", "Extra"), select_choices_or_calculations = NA, branching_logic = NA, question_number = NA
  )
  event_api <- data.frame(arm_num = 1, unique_event_name = "visit_arm_1", form = "f1")

  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    list(data = data_api)
  }
  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", function(...) list(data = dic_api))
  mockery::stub(redcap_data, "REDCapR::redcap_event_instruments", function(...) list(data = event_api))

  expect_warning(out <- suppressMessages(redcap_data(uri = "https://api.example", token = "tok")), "not linked to any event")

  expect_equal(attr(out$data$record_id, "label"), "Subject")
  expect_equal(attr(out$data$name, "label"), "Name")
  expect_equal(as.character(out$data$smoker.factor), c("Yes", "No"))
  expect_equal(as.character(out$data$f1_complete.factor), c("Complete", "Incomplete"))
  expect_false("extra" %in% names(out$data))
  expect_false("extra" %in% out$dictionary$field_name)
  # As in the exported dictionary, its empty cells are empty texts
  expect_equal(out$dictionary$branching_logic_show_field_only_if, rep("", 3))
  expect_equal(out$dictionary$choices_calculations_or_slider_labels, rep("", 3))
})

test_that("redcap_data removes the HTML of the labels of the dictionary and of the data", {
  sample_data <- data.frame(id = 1:2, weight = c(70, 80), age = c(17, 95))
  attr(sample_data$weight, "label") <- "Weight (<i>kg</i>)<br>today"
  attr(sample_data$age, "label") <- "Age (<18 or >90 years)"
  tmp_r <- tempfile(fileext = ".R")
  write_redcap_r_export(sample_data, tmp_r)

  dic_rows <- data.frame(
    field_name = c("id", "weight", "age"), form_name = "f", field_type = "text",
    field_label = c("ID", "Weight (<i>kg</i>)<br>today", "Age (<18 or >90 years)"), stringsAsFactors = FALSE
  )
  tmp_dic <- tempfile(fileext = ".csv")
  write.csv(dic_rows, tmp_dic, row.names = FALSE)

  out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic)
  expect_equal(out$dictionary$field_label[2:3], c("Weight (kg) today", "Age (<18 or >90 years)"))
  expect_equal(attr(out$data$weight, "label"), "Weight (kg) today")
  expect_equal(attr(out$data$age, "label"), "Age (<18 or >90 years)")
})

test_that("API branch: text fields that look like numbers keep their text, as with the exported files", {
  skip_if_not_installed("REDCapR")
  skip_if_not_installed("readr")
  skip_if_not_installed("mockery")
  raw_text <- "subj,phone,dose,n_txt,note,weight,f1_complete\n1,+34600111222,1.50,12,,70.5,2\n2,+34931234567,2,7,,80,0\n"
  labels_df <- data.frame("Subject" = 1:2, "Phone" = c("+34600111222", "+34931234567"), "Dose" = c("1.50", "2"), "N" = c(12, 7), "Note" = NA, "Weight" = c(70.5, 80),
    "Complete?" = c("Complete", "Incomplete"), check.names = FALSE)
  dic_api <- data.frame(
    field_name = c("subj", "phone", "dose", "n_txt", "note", "weight"), form_name = "f1", field_type = c("text", "text", "text", "text", "notes", "text"),
    field_label = c("Subject", "Phone", "Dose", "N", "Note", "Weight"), text_validation_type_or_show_slider_number = c(NA, NA, NA, NA, NA, "number")
  )
  # As REDCapR: with guess_type = FALSE every column is text, otherwise the types are guessed
  fake_read <- function(redcap_uri = NULL, token = NULL, raw_or_label = "raw", guess_type = TRUE, ...) {
    if (raw_or_label == "label") return(list(data = labels_df))
    list(data = readr::read_csv(I(raw_text), show_col_types = FALSE, col_types = if (guess_type) NULL else readr::cols(.default = "c")))
  }
  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_read)
  mockery::stub(redcap_data, "REDCapR::redcap_metadata_read", function(...) list(data = dic_api))

  out <- suppressMessages(redcap_data(uri = "https://api.example", token = "tok"))
  expect_equal(out$data$phone, c("+34600111222", "+34931234567"), ignore_attr = TRUE)
  expect_equal(out$data$dose, c("1.50", "2"), ignore_attr = TRUE)
  # Text fields whose numbers don't change and validated fields are still numbers
  expect_true(is.numeric(out$data$n_txt))
  expect_true(is.numeric(out$data$weight))
  # An empty text field is logical, as read.csv() reads it from the exported files
  expect_true(is.logical(out$data$note))
})
