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

  out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic, event_path = tmp_event)

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
  # Make a stub that raises an error with SSL phrase
  fake_ssl_error <- function(...) stop("SSL peer certificate: something went wrong", call. = FALSE)

  mockery::stub(redcap_data, "REDCapR::redcap_read", fake_ssl_error)

  expect_error(
    redcap_data(uri = "https://api.example", token = "tok"),
    "Unable to establish a secure connection"
  )
})

test_that("API branch: if labels retrieval returns zero rows a friendly error is thrown", {
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
    "The event_path argument is not required when using an API connection."
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

  out <- redcap_data(data_path = tmp_r, dic_path = tmp_dic)
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
    "removed since they are not linked"
  )
  expect_false("extra" %in% names(out$data))
})

test_that("API branch: filter_field missing record_id triggers helpful error", {
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

  out <- redcap_data(uri = "https://api.example", token = "tok")
  expect_true("var___1.factor" %in% names(out$data))
})

test_that("API branch: radio/dropdown fields produce factor versions", {
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

  out <- redcap_data(uri = "https://api.example", token = "tok")
  expect_true("record_id.factor" %in% names(out$data))
  expect_true(is.factor(out$data$record_id.factor))
})


test_that("API branch: longitudinal without event_path fetches event_form via API", {
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
