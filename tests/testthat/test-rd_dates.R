# Sample data dictionary
dic <- tibble::tibble(
  field_name = c("dob", "appointment"),
  text_validation_type_or_show_slider_number = c("date_dmy", "datetime_dmyhm")
)

# Sample dataset
data <- tibble::tibble(
  dob = c("1990-01-01", "2000-12-31", ""),
  appointment = c("2025-09-12 14:30", "2025-09-13 09:00", "")
)

# Sample event-form mapping
event_form <- list(baseline = "form1", followup = "form2")

# Add labels
data <- data |>
  labelled::set_variable_labels(dob = "Date of Birth", appointment = "Appointment DateTime")

test_that("rd_dates converts character dates and datetimes correctly", {
  res <- rd_dates(data = data, dic = dic, event_form = event_form)

  # Check class
  expect_s3_class(res$data$dob, "Date")
  expect_s3_class(res$data$appointment, "POSIXct")

  # Check NA conversion
  expect_true(is.na(res$data$dob[3]))
  expect_true(is.na(res$data$appointment[3]))

  # Labels preserved
  expect_equal(labelled::var_label(res$data$dob), "Date of Birth")
  expect_equal(labelled::var_label(res$data$appointment), "Appointment DateTime")

  # Event form preserved
  expect_equal(res$event_form, event_form)
})

test_that("rd_dates warns if all dates/datetimes are already correct", {
  # Convert manually to correct classes
  data_correct <- data |>
    dplyr::mutate(dob = as.Date(dob),
           appointment = as.POSIXct(appointment, format = "%Y-%m-%d %H:%M", tz = "UTC"))

  expect_message(
    rd_dates(data = data_correct, dic = dic),
    "All date and datetime variables are already in the correct format"
  )
})

test_that("rd_dates stops if data or dic is missing", {
  expect_error(rd_dates(data = NULL, dic = dic),
               "Both `data` and `dic`.*must be provided")
  expect_error(rd_dates(data = data, dic = NULL),
               "Both `data` and `dic`.*must be provided")
})

test_that("rd_dates handles empty input gracefully", {
  empty_data <- tibble::tibble(dob = character(), appointment = character())
  empty_dic <- tibble::tibble(field_name = character(), text_validation_type_or_show_slider_number = character())

  res <- rd_dates(data = empty_data, dic = empty_dic) |> suppressWarnings()

  expect_equal(nrow(res$data), 0)
  expect_equal(ncol(res$data), 2)
})

test_that("rd_dates converts only necessary variables", {
  data_mixed <- data |>
    dplyr::mutate(
      dob = as.Date(dob),  # already correct
      appointment = appointment  # still character
    )

  res <- rd_dates(data = data_mixed, dic = dic)

  expect_s3_class(res$data$dob, "Date")
  expect_s3_class(res$data$appointment, "POSIXct")
})

test_that("rd_dates preserves labels correctly", {
  unlabeled_data <- data
  attr(unlabeled_data$dob, "label") <- NULL

  res <- rd_dates(data = unlabeled_data, dic = dic)

  expect_equal(labelled::var_label(res$data$appointment), "Appointment DateTime")
  expect_equal(labelled::var_label(res$data$dob), "")  # unlabeled stays empty
})

test_that("rd_dates sets to NA the values that aren't dates, wherever they are, and says which ones", {
  dic <- data.frame(field_name = c("record_id", "d", "dt"), text_validation_type_or_show_slider_number = c("", "date_ymd", "datetime_seconds_ymd"))
  data <- data.frame(
    record_id = 1:4,
    d = c("8888", "2020-01-05", "2020/01/06", NA),
    dt = c("2020-01-05 10:30:15", "ND", "2020-01-05 10:30", "")
  )
  expect_warning(out <- suppressMessages(rd_dates(data = data, dic = dic))$data, "aren't dates", class = "REDCapDM_warning")
  expect_equal(out$d, as.Date(c(NA, "2020-01-05", "2020-01-06", NA)), ignore_attr = "label")
  # The seconds are kept although other values don't have them
  expect_equal(out$dt, as.POSIXct(c("2020-01-05 10:30:15", NA, "2020-01-05 10:30:00", NA), tz = "UTC"), ignore_attr = "label")

  # Without values that aren't dates, no warning
  expect_no_warning(suppressMessages(rd_dates(data = data[3:4, ], dic = dic)))
})

test_that("rd_dates only converts the values that follow a whole format: invalid times or extra text are NA", {
  dic <- data.frame(field_name = c("record_id", "d", "dt"), text_validation_type_or_show_slider_number = c("", "date_ymd", "datetime_seconds_ymd"))
  data <- data.frame(
    record_id = 1:6,
    d = c("2024-02-29", "2024-02-29trailing", "2024-02-30", "2024-02-29 extra", "2024/2/5", " 2024-03-01 "),
    dt = c("2024-02-29 12:30:00", "2024-02-29 99:00:00", "2024-02-29 12:30:00trailing", "2024-02-29 12:61", "2024-02-29", "2024/02/29 08:05")
  )
  expect_warning(out <- suppressMessages(rd_dates(data = data, dic = dic))$data, "aren't dates", class = "REDCapDM_warning")
  expect_equal(out$d, as.Date(c("2024-02-29", NA, NA, NA, "2024-02-05", "2024-03-01")), ignore_attr = "label")
  # (a datetime field with only the date is at midnight, as before)
  expect_equal(out$dt, as.POSIXct(c("2024-02-29 12:30:00", NA, NA, NA, "2024-02-29 00:00:00", "2024-02-29 08:05:00"), tz = "UTC"), ignore_attr = "label")
})
