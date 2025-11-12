# Sample data dictionary
dic <- tibble(
  field_name = c("dob", "appointment"),
  text_validation_type_or_show_slider_number = c("date_dmy", "datetime_dmyhm")
)

# Sample dataset
data <- tibble(
  dob = c("1990-01-01", "2000-12-31", ""),
  appointment = c("2025-09-12 14:30", "2025-09-13 09:00", "")
)

# Sample event-form mapping
event_form <- list(baseline = "form1", followup = "form2")

# Add labels
data <- data |>
  set_variable_labels(dob = "Date of Birth", appointment = "Appointment DateTime")

test_that("rd_dates converts character dates and datetimes correctly", {
  res <- rd_dates(data = data, dic = dic, event_form = event_form)

  # Check class
  expect_s3_class(res$data$dob, "Date")
  expect_s3_class(res$data$appointment, "POSIXct")

  # Check NA conversion
  expect_true(is.na(res$data$dob[3]))
  expect_true(is.na(res$data$appointment[3]))

  # Labels preserved
  expect_equal(var_label(res$data$dob), "Date of Birth")
  expect_equal(var_label(res$data$appointment), "Appointment DateTime")

  # Event form preserved
  expect_equal(res$event_form, event_form)
})

test_that("rd_dates warns if all dates/datetimes are already correct", {
  # Convert manually to correct classes
  data_correct <- data |>
    mutate(dob = as.Date(dob),
           appointment = as.POSIXct(appointment, format = "%Y-%m-%d %H:%M", tz = "UTC"))

  expect_warning(
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
  empty_data <- tibble(dob = character(), appointment = character())
  empty_dic <- tibble(field_name = character(), text_validation_type_or_show_slider_number = character())

  res <- rd_dates(data = empty_data, dic = empty_dic) |> suppressWarnings()

  expect_equal(nrow(res$data), 0)
  expect_equal(ncol(res$data), 2)
})

test_that("rd_dates converts only necessary variables", {
  data_mixed <- data |>
    mutate(
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

  expect_equal(var_label(res$data$appointment), "Appointment DateTime")
  expect_equal(var_label(res$data$dob), "")  # unlabeled stays empty
})
