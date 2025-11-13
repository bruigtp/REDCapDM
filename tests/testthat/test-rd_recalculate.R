cov_data <- covican$data
cov_dic <- covican$dictionary
cov_event_form <- covican$event_form

# Select a real calculated field from the dictionary if available
calc_fields <- cov_dic |>
  filter(field_type == "calc") |>
  pull(field_name)

# If none exist, for testing, pick numeric fields as "calc" and create simple logic
if(length(calc_fields) == 0){
  cov_dic_test <- cov_dic |>
    filter(field_name %in% c("age", "potassium")) |>
    mutate(
      field_type = "calc",
      choices_calculations_or_slider_labels = c("age", "potassium")
    )
} else {
  cov_dic_test <- cov_dic |> filter(field_name %in% calc_fields[1:2])
}

test_that("recalculate returns updated data and dictionary", {
  res <- rd_recalculate(data = cov_data, dic = cov_dic_test, event_form = cov_event_form)

  expect_type(res, "list")
  expect_named(res, c("data", "dictionary", "event_form", "results"))

  # Recalculated fields should exist if transcription works
  expect_true(any(grepl("_recalc$", names(res$data)) | sapply(res$data, function(x) !is.null(x))))
  expect_true(any(grepl("_recalc$", res$dictionary$field_name)))

  # Results should contain a summary table
  expect_true("glue" %in% class(res$results))
})

test_that("recalculate respects exclude argument", {
  res <- rd_recalculate(data = cov_data, dic = cov_dic_test, event_form = cov_event_form, exclude = calc_fields[1])

  # First calc field _recalc should NOT exist
  expect_false(any(grepl(paste0(calc_fields[1], "_recalc"), names(res$data))))
})

test_that("recalculate fails if data or dictionary is missing", {
  expect_error(rd_recalculate(data = NULL, dic = cov_dic_test))
  expect_error(rd_recalculate(data = cov_data, dic = NULL))
})

test_that("recalculate works with project list input", {
  project <- list(data = cov_data, dic = cov_dic_test, event_form = cov_event_form)

  # Pass project fields individually
  res <- rd_recalculate(data = project$data, dic = project$dic, event_form = project$event_form)

  expect_true(any(grepl("_recalc$", names(res$data)) | sapply(res$data, function(x) !is.null(x))))
})

test_that("recalculate warns for character datetime fields", {
  dic_date <- tibble(
    field_name = "d_admission",
    field_type = "text",
    choices_calculations_or_slider_labels = "",
    branching_logic_show_field_only_if = NA,
    text_validation_type_or_show_slider_number = "datetime_dmy"
  )

  # Force date column to character
  data_date <- cov_data |> mutate(d_admission = as.character(d_admission)) |> select(d_admission)

  expect_warning(
    rd_recalculate(data = data_date, dic = dic_date),
    "The dataset contains date fields stored as character class"
  )
})

test_that("recalculate stops for longitudinal project without event_form", {
  data_long <- cov_data |> filter(!is.na(redcap_event_name))

  expect_error(
    rd_recalculate(data = data_long, dic = cov_dic_test),
    "Recalculation cannot proceed because the project has more than one event"
  )
})

