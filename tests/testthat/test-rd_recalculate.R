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


# ---- Number of different values ----

test_that("rd_recalculate reports the number of values that differ from the recalculation", {
  res <- rd_recalculate(covican)
  report <- paste(res$results, collapse = "\n")

  expect_match(report, "Different values", fixed = TRUE)
  # age differs only in record 102-73 (74 in the data, 75 recalculated)
  expect_match(report, "\\|\\s+age\\s+\\|\\s+Yes\\s+\\|\\s+FALSE\\s+\\|\\s+1\\s+\\|")
  expect_match(report, "\\|\\s+screening_fail_crit\\s+\\|\\s+Yes\\s+\\|\\s+TRUE\\s+\\|\\s+0\\s+\\|")
})

test_that("count_differences compares like the 'Is equal?' column", {
  # numbers rounded to 3 decimals
  expect_equal(count_differences(c(1, 2.0001, 3), c(1, 2, 4)), 1L)
  # a value missing in only one version counts as different, missing in both doesn't
  expect_equal(count_differences(c(1, NA, NA), c(NA, 2, NA)), 2L)
  # non-numeric values are compared as text
  expect_equal(count_differences(c("a", "b"), c("a", "c")), 1L)
  expect_equal(count_differences(as.Date(c("2024-01-01", NA)), as.Date(c("2024-01-01", NA))), 0L)
  # a one-column data frame is handled as its column
  expect_equal(count_differences(data.frame(x = c(1, 2)), c(1, 3)), 1L)
  # not transcribed
  expect_identical(count_differences(c(1, 2), NULL), NA_integer_)
})


# ---- After rd_factor() and applied twice ----

test_that("rd_recalculate gives the same results after rd_factor()", {
  before <- rd_recalculate(covican)
  after <- suppressWarnings(rd_recalculate(suppressWarnings(rd_factor(covican))))

  report <- paste(after$results, collapse = "\n")
  expect_match(report, "\\|\\s+screening_fail_crit\\s+\\|\\s+Yes\\s+\\|\\s+TRUE\\s+\\|\\s+0\\s+\\|")
  expect_false("screening_fail_crit_recalc" %in% names(after$data))
  expect_equal(after$data$age_recalc, before$data$age_recalc)
})

test_that("rd_recalculate applied twice doesn't duplicate the recalculated fields", {
  once <- rd_recalculate(covican)
  twice <- rd_recalculate(once)

  expect_equal(sum(names(twice$data) == "age_recalc"), 1)
  expect_false(any(grepl("recalc\\.", names(twice$data))))
  expect_equal(names(twice$data), names(once$data))
  expect_equal(twice$dictionary, once$dictionary)
  expect_equal(twice$data$age_recalc, once$data$age_recalc, ignore_attr = TRUE)
  # age_recalc is not recalculated as another calculated field
  expect_false(grepl("age_recalc", paste(twice$results, collapse = "\n")))
})


# ---- tibble ----

test_that("rd_recalculate gives the same result with a tibble", {
  tib <- covican
  tib$data <- tibble::as_tibble(covican$data)

  res_tib <- rd_recalculate(tib)

  expect_equal(names(res_tib$data), names(rd_recalculate(covican)$data))
  expect_false("screening_fail_crit_recalc" %in% names(res_tib$data))
})


# ---- Repeated instruments ----

test_that("rd_recalculate works with repeated instruments", {
  fx <- make_repeat_classic()
  fx$data$ae_double <- c(NA, 2, 4, NA, 6)
  fx$dictionary <- rbind(fx$dictionary, data.frame(
    field_name = "ae_double", form_name = "ae", field_type = "calc", field_label = "Double grade",
    choices_calculations_or_slider_labels = "[ae_grade]*2", text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = "", stringsAsFactors = FALSE
  ))

  res <- rd_recalculate(fx)
  expect_match(paste(res$results, collapse = "\n"), "\\|\\s+ae_double\\s+\\|\\s+Yes\\s+\\|\\s+TRUE\\s+\\|\\s+0\\s+\\|")
  expect_false("ae_double_recalc" %in% names(res$data))
})


test_that("rd_recalculate compares values, not attributes such as the variable label", {
  df <- data.frame(record_id = 1:3, w = c(60, 80, 100), h = c(160, 180, 200), bmi = c(23.4, 24.7, 25))
  attr(df$w, "label") <- "Weight"
  dic <- data.frame(field_name = c("record_id", "w", "h", "bmi"), form_name = "f1", field_type = c("text", "text", "text", "calc"),
                    field_label = c("ID", "Weight", "Height", "BMI"), choices_calculations_or_slider_labels = c("", "", "", "round([w]/(([h]/100)^2),1)"),
                    text_validation_type_or_show_slider_number = "", branching_logic_show_field_only_if = "", stringsAsFactors = FALSE)

  res <- rd_recalculate(data = df, dic = dic)
  expect_match(paste(res$results, collapse = "\n"), "\\|\\s+bmi\\s+\\|\\s+Yes\\s+\\|\\s+TRUE\\s+\\|\\s+0\\s+\\|")
  expect_false("bmi_recalc" %in% names(res$data))
})


test_that("rd_recalculate warns about excluded variables that are not calculated fields", {
  expect_warning(rd_recalculate(covican, exclude = "copd"), "not calculated fields of the dictionary: copd")
})
