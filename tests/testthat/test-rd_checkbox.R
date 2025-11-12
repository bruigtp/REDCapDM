test_that("rd_checkbox converts checkbox variables to factors with specified labels", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"))

  # Check one checkbox variable as example
  expect_true(is.factor(result$data$inc_1.factor))
  expect_true(all(levels(result$data$inc_1.factor) == c("No", "Yes")))

  # Check another checkbox
  expect_true(is.factor(result$data$type_underlying_disease_haematological_cancer))
  expect_true(all(levels(result$data$type_underlying_disease_haematological_cancer) == c("No", "Yes")))
})

test_that("rd_checkbox renames checkbox variables when checkbox_names = TRUE", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"), checkbox_names = TRUE)

  # Check that new variable names exist (for one example)
  expect_true(any(grepl("underlying_disease_hemato_myelofibrosis", names(result$data))))
  expect_true(all(levels(result$data$underlying_disease_hemato_myelofibrosis) == c("No", "Yes")))
})

test_that("rd_checkbox keeps original names when checkbox_names = FALSE", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"), checkbox_names = FALSE)

  expect_true(any(grepl("___", names(result$data))))
})

test_that("checkbox_na applies NAs according to branching logic", {
  # Pick one checkbox with branching logic
  result <- rd_checkbox(covican, checkbox_na = TRUE)

  # Check that rows violating the branching logic are NA
  vals <- result$data |>
    dplyr::filter(is.na(type_underlying_disease_haematological_cancer)) |>
    dplyr::select(type_underlying_disease_haematological_cancer, underlying_disease_hemato_acute_lymphoblastic_leukaemia) |>
    dplyr::mutate(check = is.na(type_underlying_disease_haematological_cancer) & is.na( underlying_disease_hemato_acute_lymphoblastic_leukaemia))

  expect_true(all(vals$check))
})

test_that("long checkbox labels are truncated to 60 characters", {
  long_label <- paste(rep("X", 70), collapse = "")
  covican_test <- covican
  covican_test$data$long_checkbox___1 <- 1
  covican_test$dictionary <- covican$dictionary |> tibble::add_row(
    field_name = "long_checkbox",
    field_type = "checkbox",
    field_label = long_label,
    choices_calculations_or_slider_labels = "0, No | 1, Yes",
    .before = 1
  )

  result <- rd_checkbox(data = covican_test$data, dic = covican_test$dictionary) |> suppressWarnings()
  expect_true(all(nchar(names(result$data)) <= 60))
})


test_that("function warns about repeated instruments", {
  covican_test <- covican
  covican_test$data$redcap_repeat_instrument <- rep("some_form", nrow(covican_test$data))

  expect_warning(
    rd_checkbox(data = covican_test$data, dic = covican_test$dictionary, event_form = covican$event_form),
    "repeated instruments"
  )
})

test_that("running rd_checkbox twice cannot be done", {
  result1 <- rd_checkbox(covican)

  expect_error(rd_checkbox(result1), "No checkbox fields found in the data")
})

test_that("non-checkbox fields remain unchanged", {
  result <- rd_checkbox(covican)

  expect_equal(result$data$age, covican$data$age)
  expect_equal(result$data$potassium, covican$data$potassium)
})

