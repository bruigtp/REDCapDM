test_that("rd_checkbox converts checkbox variables to factors with specified labels", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"))

  # Check one checkbox variable as example
  expect_true(is.factor(result$data$inc_1.factor))
  expect_true(all(levels(result$data$inc_1.factor) == c("No", "Yes")))

  # Check another checkbox
  expect_true(is.numeric(result$data$type_underlying_disease_haematological_cancer))
  expect_true(is.factor(result$data$type_underlying_disease_haematological_cancer.factor))
  expect_true(all(levels(result$data$type_underlying_disease_haematological_cancer.factor) == c("No", "Yes")))
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
  # we have to account for the '.factor' so the limit is 67
  expect_true(all(nchar(names(result$data)) <= 67))
})


test_that("rd_checkbox evaluates the branching logic of checkboxes in repeating instruments", {
  fx <- make_repeat_classic()
  fx$data$sym___1 <- c(NA, 1, 0, NA, 1)
  fx$data$sym___2 <- c(NA, 0, 1, NA, 0)
  fx$data$sym___1.factor <- factor(c(NA, "Checked", "Unchecked", NA, "Checked"), levels = c("Unchecked", "Checked"))
  fx$data$sym___2.factor <- factor(c(NA, "Unchecked", "Checked", NA, "Unchecked"), levels = c("Unchecked", "Checked"))
  fx$dictionary <- rbind(fx$dictionary, data.frame(
    field_name = "sym", form_name = "ae", field_type = "checkbox", field_label = "Symptoms",
    choices_calculations_or_slider_labels = "1, Cough | 2, Fever", text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = "[age] > '40'", stringsAsFactors = FALSE
  ))

  expect_no_warning(res <- rd_checkbox(fx, checkbox_names = FALSE, na_logic = "missing"))

  # record 1 (50 years old in its base row) keeps its answers; record 2 has no age
  expect_equal(as.character(res$data$sym___1.factor), c(NA, "Yes", "No", NA, NA))
  expect_equal(as.character(res$data$sym___2.factor), c(NA, "No", "Yes", NA, NA))
  expect_equal(as.numeric(res$data$sym___1), c(NA, 1, 0, NA, NA))
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


# ---- Other data structures ----

test_that("rd_checkbox gives the same result with a tibble", {
  tib <- covican
  tib$data <- tibble::as_tibble(covican$data)

  res_df <- rd_checkbox(covican)
  res_tib <- rd_checkbox(tib)

  expect_equal(as.data.frame(res_tib$data), as.data.frame(res_df$data), ignore_attr = TRUE)
  expect_true(any(!is.na(res_tib$data$type_underlying_disease_solid_tumour)))
})

test_that("rd_checkbox names the options from the dictionary when the data has no variable labels", {
  no_labels <- covican
  no_labels$data[] <- lapply(covican$data, function(x) { attr(x, "label") <- NULL; x })

  res <- suppressWarnings(rd_checkbox(no_labels))

  expect_true(all(c("type_underlying_disease_haematological_cancer", "type_underlying_disease_solid_tumour") %in% names(res$data)))
  expect_false(any(grepl("_x(\\.\\d+)?$", names(res$data))))
  expect_equal(names(res$data), names(rd_checkbox(covican)$data))
})


test_that("rd_checkbox returns a project without checkboxes unchanged, with a warning", {
  fx <- make_repeat_classic()

  expect_warning(res <- rd_checkbox(fx), "has no checkbox fields")
  expect_identical(res$data, fx$data)
  expect_identical(res$dictionary, fx$dictionary)
})
