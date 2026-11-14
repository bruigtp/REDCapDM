# Uses of the functions that are not covered by the tests of each function: chained steps, other orders and the
# less common arguments.

test_that("the steps of $results are numbered when the functions are chained", {
  res <- suppressWarnings(covican |> rd_recalculate() |> rd_dates() |> rd_checkbox() |> rd_insert_na(vars = "copd", filter = "age < 65"))
  text <- paste(res$results, collapse = "")

  expect_match(text, "1. Recalculating calculated fields", fixed = TRUE)
  expect_match(text, "2. Transforming date and datetime fields. (rd_dates)", fixed = TRUE)
  expect_match(text, "3. Transforming checkboxes", fixed = TRUE)
  expect_match(text, "4. Inserting missing values into variable(s): copd. (rd_insert_na)", fixed = TRUE)
})

test_that("rd_checkbox() gives the same values after rd_factor() or rd_dictionary()", {
  usual <- suppressWarnings(covican |> rd_checkbox() |> rd_factor())
  expect_warning(after_factor <- covican |> rd_factor() |> rd_checkbox(), "already in factor form")
  plain <- suppressWarnings(rd_checkbox(covican))
  after_dic <- suppressWarnings(covican |> rd_dictionary() |> rd_checkbox())

  checkboxes <- grep("^type_underlying_disease_|^underlying_disease_hemato_", names(usual$data), value = TRUE)
  expect_gt(length(checkboxes), 0)
  for (v in checkboxes) {
    expect_identical(as.character(after_factor$data[[v]]), as.character(usual$data[[v]]))
    expect_identical(as.character(after_dic$data[[v]]), as.character(plain$data[[v]]))
  }
  expect_identical(levels(after_factor$data[[checkboxes[1]]]), c("No", "Yes"))
})

test_that("rd_checkbox() renames a checkbox whose new name already exists", {
  x <- covican
  x$data$type_underlying_disease_haematological_cancer <- 1

  expect_warning(res <- rd_checkbox(x), "already exists in the dataset")
  expect_true(all(c("type_underlying_disease_haematological_cancer", "type_underlying_disease_haematological_cancer.1") %in% names(res$data)))
})

test_that("rd_transform() explains what it needs to split the data", {
  no_event_form <- covican[c("data", "dictionary")]

  expect_error(rd_transform(no_event_form, final_format = "by_event"), "event_form", class = "REDCapDM_error")
  expect_error(rd_transform(no_event_form, final_format = "by_form"), "longitudinal project", class = "REDCapDM_error")
})

test_that("rd_transform() describes na_logic = 'eval' and removes the forms not linked to any event", {
  res <- suppressWarnings(rd_transform(covican, na_logic = "eval"))
  expect_match(paste(res$results, collapse = ""), "isn't satisfied", fixed = TRUE)

  x <- covican
  x$event_form <- x$event_form[x$event_form$form != "cancer", ]
  res <- suppressWarnings(rd_transform(x))
  cancer_vars <- x$dictionary$field_name[x$dictionary$form_name == "cancer"]

  expect_false(any(cancer_vars %in% names(res$data)))
  expect_false(any(cancer_vars %in% res$dictionary$field_name))
  expect_match(paste(res$results, collapse = ""), "not linked to any event", fixed = TRUE)
})

test_that("rd_transform() works with a project without events", {
  fx <- make_repeat_classic()

  raw <- suppressWarnings(rd_transform(fx))
  expect_named(raw, c("data", "dictionary", "results"))

  by_form <- suppressWarnings(rd_transform(fx, final_format = "by_form", delete_pattern = c("_complete", "_timestamp")))
  expect_equal(by_form$data$form, c("demo", "ae"))

  one_form <- suppressWarnings(rd_transform(fx, final_format = "by_form", which_form = "ae", delete_pattern = c("_complete", "_timestamp")))
  expect_equal(nrow(one_form$data), 3)
})

test_that("rd_insert_na() explains the mistakes in vars and filter", {
  x <- covican
  x$data$extra <- 1

  expect_error(rd_insert_na(covican, vars = "copd", filter = c("age > 1", "age < 3")), "exactly one filter", class = "REDCapDM_error_argument")
  expect_error(rd_insert_na(x, vars = "copd", filter = "extra == 1"), "not found in dictionary", class = "REDCapDM_error_argument")
  expect_error(rd_insert_na(covican, vars = "copd", filter = "age + 1"), "logical vector", class = "REDCapDM_error_argument")
  expect_error(rd_insert_na(covican, vars = "nope", filter = "age > 50"), "not found in data", class = "REDCapDM_error_argument")
  expect_error(rd_insert_na(x, vars = "extra", filter = "age > 50"), "not found in dictionary", class = "REDCapDM_error_argument")
})

test_that("rd_insert_na() checks the events of the variables and of the filter", {
  # demographics (age) only in the follow-up, comorbidities (copd) only at baseline
  x <- covican
  x$event_form <- x$event_form[x$event_form$form != "demographics", ]
  x$event_form <- rbind(x$event_form, data.frame(arm_num = 1, unique_event_name = "follow_up_visit_da_arm_1", form = "demographics"))

  expect_error(rd_insert_na(x, vars = "copd", filter = "age > 50"), "do not overlap", class = "REDCapDM_error_argument")
  expect_error(rd_insert_na(x, vars = "copd", filter = "age > 50 & copd == 1"), "different events", class = "REDCapDM_error")
  expect_warning(rd_insert_na(covican, vars = "potassium", filter = "age > 50"), "more events than the filter")
})
