test_that("basic factor conversion works", {
  result <- covican |>
    rd_factor()

  # Factor variables exist
  expect_true(is.factor(result$data$dm))
  expect_true(is.factor(result$data$copd))

  # .factor columns removed
  expect_false("dm.factor" %in% colnames(result$data))
  expect_false("copd.factor" %in% colnames(result$data))

  # redcap_event_name.factor is kept
  expect_true("redcap_event_name.factor" %in% colnames(result$data))
})

test_that("exclude argument prevents conversion", {
  result <- covican |>
    rd_factor(exclude = "dm")

  # dm is not converted
  expect_true(is.numeric(result$data$dm) || is.integer(result$data$dm))
  # copd is still converted
  expect_true(is.factor(result$data$copd))
})

test_that("exclude with .factor suffix throws an error", {
  expect_error(
    result <- rd_factor(covican, exclude = "dm.factor"),
    "Please use the original form of the variable"
  )
})

test_that("warning is issued if no factor variables exist", {
  data_no_factor <- covican$data |>  select(-c(ends_with(".factor") & !starts_with("redcap")))
  expect_warning(
    rd_factor(data = data_no_factor, dic = covican$dictionary),
    "There are no variables in the data which can be converted to factors"
  )
})

test_that("labels are preserved", {
  result <- rd_factor(covican)

  expect_match(attr(result$data$dm, "label"), "Diabetes")
  expect_match(attr(result$data$copd, "label"), "Chronic pulmonary disease")
})

test_that("event_form is returned unchanged", {
  result <- rd_factor(covican)
  expect_equal(result$event_form, covican$event_form)
})
