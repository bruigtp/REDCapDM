test_that("function stops if data or dic is missing", {
  expect_error(rd_transform(data = NULL, dic = covican$dictionary),
               "Both `data` and `dic`")
  expect_error(rd_transform(data = covican$data, dic = NULL),
               "Both `data` and `dic`")
})

test_that("final_format argument validation works", {
  expect_error(rd_transform(data = covican$data, dic = covican$dictionary, final_format = "invalid"),
               "final_format argument has to be one of")
})

test_that("which_event and which_form validation works", {
  expect_error(rd_transform(data = covican$data, dic = covican$dictionary, which_event = "baseline_visit_arm_1"),
               "Which event has been specified but the final format is not to split the data by event")
  expect_error(rd_transform(data = covican$data, dic = covican$dictionary, which_form = unique(covican$dictionary$form_name)[1]),
               "Which form has been specified but the final format is not to split the data by form")
})

test_that("wide argument validation works", {
  expect_error(rd_transform(data = covican$data, dic = covican$dictionary, wide = TRUE),
               "The argument wide has been specified but the final format is not to split the data by form")
})

test_that("deleting variables works", {
  # pick a variable that exists
  delete_var <- intersect("inc_1", names(covican$data))
  skip_if(length(delete_var) == 0, "No variable available to delete")
  res <- covican |>
    rd_transform(delete_vars = delete_var) |>
    suppressMessages() |>
    suppressWarnings()
  expect_false(delete_var %in% names(res$data))
})

test_that("checkbox transformation occurs", {
  res <- covican |>
    rd_transform() |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(any(!grepl("___1", res$data)))
  expect_true(any(!grepl("___1", res$dictionary$field_name)))
})

test_that("recalculation occurs when applicable", {
  res <- covican |>
    rd_transform() |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(any(grepl("_recalc", names(res$data))))
})

test_that("branching logic is transformed", {
  res <- covican |>
    rd_transform() |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(any(grepl("Converting every branching logic in the dictionary into R logic", res$results)))
})

test_that("final output contains required components", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  res <- covican |>
    rd_transform() |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(all(c("data", "dictionary", "event_form", "results") %in% names(res)))
})

test_that("final_format 'by_event' works", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  res <- covican |>
    rd_transform(final_format = "by_event") |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(is.list(res$data))
})

test_that("final_format 'by_form' works", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  res <- covican |>
    rd_transform(final_format = "by_form") |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(is.list(res$data))
})

test_that("function works when project argument is provided", {
  res <- rd_transform(project = covican) |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(is.list(res))
  expect_true("data" %in% names(res))
  expect_true("dictionary" %in% names(res))
})

test_that("deleting variables by pattern works", {
  res <- rd_transform(
    data = covican$data,
    dic = covican$dictionary,
    delete_pattern = "_complete$"
  ) |>
    suppressMessages() |>
    suppressWarnings()
  expect_false(any(grepl("_complete$", names(res$data))))
})

test_that("exclude_to_factor prevents conversion", {
  var_to_exclude <- covican$dictionary$field_name[1]
  res <- rd_transform(
    data = covican$data,
    dic = covican$dictionary,
    exclude_to_factor = var_to_exclude
  ) |>
    suppressMessages() |>
    suppressWarnings()
  expect_false(paste0(var_to_exclude, ".factor") %in% names(res$data))
})

test_that("final_format 'by_form' works with wide format", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  res <- covican |>
    rd_transform(final_format = "by_form", wide = TRUE) |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(is.list(res$data))
})

test_that("which_event extracts a single event", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  first_event <- covican$event_form$unique_event_name[1]
  res <- rd_transform(
    data = covican$data,
    dic = covican$dictionary,
    event_form = covican$event_form,
    final_format = "by_event",
    which_event = first_event
  ) |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(unique(res$data$redcap_event_name) == first_event)
})

test_that("which_form extracts a single form", {
  skip_if(is.null(covican$event_form), "covican$event_form not present")
  first_form <- unique(covican$dictionary$form_name)[1]
  res <- rd_transform(
    data = covican$data,
    dic = covican$dictionary,
    event_form = covican$event_form,
    final_format = "by_form",
    which_form = first_form
  ) |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(unique(res$data$redcap_event_name) == "baseline_visit_arm_1")
})

test_that("function handles repeated instruments", {
  first_form <- unique(covican$dictionary$form_name)[1]

  data <- covican$data |>
    dplyr::mutate(redcap_repeat_instrument = first_form,
                  redcap_repeat_instrument.factor = stringr::str_to_sentence(redcap_repeat_instrument))

  skip_if(!"redcap_repeat_instrument" %in% names(data), "No repeated instruments in covican")

  res <- rd_transform(data = data, dic = covican$dictionary, event_form = covican$event_form) |>
    suppressMessages() |>
    suppressWarnings()
  expect_true(is.list(res))
})
