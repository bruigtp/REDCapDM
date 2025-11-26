test_that("Error if data or dic missing", {
  expect_error(rd_dictionary(data = NULL, dic = covican$dictionary),
               "Both `data` and `dic`")
  expect_error(rd_dictionary(data = covican$data, dic = NULL),
               "Both `data` and `dic`")
})

test_that("Returns correct list structure with covican", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_true(all(c("data", "dictionary", "event_form") %in% names(result)))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$dictionary, "data.frame")
  expect_s3_class(result$event_form, "data.frame")
})

test_that("Factor variables are correctly transformed in branching logic", {
  dic <- covican$dictionary
  data <- covican$data

  # Force one branching logic for testing (simulate REDCap condition)
  dic$branching_logic_show_field_only_if[2] <- "[inc_1]='1'"

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_true(grepl("\\inc_1\\=='1'",
                    result$dictionary$branching_logic_show_field_only_if[2]))
})

test_that("Handles failed branching logic gracefully", {
  dic <- covican$dictionary
  dic$branching_logic_show_field_only_if[5] <- "FAIL_logic"
  dic$choices_calculations_or_slider_labels[5] <- "FAIL_calc"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  expect_true(any(grepl("exc_1", result$results)))
})

test_that("Event form is preserved", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_equal(result$event_form, covican$event_form)
})

test_that("Dictionary replacements are applied consistently", {
  dic <- covican$dictionary
  data <- covican$data

  dic$branching_logic_show_field_only_if[3] <- "[inc_2]='0'"
  dic$branching_logic_show_field_only_if[4] <- "[dm]='1'"

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_true(all(grepl("data$|==",
                        result$dictionary$branching_logic_show_field_only_if[3:4])))
})

test_that("Handles case when no factor variables exist", {
  data <- covican$data
  dic <- covican$dictionary

  # Drop factor variables
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_s3_class(result$dictionary, "data.frame")
  expect_equal(nrow(result$dictionary), nrow(dic))
})

test_that("Correctly transforms branching logic with multiple factors", {
  dic <- covican$dictionary
  dic$branching_logic_show_field_only_if[2] <- "[inc_1]='1' and [dm]='1'"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  logic <- result$dictionary$branching_logic_show_field_only_if[2]
  expect_true(grepl("data\\$inc_1=='1'", logic))
  expect_true(grepl("data\\$dm=='1'", logic))
})


test_that("Calculation fields are transformed if valid", {
  dic <- covican$dictionary
  dic$field_type[2] <- "calc"
  dic$choices_calculations_or_slider_labels[2] <- "[age] + 1"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  calc <- result$dictionary$choices_calculations_or_slider_labels[2]
  expect_false(grepl("FAIL", calc))
})


test_that("Output list has no NULL elements", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_false(any(vapply(result, is.null, logical(1))))
})


test_that("Works with project-style input (list)", {
  project <- list(
    data = covican$data,
    dictionary = covican$dictionary,
    event_form = covican$event_form
  )

  result <- rd_dictionary(project)

  expect_true(all(c("data", "dictionary", "event_form") %in% names(result)))
})
