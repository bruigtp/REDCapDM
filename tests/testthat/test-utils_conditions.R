test_that("the errors of wrong arguments have classes and show the function that the user called", {
  err <- expect_error(rd_query(covican, variables = "copd", expression = "is.na(x)", negate = "yes"), class = "REDCapDM_error_argument")

  expect_s3_class(err, "REDCapDM_error")
  expect_match(conditionMessage(err), "`negate` must be TRUE or FALSE, not a string")
  expect_identical(rlang::call_name(err$call), "rd_query")
})

test_that("the errors explain what is wrong with the value", {
  expect_error(check_strings(c("a", NA), "vars"), "It has missing or empty values")
  expect_error(check_strings(1, "vars"), "It is a number")
  expect_error(rd_split(list(data = 1, dictionary = covican$dictionary)), "`data` must be a data frame, not a number")
  expect_error(rd_dates(covican$data), "`project` must be the list returned by `redcap_data\\(\\)`")
})

test_that("the messages can be hidden with options(REDCapDM.quiet = TRUE)", {
  expect_message(rd_inform("Hello"), "Hello", class = "REDCapDM_message")

  old <- options(REDCapDM.quiet = TRUE)
  on.exit(options(old), add = TRUE)
  expect_silent(rd_inform("Hello"))
  expect_silent(suppressWarnings(rd_query(covican, variables = "copd", expression = "x > 1000")))
})

test_that("the warnings have a class", {
  expect_warning(rd_warn("Careful"), "Careful", class = "REDCapDM_warning")
})

test_that("redcap_data() and rd_query() signal classed conditions", {
  expect_error(redcap_data(data_path = "no_such_file.r", dic_path = "no_such_dic.csv"), class = "REDCapDM_error_file")
  expect_error(redcap_data(data_path = "a.r", dic_path = "d.csv", uri = "u", token = "t"), class = "REDCapDM_error_argument")
  expect_error(rd_query(covican, variables = "agee", expression = "is.na(x)"), class = "REDCapDM_error_argument")
  expect_error(rd_query(covican, variables = "copd", expression = "x >> 3"), class = "REDCapDM_error_argument")

  no_event_form <- covican[c("data", "dictionary")]
  expect_warning(rd_query(no_event_form, variables = "copd", expression = "is.na(x)"), "No event or event-form", class = "REDCapDM_warning")
})

test_that("rd_query() informs, without a warning, when it applies the branching logic", {
  fx <- make_cross_event()

  expect_no_warning(
    expect_message(rd_query(fx, variables = "preg", expression = "is.na(x)"), "applied automatically", class = "REDCapDM_message")
  )
})
