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

test_that("the rest of the functions also signal classed conditions", {
  expect_error(rd_delete_vars(project = covican, forms = "labs"), class = "REDCapDM_error_argument")
  expect_warning(rd_recalculate(covican, exclude = "copd"), class = "REDCapDM_warning")
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    expect_error(
      rd_export(queries = data.frame(a = 1), path = file.path(tempdir(), "no_such_folder", "q.xlsx")),
      class = "REDCapDM_error_file"
    )
  }

  x <- covican
  x$data$extra_var <- 1
  expect_error(rd_split(x), class = "REDCapDM_error_data")

  fx <- make_repeat_classic()
  expect_error(rd_rlogic(fx, logic = "[ae_grade] = '1'", var = "age"), class = "REDCapDM_error_logic")
})

test_that("rd_insert_na() keeps the R error of a wrong filter and names the function", {
  err <- expect_error(rd_insert_na(covican, vars = "copd", filter = "age >"), class = "REDCapDM_error_argument")
  expect_s3_class(err$parent, "error")
  expect_identical(rlang::call_name(err$call), "rd_insert_na")

  err <- expect_error(rd_insert_na(covican, vars = "copd", filter = "age > 'a' + 1"), class = "REDCapDM_error_argument")
  expect_match(conditionMessage(err$parent), "non-numeric argument")
  expect_identical(rlang::call_name(err$call), "rd_insert_na")
})

test_that("rd_query() informs, without a warning, when it applies the branching logic", {
  fx <- make_cross_event()

  expect_no_warning(
    expect_message(rd_query(fx, variables = "preg", expression = "is.na(x)"), "applied automatically", class = "REDCapDM_message")
  )
})
