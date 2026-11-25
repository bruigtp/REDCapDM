# pick numeric variables that exist in the dictionary
numeric_vars <- names(covican$data)[sapply(covican$data, is.numeric)]
vars_in_dic <- intersect(numeric_vars, covican$dictionary$field_name)
stopifnot(length(vars_in_dic) >= 2)  # ensure we have at least two numeric fields

# subset to a single event to bypass longitudinal requirement for most tests
df_single <- covican$data[covican$data$redcap_event_name == "baseline_visit_arm_1", ]

test_that("rd_insert_na inserts NA when filter condition is met (using event_form)", {
  df <- head(df_single, 6)

  # choose two numeric fields that are present in the dictionary
  v1 <- vars_in_dic[1]
  v2 <- vars_in_dic[2]

  # build dic with actual form names from covican$dictionary
  dic <- covican$dictionary[covican$dictionary$field_name %in% c(v1, v2),
                            c("field_name", "form_name")]
  names(dic) <- c("field_name", "form_name")
  dic$form_name <- as.character(dic$form_name)
  dic$field_name <- as.character(dic$field_name)

  # pick a threshold value that is not NA (2nd non-NA value of v1 in df)
  non_na_idx <- which(!is.na(df[[v1]]))
  expect_true(length(non_na_idx) >= 2)
  threshold_value <- df[[v1]][non_na_idx[2]]

  filter_expr <- paste0(v1, " < ", threshold_value)

  # The filter can match no row in these records (rd_insert_na() then warns)
  result <- suppressWarnings(rd_insert_na(
    data = df,
    dic = dic,
    vars = v2,
    filter = filter_expr,
    event_form = covican$event_form
  ))

  # construct expected result
  expected <- df[[v2]]
  expected[which(df[[v1]] < threshold_value)] <- NA

  expect_equal(result$data[[v2]], expected)
})

test_that("rd_insert_na errors if data or dic missing", {
  df <- head(df_single, 3)
  v1 <- vars_in_dic[1]

  dic_one <- data.frame(field_name = v1, form_name = covican$dictionary$form_name[
    covican$dictionary$field_name == v1], stringsAsFactors = FALSE)

  expect_error(
    rd_insert_na(dic = dic_one, vars = v1, filter = paste0(v1, " == 1")),
    "Both `data` and `dic`"
  )
  expect_error(
    rd_insert_na(data = df, vars = v1, filter = paste0(v1, " == 1")),
    "Both `data` and `dic`"
  )
})

test_that("rd_insert_na leaves data unchanged when filter matches no rows (with event_form)", {
  df <- head(df_single, 3)
  v1 <- vars_in_dic[1]
  v2 <- vars_in_dic[2]

  dic <- covican$dictionary[covican$dictionary$field_name %in% c(v1, v2),
                            c("field_name", "form_name")]
  names(dic) <- c("field_name", "form_name")
  dic$form_name <- as.character(dic$form_name)
  dic$field_name <- as.character(dic$field_name)

  # an impossible filter so no rows match
  filter_expr <- paste0(v1, " > 1e12")
  expect_warning(
    result <- rd_insert_na(
      data = df,
      dic = dic,
      vars = v2,
      filter = filter_expr,
      event_form = covican$event_form
    ),
    "No row meets the filter"
  )

  expect_equal(result$data, df)  # nothing should change
})

test_that("rd_insert_na errors if longitudinal but event_form missing", {
  # ensure we have multiple events: take first two rows of the full dataset (they are different events)
  df_multi <- head(covican$data, 2)
  v1 <- vars_in_dic[1]

  dic <- data.frame(field_name = v1,
                    form_name = covican$dictionary$form_name[
                      covican$dictionary$field_name == v1],
                    stringsAsFactors = FALSE)

  # calling without event_form on a longitudinal dataset should error
  expect_error(
    rd_insert_na(
      data = df_multi, dic = dic,
      vars = v1,
      filter = paste0(v1, " == 1")
    ),
    "The data is longitudinal (it has the column `redcap_event_name`), but the `event_form` mapping was not provided.",
    fixed = TRUE
  )
})


test_that("rd_insert_na errors if filter variables not in data", {
  df <- head(df_single, 3)
  v1 <- vars_in_dic[1]
  dic <- data.frame(field_name = v1, form_name = covican$dictionary$form_name[
    covican$dictionary$field_name == v1], stringsAsFactors = FALSE)

  # Filter uses a non-existent variable
  expect_error(
    rd_insert_na(
      data = df, dic = dic,
      vars = v1,
      filter = "nonexistent_var == 1",
      event_form = covican$event_form
    ),
    "Filter variable\\(s\\) not found in data"
  )
})

test_that("rd_insert_na errors if filter variables not in dictionary", {
  df <- head(df_single, 3)
  v1 <- vars_in_dic[1]
  dic <- data.frame(field_name = v1, form_name = covican$dictionary$form_name[
    covican$dictionary$field_name == v1], stringsAsFactors = FALSE)

  # Filter uses a variable not in dic
  expect_error(
    rd_insert_na(
      data = df, dic = dic,
      vars = v1,
      filter = paste0(v1, " < 100 & missing_in_dic < 1"),
      event_form = covican$event_form
    ),
    "Filter variable\\(s\\) not found in data"
  )
})

test_that("rd_insert_na warns when variable is present in more events than the filter", {

  # Create a filter referencing both variables
  filter_expr <- "inc_1== 1 & copd== 1"

  expect_warning(
    rd_insert_na(
      data = covican$data, dic = covican$dictionary,
      vars = "resp_rate", filter = filter_expr,
      event_form = covican$event_form
    ),
    "Only rows in common events"
  )
})

test_that("rd_insert_na increments results numbering for multiple transformations", {
  df <- head(df_single, 6)
  v1 <- vars_in_dic[1]
  v2 <- vars_in_dic[2]
  dic <- covican$dictionary[covican$dictionary$field_name %in% c(v1, v2),
                            c("field_name", "form_name")]
  names(dic) <- c("field_name", "form_name")
  dic$form_name <- as.character(dic$form_name)
  dic$field_name <- as.character(dic$field_name)

  filter1 <- paste0(v1, " >= 0")
  filter2 <- paste0(v1, " < 100")

  result <- rd_insert_na(
    data = df, dic = dic,
    vars = v2,
    filter = filter1,
    event_form = covican$event_form
  )

  # Apply second transformation
  result2 <- rd_insert_na(
    data = result$data, dic = dic,
    vars = v2,
    filter = filter2,
    event_form = covican$event_form
  )

  expect_true(!grepl("1\\.", result$results))
})

test_that("rd_insert_na reapplies variable labels after transformation", {

  filter_expr <- "inc_1== 1 & resp_rate== 1"

  # No record meets the filter (rd_insert_na() then warns)
  result <- suppressWarnings(rd_insert_na(
      data = covican$data, dic = covican$dictionary,
      vars = "copd", filter = filter_expr,
      event_form = covican$event_form
  ))

  expect_equal(labelled::var_label(result$data)[["type_dm"]], "Type of diabetes")
})



# ---- Non-longitudinal projects ----

test_that("rd_insert_na works in non-longitudinal projects", {
  data <- data.frame(record_id = 1:4, age = c(30, 70, 45, 80), smoker = c(1, 0, 1, 1))
  attr(data$smoker, "label") <- "Smoker"
  dic <- data.frame(field_name = c("record_id", "age", "smoker"), form_name = "demo")

  res <- rd_insert_na(data = data, dic = dic, vars = "smoker", filter = "age > 60")

  expect_equal(res$data$smoker, c(1, NA, 1, NA), ignore_attr = TRUE)
  expect_equal(attr(res$data$smoker, "label"), "Smoker")
  expect_null(attr(res$data$age, "label"))
  expect_false("event_form" %in% names(res))
})

test_that("rd_insert_na accepts filters with R functions, parentheses and objects of the session", {
  na_dm <- function(filter) suppressWarnings(rd_insert_na(covican, vars = "dm", filter = filter))$data$dm
  expected <- na_dm("age > 70")
  for (filter in c("abs(age) > 70", "(age > 70)", "((age > 70))", "round(age) > 70", "dplyr::between(age, 70.000001, Inf)")) {
    expect_equal(na_dm(filter), expected, info = filter)
  }
  limit <- 70
  expect_equal(na_dm("age > limit"), expected)
  # A variable that doesn't exist is still an error
  expect_error(rd_insert_na(covican, vars = "dm", filter = "abs(no_variable) > 1"), "not found in data")
})

test_that("rd_insert_na accepts columns chosen with objects of the session and logical vectors of the session", {
  na_dm <- function(filter) suppressWarnings(suppressMessages(rd_insert_na(covican, vars = "dm", filter = filter)))$data$dm
  expected <- na_dm("age > 30")
  field <- "age"
  cutoff <- 30
  for (filter in c(".data[[field]] > cutoff", "data[[field]] > cutoff", ".data[['age']] > cutoff", ".data$age > cutoff")) {
    expect_equal(na_dm(filter), expected, info = filter)
  }
  # A logical vector of the session (the events of the variable are kept)
  eligible <- !is.na(covican$data$age) & covican$data$age > 30
  expect_equal(na_dm("eligible"), expected)
  short <- c(TRUE, FALSE)
  expect_error(rd_insert_na(covican, vars = "dm", filter = "short"), "did not return a logical vector", class = "REDCapDM_error_argument")
  # A column chosen with an object that isn't one is still an error
  field <- "no_variable"
  expect_error(rd_insert_na(covican, vars = "dm", filter = ".data[[field]] > 1"), "not found in data")
})

test_that("rd_insert_na doesn't take the components of objects of the session for variables", {
  na_dm <- function(filter) suppressWarnings(suppressMessages(rd_insert_na(covican, vars = "dm", filter = filter)))$data$dm
  expected <- na_dm("age > 30")
  settings <- list(threshold = 30)
  for (filter in c("age > settings$threshold", "age > .env$settings$threshold", "age > settings[['threshold']]", "age > .env$settings[['threshold']]")) {
    expect_equal(na_dm(filter), expected, info = filter)
  }
  expect_error(rd_insert_na(covican, vars = "dm", filter = "no_variable > settings$threshold"), "not found in data")
  # Neither the arguments of a function written in the filter, nor the empty arguments of `[`
  expect_equal(na_dm("sapply(age, function(v) isTRUE(v > 30))"), expected)
  expect_equal(na_dm("data[, 'age'] > 30"), expected)
})

test_that("rd_insert_na doesn't take the arguments of the formulas of purrr for variables", {
  na_dm <- function(filter) suppressWarnings(suppressMessages(rd_insert_na(covican, vars = "dm", filter = filter)))$data$dm
  cutoff <- 30
  expected <- na_dm("age > cutoff")
  for (filter in c("purrr::map_lgl(age, ~ isTRUE(.x > cutoff))", "purrr::map_lgl(age, ~ isTRUE(..1 > cutoff))", "purrr::map2_lgl(age, cutoff, ~ isTRUE(.x > .y))")) {
    expect_equal(na_dm(filter), expected, info = filter)
  }
  expect_error(rd_insert_na(covican, vars = "dm", filter = "purrr::map_lgl(no_variable, ~ .x > 1)"), "not found in data")
})


# ---- Identifier in the filter ----

test_that("rd_insert_na takes the identifier as present in every event", {
  for (id in c("record_id", "subject_id")) {
    project <- make_custom_id(id)

    res <- rd_insert_na(project, vars = "value", filter = paste0(id, " == '003'"))
    expect_equal(res$data$value, c(NA, 5, NA, NA, NA))

    res <- rd_insert_na(project, vars = "value", filter = paste0(id, " == '003' & value > 6"))
    expect_equal(res$data$value, c(NA, 5, NA, NA, NA))
  }

  # The other variables of the filter still choose the events
  expect_error(rd_insert_na(make_custom_id(), vars = "value", filter = "subject_id == '003' & age > 50"), "do not overlap")
})

test_that("rd_insert_na checks the events of the columns chosen with [ in the filter, as with $ and [[", {
  data <- data.frame(record_id = c("1", "1", "2", "2"), redcap_event_name = c("a_arm_1", "b_arm_1", "a_arm_1", "b_arm_1"),
                     age = NA, x = c(NA, 5, NA, 6))
  dic <- data.frame(field_name = c("record_id", "age", "x"), form_name = c("fa", "fa", "fb"), field_type = "text", field_label = c("ID", "Age", "X"))
  ef <- data.frame(arm_num = 1, unique_event_name = c("a_arm_1", "a_arm_1", "b_arm_1"), form = c("fa", "base", "fb"))
  run <- function(filter) suppressWarnings(suppressMessages(rd_insert_na(data = data, dic = dic, event_form = ef, vars = "x", filter = filter)))
  # `age` is only in the event a, and `x` only in b: the values 5 and 6 of x are kept
  for (filter in c("is.na(age)", "is.na(data$age)", "is.na(data[['age']])", "is.na(data[, 'age'])", "is.na(data[, 3])", "is.na(data[, c('age')])")) {
    expect_error(run(filter), "do not overlap")
  }
  # (a column of the events of x)
  expect_equal(run("data[, 'x'] %in% 5")$data$x, c(NA, NA, NA, 6))
})
