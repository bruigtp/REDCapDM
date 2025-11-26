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

  result <- rd_insert_na(
    data = df,
    dic = dic,
    vars = v2,
    filter = filter_expr,
    event_form = covican$event_form
  )

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
  result <- rd_insert_na(
    data = df,
    dic = dic,
    vars = v2,
    filter = filter_expr,
    event_form = covican$event_form
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
    "The dataset contains multiple events, but the `event_form` mapping was not provided. Please specify it."
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

  result <- rd_insert_na(
      data = covican$data, dic = covican$dictionary,
      vars = "copd", filter = filter_expr,
      event_form = covican$event_form
  )

  expect_equal(labelled::var_label(result$data)[["type_dm"]], "Type of diabetes")
})
