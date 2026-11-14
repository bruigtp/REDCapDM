test_that("conditions written as text are parsed, with commas meaning 'and'", {
  expect_length(parse_conditions("age > 50, dm == 1"), 2)
  expect_length(parse_conditions("age > 50 # adults"), 1)
  expect_length(parse_conditions(""), 0)
  expect_error(parse_conditions("age >> 50"))
  expect_error(parse_conditions("age > 1), (dm == 1"))
})

test_that("only the variable x is replaced in an expression", {
  expect_identical(replace_x(quote(x == "x"), "copd"), quote(copd == "x"))
  expect_identical(replace_x(quote(x > max_x), "age"), quote(age > max_x))
  expect_identical(replace_x(quote(is.na(x) | x > 2), "age"), quote(is.na(age) | age > 2))
})

test_that("an x inside a text value of the expression is kept", {
  data <- data.frame(record_id = 1:3, a = c("x", "y", "x"))
  dic <- data.frame(
    field_name = c("record_id", "a"), form_name = "f", field_type = "text", field_label = c("ID", "A"),
    choices_calculations_or_slider_labels = "", text_validation_type_or_show_slider_number = "",
    branching_logic_show_field_only_if = ""
  )

  res <- rd_query(data = data, dic = dic, variables = "a", expression = "x == 'x'")
  expect_equal(nrow(res$queries), 2)
})

test_that("filters and expressions find the objects of the function that calls them", {
  f <- function() {
    tope <- 70
    list(
      filter = rd_query(covican, variables = "copd", expression = "is.na(x)", event = "baseline_visit_arm_1", filter = "age > tope"),
      expression = rd_query(covican, variables = "age", expression = "x > tope", event = "baseline_visit_arm_1"),
      event = rd_event(covican, event = "follow_up_visit_da_arm_1", filter = "age > tope"),
      insert_na = rd_insert_na(covican, vars = "copd", filter = "age < tope")
    )
  }
  res <- suppressWarnings(f())

  expect_equal(res$filter, suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", event = "baseline_visit_arm_1", filter = "age > 70")))
  # The text of each query repeats the expression ("greater than tope"): the rest must be the same
  expected <- suppressWarnings(rd_query(covican, variables = "age", expression = "x > 70", event = "baseline_visit_arm_1"))
  expect_equal(dplyr::select(res$expression$queries, -"Query"), dplyr::select(expected$queries, -"Query"))
  expect_match(res$expression$queries$Query[1], "greater than tope")
  expect_equal(res$event, suppressWarnings(rd_event(covican, event = "follow_up_visit_da_arm_1", filter = "age > 70")))
  expect_equal(res$insert_na, suppressWarnings(rd_insert_na(covican, vars = "copd", filter = "age < 70")))
})

test_that("a filter can be the branching logic translated by rd_dictionary()", {
  dic_r <- suppressWarnings(rd_dictionary(covican))
  logic <- dic_r$dictionary$branching_logic_show_field_only_if[dic_r$dictionary$field_name == "type_dm"]
  expect_match(logic, "data$", fixed = TRUE)

  with_data <- suppressWarnings(rd_query(covican, variables = "type_dm", expression = "is.na(x)", filter = logic))
  without_data <- suppressWarnings(rd_query(covican, variables = "type_dm", expression = "is.na(x)", filter = gsub("data$", "", logic, fixed = TRUE)))
  expect_equal(with_data, without_data)
})

test_that("filters use the columns of the data, not the internal objects of the functions", {
  expect_error(
    suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", filter = "length(variables) == 1")),
    class = "REDCapDM_error_argument"
  )
})

test_that("each of several filters explains its mistake", {
  expect_error(
    rd_query(covican, variables = c("copd", "age"), expression = c("is.na(x)", "x > 60"), event = "baseline_visit_arm_1", filter = c("dm == 1", "agee > 1")),
    "Invalid filter logic for the variable `age`",
    class = "REDCapDM_error_argument"
  )
  expect_error(rd_event(covican, event = "follow_up_visit_da_arm_1", filter = "agee > 50"), "agee", class = "REDCapDM_error_argument")
})
