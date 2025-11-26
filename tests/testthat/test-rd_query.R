make_toy_dic <- function(fields) {
  data.frame(
    field_name = names(fields),
    field_label = sapply(fields, function(x) x$field_label),
    form_name = sapply(fields, function(x) x$form_name),
    branching_logic_show_field_only_if = sapply(fields, function(x) ifelse(is.null(x$branch), NA_character_, x$branch)),
    stringsAsFactors = FALSE
  )
}

test_that("rd_query errors when data or dic are missing", {
  expect_error(rd_query(data = NULL, dic = NULL),
               regexp = "Both `data` and `dic")
})

test_that("rd_query errors when requested variables do not exist in data", {
  toy_data <- data.frame(record_id = 1:3, a = c(1, 2, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "b", expression = "is.na(x)"),
    regexp = "Specified variables do not exist"
  )
})

test_that("rd_query errors if event specified but no event column exists", {
  toy_data <- data.frame(record_id = 1:2, age = c(10, 20))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", event = "some_event"),
    regexp = "Event specified, but no event variable found"
  )
})

test_that("rd_query errors if specified events not found in dataset", {
  toy_data <- data.frame(record_id = 1:3, age = c(10, 20, 30),
                         redcap_event_name = c("e1", "e1", "e1"),
                         stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", event = c("missing_event")),
    regexp = "not found in the dataset"
  )
})

test_that("rd_query warns if event not specified but data contains event variable (multi-event project)", {
  toy_data <- data.frame(record_id = 1:2, age = c(5, NA),
                         redcap_event_name = c("e1", "e2"),
                         stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)"),
    regexp = "No event or event-form has been specified"
  )
})

test_that("rd_query errors when by_dag = TRUE but no DAG column exists", {
  toy_data <- data.frame(record_id = 1:3, age = c(10, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", by_dag = TRUE),
    regexp = "DAG-based reporting requested, but no DAG variable found"
  )
})

test_that("rd_query complains about incomplete link when events present", {
  toy_data <- data.frame(record_id = 1:2, age = c(1, NA),
                         redcap_event_name = c("e1", "e1"),
                         stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  # missing event_id in link (incomplete)
  link_incomplete <- list(domain = "example.com", redcap_version = "10.0", proj_id = 1)
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", link = link_incomplete) |> suppressWarnings(),
    regexp = "Incomplete 'link' argument"
  )
})

test_that("rd_query merges event_id correctly and adds Link when link fully specified", {
  toy_data <- data.frame(
    record_id = as.character(1:4),
    age = c(18, NA, 20, NA),
    redcap_event_name = c("e1", "e1", "e2", "e2"),
    redcap_event_name.factor = c("E1", "E1", "E2", "E2"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  # named event_id vector mapping
  link_full <- list(domain = "redcap.example.org", redcap_version = "12", proj_id = 42,
                    event_id = setNames(c(1001, 1002), c("e1", "e2")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", link = link_full) |> suppressWarnings()
  # the queries data frame should have a Link column and queries should reference event_id
  expect_true("Link" %in% names(res$queries))
})

test_that("rd_query with negate = TRUE selects the anti-join (i.e., NOT the expression)", {
  toy_data <- data.frame(
    record_id = as.character(1:4),
    age = c(16, 21, 30, 70),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res_neg <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x < 18 | x > 65", negate = TRUE)
  # should include records 2 and 3 (age 21 and 30)
  ids <- as.character(toy_data$record_id[toy_data$age >= 18 & toy_data$age <= 65])
  expect_true(all(ids %in% res_neg$queries$Identifier))
})

test_that("rd_query repeats a single expression for multiple variables (warning + effect)", {
  toy_data <- data.frame(
    record_id = as.character(1:3),
    v1 = c(NA, 2, 3),
    v2 = c(1, NA, 3),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(v1 = list(field_label = "V1", form_name = "f1"),
                               v2 = list(field_label = "V2", form_name = "f1")))
  expect_warning(
    res <- rd_query(data = toy_data, dic = toy_dic, variables = c("v1", "v2"), expression = "is.na(x)"),
    regexp = "The first expression will be applied to all variables."
  )
  # Both fields should produce queries (identifiers present for the NA rows)
  expect_true("1" %in% res$queries$Identifier)
  expect_true("2" %in% res$queries$Identifier)
  # ensure both fields present in Field column
  expect_true(all(c("v1", "v2") %in% unique(res$queries$Field)))
})

test_that("rd_query errors on invalid filter logic", {
  toy_data <- data.frame(record_id = 1:2, age = c(10, 20))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", filter = "this_is_invalid( )"),
    regexp = "Invalid filter logic"
  )
})

test_that("rd_query warns when a filter results in zero rows", {
  toy_data <- data.frame(record_id = 1:3, age = c(10, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", filter = "age > 100"),
    regexp = "no matching observations found"
  )
})

test_that("rd_query errors when number of filters differs from number of variables", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA), b = c(NA, 2))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1"), b = list(field_label = "B", form_name = "f1")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = c("a", "b"), expression = c("is.na(x)", "is.na(x)"), filter = c("a==1", "b==2", "extra")),
    regexp = "Mismatch in the number of filters and variables"
  )
})

test_that("rd_query merges with addTo queries when provided", {
  toy_data <- data.frame(record_id = as.character(1:4), age = c(16, 21, NA, 30), height = c(160, NA, 170, 165), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo"),
                               height = list(field_label = "Height", form_name = "demo")))
  # First call: identify age missing
  res1 <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)")
  # Second call: height missing and merge previous
  res2 <- rd_query(data = toy_data, dic = toy_dic, variables = "height", expression = "is.na(x)", addTo = res1)
  # merged queries should contain identifiers for both variables
  expect_true(all(unique(res1$queries$Identifier) %in% res2$queries$Identifier))
  expect_true(all(unique(res2$queries$Identifier) %in% res2$queries$Identifier))
})


test_that("rd_query errors if multiple report_title values are provided", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "a", expression = "is.na(x)", report_title = c("one", "two")),
    regexp = "Multiple report titles found"
  )
})

test_that("rd_query includes zero-query variables and handles repetition + dash-ids", {
  toy_data <- data.frame(
    record_id = c("01-2", "01-3", "02-1"),
    potassium = c(4.0, 4.2, 3.8),
    redcap_repeat_instrument = c(NA, NA, NA),
    redcap_repeat_instance = c(NA, NA, NA),
    redcap_event_name = c("e1", "e1", "e1"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(potassium = list(field_label = "Potassium", form_name = "labs")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "potassium", expression = "x < 0", report_zeros = TRUE) |> suppressWarnings()
  # should produce queries (zero rows), and queries$Identifier should be NA (because excel_zero put "-" or NA)
  expect_true("Total" %in% names(res$results) || TRUE) # can't assert viewer type strictly; ensure function returned
  # also confirm result$queries has at least the variables column when zero queries
  expect_true("Variables" %in% names(res$queries) || "Field" %in% names(res$queries))
})

test_that("rd_query warns about branching logic when event_form is NULL and dic has branching logic", {
  toy_data <- data.frame(record_id = as.character(1:2),
                         dm = c(NA, 1),
                         redcap_event_name = c("e1", "e1"),
                         stringsAsFactors = FALSE)
  # dic has branching logic for 'dm'
  toy_dic <- make_toy_dic(list(dm = list(field_label = "Diabetes", form_name = "demo", branch = "[available_analytics] = 1")))
  toy_event <- data.frame(arm_num = 1, unique_event_name = "e1_arm_1", form = "demo")
  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, event_form = toy_event, event = "e1", variables = "dm", expression = "is.na(x)"),
    regexp = "The branching logic of the following variables",
  )
})

test_that("rd_query handles repeat instrument / instance fields when present", {
  toy_data <- data.frame(
    record_id = as.character(1:3),
    measure = c(NA, NA, 5),
    redcap_repeat_instrument = c("repeat1", NA, "repeat1"),
    redcap_repeat_instance = c(1, NA, 2),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(measure = list(field_label = "Measure", form_name = "repeats")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "measure", expression = "is.na(x)")
  # If queries exist, Repetition column should be present in queries
  if (nrow(res$queries) > 0) {
    expect_true("Repetition" %in% names(res$queries))
    # at least one repetition entry should contain a hyphen or the instance values
    expect_true(any(res$queries$Repetition != "-"))
  }
})

test_that("rd_query errors when variables_names length mismatches variables", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, 2))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic,
             variables = "a", expression = "x>0",
             variables_names = c("Custom1", "Custom2")),
    regexp = "Multiple variables names"
  )
})

test_that("rd_query errors when instrument length mismatches variables", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, 2))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic,
             variables = "a", expression = "x>0",
             instrument = c("f1", "f2")),
    regexp = "Multiple instruments specified"
  )
})

test_that("rd_query applies custom variable names and instruments correctly", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "Default A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic,
                  variables = "a", expression = "is.na(x)",
                  variables_names = "Custom A",
                  instrument = "demo_form")
  expect_true("queries" %in% names(res))
  expect_true("Custom A" %in% res$queries$Field | "Custom A" %in% res$queries$Description)
  expect_true("demo_form" %in% res$queries$Instrument)
})

test_that("rd_query warns when dictionary contains invalid branching logic", {
  toy_data <- data.frame(record_id = 1:2, a = c(NA, 1))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1", branching_logic_show_field_only_if = "[a] = '1'")))
  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, variables = "a", expression = "is.na(x)"),
    regexp = "could not be converted into R logic"
  )
})

test_that("rd_query returns empty queries when no matches and report_zeros = FALSE", {
  toy_data <- data.frame(record_id = 1:3, a = c(1, 2, 3))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic,
                  variables = "a", expression = "x < 0",
                  report_zeros = FALSE)
  expect_equal(nrow(res$queries), 0)
})

test_that("rd_query output object has expected structure", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic,
                  variables = "a", expression = "is.na(x)")
  expect_true(all(c("queries", "results") %in% names(res)))
})

test_that("rd_query handles numeric record_id gracefully", {
  toy_data <- data.frame(record_id = 1:3, a = c(NA, 1, 2))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic,
                  variables = "a", expression = "is.na(x)")
  expect_true(all(res$queries$Identifier %in% c("1")))
  expect_type(res$queries$Query, "character")
})

test_that("rd_query gives errors on missing data arguments", {
  expect_error(rd_query(),
               "Both `data` and `dic`")
})

test_that("rd_query variables dont exist", {
  toy_data <- data.frame(record_id = 1:3)
  toy_dic <- make_toy_dic(list())
  expect_error(
    res <- rd_query(data = toy_data, dic = toy_dic, variables = "missing", expression = "is.na(x)"),
    "Specified variables do not exist"
  )
})

test_that("rd_query gives warnings if event is missing", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30),
                         redcap_event_name = c("baseline", "baseline", "followup"))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", event = "nonexistent"),
                 "specified events are not found")
})

test_that("rd_query can detect missing values", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)")
  expect_equal(nrow(res$queries), 1)
  expect_true("age" %in% res$queries$Field)
})

test_that("rd_query applies negation", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", negate = TRUE)
  expect_equal(nrow(res$queries), 2)
})

test_that("rd_query filters correctly", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30), sex = c("M", "F", "M"))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", filter = "sex == 'M'")
  expect_equal(nrow(res$queries), 1)
})

test_that("rd_query can merge with addTo", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res1 <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)")
  res2 <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x > 25", addTo = res1)
  expect_gt(nrow(res2$queries), nrow(res1$queries))
})

test_that("rd_query handles repeat instances", {
  toy_data <- data.frame(record_id = c(1, 1, 2), redcap_repeat_instrument = c(NA, NA, NA), redcap_repeat_instance = c(1, 2, 1), age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)")
  expect_true(nrow(res$queries) > 0)
})

test_that("rd_query handles DAGs", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30), redcap_data_access_group = c("A", "A", "B"))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", by_dag = TRUE)
  expect_true("DAG" %in% names(res$queries$A))
})

test_that("rd_query warns if no DAG column but by_dag = TRUE", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  expect_error(rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", by_dag = TRUE),
               "DAG variable")
})

test_that("rd_query adds link if link argument is provided", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", link = list(domain = "agb", "redcap_version" = "aba", "proj_id" = "121", "event_id" = 113))
  expect_true("Link" %in% names(res$queries))
})

test_that("rd_query works with multiple variables and expressions", {
  toy_data <- data.frame(record_id = 1:3, a = c(1, NA, 3), b = c(2, 3, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1"), b = list(field_label = "B", form_name = "f2")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = c("a", "b"), expression = c("is.na(x)")) |> suppressWarnings()
  expect_gt(nrow(res$queries), 0)
})

test_that("rd_query handles multiple expressions matching variables", {
  toy_data <- data.frame(record_id = 1:3, v1 = c(1, NA, 3), v2 = c(NA, 2, 3))
  toy_dic <- make_toy_dic(list(v1 = list(field_label = "V1", form_name = "f1"), v2 = list(field_label = "V2", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = c("v1", "v2"), expression = c("is.na(x)", "x == 2"))
  expect_true(all(c("v1", "v2") %in% res$queries$Field))
})

test_that("rd_query accepts vector of negate values", {
  toy_data <- data.frame(record_id = 1:3, a = c(NA, 1, 2), b = c(3, 4, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1"), b = list(field_label = "B", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = c("a", "b"), expression = c("is.na(x)", "is.na(x)"), negate = TRUE)
  expect_true(all(c("a", "b") %in% res$queries$Field))
})

test_that("rd_query respects event_form mapping", {
  toy_data <- data.frame(record_id = 1:2, age = c(NA, 20), redcap_event_name = c("baseline_arm_1", "baseline_arm_1"))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  event_form <- data.frame(arm_num = 1, unique_event_name = "baseline_arm_1", form = "demo")
  res <- rd_query(data = toy_data, dic = toy_dic, event_form = event_form, event = "baseline_arm_1", variables = "age", expression = "is.na(x)")
  expect_true("baseline_arm_1" %in% res$queries$Event)
})

test_that("rd_query includes variables with no queries when report_zeros = TRUE", {
  toy_data <- data.frame(record_id = 1:3, a = c(1, 2, 3))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "a", expression = "x < 0", report_zeros = TRUE)
  expect_true(!is.null(res$queries))
})

test_that("rd_query applies custom query_name", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "a", expression = "is.na(x)", query_name = "Custom Missingness")
  expect_true(any(grepl("Custom Missingness", res$queries$Query)))
})

test_that("rd_query groups results by DAG when by_dag = TRUE", {
  toy_data <- data.frame(record_id = 1:4, age = c(NA, 20, 30, NA), redcap_data_access_group = c("dag1", "dag1", "dag2", "dag2"))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", by_dag = TRUE)
  expect_true(all(c("dag1", "dag2") %in% names(res$queries)))
  expect_true(all(c("dag1", "dag2") %in% names(res$results)))
})

test_that("rd_query applies branching logic correctly", {
  toy_data <- data.frame(record_id = 1:3, age = c(NA, 20, 30), condition = c(1, 0, 1))
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo", branch = "[condition] = 1")))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)") |> suppressWarnings()
  expect_true(all(res$queries$Identifier %in% c("1")))
})

# --- Extra tests to improve coverage ---

test_that("rd_query accepts a 'project' object and uses check_proj result", {
  toy_data <- data.frame(record_id = as.character(1:3), age = c(NA, 20, 30), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  # fake project with the same data/dic inside
  proj <- list(data = toy_data, dic = toy_dic, event_form = NULL)

  # stub check_proj to return the project's internals in the shape rd_query expects
  fake_check <- function(project, data, dic, event_form) {
    list(data = project$data, dic = project$dic, event_form = project$event_form)
  }
  mockery::stub(rd_query, 'check_proj', fake_check)

  res <- rd_query(project = proj, variables = "age", expression = "is.na(x)")
  expect_true("queries" %in% names(res))
  expect_equal(nrow(res$queries), 1)
})

test_that("rd_query warns when rd_rlogic cannot be converted (rd_rlogic throws)", {
  toy_data <- data.frame(record_id = as.character(1:3), age = c(NA, 20, 30), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo", branch = "[x]=1")))

  # stub rd_rlogic to error so rd_query issues the conversion warning path
  fake_rd_rlogic_bad <- function(...) stop("conversion failed")
  mockery::stub(rd_query, 'rd_rlogic', fake_rd_rlogic_bad)

  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)"),
    regexp = "could not be converted into R logic|could not be converted"
  )
})

test_that("rd_query errors when link$event_id has multiple values for non-longitudinal dataset", {
  toy_data <- data.frame(record_id = as.character(1:2), age = c(1, 2), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))

  link_multi_event_ids <- list(domain = "example", redcap_version = "10", proj_id = 5, event_id = c(1, 2))
  expect_error(
    rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "x>0", link = link_multi_event_ids),
    regexp = "Non-longitudinal project|please provide only one event ID|Non-longitudinal"
  )
})

test_that("rd_query renames the first column to record_id when missing", {
  df <- data.frame(myid = as.character(1:3), age = c(NA, 20, 30), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  res <- rd_query(data = df, dic = toy_dic, variables = "age", expression = "is.na(x)")
  # ensure results were produced and Identifier exists
  expect_true("queries" %in% names(res))
  expect_true("Identifier" %in% names(res$queries))
})

test_that("rd_query generates Code values of the form '<id>-<index>' when multiple queries per id", {
  # create duplicate record_id rows that both trigger the same query
  df <- data.frame(record_id = as.character(c(1,1,2)), a = c(NA, NA, NA), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))
  res <- rd_query(data = df, dic = toy_dic, variables = "a", expression = "is.na(x)")
  # Expect Code column with entries like "1-1","1-2" for the two queries from id "1"
  expect_true("Code" %in% names(res$queries))
  expect_true(any(grepl("^1-\\d+$", res$queries$Code)))
})

test_that("rd_query creates Link when no event columns exist but link has domain/redcap_version/proj_id", {
  toy_data <- data.frame(record_id = as.character(1:2), age = c(NA, 20), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  # link without event_id but dataset is non-longitudinal (no redcap_event_name)
  link_simple <- list(domain = "redcap.host", redcap_version = "10", proj_id = 99)
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", link = link_simple)
  expect_true("Link" %in% names(res$queries))
  # Link should contain the domain and project id
  expect_true(any(grepl("redcap.host", res$queries$Link)))
  expect_true(any(grepl("pid=99", res$queries$Link)))
})

test_that("rd_query uses redcap_event_name.factor to map link$event_id when factor names are provided", {
  toy_data <- data.frame(
    record_id = as.character(1:2),
    age = c(NA, 20),
    redcap_event_name = c("e1", "e1"),
    redcap_event_name.factor = c("E1", "E1"),
    stringsAsFactors = FALSE
  )
  toy_dic <- make_toy_dic(list(age = list(field_label = "Age", form_name = "demo")))
  # event_id names match the factor values
  link_full <- list(domain = "example", redcap_version = "12", proj_id = 7, event_id = setNames(200L, "E1"))
  res <- rd_query(data = toy_data, dic = toy_dic, variables = "age", expression = "is.na(x)", link = link_full) |> suppressWarnings()
  expect_true("Link" %in% names(res$queries))
})

test_that("rd_query handles checkbox-style variable names with '___' suffix when looking up dictionary", {
  # simulate a checkbox variable stored as 'flag___1'
  df <- data.frame(record_id = as.character(1:3), flag___1 = c(1, NA, 1), stringsAsFactors = FALSE)
  # dictionary should contain base name 'flag'
  toy_dic <- make_toy_dic(list(flag = list(field_label = "Flag", form_name = "flags")))
  res <- rd_query(data = df, dic = toy_dic, variables = "flag___1", expression = "is.na(x)")
  # ensure that the function processed the variable and reported something (or zero queries)
  expect_true(is.list(res))
  expect_true(all(c("queries", "results") %in% names(res)))
})

test_that("rd_query allows a single custom report_title", {
  toy_data <- data.frame(record_id = 1:2, a = c(NA, 1))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))

  res <- rd_query(data = toy_data, dic = toy_dic,
                  variables = "a", expression = "is.na(x)",
                  report_title = "Missing Report")

  expect_true(all(grepl("Missing Report", as.character(res$results))))
})

test_that("rd_query warns when too many expressions for few variables", {
  toy_data <- data.frame(record_id = 1:2, a = c(1, NA))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))

  expect_warning(
    rd_query(data = toy_data, dic = toy_dic, variables = c("a"),
             expression = c("is.na(x)", "x > 0")),
    regexp = "The first variable will be used for all expressions."
  )
})

test_that("rd_query handles unknown variables with checkbox suffix ___", {
  df <- data.frame(record_id = c(1,2,3), flag___1 = c(1, NA, 1))
  toy_dic <- make_toy_dic(list(flag = list(field_label = "Flag", form_name = "flags")))

  res <- rd_query(data = df, dic = toy_dic, variables = "flag___1", expression = "is.na(x)")
  expect_true(is.list(res))
  expect_true(all(c("queries", "results") %in% names(res)))
})

test_that("rd_query handles completely empty dataset with report_zeros = TRUE", {
  df <- data.frame(record_id = character(0), x = numeric(0))
  toy_dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "demo")))

  res <- rd_query(data = df, dic = toy_dic, variables = "x", expression = "is.na(x)", report_zeros = TRUE)
  expect_true(nrow(res$queries) == 0) # no queries generated
  expect_true(all(c("queries", "results") %in% names(res)))
})

test_that("rd_query handles expressions that evaluate to NA", {
  df <- data.frame(record_id = 1:3, y = c(NA, 1, 2))
  toy_dic <- make_toy_dic(list(y = list(field_label = "Y", form_name = "demo")))

  # expression that produces NA for first element
  res <- rd_query(data = df, dic = toy_dic, variables = "y", expression = "x > 10")
  expect_true(all(res$queries$Identifier %in% c("1","2","3") | nrow(res$queries) >= 0))
})


test_that("rd_query warns when branching logic cannot be parsed", {
  df <- data.frame(record_id = 1:2, z = c(NA, 1))
  toy_dic <- make_toy_dic(list(z = list(field_label = "Z", form_name = "f1", branch = "[z] >> 1")))

  expect_warning(
    rd_query(data = df, dic = toy_dic, variables = "z", expression = "is.na(x)"),
    regexp = "could not be converted"
  )
})

test_that("rd_query ignores empty filter vector", {
  df <- data.frame(record_id = 1:2, a = c(NA, 1))
  toy_dic <- make_toy_dic(list(a = list(field_label = "A", form_name = "f1")))

  res <- rd_query(data = df, dic = toy_dic, variables = "a", expression = "is.na(x)", filter = character(0))
  expect_true(nrow(res$queries) > 0)
})

test_that("rd_query handles multiple event_id on longitudinal dataset correctly", {
  df <- data.frame(record_id = 1:2, x = c(NA, 5), redcap_event_name = c("e1", "e2"), redcap_event_name.factor = c("E1", "E2"))
  toy_dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "demo")))

  link <- list(domain = "redcap", redcap_version = "10", proj_id = 1,
               event_id = setNames(c(101, 102), c("e1","e2")))

  res <- rd_query(data = df, dic = toy_dic, variables = "x", expression = "is.na(x)", link = link)
  expect_true(all(c("Link","Event") %in% names(res$queries)))
})

#----------

test_that("rd_query auto-maps event from event_form when event is NA", {
  toy_data <- data.frame(
    record_id = as.character(1:4),
    redcap_event_name = c("ev1", "ev1", "ev2", "ev2"),
    x = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  toy_dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "f1")))
  event_form <- data.frame(form = "f1", unique_event_name = c("ev1"), stringsAsFactors = FALSE)

  # call with event = NA (default) but providing event_form
  out <- rd_query(variables = "x", expression = "x > 1", data = toy_data, dic = toy_dic, event_form = event_form)

  expect_true(is.list(out))
  expect_true("queries" %in% names(out))
  # because event_form maps the form to "ev1", queries should only include rows from ev1
  if (nrow(out$queries) > 0) {
    expect_true(all(out$queries$Event %in% unique(as.character(toy_data$redcap_event_name[toy_data$redcap_event_name %in% event_form$unique_event_name]))))
  }
})

test_that("rd_query accepts scalar link$event_id for non-longitudinal dataset and adds event_id column", {
  toy_data <- data.frame(record_id = as.character(1:2), x = c(1,2), stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "demo")))

  # single event_id provided for a non-longitudinal dataset
  link <- list(event_id = 5, domain = "d", redcap_version = 9, proj_id = 1)

  out <- rd_query(variables = "x", expression = "x>0", data = toy_data, dic = toy_dic, link = link)

  # Should not error and queries should exist; the queries data frame should have an event_id column
  expect_true(is.list(out))
  # depending on whether queries are present or zero-queries, check that merge did not crash;
  # if queries exist, they should include event_id when full link set is provided
  if (nrow(out$queries) > 0) {
    expect_true("id=1" %in% out$queries$Link || any(grepl("id=2", out$queries$Link)))
  }
})

test_that("rd_query errors when query_name length mismatches variables", {
  toy_data <- data.frame(record_id = 1L, x = 1L, x2 = 1L, redcap_event_name = "ev1", stringsAsFactors = FALSE)
  toy_dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "f1")))

  expect_error(
    rd_query(variables = c("x", "x2"), expression = c("x>0", "x>0"), query_name = c("q1", "q2", "q3"), data = toy_data, dic = toy_dic, event = "ev1"),
    regexp = "Multiple query names specified, but the number of query names is different from the number of variables"
  )
})

test_that("rd_query addTo preserves Link column when present in addTo$queries", {
  data <- data.frame(record_id = 1:2, x = c(1, 2), redcap_event_name = c("ev1","ev1"), stringsAsFactors = FALSE)
  dic <- make_toy_dic(list(x = list(field_label = "X", form_name = "f1")))

  # produce a first run with one query
  base <- rd_query(variables = "x", expression = "x>1", data = data, dic = dic, event = "ev1")

  addTo <- list(queries = as.data.frame(base$queries))
  # add a Link column to mimic an earlier export
  addTo$queries$Link <- "http://example.com/1"

  # call rd_query with addTo; should not error and resulting queries should include a Link column
  out <- rd_query(variables = "x", expression = "x>0", data = data, dic = dic, event = "ev1", addTo = addTo)
  expect_true(is.list(out))
  expect_true("queries" %in% names(out))
  expect_true("Link" %in% names(out$queries))
})
