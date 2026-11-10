make_missplot_data <- function() {
  df <- data.frame(
    record_id = 1:6,
    age = c(20, NA, 22, NA, NA, 25),           # 3 missing
    weight = c(70, 71, NA, NA, NA, NA),        # 4 missing
    height = c(160, 161, 162, 163, 164, 165),  # 0 missing -> fully complete
    sex = c("1", "2", "1", "2", "1", "2"),     # 0 missing -> fully complete
    stringsAsFactors = FALSE
  )

  # weight's field_label is deliberately NA: regression fixture for the
  # nzchar(NA) crash that used to happen in get_label_mapping().
  dic <- data.frame(
    field_name = c("record_id", "age", "weight", "height", "sex"),
    field_label = c("Record ID", "Age (years)", NA, "Height (cm)", "Sex"),
    stringsAsFactors = FALSE
  )

  list(data = df, dic = dic)
}

make_longitudinal_missplot_data <- function() {
  # Note the raw event order of appearance in the data is visit_2, visit_1 --
  # the opposite of the order declared in event_form -- so that a test can
  # tell whether event_form's order is actually being honored.
  df <- data.frame(
    record_id = c(1, 1, 2, 2),
    redcap_event_name = c("visit_2_arm_1", "visit_1_arm_1", "visit_2_arm_1", "visit_1_arm_1"),
    age = c(NA, 30, 31, NA),
    weight = c(70, NA, NA, 72),
    stringsAsFactors = FALSE
  )

  dic <- data.frame(
    field_name = c("record_id", "age", "weight"),
    field_label = c("Record ID", "Age (years)", NA),
    stringsAsFactors = FALSE
  )

  event_form <- data.frame(
    unique_event_name = c("visit_1_arm_1", "visit_2_arm_1"),
    form = "form1",
    stringsAsFactors = FALSE
  )

  list(data = df, dictionary = dic, event_form = event_form)
}

test_that("rd_missplot errors when data is neither a data frame nor a list with a 'data' element", {
  expect_error(rd_missplot(1:5), "data frame")
})

test_that("rd_missplot errors when the list provided lacks a 'data' element", {
  expect_error(rd_missplot(list(foo = 1)), "'data' element")
})

test_that("rd_missplot does not crash on a NA field_label and falls back to the variable name", {
  fx <- make_missplot_data()

  p <- suppressMessages(rd_missplot(list(data = fx$data, dictionary = fx$dic), drop_complete = FALSE))

  expect_s3_class(p, "ggplot")
  expect_true("weight" %in% as.character(p$data$display_label))
})

test_that("drop_complete = TRUE messages and filters out fully complete variables", {
  fx <- make_missplot_data()

  expect_message(
    p <- rd_missplot(list(data = fx$data, dictionary = fx$dic), use_field_labels = FALSE),
    "fully complete"
  )

  vars_shown <- as.character(p$data$variable)
  expect_false("height" %in% vars_shown)
  expect_false("sex" %in% vars_shown)
  expect_true("age" %in% vars_shown)
  expect_true("weight" %in% vars_shown)
})

test_that("max_vars limits the number of variables plotted to the ones with the most missingness", {
  fx <- make_missplot_data()

  p <- suppressMessages(
    rd_missplot(list(data = fx$data, dictionary = fx$dic), use_field_labels = FALSE, max_vars = 1)
  )

  expect_equal(nrow(p$data), 1)
  expect_equal(as.character(p$data$variable), "weight")
})

test_that("longitudinal heatmap orders events using event_form, not first appearance in the data", {
  fx <- make_longitudinal_missplot_data()

  p <- suppressMessages(rd_missplot(fx, use_field_labels = FALSE))

  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$clean_event_name), c("Visit 2", "Visit 1"))
})

test_that("longitudinal heatmap with sort_by_missingness = FALSE and drop_complete = TRUE does not error", {
  fx <- make_longitudinal_missplot_data()

  p <- suppressMessages(
    rd_missplot(fx, sort_by_missingness = FALSE, use_field_labels = FALSE)
  )

  expect_s3_class(p, "ggplot")
})
