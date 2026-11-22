test_that("the argument checks accept valid values and name the argument otherwise", {
  expect_silent(check_flag(TRUE, "x"))
  expect_error(check_flag(NA, "x"), "`x` must be TRUE or FALSE")
  expect_error(check_flag(c(TRUE, FALSE), "x"), "`x` must be TRUE or FALSE")

  expect_silent(check_string("a", "x"))
  expect_silent(check_string(NULL, "x", null_ok = TRUE))
  expect_silent(check_string(NA, "x", na_ok = TRUE))
  expect_error(check_string(c("a", "b"), "x"), "`x` must be a single text value")
  expect_error(check_string("", "x"), "`x` must be a single text value")
  expect_error(check_string(list(), "x", na_ok = TRUE), "`x` must be a single text value")

  expect_silent(check_strings(c("a", "b"), "x"))
  expect_silent(check_strings(NULL, "x"))
  expect_error(check_strings(NULL, "x", null_ok = FALSE), "`x` must be a character vector")
  expect_error(check_strings(c("a", NA), "x"), "`x` must be a character vector")
  expect_error(check_strings(data.frame(), "x"), "`x` must be a character vector")

  expect_silent(check_text(NA, "x"))
  expect_silent(check_text(c("a", "b"), "x"))
  expect_error(check_text(NULL, "x"), "`x` must be text")

  expect_equal(check_choice("ev", "x", c("none", "eval"), partial = TRUE), "eval")
  expect_error(check_choice("ev", "x", c("none", "eval")), "`x` should be one of \"none\" or \"eval\"")
  expect_error(check_choice(NA, "x", c("none", "eval")), "should be one of")

  expect_silent(check_number(5, "x"))
  expect_silent(check_number(NULL, "x"))
  expect_error(check_number(0, "x"), "`x` must be a number greater than or equal to 1")
  expect_error(check_number("5", "x"), "`x` must be a number")

  expect_error(check_inputs(as.matrix(covican$data), covican$dictionary), "`data` must be a data frame")
  expect_error(check_inputs(covican$data, "dic"), "`dic` must be a data frame")
  expect_error(check_inputs(covican$data, covican$dictionary[, 1, drop = FALSE], dic_cols = "form_name"), "doesn't have the column `form_name`")
})

test_that("the functions explain wrong argument values instead of failing with an R error", {
  expect_error(redcap_data(data_path = c("a.r", "b.r"), dic_path = "d.csv"), "`data_path` must be a single text value")
  expect_error(redcap_data(data_path = "a.r", dic_path = "d.csv", sep = ";;"), "`sep` must be a single character")
  expect_error(suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", query_name = NULL)), "`query_name` must be text")
  expect_error(suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", link = "x")), "`link` must be a list")
  expect_error(suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", add_to = data.frame())), "`add_to` must be the output")
  expect_error(rd_event(covican, event = "follow_up_visit_da_arm_1", report_zeros = NA), "`report_zeros` must be TRUE or FALSE")
  expect_error(rd_event(covican, event = "follow_up_visit_da_arm_1", filter = c("a", "b")), "`filter` must be a single text value")
  expect_error(rd_rlogic(covican, logic = NA, var = "age"), "`logic` must be a character vector")
  expect_error(rd_split(covican, wide = "yes"), "`wide` must be TRUE or FALSE")
  expect_error(rd_transform(covican, na_logic = "all"), "`na_logic` should be one of")
  expect_error(rd_transform(covican, delete_pattern = ""), "`delete_pattern` must be a character vector")
  expect_error(rd_checkbox(covican, checkbox_names = NA), "`checkbox_names` must be TRUE or FALSE")
  expect_error(rd_missplot(covican, show_percentages = "no"), "`show_percentages` must be TRUE or FALSE")
  expect_error(rd_missplot(covican, max_label_length = 2), "`max_label_length` must be a number")
  expect_error(rd_delete_vars(covican, forms = data.frame()), "`forms` must be a character vector")
  expect_error(rd_factor(covican, exclude = 1), "`exclude` must be a character vector")
  expect_error(rd_export(queries = list(a = data.frame(x = 1))), "`queries` must be a data frame")
  expect_error(rd_export(queries = data.frame()), "has no columns")
  expect_error(check_queries(old = data.frame(), new = data.frame(), return_viewer = NA), "`return_viewer` must be TRUE or FALSE")
  expect_error(rd_dates(list(data = as.matrix(covican$data), dictionary = covican$dictionary)), "`data` must be a data frame")
})

test_that("rd_delete_vars keeps the REDCap columns of events and repetitions when a pattern matches them", {
  expect_warning(out <- rd_delete_vars(covican, pattern = "_name"), "nothing was removed")
  expect_true(all(c("redcap_event_name", "redcap_event_name.factor") %in% names(out$data)))
})

test_that("rd_dates and rd_rlogic work with a dataset without rows", {
  empty <- covican
  empty$data <- empty$data[0, ]

  expect_equal(nrow(suppressWarnings(rd_dates(empty))$data), 0)
  expect_length(suppressWarnings(rd_rlogic(empty, logic = "[age] > 70", var = "age"))$eval, 0)
})


test_that("the functions name the dictionary columns they need", {
  no_col <- function(col) {
    x <- covican
    x$dictionary[[col]] <- NULL
    x
  }

  expect_error(rd_dates(no_col("text_validation_type_or_show_slider_number")), "`text_validation_type_or_show_slider_number`")
  expect_error(rd_recalculate(no_col("field_type")), "doesn't have the column `field_type`")
  expect_error(rd_checkbox(no_col("branching_logic_show_field_only_if")), "`branching_logic_show_field_only_if`")
  expect_error(rd_rlogic(no_col("field_name"), logic = "[age] > 70", var = "age"), "`field_name`")
  expect_error(rd_split(no_col("field_name")), "`field_name`")
  expect_error(rd_delete_vars(no_col("field_name"), vars = "age"), "`field_name`")
  expect_error(rd_insert_na(no_col("form_name"), vars = "age", filter = "copd == 1"), "`form_name`")
  expect_error(suppressWarnings(rd_query(no_col("field_label"), variables = "age", expression = "is.na(x)")), "`field_label`")
  expect_error(rd_transform(no_col("field_label")), "`field_label`")

  empty_dic <- covican
  empty_dic$dictionary <- empty_dic$dictionary[0, ]
  expect_error(rd_split(empty_dic), "The dictionary has no rows")
})

test_that("rd_split names the variables of the data that are not in the dictionary", {
  x <- covican
  x$data$extra_var <- 1
  err <- expect_error(rd_split(x), class = "REDCapDM_error_data")
  expect_match(conditionMessage(err), "not\\s+found\\s+in\\s+the\\s+dictionary")
  expect_match(conditionMessage(err), "`extra_var`", fixed = TRUE)
})


test_that("the reports work without kableExtra", {
  table <- knitr::kable(data.frame(a = 1), format = "html")

  mockery::stub(style_report, "requireNamespace", FALSE)
  expect_identical(style_report(table), table)
})

test_that("grouped data and dictionaries with factor columns give the same results", {
  ref_recalc <- suppressWarnings(suppressMessages(rd_recalculate(covican)))
  ref_transform <- suppressWarnings(suppressMessages(rd_transform(covican)))

  grouped <- covican
  grouped$data <- dplyr::group_by(grouped$data, record_id)
  out <- suppressWarnings(suppressMessages(rd_recalculate(grouped)))
  expect_false(dplyr::is_grouped_df(out$data))
  expect_equal(names(out$data), names(ref_recalc$data))
  expect_equal(dim(suppressWarnings(suppressMessages(rd_transform(grouped)))$data), dim(ref_transform$data))

  factor_dic <- covican
  factor_dic$dictionary[] <- lapply(factor_dic$dictionary, factor)
  out <- suppressWarnings(suppressMessages(rd_recalculate(factor_dic)))
  expect_equal(names(out$data), names(ref_recalc$data))
  expect_equal(dim(suppressWarnings(suppressMessages(rd_transform(factor_dic)))$data), dim(ref_transform$data))
})

test_that("clean_label removes the HTML tags and keeps the rest of the text", {
  expect_equal(clean_label("<b>Potassium</b> &amp; <i>K+</i>"), "Potassium & K+")
  expect_equal(clean_label("Age (<18 or >90 years: check)"), "Age (<18 or >90 years: check)")
  expect_equal(clean_label("x<5 & y>3"), "x<5 & y>3")
  expect_equal(clean_label("<span style=\"color:red\">Red</span><br/>text"), "Red text")
  expect_equal(clean_label("<p class=x>Word<o:p></o:p></p>"), "Word")
  expect_equal(clean_label("<!-- note -->Value &lt;5"), "Value <5")
  expect_equal(clean_label(c(NA, "")), c(NA, ""))
  # Tags inside a line are removed without adding spaces; tags that break the line become a space
  expect_equal(clean_label("Name (<i>full</i>)"), "Name (full)")
  expect_equal(clean_label("word<b>s</b>"), "words")
  expect_equal(clean_label("Age<br>(years)"), "Age (years)")
  expect_equal(clean_label("<div>A</div><div>B</div>"), "A B")
})

test_that("the functions explain an event_form that is not the event-form mapping", {
  for (ef in list(1, "x", TRUE, data.frame(), data.frame(event = "a"))) {
    expect_error(rd_query(data = covican$data, dic = covican$dictionary, event_form = ef, variables = "copd", expression = "is.na(x)"), class = "REDCapDM_error_argument")
    expect_error(rd_transform(data = covican$data, dic = covican$dictionary, event_form = ef), class = "REDCapDM_error_argument")
    expect_error(rd_rlogic(data = covican$data, dic = covican$dictionary, event_form = ef, logic = "[age] > 70", var = "age"), class = "REDCapDM_error_argument")
    expect_error(rd_insert_na(data = covican$data, dic = covican$dictionary, event_form = ef, vars = "copd", filter = "age > 70"), class = "REDCapDM_error_argument")
  }
  expect_silent(check_event_form(NULL))
  expect_silent(check_event_form(covican$event_form))
})

test_that("rd_dates and rd_recalculate work with some of the columns of the dictionary", {
  d <- covican$data[, c("record_id", "redcap_event_name", "redcap_event_name.factor", "age", "copd", "copd.factor")]
  expect_no_error(suppressMessages(rd_dates(data = d, dic = covican$dictionary, event_form = covican$event_form)))
  expect_no_error(suppressWarnings(suppressMessages(rd_recalculate(data = d, dic = covican$dictionary, event_form = covican$event_form))))
})

test_that("the arguments that receive a function by mistake stop with an explanation", {
  expect_error(rd_event(data = mean, dic = covican$dictionary, event = "x"), "must be a data frame", class = "REDCapDM_error_argument")
  expect_error(rd_query(covican, variables = mean, expression = "is.na(x)"), "`variables` must be text", class = "REDCapDM_error_argument")
  expect_error(rd_query(covican, variables = "age", expression = mean), "`expression` must be text", class = "REDCapDM_error_argument")
  expect_error(rd_split(covican, by = mean), "Invalid `by` argument", class = "REDCapDM_error_argument")
  # a factor is still taken as text
  expect_equal(nrow(rd_query(covican, variables = factor("age"), expression = "is.na(x)", event = "baseline_visit_arm_1")$queries), 5)
})
