test_that("rd_delete_vars removes specified vars and updates dictionary in covican", {
  out <- rd_delete_vars(
    data = covican$data,
    dic = covican$dictionary,
    vars = c("potassium")
  )

  expect_false("potassium" %in% names(out$data))
  expect_false("potassium" %in% out$dictionary$field_name)
  # label for a remaining variable is preserved
  expect_equal(attr(out$data$leuk_lymph, "label"), "Leukaemia or Lymphoma (include myeloma)")
})

test_that("rd_delete_vars removes by pattern and dictionary updated in covican", {
  # covican has no `_factor`/`_complete` variables: nothing is removed, with a warning
  expect_warning(
    out <- rd_delete_vars(
      data = covican$data,
      dic = covican$dictionary,
      pattern = c("_factor$", "_complete$")
    ),
    "No variable matches the pattern"
  )

  # factor/complete variables should be gone
  expect_false(any(grepl("_factor$|_complete$", names(out$data))))
  expect_false(any(grepl("_factor$|_complete$", out$dictionary$field_name)))

  # original variable still present with label
  expect_true("dm" %in% names(out$data))
  expect_equal(attr(out$data$dm, "label"), "Diabetes (treated with insulin or antidiabetic oral drugs)")
})

test_that("rd_delete_vars errors if var not present in covican", {
  expect_error(
    rd_delete_vars(
      data = covican$data,
      dic = covican$dictionary,
      vars = c("missing_var")
    ),
    "The following variables are not present in the dataset"
  )
})

test_that("rd_delete_vars warns about factor-suffixed columns in covican", {
  # artificially add a factor-suffixed column to mimic REDCap conversion
  df <- covican$data
  df[["copd.factor"]] <- covican$data$copd.factor

  dic <- covican$dictionary
  dic <- rbind(dic, tibble::tibble(
    field_name = "copd.factor",
    form_name = "comorbidities",
    section_header = "",
    field_type = "text",
    field_label = "COPD factor",
    choices_calculations_or_slider_labels = "",
    field_note = "",
    text_validation_type_or_show_slider_number = "",
    text_validation_min = "",
    text_validation_max = "",
    identifier = "",
    branching_logic_show_field_only_if = "",
    required_field = "",
    custom_alignment = "",
    question_number_surveys_only = "",
    matrix_group_name = "",
    matrix_ranking = "",
    field_annotation = ""
  ))

  expect_warning(
    rd_delete_vars(data = df, dic = dic, pattern = c("^copd$")),
    "The dataset contains factor versions of variables matching the specified patterns"
  )
})


# ---- forms ----

test_that("rd_delete_vars removes every variable of a form", {
  lab_fields <- covican$dictionary$field_name[covican$dictionary$form_name == "laboratory_findings"]
  out <- rd_delete_vars(project = covican, forms = "laboratory_findings")

  expect_false(any(lab_fields %in% names(out$data)))
  expect_false(any(paste0(lab_fields, ".factor") %in% names(out$data)))
  expect_false(any(lab_fields %in% out$dictionary$field_name))
  expect_false("laboratory_findings" %in% out$dictionary$form_name)
  expect_false("laboratory_findings" %in% out$event_form$form)

  # the rest is untouched
  expect_equal(nrow(out$data), nrow(covican$data))
  expect_true(all(c("record_id", "redcap_event_name", "copd", "copd.factor") %in% names(out$data)))
  expect_equal(nrow(out$dictionary), nrow(covican$dictionary) - length(lab_fields))
  expect_equal(attr(out$data$dm, "label"), "Diabetes (treated with insulin or antidiabetic oral drugs)")
})

test_that("rd_delete_vars never removes the record identifier with forms", {
  out <- rd_delete_vars(project = covican, forms = "inclusionexclusion_criteria")

  expect_true("record_id" %in% names(out$data))
  expect_true("record_id" %in% out$dictionary$field_name)
  expect_false("inc_1" %in% names(out$data))
  expect_false("inc_1" %in% out$dictionary$field_name)
})

test_that("rd_delete_vars removes checkboxes, recalculated, _complete fields and repeating rows of a form", {
  data <- data.frame(
    record_id = c(1, 1, 2),
    redcap_repeat_instrument = c(NA, "visits", NA),
    redcap_repeat_instance = c(NA, 1, NA),
    age = c(50, NA, 60),
    age_recalc = c(50, NA, 61),
    symp___1 = c(1, NA, 0),
    symp___1.factor = c("Checked", NA, "Unchecked"),
    demo_complete = c(2, NA, 2),
    visit_date = c(NA, "2024-01-01", NA),
    visits_complete = c(NA, 2, NA)
  )
  dic <- data.frame(
    field_name = c("record_id", "age", "age_recalc", "symp", "visit_date"),
    form_name = c("demo", "demo", "demo", "demo", "visits")
  )

  out <- rd_delete_vars(data = data, dic = dic, forms = "demo")
  expect_equal(names(out$data), c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", "visit_date", "visits_complete"))
  expect_equal(out$dictionary$field_name, c("record_id", "visit_date"))
  expect_equal(nrow(out$data), 3)

  out <- rd_delete_vars(data = data, dic = dic, forms = "visits")
  expect_false(any(c("visit_date", "visits_complete") %in% names(out$data)))
  # the rows of the removed repeating instrument are dropped
  expect_equal(nrow(out$data), 2)
  expect_true(all(is.na(out$data$redcap_repeat_instrument)))
})

test_that("rd_delete_vars combines forms with vars", {
  out <- rd_delete_vars(project = covican, forms = "laboratory_findings", vars = "copd")

  expect_false(any(c("copd", "potassium") %in% names(out$data)))
  expect_false(any(c("copd", "potassium") %in% out$dictionary$field_name))
})

test_that("rd_delete_vars errors with unknown forms or without vars, pattern or forms", {
  expect_error(
    rd_delete_vars(project = covican, forms = c("laboratory_findings", "labs")),
    "The following forms are not present in the dictionary: \"labs\"",
    class = "REDCapDM_error_argument"
  )
  expect_error(
    rd_delete_vars(project = covican),
    "At least one of the `vars`, `pattern` or `forms` arguments must be provided",
    class = "REDCapDM_error_argument"
  )
})


test_that("rd_delete_vars also removes the `_mdc` columns of redcap_data(rm_mdc = FALSE)", {
  mdc <- covican
  mdc$data <- dplyr::mutate(covican$data, potassium_mdc = ifelse(is.na(potassium), "UNK", NA), .after = "potassium")

  out <- rd_delete_vars(mdc, vars = "potassium")
  expect_false(any(c("potassium", "potassium_mdc") %in% names(out$data)))

  out <- rd_delete_vars(mdc, forms = "laboratory_findings")
  expect_false(any(c("potassium", "potassium_mdc") %in% names(out$data)))

  # deleting another variable keeps it
  out <- rd_delete_vars(mdc, vars = "fio2")
  expect_true("potassium_mdc" %in% names(out$data))
})


test_that("rd_delete_vars never removes the record identifier", {
  expect_error(rd_delete_vars(covican, vars = "record_id"), "`record_id` can't be removed")

  # A pattern that matches the identifier keeps it
  expect_warning(out <- rd_delete_vars(covican, pattern = "^record"), "nothing was removed")
  expect_true("record_id" %in% names(out$data))
  expect_true("record_id" %in% out$dictionary$field_name)
})
