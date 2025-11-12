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
  out <- rd_delete_vars(
    data = covican$data,
    dic = covican$dictionary,
    pattern = c("_factor$", "_complete$")
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

