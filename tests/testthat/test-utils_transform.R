test_that("recalculate works as expected", {

  dic <- tibble::tibble(
    field_name = c("a", "b", "c"),
    field_label = c("a", "b", "c"),
    field_type = c("calc", "calc", "calc"),
    text_validation_type_or_show_slider_number = "",
    choices_calculations_or_slider_labels = c("1+1", "2+2", "error"),
    branching_logic_show_field_only_if = ""
  )

  data <- tibble::tibble(
    a = 1:3,
    b = 2:4,
    c = 3:5
  )

  # Exclude one variable
  res <- suppressWarnings(recalculate(data = data, dic = dic, exclude_recalc = "b"))

  expect_true("a_recalc" %in% names(res$data))
  expect_false("b_recalc" %in% names(res$data))
})


test_that("transform_checkboxes works correctly", {
  dic <- tibble::tibble(
    field_name = c("chk1", "chk2"),
    field_type = c("checkbox", "checkbox"),
    branching_logic_show_field_only_if = c(NA, "error")
  )

  data <- tibble::tibble(
    chk1___1 = c("0", "1", "0"),
    chk1___2 = c("1", "0", "1"),
    chk2___1 = c("0", "0", "1")
  )

  res <- suppressWarnings(transform_checkboxes(data, dic, checkbox_na = TRUE))
  expect_true(all(c("chk1___1", "chk1___2") %in% names(res$data)))
  expect_true(length(res$results) > 0)
})


test_that("checkbox_names works and renames", {
  data <- tibble::tibble(chk___1 = 1:2, chk___2 = 2:3)
  dic <- tibble::tibble(field_name = "chk", field_label = "Check", choices_calculations_or_slider_labels = "0, No | 1, Yes", branching_logic_show_field_only_if = "")
  labels <- c("chk___1" = "choice=No)", "chk___2" = "choice=Yes)")

  res <- suppressWarnings(checkbox_names(data, dic, labels))
  expect_true(any(grepl("chk_", names(res$data))))
  expect_true(all(c("chk_no", "chk_yes") %in% res$dic$field_name))
})

test_that("split_event handles errors", {
  data <- tibble(a=1:2, record_id=1:2)
  dic <- tibble(field_name="a", field_type="text", form_name="f1")
  event_form <- tibble(form="f1", unique_event_name="e1")

  expect_error(suppressWarnings(split_event(data, dic, event_form))) # Missing redcap_event_name
})

test_that("split_form works with wide and longitudinal", {
  data <- tibble(record_id = 1:2, redcap_event_name="e1", a=1:2)
  dic <- tibble(field_name="a", form_name="f1", field_type="text")
  event_form <- tibble(form="f1", unique_event_name="e1")

  res <- suppressWarnings(split_form(data, dic, event_form, wide=TRUE))
  expect_true(is.list(res))
})

test_that("to_factor converts correctly", {
  data <- tibble(a = c("x","y"), a.factor=as.factor(a), redcap_event_name="e1", redcap_event_name.factor = "E1", redcap_repeat_instrument=NA, redcap_repeat_instrument.factor =NA)
  dic <- tibble(field_name="a", choices_calculations_or_slider_labels="1, x | 2, y", field_type="radio", branching_logic_show_field_only_if = "")

  res <- suppressWarnings(to_factor(data, dic))
  expect_true(is.factor(res$data$a))
})
