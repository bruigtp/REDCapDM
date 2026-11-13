test_that("basic factor conversion works", {
  result <- covican |>
    rd_factor()

  # Factor variables exist
  expect_true(is.factor(result$data$dm))
  expect_true(is.factor(result$data$copd))

  # .factor columns removed
  expect_false("dm.factor" %in% colnames(result$data))
  expect_false("copd.factor" %in% colnames(result$data))

  # redcap_event_name.factor is kept
  expect_true("redcap_event_name.factor" %in% colnames(result$data))
})

test_that("exclude argument prevents conversion", {
  result <- covican |>
    rd_factor(exclude = "dm")

  # dm is not converted
  expect_true(is.numeric(result$data$dm) || is.integer(result$data$dm))
  # copd is still converted
  expect_true(is.factor(result$data$copd))
})

test_that("exclude with .factor suffix throws an error", {
  expect_error(
    result <- rd_factor(covican, exclude = "dm.factor"),
    "Please use the original form of the variable"
  )
})

test_that("warning is issued if no factor variables exist", {
  data_no_factor <- covican$data |>  select(-c(ends_with(".factor") & !starts_with("redcap")))
  expect_warning(
    rd_factor(data = data_no_factor, dic = covican$dictionary),
    "There are no variables in the data which can be converted to factors"
  )
})

test_that("labels are preserved", {
  result <- rd_factor(covican)

  expect_match(attr(result$data$dm, "label"), "Diabetes")
  expect_match(attr(result$data$copd, "label"), "Chronic pulmonary disease")
})

test_that("event_form is returned unchanged", {
  result <- rd_factor(covican)
  expect_equal(result$event_form, covican$event_form)
})


# ---- Logic already translated to R ----

test_that("rd_factor warns when the dictionary has R logic that uses the converted variables", {
  dic_r <- suppressWarnings(rd_dictionary(covican))

  expect_warning(
    rd_factor(dic_r),
    "already translated to R uses the original codes of variables now converted to factors \\(.*dm.*\\)\\. Run rd_factor\\(\\) before rd_dictionary\\(\\)\\."
  )
})

test_that("rd_factor doesn't warn with the original dictionary or when the R logic doesn't use the converted variables", {
  expect_no_warning(rd_factor(covican))

  dic_r <- suppressWarnings(rd_dictionary(covican))
  logic_vars <- unique(unlist(stringr::str_extract_all(
    c(dic_r$dictionary$branching_logic_show_field_only_if, dic_r$dictionary$choices_calculations_or_slider_labels),
    "(?<=data\\$)\\w+"
  )))
  expect_no_warning(rd_factor(dic_r, exclude = intersect(logic_vars, names(covican$data))))
})

test_that("rd_factor lists at most 10 affected variables in the warning", {
  vars <- paste0("v", 1:12)
  data <- as.data.frame(setNames(rep(list(c(1, 0)), 12), vars))
  for (v in vars) data[[paste0(v, ".factor")]] <- factor(c("Yes", "No"))
  dic <- data.frame(
    field_name = vars,
    branching_logic_show_field_only_if = paste0("data$", vars, "=='1'"),
    choices_calculations_or_slider_labels = "1, Yes | 0, No"
  )

  expect_warning(rd_factor(data = data, dic = dic), "\\(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, \\.\\.\\.\\)")
})


test_that("rd_factor warns about excluded variables that are not in the data", {
  expect_warning(rd_factor(covican, exclude = c("copd", "no_such_var")), "no_such_var")
})
