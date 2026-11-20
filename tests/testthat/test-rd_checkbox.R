test_that("rd_checkbox converts checkbox variables to factors with specified labels", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"))

  # Check one checkbox variable as example
  expect_true(is.factor(result$data$inc_1.factor))
  expect_true(all(levels(result$data$inc_1.factor) == c("No", "Yes")))

  # Check another checkbox
  expect_true(is.numeric(result$data$type_underlying_disease_haematological_cancer))
  expect_true(is.factor(result$data$type_underlying_disease_haematological_cancer.factor))
  expect_true(all(levels(result$data$type_underlying_disease_haematological_cancer.factor) == c("No", "Yes")))
})

test_that("rd_checkbox renames checkbox variables when checkbox_names = TRUE", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"), checkbox_names = TRUE)

  # Check that new variable names exist (for one example)
  expect_true(any(grepl("underlying_disease_hemato_myelofibrosis", names(result$data))))
  expect_true(all(levels(result$data$underlying_disease_hemato_myelofibrosis) == c("No", "Yes")))
})

test_that("rd_checkbox keeps original names when checkbox_names = FALSE", {
  result <- rd_checkbox(covican, checkbox_labels = c("No", "Yes"), checkbox_names = FALSE)

  expect_true(any(grepl("___", names(result$data))))
})

test_that("checkbox_na applies NAs according to branching logic", {
  # Pick one checkbox with branching logic
  result <- rd_checkbox(covican, checkbox_na = TRUE)

  # Check that rows violating the branching logic are NA
  vals <- result$data |>
    dplyr::filter(is.na(type_underlying_disease_haematological_cancer)) |>
    dplyr::select(type_underlying_disease_haematological_cancer, underlying_disease_hemato_acute_lymphoblastic_leukaemia) |>
    dplyr::mutate(check = is.na(type_underlying_disease_haematological_cancer) & is.na( underlying_disease_hemato_acute_lymphoblastic_leukaemia))

  expect_true(all(vals$check))
})

test_that("long checkbox labels are truncated to 60 characters", {
  long_label <- paste(rep("X", 70), collapse = "")
  covican_test <- covican
  covican_test$data$long_checkbox___1 <- 1
  covican_test$dictionary <- covican$dictionary |> tibble::add_row(
    field_name = "long_checkbox",
    field_type = "checkbox",
    field_label = long_label,
    choices_calculations_or_slider_labels = "0, No | 1, Yes",
    .before = 1
  )

  result <- rd_checkbox(data = covican_test$data, dic = covican_test$dictionary) |> suppressWarnings()
  # we have to account for the '.factor' so the limit is 67
  expect_true(all(nchar(names(result$data)) <= 67))
})


test_that("rd_checkbox evaluates the branching logic of checkboxes in repeating instruments", {
  fx <- make_repeat_classic()
  fx$data$sym___1 <- c(NA, 1, 0, NA, 1)
  fx$data$sym___2 <- c(NA, 0, 1, NA, 0)
  fx$data$sym___1.factor <- factor(c(NA, "Checked", "Unchecked", NA, "Checked"), levels = c("Unchecked", "Checked"))
  fx$data$sym___2.factor <- factor(c(NA, "Unchecked", "Checked", NA, "Unchecked"), levels = c("Unchecked", "Checked"))
  fx$dictionary <- rbind(fx$dictionary, data.frame(
    field_name = "sym", form_name = "ae", field_type = "checkbox", field_label = "Symptoms",
    choices_calculations_or_slider_labels = "1, Cough | 2, Fever", text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = "[age] > '40'", stringsAsFactors = FALSE
  ))

  expect_no_warning(res <- rd_checkbox(fx, checkbox_names = FALSE, na_logic = "missing"))

  # record 1 (50 years old in its base row) keeps its answers; record 2 has no age
  expect_equal(as.character(res$data$sym___1.factor), c(NA, "Yes", "No", NA, NA))
  expect_equal(as.character(res$data$sym___2.factor), c(NA, "No", "Yes", NA, NA))
  expect_equal(as.numeric(res$data$sym___1), c(NA, 1, 0, NA, NA))
})

test_that("running rd_checkbox twice cannot be done", {
  result1 <- rd_checkbox(covican)

  expect_error(rd_checkbox(result1), "No checkbox fields found in the data")
})

test_that("non-checkbox fields remain unchanged", {
  result <- rd_checkbox(covican)

  expect_equal(result$data$age, covican$data$age)
  expect_equal(result$data$potassium, covican$data$potassium)
})


# ---- Other data structures ----

test_that("rd_checkbox gives the same result with a tibble", {
  tib <- covican
  tib$data <- tibble::as_tibble(covican$data)

  res_df <- rd_checkbox(covican)
  res_tib <- rd_checkbox(tib)

  expect_equal(as.data.frame(res_tib$data), as.data.frame(res_df$data), ignore_attr = TRUE)
  expect_true(any(!is.na(res_tib$data$type_underlying_disease_solid_tumour)))
})

test_that("rd_checkbox names the options from the dictionary when the data has no variable labels", {
  no_labels <- covican
  no_labels$data[] <- lapply(covican$data, function(x) { attr(x, "label") <- NULL; x })

  res <- suppressWarnings(rd_checkbox(no_labels))

  expect_true(all(c("type_underlying_disease_haematological_cancer", "type_underlying_disease_solid_tumour") %in% names(res$data)))
  expect_false(any(grepl("_x(\\.\\d+)?$", names(res$data))))
  expect_equal(names(res$data), names(rd_checkbox(covican)$data))
})


test_that("rd_checkbox returns a project without checkboxes unchanged, with a warning", {
  fx <- make_repeat_classic()

  expect_warning(res <- rd_checkbox(fx), "has no checkbox fields")
  expect_identical(res$data, fx$data)
  expect_identical(res$dictionary, fx$dictionary)
})


test_that("rd_checkbox needs two labels in checkbox_labels", {
  expect_error(rd_checkbox(covican, checkbox_labels = "Yes"), "must have two labels")
})

test_that("rd_checkbox translates the branching logic of checkbox codes with letters, negative codes and <>", {
  data <- data.frame(
    record_id = as.character(1:3), cb___a = c(1, 0, 0), cb____1 = c(0, 1, 0),
    f1 = c("x", NA, NA), f2 = c(NA, "y", NA), f3 = c(NA, "z", "z"), f4 = c("w", NA, NA)
  )
  dic <- data.frame(
    field_name = c("record_id", "cb", "f1", "f2", "f3", "f4"), form_name = "form1",
    field_type = c("text", "checkbox", "text", "text", "text", "text"),
    field_label = c("ID", "Options", "F1", "F2", "F3", "F4"),
    choices_calculations_or_slider_labels = c("", "A, Letter | -1, Negative", "", "", "", ""),
    text_validation_type_or_show_slider_number = "",
    branching_logic_show_field_only_if = c("", "", "[cb(A)] = '1'", "[cb(-1)] = \"1\"", "[cb(A)] <> '1'", "[cb(-1)] <> '1'"),
    stringsAsFactors = FALSE
  )

  out <- rd_checkbox(data = data, dic = dic)
  logic <- stats::setNames(out$dictionary$branching_logic_show_field_only_if, out$dictionary$field_name)

  expect_true(all(c("cb_letter", "cb_negative") %in% names(out$data)))
  expect_equal(as.character(out$data$cb_letter), c("Yes", "No", "No"))
  expect_equal(as.character(out$data$cb_negative), c("No", "Yes", "No"))
  # (the options are renamed and keep their codes, as REDCap logic)
  expect_equal(unname(logic[c("f1", "f2", "f3", "f4")]), c("[cb_letter] = '1'", "[cb_negative] = \"1\"", "[cb_letter] <> '1'", "[cb_negative] <> '1'"))
  # and they select the same rows as the original logic
  for (field in c("f1", "f2", "f3", "f4")) {
    expect_equal(
      as.vector(rd_rlogic(data = out$data, dic = out$dictionary, logic = logic[[field]], var = field)$eval),
      as.vector(rd_rlogic(data = data, dic = dic, logic = dic$branching_logic_show_field_only_if[dic$field_name == field], var = field)$eval),
      info = field
    )
  }
})

test_that("rd_checkbox leaves unchanged the checkboxes whose branching logic can't be evaluated", {
  data <- data.frame(record_id = as.character(1:3), sex = c(1, 2, 2), a___1 = c(1, 0, 1), b___1 = c(1, 1, 0))
  dic <- data.frame(
    field_name = c("record_id", "sex", "a", "b"), form_name = "form1", field_type = c("text", "radio", "checkbox", "checkbox"),
    field_label = c("ID", "Sex", "A", "B"), choices_calculations_or_slider_labels = c("", "1, Man | 2, Woman", "1, Yes", "1, Yes"),
    text_validation_type_or_show_slider_number = "", branching_logic_show_field_only_if = c("", "", "[sex] = '2'", "[user-role-name] = 'x'"),
    stringsAsFactors = FALSE
  )

  # (the checkboxes are numbers without .factor columns: they are not factors already)
  warnings <- character(0)
  out <- withCallingHandlers(
    rd_checkbox(data = data, dic = dic, na_logic = "eval", checkbox_names = FALSE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("could not be evaluated", warnings)))
  expect_false(any(grepl("already in factor form", warnings)))
  # `a` follows its branching logic and `b` is not changed
  expect_equal(as.character(out$data$a___1), c(NA, "No", "Yes"))
  expect_equal(as.character(out$data$b___1), c("Yes", "Yes", "No"))
  expect_match(paste(out$results, collapse = " "), "can't be transcribed")
})

test_that("rd_checkbox keeps the variable labels of the renamed checkbox options", {
  out <- suppressWarnings(rd_checkbox(covican))
  expect_equal(attr(out$data$type_underlying_disease_haematological_cancer, "label"), "Type of underlying disease (choice=Haematological cancer)")
  expect_equal(attr(out$data$type_underlying_disease_solid_tumour, "label"), "Type of underlying disease (choice=Solid tumour)")
})

test_that("rd_checkbox works with a dictionary with NA in its empty cells and checkboxes without branching logic", {
  cov <- covican
  cov$dictionary$branching_logic_show_field_only_if[cov$dictionary$field_type == "checkbox"] <- NA
  cov$dictionary$branching_logic_show_field_only_if[cov$dictionary$branching_logic_show_field_only_if %in% ""] <- NA
  ref <- covican
  ref$dictionary$branching_logic_show_field_only_if[ref$dictionary$field_type == "checkbox"] <- ""
  expect_equal(suppressWarnings(rd_checkbox(cov))$data, suppressWarnings(rd_checkbox(ref))$data)
})

test_that("rd_checkbox works with fields called like its objects (e.g. `data`)", {
  cb <- function(p, ...) suppressWarnings(suppressMessages(rd_checkbox(p, ...)))$data
  for (name in c("data", "checkbox_labels")) {
    p <- add_covican_field(name)
    for (args in list(list(), list(na_logic = "eval", checkbox_labels = c("No", "Yes")))) {
      ref <- do.call(cb, c(list(covican), args))
      out <- do.call(cb, c(list(p), args))
      expect_equal(out[names(ref)], ref)
      expect_equal(out[[name]], p$data[[name]], ignore_attr = TRUE)
    }
  }
  expect_no_error(suppressWarnings(suppressMessages(rd_transform(add_covican_field("data")))))
})

test_that("rd_checkbox gives the same labels to an option without its .factor column", {
  project <- covican
  project$data$type_underlying_disease___0.factor <- NULL
  out <- suppressWarnings(rd_factor(rd_checkbox(project)))$data
  ref <- suppressWarnings(rd_factor(rd_checkbox(covican)))$data
  expect_setequal(names(out), names(ref))
  expect_equal(out[names(ref)], ref)
})

# A project with a checkbox `cb` (options 1 and 2) and a text field `b` whose branching logic uses it
checkbox_project <- function(logic, calc = "") {
  data <- data.frame(record_id = as.character(1:4), cb___1 = c(1, 1, 0, 0), cb___2 = c(1, 0, 1, 0), b = NA, s = NA)
  data$cb___1.factor <- factor(ifelse(data$cb___1 == 1, "Checked", "Unchecked"), levels = c("Unchecked", "Checked"))
  data$cb___2.factor <- factor(ifelse(data$cb___2 == 1, "Checked", "Unchecked"), levels = c("Unchecked", "Checked"))
  dic <- data.frame(
    field_name = c("record_id", "cb", "b", "s"), form_name = "form1", field_type = c("text", "checkbox", "text", "calc"),
    field_label = c("Record", "CB", "B", "S"), choices_calculations_or_slider_labels = c("", "1, One | 2, Two", "", calc),
    text_validation_type_or_show_slider_number = "", branching_logic_show_field_only_if = c("", "", logic, "")
  )
  list(data = data, dictionary = dic)
}

test_that("the logic that uses the options of a checkbox as numbers keeps working after rd_transform()", {
  cases <- list(
    c("[cb(1)] + [cb(2)] >= 2", "1", "[cb_one] + [cb_two] >= 2"),
    c("[cb(1)] = '1' or [cb(1)] + [cb(2)] >= 2", "1,2", "[cb_one] = '1' or [cb_one] + [cb_two] >= 2"),
    c("sum([cb(1)], [cb(2)]) = 1", "2,3", "sum([cb_one], [cb_two]) = 1"),
    c("[cb(2)] = '1'", "1,3", "data$cb_two=='Yes'")
  )
  queries <- function(project) paste(suppressWarnings(suppressMessages(rd_query(project, variables = "b", expression = "is.na(x)")))$queries$Identifier, collapse = ",")
  for (case in cases) {
    project <- checkbox_project(case[1], calc = "[cb(1)] + [cb(2)]")
    expect_equal(queries(project), case[2], info = case[1])

    expect_no_warning(transformed <- suppressMessages(rd_transform(project)))
    dic <- transformed$dictionary
    expect_equal(dic$branching_logic_show_field_only_if[dic$field_name == "b"], case[3], info = case[1])
    expect_equal(dic$choices_calculations_or_slider_labels[dic$field_name == "s"], "[cb_one] + [cb_two]")
    expect_equal(queries(transformed), case[2], info = case[1])

    # Without renaming the options, and with rd_factor() and rd_dictionary() only
    kept <- suppressMessages(rd_dictionary(rd_factor(rd_checkbox(project, checkbox_names = FALSE))))
    expect_equal(queries(kept), case[2], info = case[1])
    factors <- suppressWarnings(suppressMessages(rd_dictionary(rd_factor(project))))
    expect_equal(queries(factors), case[2], info = case[1])
  }
})

test_that("rd_checkbox keeps the texts between quotes and reads whole numbers", {
  queries <- function(project) paste(suppressWarnings(suppressMessages(rd_query(project, variables = "b", expression = "is.na(x)")))$queries$Identifier, collapse = ",")
  cases <- list(
    c("[t] = '[cb(1)]'", "data$t == '[cb(1)]'"),
    c("[t] = '[cb(1)]' and [cb(1)] + [cb(2)] >= 1", "[t] = '[cb(1)]' and [cb_one] + [cb_two] >= 1"),
    c("[cb(1)] = 1.0", "data$cb_one=='Yes'"),
    c("[cb(2)] = 10 or [cb(1)] = 1", "[cb_two] = 10 or [cb_one] = 1")
  )
  for (case in cases) {
    project <- checkbox_project(case[1])
    project$data$t <- c("[cb(1)]", "x", "[cb(1)]", "x")
    project$dictionary <- rbind(project$dictionary, transform(project$dictionary[3, ], field_name = "t"))
    transformed <- suppressMessages(rd_transform(project))
    dic <- transformed$dictionary
    expect_equal(dic$branching_logic_show_field_only_if[dic$field_name == "b"], case[2], info = case[1])
    expect_equal(queries(transformed), queries(project), info = case[1])
  }
})

test_that("rd_checkbox and rd_dictionary don't change a comparison with an operation", {
  queries <- function(project) paste(suppressWarnings(suppressMessages(rd_query(project, variables = "b", expression = "is.na(x)")))$queries$Identifier, collapse = ",")
  project <- checkbox_project("[cb(1)] = 0 + 1")
  expect_equal(queries(project), "1,2")
  transformed <- suppressMessages(rd_transform(project))
  expect_equal(transformed$dictionary$branching_logic_show_field_only_if[transformed$dictionary$field_name == "b"], "[cb_one] = 0 + 1")
  expect_equal(queries(transformed), "1,2")
  expect_equal(queries(suppressMessages(rd_dictionary(rd_factor(project)))), "1,2")
})

test_that("rd_checkbox keeps the codes with labels that look like codes", {
  data <- data.frame(record_id = c("A", "B", "C"), cb___1 = c(1, 0, 1), b = NA)
  data$cb___1.factor <- factor(ifelse(data$cb___1 == 1, "Checked", "Unchecked"), levels = c("Unchecked", "Checked"))
  dic <- data.frame(
    field_name = c("record_id", "cb", "b"), form_name = "form1", field_type = c("text", "checkbox", "text"), field_label = c("ID", "CB", "B"),
    choices_calculations_or_slider_labels = c("", "1, One", ""), text_validation_type_or_show_slider_number = "",
    branching_logic_show_field_only_if = c("", "", "[cb(1)] = '1'")
  )
  project <- list(data = data, dictionary = dic)
  queries <- function(p) suppressWarnings(suppressMessages(rd_query(p, variables = "b", expression = "is.na(x)")))$queries$Identifier
  expect_equal(queries(project), c("A", "C"))
  for (labels in list(c("No", "Yes"), c("1", "0"), c("0", "1"), c("No", "1"))) {
    checkbox <- suppressMessages(rd_checkbox(project, checkbox_labels = labels))
    expect_equal(checkbox$dictionary$branching_logic_show_field_only_if[3], "[cb_one] = '1'")
    expect_equal(queries(checkbox), c("A", "C"), info = paste(labels, collapse = "/"))
    expect_equal(queries(suppressMessages(rd_dictionary(rd_factor(checkbox)))), c("A", "C"), info = paste(labels, collapse = "/"))
    expect_equal(queries(suppressMessages(rd_transform(project, checkbox_labels = labels))), c("A", "C"), info = paste(labels, collapse = "/"))
  }
})

test_that("rd_checkbox gives names that the logic can use and keeps apart options with the same clean name", {
  queries <- function(p, v = "b") suppressWarnings(suppressMessages(rd_query(p, variables = v, expression = "is.na(x)")))$queries$Identifier
  dic <- function(fields, types, choices, logic) data.frame(
    field_name = fields, form_name = "form1", field_type = types, field_label = fields, choices_calculations_or_slider_labels = choices,
    text_validation_type_or_show_slider_number = "", branching_logic_show_field_only_if = logic
  )

  # The new name already exists
  data <- data.frame(record_id = c("A", "B", "C"), cb___1 = c(1, 0, 1), cb_one = c(9, 9, 9), b = NA)
  project <- list(data = data, dictionary = dic(c("record_id", "cb", "cb_one", "b"), c("text", "checkbox", "text", "text"), c("", "1, One", "", ""), c("", "", "", "[cb(1)] = '1'")))
  expect_warning(checkbox <- suppressMessages(rd_checkbox(project)), "already exists")
  expect_true("cb_one_1" %in% names(checkbox$data))
  expect_equal(queries(checkbox), c("A", "C"))
  expect_equal(queries(suppressWarnings(suppressMessages(rd_transform(project)))), c("A", "C"))

  # Two options with the same clean name
  data <- data.frame(record_id = c("A", "B", "C"), cb___1 = c(1, 0, 1), cb___2 = c(0, 1, 1), b = NA, b2 = NA)
  project <- list(data = data, dictionary = dic(c("record_id", "cb", "b", "b2"), c("text", "checkbox", "text", "text"), c("", "1, A-B | 2, A B", "", ""), c("", "", "[cb(1)] = '1'", "[cb(2)] = '1'")))
  checkbox <- suppressWarnings(suppressMessages(rd_checkbox(project)))
  expect_equal(queries(checkbox), c("A", "C"))
  expect_equal(queries(checkbox, "b2"), c("B", "C"))
  transformed <- suppressWarnings(suppressMessages(rd_transform(project)))
  expect_equal(queries(transformed), c("A", "C"))
  expect_equal(queries(transformed, "b2"), c("B", "C"))
})
