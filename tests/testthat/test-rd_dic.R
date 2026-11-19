test_that("Error if data or dic missing", {
  expect_error(rd_dictionary(data = NULL, dic = covican$dictionary),
               "Both `data` and `dic`")
  expect_error(rd_dictionary(data = covican$data, dic = NULL),
               "Both `data` and `dic`")
})

test_that("Returns correct list structure with covican", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_true(all(c("data", "dictionary", "event_form") %in% names(result)))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$dictionary, "data.frame")
  expect_s3_class(result$event_form, "data.frame")
})

test_that("Factor variables are correctly transformed in branching logic", {
  dic <- covican$dictionary
  data <- covican$data

  # Force one branching logic for testing (simulate REDCap condition)
  dic$branching_logic_show_field_only_if[2] <- "[inc_1]='1'"

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_true(grepl("\\inc_1\\=='1'",
                    result$dictionary$branching_logic_show_field_only_if[2]))
})

test_that("Handles failed branching logic gracefully", {
  dic <- covican$dictionary
  dic$branching_logic_show_field_only_if[5] <- "FAIL_logic"
  dic$choices_calculations_or_slider_labels[5] <- "FAIL_calc"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  expect_true(any(grepl("exc_1", result$results)))
})

test_that("Event form is preserved", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_equal(result$event_form, covican$event_form)
})

test_that("Dictionary replacements are applied consistently", {
  dic <- covican$dictionary
  data <- covican$data

  dic$branching_logic_show_field_only_if[3] <- "[inc_2]='0'"
  dic$branching_logic_show_field_only_if[4] <- "[dm]='1'"

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_true(all(grepl("data$|==",
                        result$dictionary$branching_logic_show_field_only_if[3:4])))
})

test_that("Handles case when no factor variables exist", {
  data <- covican$data
  dic <- covican$dictionary

  # Drop factor variables
  data[] <- lapply(data, function(x) if (is.factor(x)) as.character(x) else x)

  result <- rd_dictionary(data = data, dic = dic, event_form = covican$event_form)

  expect_s3_class(result$dictionary, "data.frame")
  expect_equal(nrow(result$dictionary), nrow(dic))
})

test_that("Correctly transforms branching logic with multiple factors", {
  dic <- covican$dictionary
  dic$branching_logic_show_field_only_if[2] <- "[inc_1]='1' and [dm]='1'"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  logic <- result$dictionary$branching_logic_show_field_only_if[2]
  expect_true(grepl("data\\$inc_1=='1'", logic))
  expect_true(grepl("data\\$dm=='1'", logic))
})


test_that("Calculation fields are transformed if valid", {
  dic <- covican$dictionary
  dic$field_type[2] <- "calc"
  dic$choices_calculations_or_slider_labels[2] <- "[age] + 1"

  result <- rd_dictionary(data = covican$data, dic = dic, event_form = covican$event_form)

  calc <- result$dictionary$choices_calculations_or_slider_labels[2]
  expect_false(grepl("FAIL", calc))
})


test_that("Output list has no NULL elements", {
  result <- rd_dictionary(data = covican$data,
                          dic = covican$dictionary,
                          event_form = covican$event_form)

  expect_false(any(vapply(result, is.null, logical(1))))
})


test_that("Works with project-style input (list)", {
  project <- list(
    data = covican$data,
    dictionary = covican$dictionary,
    event_form = covican$event_form
  )

  result <- rd_dictionary(project)

  expect_true(all(c("data", "dictionary", "event_form") %in% names(result)))
})


test_that("rd_dictionary works after rd_factor when the only factors are yes/no fields", {
  fx <- make_cross_event()
  fx$dictionary$choices_calculations_or_slider_labels <- NA
  fx$dictionary$field_type <- c("text", "yesno", "yesno")
  fx$data$preg.factor <- factor(fx$data$preg, levels = 0:1, labels = c("No", "Yes"))
  fx$data$sex <- NULL

  ff <- suppressWarnings(rd_factor(data = fx$data, dic = fx$dictionary[-2, ], event_form = fx$event_form))
  expect_no_error(suppressWarnings(rd_dictionary(ff)))
})

test_that("rd_dictionary translates the codes of yes/no fields converted with rd_factor and keeps the logic of other events", {
  fx <- make_cross_event()
  fx$data$preg[5] <- 1
  fx$data$preg.factor <- factor(fx$data$preg, levels = c(1, 0), labels = c("Yes", "No"))
  fx$data$sex.factor <- factor(fx$data$sex, levels = 1:2, labels = c("Man", "Woman"))
  fx$data$visit_note <- c(NA, "a", NA, NA, NA, NA)
  fx$dictionary <- rbind(fx$dictionary, data.frame(
    field_name = c("visit_note", "pain_note"), form_name = "visit", field_type = "text", field_label = c("Note", "Pain note"),
    choices_calculations_or_slider_labels = NA, text_validation_type_or_show_slider_number = NA,
    branching_logic_show_field_only_if = c("[preg] <> '1'", "[sex] > 1")
  ))
  fx$data$pain_note <- NA

  out <- suppressWarnings(rd_dictionary(rd_factor(fx)))
  logic <- stats::setNames(out$dictionary$branching_logic_show_field_only_if, out$dictionary$field_name)

  # yes/no codes are translated to the labels of the factor
  expect_match(logic[["visit_note"]], "'No'|'Yes'")
  expect_equal(as.vector(eval(parse(text = logic[["visit_note"]]), list(data = rd_factor(fx)$data))), c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  # a > comparison is not turned into <>
  expect_false(grepl("<>|!=", logic[["pain_note"]]))
  # logic with a variable of another event stays in REDCap syntax
  expect_equal(logic[["preg"]], "[baseline_arm_1][sex]='Woman'")
  expect_match(paste(out$results, collapse = " "), "other events")
})

# A project with a radio field `a` (already a factor after rd_factor()) and a text field `b` whose branching logic uses `a`
factor_project <- function(choices, logic, calc = "") {
  data <- data.frame(record_id = as.character(1:4), a = c(1, 2, 3, 5), b = NA, s = NA)
  dic <- data.frame(
    field_name = c("record_id", "a", "b", "s"), form_name = "form1", field_type = c("text", "radio", "text", "calc"),
    field_label = c("Record", "A", "B", "S"), choices_calculations_or_slider_labels = c("", choices, "", calc),
    text_validation_type_or_show_slider_number = "", branching_logic_show_field_only_if = c("", "", logic, "")
  )
  options <- trimws(strsplit(choices, "\\|")[[1]])
  data$a.factor <- factor(trimws(sub("^[^,]*,", "", options))[match(data$a, trimws(sub(",.*$", "", options)))], levels = trimws(sub("^[^,]*,", "", options)))
  list(data = data, dictionary = dic)
}

test_that("rd_dictionary reads the options without a space after the comma", {
  project <- suppressMessages(rd_factor(factor_project("1, Never | 2, Sometimes | 3, Often | 5,Always", "[a] = '5'")))
  expect_no_warning(out <- suppressMessages(rd_dictionary(project)))
  expect_equal(out$dictionary$branching_logic_show_field_only_if[3], "data$a=='Always'")
})

test_that("rd_dictionary keeps in REDCap syntax, with the codes, the logic that uses a factor as a number", {
  choices <- "1, Never | 2, Sometimes | 3, Often | 5, Always"
  for (case in list(c("[a] >= 3", "3,4"), c("[a] = '1' or [a] >= 3", "1,3,4"), c("[a] = '1' or [a] = '5'", "1,4"))) {
    project <- factor_project(choices, case[1], calc = "if([a] = '1', 0, [a] * 2)")
    before <- suppressWarnings(suppressMessages(rd_query(project, variables = "b", expression = "is.na(x)")))$queries$Identifier
    expect_equal(paste(before, collapse = ","), case[2])

    expect_no_warning(transformed <- suppressMessages(rd_transform(project)))
    logic <- transformed$dictionary$branching_logic_show_field_only_if[transformed$dictionary$field_name == "b"]
    if (grepl(">=", case[1])) {
      expect_equal(logic, case[1])
    } else {
      # (only comparisons with labels: R logic of the factor)
      expect_equal(logic, "data$a=='Never' | data$a=='Always'")
    }
    expect_equal(transformed$dictionary$choices_calculations_or_slider_labels[transformed$dictionary$field_name == "s"], "if([a] = '1', 0, [a] * 2)")
    after <- suppressWarnings(suppressMessages(rd_query(transformed, variables = "b", expression = "is.na(x)")))$queries$Identifier
    expect_equal(after, before, info = case[1])
  }
  out <- suppressMessages(rd_dictionary(suppressMessages(rd_factor(factor_project(choices, "[a] >= 3")))))
  expect_true(any(grepl("as numbers", out$results)))
})

test_that("rd_dictionary doesn't change again a label that is the code of another option", {
  project <- factor_project("1, 2 | 2, 1 | 3, 3 | 5, 5", "[a] = '2' or [a] = '1'")
  transformed <- suppressMessages(rd_transform(project))
  expect_equal(transformed$dictionary$branching_logic_show_field_only_if[3], "data$a=='1' | data$a=='2'")
  queries <- function(p) suppressWarnings(suppressMessages(rd_query(p, variables = "b", expression = "is.na(x)")))$queries$Identifier
  expect_equal(queries(transformed), queries(project))

  project <- factor_project("1, 2 | 2, 1 | 3, 3 | 5, 5", "[a] = '2'")
  expect_equal(queries(suppressMessages(rd_transform(project))), 2, ignore_attr = TRUE)
})
