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
  data <- tibble::tibble(a=1:2, record_id=1:2)
  dic <- tibble::tibble(field_name="a", field_type="text", form_name="f1")
  event_form <- tibble::tibble(form="f1", unique_event_name="e1")

  expect_error(suppressWarnings(split_event(data, dic, event_form))) # Missing redcap_event_name
})

test_that("split_form works with wide and longitudinal", {
  data <- tibble::tibble(record_id = 1:2, redcap_event_name="e1", a=1:2)
  dic <- tibble::tibble(field_name="a", form_name="f1", field_type="text")
  event_form <- tibble::tibble(form="f1", unique_event_name="e1")

  res <- suppressWarnings(split_form(data, dic, event_form, wide=TRUE))
  expect_true(is.list(res))
})

test_that("to_factor converts correctly", {
  data <- tibble::tibble(a = c("x","y"), a.factor=as.factor(a), redcap_event_name="e1", redcap_event_name.factor = "E1", redcap_repeat_instrument=NA, redcap_repeat_instrument.factor =NA)
  dic <- tibble::tibble(field_name="a", choices_calculations_or_slider_labels="1, x | 2, y", field_type="radio", branching_logic_show_field_only_if = "")

  res <- suppressWarnings(to_factor(data, dic))
  expect_true(is.factor(res$data$a))
})

legacy_dictionary <- function(fields, forms = "alpha", types = "text",
                              logic = "", choices = "") {
  n <- length(fields)
  data.frame(
    field_name = fields,
    form_name = rep_len(forms, n),
    field_type = rep_len(types, n),
    field_label = fields,
    choices_calculations_or_slider_labels = rep_len(choices, n),
    text_validation_type_or_show_slider_number = "",
    branching_logic_show_field_only_if = rep_len(logic, n)
  )
}

legacy_split_fixture <- function(longitudinal = FALSE) {
  dic <- legacy_dictionary(
    c("record_id", "a", "b", "info"),
    c("alpha", "alpha", "beta", "alpha"),
    c("text", "text", "text", "descriptive")
  )
  if (longitudinal) {
    data <- data.frame(
      record_id = c(1, 1, 2, 2),
      redcap_event_name = c("base", "follow", "base", "follow"),
      a = c(10, NA, 20, NA),
      b = c(NA, 30, NA, 40)
    )
  } else {
    data <- data.frame(
      record_id = 1:2, a = c(10, 20), b = c(30, 40)
    )
  }
  data$redcap_repeat_instrument <- NA_character_
  data$redcap_repeat_instrument.factor <- NA_character_
  data$redcap_repeat_instance <- NA_integer_

  list(
    data = data, dic = dic,
    event_form = data.frame(
      form = c("alpha", "beta"),
      unique_event_name = c("base", "follow")
    )
  )
}

test_that("legacy recalculation preserves original values", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(
    record_id = 1:2, a = c(1, 2),
    q = c(2, 99), blank = c(NA, NA)
  )
  dic <- legacy_dictionary(
    c("record_id", "a", "q", "blank"),
    types = c("text", "text", "calc", "calc"),
    choices = c("", "", "[a] * 2", "''")
  )
  out <- recalculate(data, dic)

  expect_equal(as.numeric(out$data$q_recalc), c(2, 4))
  expect_true(all(is.na(out$data$blank_recalc)))
  expect_equal(out$data$q, c(2, 99))
  expect_true(all(c("q_recalc", "blank_recalc") %in% out$dic$field_name))
})

test_that("legacy checkbox logic distinguishes false and unknown", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(
    record_id = 1:3, flag = c(1, 0, NA),
    cb___1 = 1, cb___2 = c(0, 1, 0)
  )
  dic <- legacy_dictionary(
    c("record_id", "flag", "cb"),
    types = c("text", "text", "checkbox"),
    logic = c("", "", "[flag] = 1"),
    choices = c("", "", "1, One | 2, Two")
  )
  unknown <- transform_checkboxes(data, dic, checkbox_na = FALSE)
  hidden <- transform_checkboxes(data, dic, checkbox_na = TRUE)

  expect_equal(unknown$data$cb___1, c("1", "1", NA))
  expect_equal(unknown$data$cb___2, c("0", "1", NA))
  expect_equal(hidden$data$cb___1, c("1", NA, NA))
  expect_equal(hidden$data$cb___2, c("0", NA, NA))
  expect_identical(hidden$data$flag, data$flag)
})

test_that("legacy checkbox helper preserves data without checkboxes", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(record_id = 1:2, a = 3:4)
  dic <- legacy_dictionary(c("record_id", "a"))

  expect_warning(
    out <- transform_checkboxes(data, dic),
    "There isn't any checkbox"
  )
  expect_identical(out$data, data)
})

test_that("legacy event splitting selects correct rows and fields", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- legacy_split_fixture(TRUE)
  all <- split_event(fx$data, fx$dic, fx$event_form)

  expect_setequal(as.character(all$events), c("base", "follow"))

  base <- all$df[[match("base", all$events)]]
  follow <- split_event(
    fx$data, fx$dic, fx$event_form, which = "follow"
  )
  expect_equal(base$record_id, c(1, 2))
  expect_equal(base$a, c(10, 20))
  expect_false("b" %in% names(base))
  expect_equal(follow$b, c(30, 40))
  expect_false("a" %in% names(follow))
})

test_that("legacy splitters reject inconsistent fields", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fx <- legacy_split_fixture(TRUE)

  for (splitter in list(split_event, split_form)) {
    missing <- fx$data
    missing$a <- NULL
    expect_error(
      splitter(missing, fx$dic, fx$event_form), "dictionary than"
    )

    extra <- fx$data
    extra$unexpected <- 1
    expect_error(
      splitter(extra, fx$dic, fx$event_form), "data base than"
    )

    for (columns in list(
      "alpha_complete", "alpha_timestamp",
      c("alpha_complete", "alpha_timestamp")
    )) {
      extra <- fx$data
      for (column in columns) extra[[column]] <- 0

      error <- tryCatch(
        splitter(extra, fx$dic, fx$event_form),
        error = identity
      )
      expect_true(inherits(error, "error"))
      if (inherits(error, "error")) {
        expect_match(conditionMessage(error), "delete_pattern")
        for (column in columns) {
          expect_match(
            conditionMessage(error), sub("^alpha", "", column),
            fixed = TRUE
          )
        }
      }
    }
  }
})

test_that("legacy form splitting supports both formats", {
  withr::local_options(lifecycle_verbosity = "quiet")
  classic <- legacy_split_fixture(FALSE)
  all <- split_form(classic$data, classic$dic)
  expect_setequal(as.character(all$form), c("alpha", "beta"))

  beta <- split_form(classic$data, classic$dic, which = "beta")
  expect_equal(beta$b, c(30, 40))
  expect_false("a" %in% names(beta))

  longitudinal <- legacy_split_fixture(TRUE)
  expect_error(
    split_form(longitudinal$data, longitudinal$dic),
    "event_form.*provided"
  )

  alpha <- split_form(
    longitudinal$data, longitudinal$dic,
    longitudinal$event_form, which = "alpha"
  )
  expect_equal(alpha$record_id, c(1, 2))
  expect_equal(alpha$a, c(10, 20))

  wide <- split_form(
    longitudinal$data, longitudinal$dic,
    longitudinal$event_form, which = "alpha", wide = TRUE
  )
  expect_equal(wide$record_id, c(1, 2))
  expect_equal(as.numeric(wide[["1"]]), c(10, 20))
})

test_that("legacy factor conversion preserves system identifiers", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(
    record_id = 1:2, redcap_event_name = c("base", "base"),
    redcap_repeat_instrument = NA_character_,
    redcap_data_access_group = c("a", "b"), x = c(0, 1)
  )
  data$redcap_event_name.factor <- factor(c("Base", "Base"))
  data$redcap_repeat_instrument.factor <-
    factor(c(NA, NA), levels = "Visits")
  data$redcap_data_access_group.factor <-
    factor(c("Group A", "Group B"))
  data$x.factor <- factor(c("No", "Yes"), levels = c("No", "Yes"))

  dic <- legacy_dictionary(
    c("record_id", "x"), types = c("text", "yesno"),
    logic = c("", "[x] = '1'"),
    choices = c("", "0, No | 1, Yes")
  )
  out <- to_factor(data, dic)

  expect_true(is.factor(out$data$x))
  expect_equal(as.character(out$data$x), c("No", "Yes"))
  expect_identical(out$data$redcap_event_name, data$redcap_event_name)
  expect_identical(
    out$data$redcap_data_access_group,
    data$redcap_data_access_group
  )
  expect_equal(
    as.character(out$data$redcap_data_access_group.factor),
    c("Group A", "Group B")
  )
  expect_match(out$dic$branching_logic_show_field_only_if[2], "Yes")

  excluded <- to_factor(data, dic, exclude = "x")
  expect_identical(excluded$data$x, data$x)
})

test_that("legacy repeating forms retain base rows and instances", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(
    record_id = 1,
    redcap_repeat_instrument = c(NA, "alpha", "alpha"),
    redcap_repeat_instrument.factor = c(NA, "Alpha", "Alpha"),
    redcap_repeat_instance = c(NA, 1, 2),
    a = c(NA, 10, 20),
    b = c(9, NA, NA)
  )
  dic <- legacy_dictionary(
    c("record_id", "a", "b"),
    forms = c("beta", "alpha", "beta")
  )
  out <- split_form(data, dic)

  alpha <- out$df[[match("alpha", out$form)]]
  beta <- out$df[[match("beta", out$form)]]

  expect_equal(alpha$record_id, c(1, 1))
  expect_equal(alpha$a, c(10, 20))
  expect_equal(alpha$redcap_repeat_instance, c(1, 2))
  expect_equal(nrow(beta), 1L)
  expect_equal(beta$b, 9)
  expect_true(all(vapply(
    seq_len(nrow(out)),
    function(i) all(out$vars[[i]] %in% names(out$df[[i]])),
    logical(1)
  )))

  wide <- split_form(data, dic, which = "alpha", wide = TRUE)
  expect_equal(wide$record_id, 1)
  expect_equal(wide$a_1, 10)
  expect_equal(wide$a_2, 20)
})


# ---- Forms of repeating instruments chosen by their code, not by their label ----

split_label_data <- function() {
  data <- data.frame(
    record_id = c("1", "1", "1", "1", "1", "2"),
    redcap_repeat_instrument = c(NA, "labs", "labs", "meds", "meds", NA),
    redcap_repeat_instance = c(NA, 1, 2, 1, 2, NA),
    b = c(5, NA, NA, NA, NA, 6), redcap_repeat_instrument_note = c("n1", NA, NA, NA, NA, "n2"),
    l = c(NA, 1, 2, NA, NA, NA), m = c(NA, NA, NA, 3, 4, NA)
  )
  data$redcap_repeat_instrument.factor <- factor(c(NA, "Repeated", "Repeated", "Repeated", "Repeated", NA))
  data
}
split_label_dic <- function() {
  data.frame(field_name = c("record_id", "b", "redcap_repeat_instrument_note", "l", "m"), form_name = c("base", "base", "base", "labs", "meds"),
             field_type = "text", field_label = c("ID", "B", "Note", "L", "M"))
}

test_that("split_form chooses the rows of each repeating instrument by its code, also without its factor version", {
  withr::local_options(lifecycle_verbosity = "quiet")
  out <- split_form(split_label_data(), split_label_dic())
  expect_equal(out$form, c("base", "labs", "meds"))
  expect_equal(vapply(out$df, nrow, integer(1)), c(2L, 2L, 2L))
  expect_equal(out$df[[1]]$redcap_repeat_instrument_note, c("n1", "n2"))

  data <- split_label_data()
  data$redcap_repeat_instrument.factor <- NULL
  out <- split_form(data, split_label_dic())
  expect_equal(vapply(out$df, nrow, integer(1)), c(2L, 2L, 2L))
  expect_equal(out$df[[2]]$redcap_repeat_instrument, c("labs", "labs"))
})

test_that("split_form keeps the rows of a form in the events where it doesn't repeat, and the fields called `id`", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dic <- legacy_dictionary(c("record_id", "hb", "id"), forms = c("demo", "lab", "lab"))
  ef <- data.frame(arm_num = 1, unique_event_name = c("a_arm_1", "a_arm_1", "b_arm_1"), form = c("demo", "lab", "lab"))
  data <- data.frame(record_id = "1", redcap_event_name = c("a_arm_1", "b_arm_1", "b_arm_1"), redcap_repeat_instrument = c(NA, "lab", "lab"),
                     redcap_repeat_instance = c(NA, 1, 2), hb = c(12, 11, NA), id = c("C-7", "C-8", "C-9"))

  expect_equal(split_form(data, dic, ef, which = "lab")$hb, c(12, 11, NA))
  wide <- split_form(data, dic, ef, wide = TRUE)
  lab <- wide$df[[match("lab", wide$form)]]
  expect_equal(unlist(lab[c("hb_1", "hb_2", "hb_3")], use.names = FALSE), c(12, 11, NA))
  expect_equal(unlist(lab[c("id_1", "id_2", "id_3")], use.names = FALSE), c("C-7", "C-8", "C-9"))
  # A form with only the record identifier: one row per record
  expect_equal(wide$df[[match("demo", wide$form)]]$record_id, "1")

  # An instrument written with spaces is the base row; an instrument without label shows its code
  data$redcap_repeat_instrument[1] <- " "
  data$redcap_repeat_instrument.factor <- factor(c(NA, NA, "Labs"))
  out <- split_form(data, dic, ef)
  expect_equal(nrow(out$df[[match("demo", out$form)]]), 1)
  expect_equal(as.character(out$df[[match("lab", out$form)]]$redcap_repeat_instrument), c(NA, "lab", "Labs"))
})
