# ---- 1) Checkbox-specific missing-variable error ----
test_that("rd_split errors with missing dictionary vars that are checkbox type", {
  data <- tibble(record_id = 1)
  dic  <- tibble(
    field_name = c("record_id", "cb_1"),
    field_type = c("text", "checkbox"),
    form_name  = c("form1", "form1")
  )

  expect_error(
    rd_split(data = data, dic = dic),
    regexp = "rd_checkbox",
    fixed = FALSE
  )
})

# ---- 2) _complete / _timestamp extra-variable message ----
test_that("rd_split errors with default REDCap _complete/_timestamp variables present in data but not dictionary", {
  data <- tibble(
    record_id = 1,
    someform_complete = 1,
    someform_timestamp = Sys.time()
  )

  dic <- tibble(
    field_name = "record_id",
    field_type = "text",
    form_name  = "meta"
  )

  expect_error(
    rd_split(data = data, dic = dic),
    regexp = "_complete|_timestamp|delete_vars",
    ignore.case = TRUE
  )
})

# ---- 3) Factor-version variables in data not in dictionary -> rd_factor message ----
test_that("rd_split errors when .factor versions of variables are present but not in dictionary", {
  data <- tibble(record_id = 1, var1.factor = "A")
  dic  <- tibble(field_name = "record_id", field_type = "text", form_name = "meta")

  expect_error(
    rd_split(data = data, dic = dic),
    regexp = "rd_factor",
    ignore.case = TRUE
  )
})

# ---- 4) Non-longitudinal split by form (self-contained) ----
test_that("rd_split splits non-longitudinal data by form (self-contained example)", {
  dic <- tibble(
    field_name = c("record_id", "a1", "a2", "b1"),
    field_type = c("text", "text", "text", "text"),
    form_name  = c("meta", "form_a", "form_a", "form_b")
  )

  data <- tibble(
    record_id = 1:3,
    a1 = c("x","y","z"),
    a2 = c(10, 20, 30),
    b1 = c(100, 200, 300)
  )

  res <- rd_split(data = data, dic = dic, by = "form")

  expect_true("form" %in% names(res$data))
  forms_expected <- unique(dic$form_name)
  expect_true(all(forms_expected %in% res$data$form))

  # each df must contain record_id and corresponding variables
  df_list <- res$data$df
  names_by_form <- purrr::map_chr(df_list, ~ {
    nm <- names(.x)
    # combine for assertion
    paste(sort(nm), collapse = ",")
  })

  # ensure that form_a DF contains a1 and a2
  fa_row <- res$data |> filter(form == "form_a") |> pull(df) |> pluck(1)
  expect_true(all(c("a1", "a2", "record_id") %in% names(fa_row)))
})

# ---- 5) Longitudinal form-splitting requires event_form ----
test_that("rd_split errors when splitting by form in longitudinal project without event_form", {
  dic <- tibble(
    field_name = c("record_id", "x1"),
    field_type = c("text", "text"),
    form_name  = c("meta", "formX")
  )

  data <- tibble(
    record_id = 1:2,
    redcap_event_name = c("event_1", "event_2"),
    x1 = c("a","b")
  )

  expect_error(
    rd_split(data = data, dic = dic, by = "form", event_form = NULL),
    regexp = "event-form correspondence is required|provide the `event_form`",
    ignore.case = TRUE
  )
})

# ---- 6) Splitting by event (self-contained) ----
test_that("rd_split splits longitudinal data by event when event_form is provided", {
  dic <- tibble(
    field_name = c("record_id", "a1", "b1"),
    field_type = c("text", "text", "text"),
    branching_logic_show_field_only_if = c("", "", ""),
    form_name  = c("meta", "form_a", "form_b")
  )

  event_form <- tibble(
    form = c("form_a", "form_b"),
    unique_event_name = c("ev1", "ev2")
  )

  data <- tibble(
    record_id = c(1,2,3,4),
    redcap_event_name = c("ev1", "ev1", "ev2", "ev2"),
    a1 = c("x", NA, NA, NA),
    b1 = c(NA, NA, "z", "w")
  )

  res <- rd_split(data = data, dic = dic, event_form = event_form, by = "event")
  expect_true("events" %in% names(res$data))
  expect_true(all(c("ev1", "ev2") %in% res$data$events))

  # each event df should contain record_id
  ev1_df <- res$data |> filter(events == "ev1") |> pull(df) |> pluck(1)
  expect_true("record_id" %in% names(ev1_df))
})

# ---- 7) invalid `by` argument is rejected ----
test_that("rd_split rejects invalid `by` argument", {
  dic <- tibble(field_name = "record_id", field_type = "text", form_name = "meta")
  data <- tibble(record_id = 1)

  expect_error(
    rd_split(data = data, dic = dic, by = "nonsense"),
    regexp = "Invalid `by` argument|either 'form' or 'event'",
    ignore.case = TRUE
  )
})

# ---- 8) project argument overrides data/dic ----
test_that("rd_split respects project argument over data/dic", {
  fake_proj <- list(
    data = tibble(record_id = 1),
    dic  = tibble(field_name = "record_id", field_type = "text", form_name = "meta"),
    event_form = NULL
  )

  mock_check_proj <- function(project, data, dic, event_form) {
    list(data = fake_proj$data, dic = fake_proj$dic, event_form = fake_proj$event_form)
  }

  with_mocked_bindings(
    check_proj = mock_check_proj,
    {
      res <- rd_split(project = fake_proj)
      expect_true("form" %in% names(res$data))
    }
  )
})

# ---- 9) which argument selects a single form ----
test_that("rd_split returns only selected form when which is supplied", {
  dic <- tibble(
    field_name = c("record_id", "a1", "b1"),
    field_type = c("text", "text", "text"),
    form_name  = c("meta", "form_a", "form_b")
  )
  data <- tibble(record_id = 1:2, a1 = 1:2, b1 = 3:4)

  res <- rd_split(data = data, dic = dic, by = "form", which = "form_a")
  expect_true(all(c("record_id", "a1") %in% names(res$data)))
  expect_false("b1" %in% names(res$data))
})

# ---- 10) which argument with multiple values warns ----
test_that("rd_split warns if multiple which values are supplied", {
  dic <- tibble(
    field_name = c("record_id", "a1"),
    field_type = c("text", "text"),
    form_name  = c("meta", "form_a")
  )
  data <- tibble(record_id = 1:2, a1 = c("x", "y"))

  expect_warning(
    res <- rd_split(data = data, dic = dic, by = "form", which = c("form_a", "form_b")),
    regexp = "only the first will be used"
  )
})

# ---- 11) wide = TRUE expands into wide format ----
test_that("rd_split expands to wide format when wide = TRUE", {
  dic <- tibble(
    field_name = c("record_id", "abc", "abc_aba"),
    field_type = c("text", "text", "text"),
    form_name  = c("meta", "meta", "form_a")
  )
  data <- tibble(
    record_id = c(1,1,2),
    abc = c("x","y","z"),
    abc_aba = c("x","y","z")
  )

  res <- rd_split(data = data, dic = dic, by = "form", wide = TRUE)
  df_form_a <- res$data |> filter(form == "form_a") |> pull(df) |> pluck(1)

  expect_true(any(grepl("1", names(df_form_a))))
})

# ---- 12) repeated instruments branch ----
test_that("rd_split handles repeated instruments correctly", {
  dic <- tibble(
    field_name = c("record_id", "var0", "var1"),
    field_type = c("text", "text", "text"),
    form_name  = c("formX", "formX", "formX")
  )

  data <- tibble(
    record_id = c(1,1,2),
    # redcap_event_name = c("visit1", "visit1", "visit1"),
    # redcap_event_name.factor = c("Visit1", "Visit1", "Visit1"),
    # redcap_data_access_group = c("hosp1", "hosp1", "hosp2"),
    # redcap_data_access_group.factor = c("Hosp1", "Hosp1", "Hosp2"),
    redcap_repeat_instrument = c("formX", "formX", "formX"),
    redcap_repeat_instrument.factor = c("FormX", "FormX", "FormX"),
    redcap_repeat_instance = c(1:3),
    var0 = c("2", "a", "3"),
    var1 = c("a","b","c")
  )

  res <- rd_split(data = data, dic = dic, by = "form")
  df_formX <- res$data |> filter(form == "formX") |> pull(df) |> pluck(1)

  expect_true("var1" %in% names(df_formX))
  expect_false("redcap_repeat_instrument.factor" %in% names(df_formX))
})

# ---- 13) errors if data or dic missing ----
test_that("rd_split errors if data or dic are missing", {
  dic <- tibble(field_name = "record_id", field_type = "text", form_name = "meta")
  expect_error(rd_split(data = NULL, dic = dic), regexp = "must be provided")
  expect_error(rd_split(data = tibble(record_id = 1), dic = NULL), regexp = "must be provided")
})

