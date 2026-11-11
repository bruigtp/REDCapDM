test_that("repeat_info detects repeating instruments, repeating events and their event/form pairs", {
  classic <- repeat_info(make_repeat_classic()$data)
  expect_true(classic$instruments)
  expect_false(classic$events)
  expect_equal(classic$pairs$form, "ae")

  long <- repeat_info(make_repeat_longitudinal()$data)
  expect_true(long$instruments)
  expect_true(long$events)
  expect_equal(long$pairs, data.frame(event = "treatment_arm_1", form = "labs"), ignore_attr = TRUE)
})

test_that("repeat_info detects a project with only repeating events", {
  data <- data.frame(record_id = "1", redcap_repeat_instrument = NA, redcap_repeat_instance = c(1, 2), x = 1:2)
  info <- repeat_info(data)
  expect_false(info$instruments)
  expect_true(info$events)
  expect_true(info$any)
})

test_that("repeat_info reports nothing for a project without repetitions", {
  info <- repeat_info(data.frame(record_id = 1:2, x = 1:2))
  expect_false(info$any)
  expect_equal(nrow(info$pairs), 0)
})

test_that("repeat_home_rows keeps each variable in the rows where it is collected (classic project)", {
  fx <- make_repeat_classic()
  expect_equal(repeat_home_rows(fx$data, fx$dictionary, "age"), c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(repeat_home_rows(fx$data, fx$dictionary, "ae_term"), c(FALSE, TRUE, TRUE, FALSE, TRUE))
  # REDCap system variables live in every row
  expect_true(all(repeat_home_rows(fx$data, fx$dictionary, "record_id")))
  expect_true(all(repeat_home_rows(fx$data, fx$dictionary, "redcap_repeat_instance")))
})

test_that("repeat_home_rows handles forms repeating in only some events and repeating events", {
  fx <- make_repeat_longitudinal()
  # labs: base row at baseline, instance rows at treatment, never the treatment base row
  expect_equal(repeat_home_rows(fx$data, fx$dictionary, "hb"), c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
  # repeating-event rows are base rows for the forms of that event
  expect_equal(repeat_home_rows(fx$data, fx$dictionary, "fu_status"), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("repeat_home_rows uses the pairs of the full dataset when given a subset", {
  fx <- make_repeat_longitudinal()
  pairs <- repeat_info(fx$data)$pairs
  treatment_base <- fx$data[2, ]
  expect_false(repeat_home_rows(treatment_base, fx$dictionary, "hb", pairs))
})

test_that("repeat_home_rows falls back to all rows when it can't tell the structure", {
  fx <- make_repeat_classic()
  n <- nrow(fx$data)
  expect_equal(repeat_home_rows(fx$data[, c("record_id", "age")], fx$dictionary, "age"), rep(TRUE, n))
  expect_equal(repeat_home_rows(fx$data, fx$dictionary, "not_in_dic"), rep(TRUE, n))
  expect_equal(repeat_home_rows(fx$data, NULL, "age"), rep(TRUE, n))

  labelled_instr <- fx$data
  labelled_instr$redcap_repeat_instrument <- as.character(labelled_instr$redcap_repeat_instrument.factor)
  expect_equal(repeat_home_rows(labelled_instr, fx$dictionary, "age"), rep(TRUE, n))
})

test_that("var_form resolves factor, checkbox, recalculated and form-level variables", {
  dic <- data.frame(field_name = c("record_id", "age", "chk"), form_name = c("demo", "demo", "visit"))
  expect_equal(var_form("age", dic), "demo")
  expect_equal(var_form("age.factor", dic), "demo")
  expect_equal(var_form("chk___1", dic), "visit")
  expect_equal(var_form("chk___1.factor", dic), "visit")
  expect_equal(var_form("age_recalc", dic), "demo")
  expect_equal(var_form("visit_complete", dic), "visit")
  expect_equal(var_form("visit_timestamp", dic), "visit")
  expect_true(is.na(var_form("record_id", dic)))
  expect_true(is.na(var_form("unknown", dic)))
})

test_that("repetition_label describes each row", {
  expect_equal(repetition_label(make_repeat_classic()$data), c("-", "ae-1", "ae-2", "-", "ae-1"))
  expect_equal(repetition_label(make_repeat_longitudinal()$data), c("-", "-", "labs-1", "labs-2", "1", "2"))
  expect_equal(repetition_label(data.frame(record_id = 1:2)), c("-", "-"))
})
