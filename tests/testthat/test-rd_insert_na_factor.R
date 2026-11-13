test_that("rd_insert_na also sets NA in the .factor version of the variable", {
  res <- rd_insert_na(covican, vars = "dm", filter = "age < 65")

  baseline <- covican$data$redcap_event_name == "baseline_visit_arm_1"
  young <- baseline & !is.na(covican$data$age) & covican$data$age < 65
  older <- baseline & !is.na(covican$data$age) & covican$data$age >= 65

  expect_true(all(is.na(res$data$dm[young])))
  expect_true(all(is.na(res$data$dm.factor[young])))
  # the other patients keep their values in both versions
  expect_equal(as.character(res$data$dm.factor[older]), as.character(covican$data$dm.factor[older]))

  # rd_factor() afterwards keeps the NA
  fct <- suppressWarnings(rd_factor(res))
  expect_true(all(is.na(fct$data$dm[young])))
})
