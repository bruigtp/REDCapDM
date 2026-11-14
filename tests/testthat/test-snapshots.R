# Snapshots (tests/testthat/_snaps/snapshots.md) keep the exact text of the printed reports and of the main
# messages. If a change is intended, review it and accept it with testthat::snapshot_accept().

test_that("the query reports keep being lists with the queries and the results", {
  q <- suppressWarnings(rd_query(covican, variables = c("copd", "age"), expression = c("is.na(x)", "x > 70"), event = "baseline_visit_arm_1"))

  expect_s3_class(q, "REDCapDM_report")
  expect_type(q, "list")
  expect_named(q, c("queries", "results"))
  expect_s3_class(q$queries, "data.frame")
  expect_s3_class(rd_event(covican, event = "follow_up_visit_da_arm_1"), "REDCapDM_report")
  expect_s3_class(check_queries(old = q$queries, new = q$queries), "REDCapDM_report")

  output <- utils::capture.output(printed <- withVisible(print(q)))
  expect_false(printed$visible)
  expect_identical(printed$value, q)
  expect_match(output[1], "82 queries in 81 records", fixed = TRUE)

  # The report can still be added to another one
  q2 <- suppressWarnings(rd_query(covican, variables = "dm", expression = "is.na(x)", event = "baseline_visit_arm_1", add_to = q))
  expect_gt(nrow(q2$queries), nrow(q$queries))
})

test_that("the query reports print a summary", {
  report <- suppressWarnings(rd_query(covican, variables = c("copd", "age"), expression = c("is.na(x)", "x > 70"), event = "baseline_visit_arm_1"))
  events <- rd_event(covican, event = "follow_up_visit_da_arm_1")
  control <- check_queries(old = report$queries, new = report$queries[1:3, ])
  none <- suppressMessages(suppressWarnings(rd_query(covican, variables = "age", expression = "x > 500", event = "baseline_visit_arm_1")))
  by_dag <- suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)", event = "baseline_visit_arm_1", by_dag = TRUE))

  expect_snapshot({
    report
    events
    control
    none
    by_dag
  })
})

test_that("the main errors and warnings keep their format", {
  expect_snapshot(error = TRUE, rd_query(covican, variables = "copd", expression = "is.na(x)", negate = "yes"))
  expect_snapshot(error = TRUE, rd_query(covican, variables = "agee", expression = "is.na(x)"))
  expect_snapshot(error = TRUE, rd_delete_vars(covican, vars = c("copd", "nope1")))
  expect_snapshot(error = TRUE, rd_split(covican, by = "form", which = "nope"))
  expect_snapshot(error = TRUE, rd_rlogic(covican, logic = "[nope] = '1'", var = "age"))
  expect_snapshot(error = TRUE, rd_event(covican, event = "nope"))
  expect_snapshot(res <- rd_recalculate(covican, exclude = "copd"))
})
