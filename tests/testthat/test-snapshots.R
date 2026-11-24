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

test_that("printing returns the same report invisibly", {
  q <- data.frame(
    Identifier = c("1", "2"),
    Event = "base", EventLabel = "Baseline", Field = "age"
  )
  x <- new_report(list(queries = q, results = "summary"))

  output <- utils::capture.output(
    result <- withVisible(print.REDCapDM_report(x))
  )

  expect_identical(result$value, x)
  expect_false(result$visible)
  expect_match(
    paste(output, collapse = "\n"),
    "2 queries in 2 records", fixed = TRUE
  )
  expect_match(paste(output, collapse = "\n"), "age", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Baseline", fixed = TRUE)
})

test_that("printing handles event, DAG, control and empty reports", {
  event <- data.frame(Identifier = "1", Event = "visit", Field = "-")

  text <- utils::capture.output(
    print.REDCapDM_report(new_report(list(queries = event)))
  )
  expect_match(
    paste(text, collapse = "\n"), "1 query in 1 record", fixed = TRUE
  )

  dag <- new_report(list(queries = list(A = event, B = event)))
  text <- utils::capture.output(print.REDCapDM_report(dag))
  expect_match(
    paste(text, collapse = "\n"), "2 queries in 1 record", fixed = TRUE
  )

  control <- event
  control$Modification <- "Pending"
  text <- utils::capture.output(
    print.REDCapDM_report(new_report(list(queries = control)))
  )
  expect_match(paste(text, collapse = "\n"), "query control", fixed = TRUE)
  expect_match(paste(text, collapse = "\n"), "Pending", fixed = TRUE)

  for (empty in list(event[FALSE, ], list())) {
    text <- utils::capture.output(
      print.REDCapDM_report(new_report(list(queries = empty)))
    )
    expect_match(paste(text, collapse = "\n"), "no queries", fixed = TRUE)
  }

  x <- new_report(list(queries = "invalid"))
  text <- utils::capture.output(
    result <- withVisible(print.REDCapDM_report(x))
  )
  expect_identical(result$value, x)
  expect_false(result$visible)
  expect_match(paste(text, collapse = "\n"), "invalid", fixed = TRUE)
})

test_that("printing distinguishes events sharing a label", {
  q <- data.frame(
    Identifier = "1",
    Event = c("base_arm_1", "base_arm_2"),
    EventLabel = "Baseline", Field = "age"
  )
  output <- utils::capture.output(
    print.REDCapDM_report(new_report(list(queries = q)))
  )
  expect_match(paste(output, collapse = "\n"), "base_arm_1", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "base_arm_2", fixed = TRUE)
})
