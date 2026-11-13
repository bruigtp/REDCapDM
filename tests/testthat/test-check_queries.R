test_that("check_queries validates inputs with covican", {
  expect_error(check_queries(old = "not a df", new = covican$data), "must be a data frame")
  expect_error(check_queries(old = covican$data, new = "nope"), "must be a data frame")
  expect_error(check_queries(
    old = covican$data[1:5, ],
    new = covican$data[1:5, ],
    report_title = c("a", "b")),
    "more than one title"
  )
})

test_that("check_queries detects Pending, Solved, New and Miscorrected with covican", {
  # Old query report: record_id A (pending), record_id B (solved)
  old <- data.frame(
    Identifier = c("100-6", "100-13"),
    Description = c("age", "potassium"),
    Query = c("age out of range", "potassium missing"),
    Code = c("100-6-1", "100-13-1"),
    stringsAsFactors = FALSE
  )

  # New query report:
  # - 100-6 still has same query -> Pending
  # - 100-13 missing -> Solved
  # - 100-20 new -> New
  # - 100-21 has two different queries -> Miscorrected
  new <- data.frame(
    Identifier = c("100-6", "100-20", "100-21", "100-21"),
    Description = c("age", "dm", "fio2", "fio2"),
    Query = c("age out of range", "dm missing", "fio2 invalid", "fio2 not recorded"),
    stringsAsFactors = FALSE
    # Code intentionally missing
  )

  res <- check_queries(old = old, new = new)
  qdf <- res$queries

  # Ensure expected modification categories exist
  expect_true(all(c("Pending", "Solved", "Miscorrected", "New") %in% levels(qdf$Modification)))

  counts <- table(as.character(qdf$Modification), useNA = "ifany")

  # Pending: 100-6
  expect_equal(as.integer(counts["Pending"]), 1L)
  # Solved: 100-13
  expect_equal(as.integer(counts["Solved"]), 1L)
  # New: 100-20
  expect_equal(as.integer(counts["New"]), 1L)
  # Miscorrected: 100-21 has 2 queries
  expect_equal(as.integer(counts["Miscorrected"]), 2L)
})

test_that("check_queries preserves Code creation and returns results viewer with covican", {
  old <- data.frame(
    Identifier = c("200-1", "200-2"),
    Description = c("dm", "copd"),
    Query = c("dm missing", "copd missing"),
    Code = c("200-1-1", "200-2-1"),
    stringsAsFactors = FALSE
  )

  new <- old[1, , drop = FALSE]  # only first row remains, second should be Solved

  out <- check_queries(old, new)

  expect_true(is.list(out))
  expect_true("queries" %in% names(out))
  expect_true("results" %in% names(out))
  expect_true("Code" %in% names(out$queries))
  expect_true(is.character(out$queries$Code))
})


# ---- Events and repeated instruments ----

make_report <- function(identifier, event, repetition, query = "The value is NA and it should not be missing") {
  data.frame(
    Identifier = identifier, DAG = "-", Event = event, Instrument = "Ae", Field = "ae_term",
    Repetition = repetition, Description = "AE term", Query = query, stringsAsFactors = FALSE
  )
}

test_that("check_queries tells instances of a repeated instrument apart", {
  old <- make_report(c("1", "1"), "-", c("ae-1", "ae-2"))
  old$Code <- c("1-1", "1-2")
  new <- make_report("1", "-", "ae-2")

  res <- check_queries(old, new, return_viewer = FALSE)

  expect_equal(as.character(res$queries$Modification[res$queries$Repetition == "ae-1"]), "Solved")
  expect_equal(as.character(res$queries$Modification[res$queries$Repetition == "ae-2"]), "Pending")
})

test_that("check_queries tells events apart", {
  old <- make_report(c("1", "1"), c("baseline", "follow_up"), "-")
  old$Code <- c("1-1", "1-2")
  new <- make_report("1", "follow_up", "-")

  res <- check_queries(old, new, return_viewer = FALSE)

  expect_equal(as.character(res$queries$Modification[res$queries$Event == "baseline"]), "Solved")
  expect_equal(as.character(res$queries$Modification[res$queries$Event == "follow_up"]), "Pending")
})

test_that("check_queries does not flag a query in another instance as Miscorrected", {
  old <- make_report("1", "-", "ae-1")
  old$Code <- "1-1"
  new <- make_report(c("1", "1"), "-", c("ae-1", "ae-2"))

  res <- check_queries(old, new, return_viewer = FALSE)

  expect_equal(as.character(res$queries$Modification[res$queries$Repetition == "ae-2"]), "New")
})

test_that("check_queries keeps alphanumeric identifiers and sorts them naturally", {
  old <- data.frame(Identifier = c("ABC10", "ABC2"), Description = "x", Query = "q", Code = c("ABC10-1", "ABC2-1"))
  new <- data.frame(Identifier = c("ABC10", "ABC2"), Description = "x", Query = "q")

  res <- check_queries(old, new, return_viewer = FALSE)

  expect_equal(res$queries$Identifier, c("ABC2", "ABC10"))
  expect_equal(res$queries$Code, c("ABC2-1", "ABC10-1"))
})


test_that("check_queries needs query reports", {
  q <- suppressWarnings(rd_query(covican, variables = "copd", expression = "is.na(x)"))$queries
  expect_error(check_queries(old = q, new = data.frame(a = 1)), "Missing columns: Identifier, Description, Query")
})
