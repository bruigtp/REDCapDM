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

