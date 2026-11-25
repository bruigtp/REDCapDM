test_that("check_queries validates inputs with covican", {
  expect_error(check_queries(old = "not a df", new = covican$data), "must be data frames")
  expect_error(check_queries(old = covican$data, new = "nope"), "must be data frames")
  expect_error(check_queries(
    old = covican$data[1:5, ],
    new = covican$data[1:5, ],
    report_title = c("a", "b")),
    "more than one title"
  )
})

test_that("check_queries detects Pending, Solved, New and Miscorrected with covican", {
  # Old query report: record_id A (pending), record_id B (solved), and a query of fio2 in 100-21
  old <- data.frame(
    Identifier = c("100-6", "100-13", "100-21"),
    Description = c("age", "potassium", "fio2"),
    Query = c("age out of range", "potassium missing", "fio2 missing"),
    Code = c("100-6-1", "100-13-1", "100-21-1"),
    stringsAsFactors = FALSE
  )

  # New query report:
  # - 100-6 still has same query -> Pending
  # - 100-13 missing -> Solved
  # - 100-20 new -> New
  # - 100-21 has two different queries of fio2, which had another query in the old report -> Miscorrected
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
  # Solved: 100-13 and the old query of 100-21
  expect_equal(as.integer(counts["Solved"]), 2L)
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
  err <- expect_error(check_queries(old = q, new = data.frame(a = 1)), class = "REDCapDM_error_argument")
  expect_match(conditionMessage(err), "Missing columns: Identifier, Description, and Query", fixed = TRUE)
})

test_that("check_queries compares two reports without queries", {
  q0 <- suppressMessages(rd_query(covican, variables = "age", expression = "x > 500", event = "baseline_visit_arm_1"))
  out <- check_queries(q0$queries, q0$queries)
  expect_equal(nrow(out$queries), 0)
  expect_true(all(c("Identifier", "Code", "Modification") %in% names(out$queries)))
  expect_equal(levels(out$queries$Modification), c("Pending", "Solved", "Miscorrected", "New"))
})

test_that("check_queries keeps one pending query when only its link changed", {
  link <- function(domain) list(domain = domain, redcap_version = "13.0.0", proj_id = "1", event_id = c(baseline_visit_arm_1 = "10", follow_up_visit_da_arm_1 = "11"))
  old <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80", link = link("redcap.a.org"))))$queries
  new <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80", link = link("redcap.b.org"))))$queries
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(nrow(out), nrow(new))
  expect_true(all(out$Modification == "Pending"))
  # The link of the new report
  expect_true(all(grepl("redcap.b.org", out$Link, fixed = TRUE)))
})

test_that("check_queries tells apart two fields with the same description", {
  base <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80")))$queries[1, ]
  old <- base
  old$Field <- "weight_v1"
  old$Description <- "Weight"
  new <- old
  new$Field <- "weight_v2"
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification[order(out$Field)]), c("Solved", "New"))
})

test_that("check_queries stops with queries without identifier", {
  base <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80")))$queries[1:2, ]
  old <- base
  old$Identifier <- c(NA, old$Identifier[2])
  new <- base
  new$Identifier <- c("NA", new$Identifier[2])
  expect_error(check_queries(old = old, new = new, return_viewer = FALSE), class = "REDCapDM_error_argument")
})

test_that("check_queries keeps one pending query when the link or the DAG are factors with different levels", {
  link <- function(domain) list(domain = domain, redcap_version = "13.0.0", proj_id = "1", event_id = c(baseline_visit_arm_1 = "10", follow_up_visit_da_arm_1 = "11"))
  old <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80", link = link("redcap.a.org"))))$queries
  new <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80", link = link("redcap.b.org"))))$queries
  new$DAG[1] <- "hospital_99"
  old$Link <- factor(old$Link)
  new$Link <- factor(new$Link)
  old$DAG <- factor(old$DAG)
  new$DAG <- factor(new$DAG)
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(nrow(out), nrow(new))
  expect_true(all(out$Modification == "Pending"))
  # The link and the DAG of the new report
  expect_false(anyNA(out$Link))
  expect_true(all(grepl("redcap.b.org", out$Link, fixed = TRUE)))
  expect_true("hospital_99" %in% out$DAG)

  # Factors with the same levels in both reports are kept as factors
  same <- suppressMessages(check_queries(old = old, new = old, return_viewer = FALSE))$queries
  expect_s3_class(same$Link, "factor")
  expect_true(all(same$Modification == "Pending"))
})

test_that("check_queries tells apart queries whose texts have carriage returns", {
  base <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80")))$queries[1, ]
  # (without Field, the description identifies the field of a query)
  base$Field <- NULL
  old <- base
  old$Description <- "Age\rRule"
  old$Query <- "Check"
  new <- base
  new$Description <- "Age"
  new$Query <- "Rule\rCheck"
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(nrow(out), 2)
  expect_setequal(as.character(out$Modification), c("Solved", "New"))
  expect_setequal(out$Description, c("Age\rRule", "Age"))
})

test_that("check_queries takes the same text in different encodings for the same query", {
  base <- suppressWarnings(suppressMessages(rd_query(covican, variables = "age", expression = "x > 80")))$queries[1:2, ]
  utf8 <- "M\u00fcller"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  old <- base
  old$Description <- utf8
  old$Query <- paste("Check", utf8)
  new <- base
  new$Description <- latin1
  new$Query <- paste("Check", latin1)
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), c("Pending", "Pending"))

  # As factors, and with both encodings in each report
  new$Description <- factor(new$Description)
  expect_equal(nrow(suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries), 2)
  old$Description <- c(latin1, utf8)
  new$Description <- c(utf8, latin1)
  out <- suppressMessages(check_queries(old = old, new = new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), c("Pending", "Pending"))
})

test_that("check_queries only marks as Miscorrected a new query whose record and field had a query in the old report", {
  q <- function(id, query, field = "a", ...) data.frame(Identifier = id, DAG = "-", Event = "E1", Instrument = "f", Field = field, Repetition = "-",
                                                     Description = "A", Query = query, Code = paste0(id, "-1"), ..., stringsAsFactors = FALSE)
  new <- rbind(q("1", "q1"), q("1", "q2"))
  status <- function(old) as.character(check_queries(old, new, return_viewer = FALSE)$queries$Modification)
  expect_equal(status(q("1", "x")[0, ]), c("New", "New"))
  expect_equal(sort(status(q("2", "q0"))), c("New", "New", "Solved"))
  expect_equal(sort(status(q("1", "q0", field = "b"))), c("New", "New", "Solved"))
  expect_equal(sort(status(q("1", "q0"))), c("Miscorrected", "Miscorrected", "Solved"))
  expect_equal(sort(status(q("1", "q1"))), c("Miscorrected", "Pending"))
})

test_that("check_queries keeps the columns of the reports called like its internal columns", {
  q <- function(query, ...) data.frame(Identifier = "1", Description = "A", Query = query, Code = "1-1", ..., stringsAsFactors = FALSE)
  for (to_tibble in c(FALSE, TRUE)) {
    old <- rbind(q("q1", comp = "c1", comp2 = "d1", n = 1, cod = "k1", note = "n1"), q("q2", comp = "c2", comp2 = "d2", n = 2, cod = "k2", note = "n2"))
    new <- rbind(q("q1", comp = "c3", comp2 = "d3", n = 3, cod = "k3", note = "n3"), q("q3", comp = "c4", comp2 = "d4", n = 4, cod = "k4", note = "n4"))
    if (to_tibble) {
      old <- dplyr::as_tibble(old)
      new <- dplyr::as_tibble(new)
    }
    out <- check_queries(old, new, return_viewer = FALSE)$queries
    expect_true(all(c("comp", "comp2", "n", "cod", "note") %in% names(out)))
    expect_equal(out$comp[match(c("q1", "q2", "q3"), out$Query)], c("c3", "c2", "c4"))
    expect_equal(out$n[match(c("q1", "q2", "q3"), out$Query)], c(3, 2, 4))
  }
})


# ---- The event of a query is its unique name: renaming labels doesn't solve queries ----

event_project <- function(label1 = "Enrollment (Arm 1: Drug A)", label2 = "Enrollment (Arm 2: Placebo)", age = c(NA, 40, NA, 50)) {
  data <- data.frame(
    record_id = c("1", "2", "3", "4"),
    redcap_event_name = c("enrollment_arm_1", "enrollment_arm_1", "enrollment_arm_2", "enrollment_arm_2"),
    age = age
  )
  data$redcap_event_name.factor <- factor(c(label1, label1, label2, label2))
  list(
    data = data,
    dictionary = data.frame(field_name = c("record_id", "age"), form_name = "enrollment", field_type = "text", field_label = c("Record ID", "Age"), branching_logic = "")
  )
}
event_queries <- function(project) suppressWarnings(suppressMessages(rd_query(project, variables = "age", expression = "is.na(x)")))$queries

test_that("renaming an event, an arm or a field in REDCap doesn't solve its queries", {
  old <- event_queries(event_project())
  expect_equal(old$Event, c("enrollment_arm_1", "enrollment_arm_2"))
  expect_equal(old$EventLabel, c("Enrollment (Arm 1: Drug A)", "Enrollment (Arm 2: Placebo)"))

  # The event and the arm renamed: the same queries, with the new labels
  new <- event_queries(event_project("Inclusion (Arm 1: Drug A)", "Enrollment (Arm 2: Active comparator)"))
  out <- suppressMessages(check_queries(old, new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), c("Pending", "Pending"))
  expect_equal(out$EventLabel, c("Inclusion (Arm 1: Drug A)", "Enrollment (Arm 2: Active comparator)"))

  # The label of the field renamed
  renamed <- event_project()
  renamed$dictionary$field_label[2] <- "Age (years)"
  out <- suppressMessages(check_queries(old, event_queries(renamed), return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), c("Pending", "Pending"))
  expect_equal(out$Description, c("Age (years)", "Age (years)"))
})

test_that("check_queries doesn't merge the queries of events with the same label", {
  shared <- function(age) {
    p <- event_project("Enrollment", "Enrollment", age)
    p$data$record_id <- c("1", "2", "1", "2")
    p
  }
  # Record 1: the query of arm 1 is solved and there is a new one in arm 2
  out <- suppressMessages(check_queries(event_queries(shared(c(NA, 40, 50, 60))), event_queries(shared(c(30, 40, NA, 60))), return_viewer = FALSE))$queries
  expect_equal(out$Event[order(out$Event)], c("enrollment_arm_1", "enrollment_arm_2"))
  expect_equal(as.character(out$Modification[order(out$Event)]), c("Solved", "New"))
})

test_that("check_queries compares the reports created before EventLabel existed (the label in Event)", {
  new <- event_queries(event_project())
  old <- new
  old$Event <- old$EventLabel
  old$EventLabel <- NULL
  out <- suppressMessages(check_queries(old, new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), c("Pending", "Pending"))
  expect_equal(out$Event, c("enrollment_arm_1", "enrollment_arm_2"))

  # A label of several events can't be told apart: its queries are compared with the label
  shared <- event_queries(event_project("Enrollment", "Enrollment"))
  old <- shared
  old$Event <- old$EventLabel
  old$EventLabel <- NULL
  out <- suppressMessages(check_queries(old, shared, return_viewer = FALSE))$queries
  expect_setequal(as.character(out$Modification), c("Solved", "New"))
})

test_that("renaming an event doesn't solve the queries of rd_event()", {
  project <- function(label) {
    data <- data.frame(record_id = c("1", "1", "2"), redcap_event_name = c("base_arm_1", "visit_arm_1", "base_arm_1"))
    data$redcap_event_name.factor <- factor(c("Baseline", label, "Baseline"))
    list(data = data, dictionary = data.frame(field_name = "record_id", form_name = "f1", field_type = "text", field_label = "ID"))
  }
  old <- suppressMessages(rd_event(project("Visit"), event = "visit_arm_1"))$queries
  new <- suppressMessages(rd_event(project("Visita"), event = "visit_arm_1"))$queries
  expect_equal(old$Query, "The event 'visit_arm_1' is missing.")
  expect_equal(new$EventLabel, "Visita")
  out <- suppressMessages(check_queries(old, new, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), "Pending")
  expect_equal(out$EventLabel, "Visita")

  # (a report created before EventLabel existed, with the label in its text)
  older <- old
  older$Event <- "Visit"
  older$EventLabel <- NULL
  older$Query <- "The event 'Visit' is missing."
  out <- suppressMessages(check_queries(older, old, return_viewer = FALSE))$queries
  expect_equal(as.character(out$Modification), "Pending")
})


test_that("the record identifiers are written as text without scientific notation or lost decimals", {
  expect_equal(id_text(c(1, -5, 0, -0, 123456789012, 1e15, 1e20, NA)),
               c("1", "-5", "0", "0", "123456789012", "1000000000000000", "100000000000000000000", NA))
  expect_equal(id_text(c(0.1 + 0.2, 2.5, Inf)), c("0.30000000000000004", "2.5", "Inf"))
  expect_equal(id_text(c("007", NA)), c("007", NA))
  # (each number is the identifier again)
  x <- c(sample(1:1e6, 50), runif(50) * 1e6, rnorm(50) * 1e17)
  expect_equal(as.numeric(id_text(x)), x)
})
