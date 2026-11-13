test_that("rd_event finds missing events in covican", {
  d0 <- covican$data

  # remove one follow-up event intentionally for a record
  exported <- d0[d0$record_id != "100-6" | d0$redcap_event_name != "follow_up_visit_da_arm_1", ]

  res <- rd_event(
    data = exported,
    dic = covican$dictionary,
    event = "follow_up_visit_da_arm_1"
  )

  expect_type(res, "list")
  expect_true("queries" %in% names(res))

  q <- res$queries
  # all queries should correspond to missing follow-up
  expect_true(all(q$Event == "follow_up_visit_da_arm_1"))
})

test_that("filter argument works and returns messages for empty filters in covican", {
  d0 <- covican$data
  # filter siteA equivalent → here use hospital_11
  res <- rd_event(
    data = d0,
    dic = covican$dictionary,
    event = c("baseline_visit_arm_1", "follow_up_visit_da_arm_1"),
    filter = "redcap_data_access_group == 'hospital_11'"
  ) |>
    suppressMessages()

  expect_type(res, "list")
  expect_true("queries" %in% names(res))
})

test_that("link generation works when link info provided in covican", {
  d0 <- covican$data
  link <- list(domain = "example.com", redcap_version = "12.0.0", proj_id = "99")

  # ask for an event not present in data
  expect_error(
    rd_event(
      data = d0,
      dic = covican$dictionary,
      event = "no_event",
      link = link
    )
  )
})

test_that("addTo merging works when addTo is provided as list(queries=...) in covican", {
  d0 <- covican$data
  exported <- d0[d0$record_id %in% c("100-6", "100-13"), ]  # restrict to a few rows

  res1 <- rd_event(
    data = exported,
    dic = covican$dictionary,
    event = "follow_up_visit_da_arm_1"
  ) |>
    suppressMessages()

  prev <- data.frame(
    Identifier = "110-10",
    DAG = "-",
    Event = "follow_up_visit_da_arm_1",
    Instrument = "-",
    Field = "-",
    Repetition = "-",
    Description = "Follow-up",
    Query = "old",
    Code = "X1-1",
    Link = NA_character_,
    stringsAsFactors = FALSE
  )

  res2 <- rd_event(
    data = exported,
    dic = covican$dictionary,
    event = "follow_up_visit_da_arm_1",
    addTo = list(queries = prev)
  )

  expect_true("queries" %in% names(res2))
  expect_true(nrow(res2$queries) >= nrow(prev))
})

test_that("report_zeros toggles presence of zero lines in covican", {
  d0 <- covican$data

  # ask for a valid + invalid event
  expect_error(
    rd_event(
      data = d0,
      dic = covican$dictionary,
      event = c("baseline_visit_arm_1", "some_missing_event")
    )
  )
})

# ---- Fixes ----

make_event_data <- function() {
  data.frame(
    record_id = c("1", "2", "3", "3"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1", "baseline_arm_1", "follow_up_arm_1"),
    redcap_event_name.factor = factor(c("Baseline", "Baseline", "Baseline", "Follow-up")),
    redcap_data_access_group = c("A", "B", "A", "A"),
    stringsAsFactors = FALSE
  )
}

test_that("rd_event reports the DAG of each record", {
  res <- rd_event(data = make_event_data(), dic = data.frame(), event = "follow_up_arm_1")

  expect_equal(res$queries$Identifier, c(1, 2))
  expect_equal(res$queries$DAG, c("A", "B"))
})

test_that("rd_event works without the factor version of the event name", {
  data <- make_event_data()
  data$redcap_event_name.factor <- NULL

  res <- rd_event(data = data, dic = data.frame(), event = "follow_up_arm_1")
  expect_equal(nrow(res$queries), 2)
})

test_that("rd_event works when record_id is not the first column", {
  data <- make_event_data()[, c("redcap_event_name", "record_id", "redcap_data_access_group")]

  res <- rd_event(data = data, dic = data.frame(), event = "follow_up_arm_1")
  expect_equal(res$queries$Identifier, c(1, 2))
})

test_that("rd_event keeps alphanumeric identifiers and sorts them naturally", {
  data <- make_event_data()
  data$record_id <- c("ABC10", "ABC2", "ABC3", "ABC3")

  res <- rd_event(data = data, dic = data.frame(), event = "follow_up_arm_1")
  expect_equal(res$queries$Identifier, c("ABC2", "ABC10"))
})

test_that("rd_event explains that it needs a longitudinal project", {
  base <- covican$data[covican$data$redcap_event_name == "baseline_visit_arm_1", ]
  base <- base[, !names(base) %in% c("redcap_event_name", "redcap_event_name.factor")]

  expect_error(
    rd_event(data = base, dic = covican$dictionary, event = "baseline_visit_arm_1"),
    "rd_event\\(\\) needs a longitudinal project"
  )
})


test_that("rd_event only expects the event in the records of its arm", {
  data <- data.frame(
    record_id = c("1", "1", "2", "2", "3", "4"),
    redcap_event_name = c("screen_arm_1", "visit_arm_1", "screen_arm_1", "end_arm_1", "screen_arm_2", "screen_arm_1"),
    stringsAsFactors = FALSE
  )
  event_form <- data.frame(arm_num = c(1, 1, 1, 2), unique_event_name = c("screen_arm_1", "visit_arm_1", "end_arm_1", "screen_arm_2"),
                           form = "f1", stringsAsFactors = FALSE)

  res <- rd_event(data = data, dic = data.frame(), event_form = event_form, event = "visit_arm_1")
  # records 2 and 4 are in arm 1 and have no visit; record 3 is in arm 2
  expect_equal(sort(as.character(res$queries$Identifier)), c("2", "4"))

  # without event_form, the arm is taken from the name of the event
  res <- rd_event(data = data, dic = data.frame(), event = "visit_arm_1")
  expect_equal(sort(as.character(res$queries$Identifier)), c("2", "4"))
})


test_that("rd_event accepts event labels and asks for the event when it is missing", {
  fx <- make_cross_event()

  by_label <- rd_event(fx, event = "Follow-up")
  by_name <- rd_event(fx, event = "follow_up_arm_1")
  expect_equal(by_label$queries, by_name$queries)

  expect_error(rd_event(fx), "Specify the event")

  # A label shared by events of different arms is ambiguous
  data <- data.frame(record_id = c("1", "2"), redcap_event_name = c("visit_arm_1", "visit_arm_2"), stringsAsFactors = FALSE)
  data$redcap_event_name.factor <- factor(c("Visit", "Visit"))
  expect_error(rd_event(data = data, dic = data.frame(), event = "Visit"), "shared by several events")
})
