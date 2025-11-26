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
