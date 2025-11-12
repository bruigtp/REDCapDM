test_that("rd_export writes an xlsx file without password and returns success message", {
  df <- head(covican$data, 3)
  path <- tempfile(fileext = ".xlsx")

  expect_message(
    rd_export(queries = df, path = path),
    regexp = "successfully created"
  )

  expect_true(file.exists(path))

  df_read <- openxlsx::read.xlsx(path, sheet = 1, detectDates = TRUE)

  # Convert both to character for a robust comparison
  df_chr     <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  df_read_chr <- as.data.frame(lapply(df_read, as.character), stringsAsFactors = FALSE)

  expect_equal(names(df_read), names(df))
  expect_equal(nrow(df_read), nrow(df))
})


test_that("rd_export warns if links detected but no column argument and no 'Link' column", {
  df_links <- data.frame(
    urlcol = c("https://example.com/a", "no-link"),
    note = "x",
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".xlsx")

  expect_warning(
    rd_export(queries = df_links, path = path, column = NULL),
    regexp = "Links were detected in the dataset"
  )

  expect_true(file.exists(path))
})

test_that("rd_export errors when 'column' argument is provided but column doesn't exist", {
  df <- head(covican$queries, 2)
  path <- tempfile(fileext = ".xlsx")

  expect_error(
    rd_export(queries = df, column = "nonexistent_col", path = path),
    regexp = "The specified column for hyperlinks does not exist"
  )
  expect_false(file.exists(path))
})

test_that("rd_export accepts existing 'column' argument and does not issue link-detection warning", {
  df <- data.frame(
    id = 1:2,
    Link = c("https://x", "https://y"),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".xlsx")

  expect_message(
    rd_export(queries = df, column = "Link", path = path),
    regexp = "successfully created"
  )

  expect_true(file.exists(path))
  df_read <- openxlsx::read.xlsx(path, sheet = 1)
  expect_equal(as.data.frame(df_read, stringsAsFactors = FALSE), df)
})

test_that("rd_export saves with password and returns password-protection message", {
  df <- head(covican$data, 2)
  path <- tempfile(fileext = ".xlsx")
  pw <- "secret_pw"

  expect_message(
    rd_export(queries = df, path = path, password = pw),
    regexp = "with password protection"
  )

  expect_true(file.exists(path))

  df_read <- openxlsx::read.xlsx(path, sheet = 1)

  # --- Normalisation helpers ---
  # Convert Excel serials to Date if column was Date originally
  fix_dates <- function(read_col, original_col) {
    if (inherits(original_col, "Date") && is.numeric(read_col)) {
      as.Date(read_col, origin = "1899-12-30")
    } else {
      read_col
    }
  }

  # Apply to all columns
  for (nm in names(df)) {
    df_read[[nm]] <- fix_dates(df_read[[nm]], df[[nm]])
  }

  # Convert both to character for comparison (ignore factors/levels)
  df_chr      <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  df_read_chr <- as.data.frame(lapply(df_read, as.character), stringsAsFactors = FALSE)

  expect_equal(df_read_chr, df_chr)
})


