# Run with: testthat::test_dir("tests/testthat")
# Synthetic fixtures only; never read real data in tests.
library(testthat)
suppressMessages(source(here::here("scripts", "functions_data.R")))

feature <- function(name, type = "rating_scale", cats = "5") {
  list(name = name, variable_type = type, answer_categories = cats)
}
meta <- list(n_participants = 2, features = list(feature("mood")))
df <- data.frame(id = c(1, 1, 2), day = 1, beep = c(1, 2, 1), mood = c(1, 5, 3))


# check_data() -----------------------------------------------------------------

test_that("matching data and metadata give no warnings", {
  # guards against false alarms, which train people to ignore warnings
  expect_no_warning(result <- check_data(df, meta))
  expect_equal(result, "Data are clean.")
})

test_that("variable in metadata but not in data is flagged", {
  # guards against metadata drifting from the data (e.g. columns removed in cleaning)
  meta$features <- c(meta$features, list(feature("stress")))
  expect_warning(check_data(df, meta), "Variables in metadata but not in data: stress")
})

test_that("column in data but not in metadata is flagged", {
  # guards against columns added in cleaning but never annotated
  df$sleep <- 1
  expect_warning(check_data(df, meta), "not annotated in metadata: sleep")
})

test_that("values outside answer_categories are flagged", {
  # guards against a wrong scale annotation (here: 7-point data, 5 categories)
  df$mood <- c(1, 7, 3)
  expect_warning(check_data(df, meta), "exceeds answer_categories = 5")
})


# read_metadata() --------------------------------------------------------------

test_that("read_metadata() errors unless exactly one file matches", {
  # guards against silently reading the wrong (or no) file
  dir <- tempfile(); dir.create(dir)
  expect_error(read_metadata("9999", dir), "found 0")
  writeLines("{}", file.path(dir, "9999_smith_metadata.json"))
  writeLines("{}", file.path(dir, "9999_jones_metadata.json"))
  expect_error(read_metadata("9999", dir), "found 2")
})


# new_metadata_json() ----------------------------------------------------------

test_that("new_metadata_json() never creates a second file for an existing ID", {
  # guards against overwriting or duplicating metadata via a different author spelling
  dir <- tempfile(); dir.create(dir)
  new_metadata_json("9999", "Smith", df, dir = dir)
  expect_error(new_metadata_json("9999", "J Smith", df, dir = dir), "already exists")
  expect_length(list.files(dir), 1)
})
