# One-time reverse sync: copy metadata JSONs from openesm-metadata to openesm-cleaning
#
# Pairs files by dataset_id (read from JSON contents).
# Uses cleaning's existing filenames as copy targets.
# Validates each copied file against the schema.
#
# Usage:
#   Rscript scripts/sync_metadata_from_repo.R <path-to-openesm-metadata>

library(jsonlite)
library(here)
source(here("scripts", "functions_data.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript scripts/sync_metadata_from_repo.R <path-to-openesm-metadata>")
}
metadata_repo <- normalizePath(args[1], mustWork = TRUE)
cleaning_dir <- here("data", "metadata")
metadata_datasets_dir <- file.path(metadata_repo, "datasets")

# Build cleaning map: dataset_id -> filename
cleaning_files <- list.files(cleaning_dir, pattern = "_metadata\\.json$",
                             full.names = TRUE)
cleaning_map <- list()
for (f in cleaning_files) {
  j <- read_json(f)
  did <- j$dataset_id
  if (!is.null(did) && nzchar(did)) {
    cleaning_map[[did]] <- f
  }
}

# Iterate metadata-repo folders
metadata_folders <- list.dirs(metadata_datasets_dir, recursive = FALSE)
copied <- 0
errors <- character(0)

for (folder in sort(metadata_folders)) {
  folder_name <- basename(folder)
  src <- file.path(folder, paste0(folder_name, "_metadata.json"))
  if (!file.exists(src)) {
    message("SKIP ", folder_name, ": no metadata JSON found")
    next
  }

  j <- read_json(src)
  did <- j$dataset_id

  # Determine destination: use cleaning's existing filename if available
  if (did %in% names(cleaning_map)) {
    dest <- cleaning_map[[did]]
  } else {
    dest <- file.path(cleaning_dir, paste0(folder_name, "_metadata.json"))
    message("NEW  ", did, ": not in cleaning, creating ", basename(dest))
  }

  file.copy(src, dest, overwrite = TRUE)
  copied <- copied + 1
  message(sprintf("  %s: %s -> %s", did, basename(src), basename(dest)))

  # Validate against schema
  result <- tryCatch(
    validate_metadata_json(dest),
    error = function(e) e$message
  )
  if (is.character(result)) {
    errors <- c(errors, sprintf("%s: %s", did, result))
  }
}

cat(sprintf("\nCopied %d files\n", copied))
if (length(errors) > 0) {
  cat("\nValidation errors:\n")
  cat(paste(" ", errors, collapse = "\n"), "\n")
} else {
  cat("All copied files pass schema validation.\n")
}
