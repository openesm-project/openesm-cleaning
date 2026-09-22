# Diff metadata JSONs between openesm-cleaning and openesm-metadata
#
# Pairs files by dataset_id (read from JSON contents, not filenames).
# Produces a CSV report classifying each dataset as identical, differs,
# cleaning_only, or metadata_only.
#
# Usage:
#   Rscript scripts/diff_metadata_repos.R <path-to-openesm-metadata>
#
# Output:
#   data/metadata_sync_report.csv

library(jsonlite)
library(here)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript scripts/diff_metadata_repos.R <path-to-openesm-metadata>")
}
metadata_repo <- normalizePath(args[1], mustWork = TRUE)

# Load all metadata JSONs from a directory tree, keyed by dataset_id
load_jsons <- function(dir) {
  files <- list.files(dir, pattern = "_metadata\\.json$",
                      recursive = TRUE, full.names = TRUE)
  result <- list()
  for (f in files) {
    tryCatch({
      j <- read_json(f)
      did <- j$dataset_id
      if (!is.null(did) && nzchar(did)) {
        result[[did]] <- list(path = f, data = j)
      }
    }, error = function(e) {
      warning(sprintf("Failed to read %s: %s", f, e$message))
    })
  }
  result
}

cleaning_dir <- here("data", "metadata")
cat("Loading cleaning metadata from:", cleaning_dir, "\n")
cleaning <- load_jsons(cleaning_dir)
cat("Loading metadata-repo files from:", metadata_repo, "\n")
metadata <- load_jsons(file.path(metadata_repo, "datasets"))

all_ids <- sort(unique(c(names(cleaning), names(metadata))))
cat(sprintf("Found %d cleaning, %d metadata, %d unique dataset IDs\n",
            length(cleaning), length(metadata), length(all_ids)))

# Compare each dataset
rows <- lapply(all_ids, function(did) {
  c_entry <- cleaning[[did]]
  m_entry <- metadata[[did]]

  if (is.null(c_entry)) {
    return(data.frame(dataset_id = did, status = "metadata_only",
                      changelog_diff = "", version_diff = "",
                      feature_count_diff = "", other_diffs = "",
                      stringsAsFactors = FALSE))
  }
  if (is.null(m_entry)) {
    return(data.frame(dataset_id = did, status = "cleaning_only",
                      changelog_diff = "", version_diff = "",
                      feature_count_diff = "", other_diffs = "",
                      stringsAsFactors = FALSE))
  }

  cj <- c_entry$data
  mj <- m_entry$data

  # Changelog
  cc <- cj$changelog
  mc <- mj$changelog
  cc_len <- if (is.list(cc) && !is.null(names(cc)) && length(cc) == 0) 0L
             else if (is.list(cc)) length(cc) else 0L
  mc_len <- if (is.list(mc) && !is.null(names(mc)) && length(mc) == 0) 0L
             else if (is.list(mc)) length(mc) else 0L
  changelog_diff <- if (identical(cc, mc)) "" else
    sprintf("cleaning=%d entries, metadata=%d entries", cc_len, mc_len)

  # Version
  cv <- cj$dataset_version %||% ""
  mv <- mj$dataset_version %||% ""
  version_diff <- if (identical(cv, mv)) "" else
    sprintf("%s vs %s", cv, mv)

  # Feature count
  cf <- length(cj$features)
  mf <- length(mj$features)
  feature_count_diff <- if (cf == mf) "" else
    sprintf("%d vs %d", cf, mf)

  # Other top-level fields
  skip <- c("features", "changelog", "dataset_version")
  all_keys <- unique(c(names(cj), names(mj)))
  all_keys <- setdiff(all_keys, skip)
  other <- character(0)
  for (key in all_keys) {
    if (!identical(cj[[key]], mj[[key]])) {
      vc <- if (is.null(cj[[key]])) "NULL" else substr(as.character(cj[[key]]), 1, 60)
      vm <- if (is.null(mj[[key]])) "NULL" else substr(as.character(mj[[key]]), 1, 60)
      other <- c(other, sprintf("%s: %s vs %s", key, vc, vm))
    }
  }

  # Feature content
  features_differ <- FALSE
  if (cf == mf) {
    for (i in seq_along(cj$features)) {
      if (!identical(cj$features[[i]], mj$features[[i]])) {
        features_differ <- TRUE
        break
      }
    }
  } else {
    features_differ <- TRUE
  }
  if (features_differ) other <- c(other, "features: content differs")

  status <- if (length(changelog_diff) == 0 || changelog_diff == "") {
    if (length(version_diff) == 0 || version_diff == "") {
      if (length(feature_count_diff) == 0 || feature_count_diff == "") {
        if (length(other) == 0) "identical" else "differs"
      } else "differs"
    } else "differs"
  } else "differs"

  data.frame(dataset_id = did, status = status,
             changelog_diff = changelog_diff,
             version_diff = version_diff,
             feature_count_diff = feature_count_diff,
             other_diffs = paste(other, collapse = "; "),
             stringsAsFactors = FALSE)
})

report <- do.call(rbind, rows)

# Summary
cat("\n--- Summary ---\n")
cat(sprintf("  Identical: %d\n", sum(report$status == "identical")))
cat(sprintf("  Differs:   %d\n", sum(report$status == "differs")))
cat(sprintf("  Cleaning-only: %d\n", sum(report$status == "cleaning_only")))
cat(sprintf("  Metadata-only: %d\n", sum(report$status == "metadata_only")))

# Print differing datasets
cat("\n--- Differing datasets ---\n")
for (i in which(report$status != "identical")) {
  r <- report[i, ]
  cat(sprintf("\n%s (%s)\n", r$dataset_id, r$status))
  if (nzchar(r$changelog_diff))     cat(sprintf("  changelog: %s\n", r$changelog_diff))
  if (nzchar(r$version_diff))       cat(sprintf("  version: %s\n", r$version_diff))
  if (nzchar(r$feature_count_diff)) cat(sprintf("  feature_count: %s\n", r$feature_count_diff))
  if (nzchar(r$other_diffs))        cat(sprintf("  other: %s\n", r$other_diffs))
}

# Write CSV
out_path <- here("data", "metadata_sync_report.csv")
write.csv(report, out_path, row.names = FALSE)
cat(sprintf("\nReport written to %s\n", out_path))
