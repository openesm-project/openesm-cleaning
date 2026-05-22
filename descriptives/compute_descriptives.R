# descriptives/compute_descriptives.R
#
# Computes per-participant descriptive statistics for every rating-scale ESM
# item across all cleaned datasets, then writes one JSON file per dataset to
# descriptives/output/.
#
# Usage (from repo root):
#   Rscript descriptives/compute_descriptives.R
#
# Output files:
#   descriptives/output/<dataset_id>_<author>.json   — one per dataset
#   descriptives/output/descriptives_index.json       — flat index of all items

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(e1071)
  library(diptest)
  library(jsonlite)
  library(here)
})

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

MIN_OBS          <- 5L   # minimum non-missing obs per participant to be included
MIN_PARTICIPANTS <- 3L   # minimum included participants to write an item

# Columns to drop when no metadata is available (heuristic fallback)
FALLBACK_EXCL <- c(
  "id", "day", "beep", "scheduled", "issued", "response",
  "duration", "time", "date", "time_ms", "sent", "expired"
)

dir.create(here("descriptives", "output"), showWarnings = FALSE, recursive = TRUE)


# --------------------------------------------------------------------------- #
# Helpers
# --------------------------------------------------------------------------- #

# Count local maxima in the frequency table — each peak is one "mode".
# Returns 1 for flat or single-valued distributions.
count_modes <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_integer_)
  freq <- as.integer(table(x))
  n    <- length(freq)
  if (n == 1L) return(1L)
  padded  <- c(-1L, freq, -1L)
  n_peaks <- sum(vapply(seq_len(n), function(i) {
    padded[i + 1L] > padded[i] && padded[i + 1L] > padded[i + 2L]
  }, logical(1L)))
  if (n_peaks == 0L) 1L else as.integer(n_peaks)
}

# Hartigan's dip test with guards for small samples and numeric edge cases.
safe_dip_p <- function(x) {
  if (length(x) < 4L) return(NA_real_)
  tryCatch(
    suppressWarnings(diptest::dip.test(x)$p.value),
    error = function(e) NA_real_
  )
}

# ICC(1) via one-way ANOVA (algebraically identical to the lme4 unconditional
# means model but orders of magnitude faster).
# df must have columns: id, value (numeric, NA-free).
compute_icc <- function(df) {
  tryCatch({
    fit  <- aov(value ~ factor(id), data = df)
    ms   <- summary(fit)[[1L]][["Mean Sq"]]
    ms_b <- ms[1L]
    ms_w <- ms[2L]
    n_k  <- length(unique(df$id))
    # harmonic mean of group sizes
    n_hm <- (nrow(df) - sum(tabulate(factor(df$id))^2) / nrow(df)) / (n_k - 1L)
    icc  <- max(0, (ms_b - ms_w) / (ms_b + (n_hm - 1) * ms_w))
    list(icc = round(icc, 4), singular = FALSE)
  }, error = function(e) list(icc = NA_real_, singular = TRUE))
}

# Return the names of rating-scale ESM columns for a dataset.
# Falls back to excluding known administrative columns if no metadata JSON exists.
get_items <- function(dataset_id, author, data_cols) {
  meta_path <- here("data", "metadata",
                    paste0(dataset_id, "_", author, "_metadata.json"))
  if (file.exists(meta_path)) {
    meta  <- jsonlite::read_json(meta_path)
    items <- vapply(meta$features, function(f) {
      if (identical(f$variable_type, "rating_scale")) f$name else NA_character_
    }, character(1L))
    intersect(items[!is.na(items)], data_cols)
  } else {
    message("  No metadata found — heuristic column exclusion")
    excl_rx <- paste0("^(", paste(FALLBACK_EXCL, collapse = "|"), ")$")
    data_cols[!grepl(excl_rx, data_cols)]
  }
}

# Build one person_stats entry.
# Excluded participants get filter_reason set and all numeric fields as NA.
# Skewness: e1071 type 2 (bias-corrected, a.k.a. Excel SKEW).
# Kurtosis: e1071 type 2 (bias-corrected excess kurtosis).
# floor_val/ceil_val: item-level empirical min/max across included participants.
build_person_row <- function(pid, vals, n_obs, filter_reason,
                              floor_val, ceil_val) {
  if (!is.na(filter_reason)) {
    return(list(
      id = as.character(pid), filter_reason = filter_reason,
      n_obs = as.integer(n_obs),
      mean = NA_real_, sd = NA_real_, skewness = NA_real_,
      kurtosis = NA_real_, prop_floor = NA_real_, prop_ceil = NA_real_,
      dip_p = NA_real_, n_modes = NA_integer_,
      empirical_min = NA_real_, empirical_max = NA_real_
    ))
  }
  v <- vals[!is.na(vals)]
  list(
    id            = as.character(pid),
    filter_reason = NA_character_,
    n_obs         = as.integer(length(v)),
    mean          = round(mean(v), 4),
    sd            = round(sd(v), 4),
    skewness      = if (length(v) >= 3L)
                      round(e1071::skewness(v, type = 2L), 4) else NA_real_,
    kurtosis      = if (length(v) >= 4L)
                      round(e1071::kurtosis(v, type = 2L), 4) else NA_real_,
    prop_floor    = round(mean(v == floor_val), 4),
    prop_ceil     = round(mean(v == ceil_val), 4),
    dip_p         = round(safe_dip_p(v), 4),
    n_modes       = count_modes(v),
    empirical_min = min(v),
    empirical_max = max(v)
  )
}


# --------------------------------------------------------------------------- #
# Per-dataset function
# --------------------------------------------------------------------------- #

# Process one _ts.tsv file and write its output JSON.
# Returns TRUE on success, FALSE if skipped.
compute_dataset <- function(ts_file) {
  stem       <- sub("_ts$", "", tools::file_path_sans_ext(basename(ts_file)))
  parts      <- strsplit(stem, "_")[[1L]]
  dataset_id <- parts[1L]
  author     <- parts[2L]

  message("[", dataset_id, "_", author, "] Reading...")

  data <- suppressMessages(readr::read_tsv(ts_file, show_col_types = FALSE))

  if (!"id" %in% colnames(data)) {
    message("  Skipped: no 'id' column"); return(FALSE)
  }

  n_na_id <- sum(is.na(data$id))
  if (n_na_id > 0L) {
    message("  Dropping ", n_na_id, " row(s) with NA id")
    data <- data[!is.na(data$id), ]
  }

  items <- get_items(dataset_id, author, colnames(data))
  if (length(items) == 0L) { message("  Skipped: no items found"); return(FALSE) }

  message("  Items (", length(items), "): ", paste(items, collapse = ", "))

  n_total     <- length(unique(data$id))
  all_ids     <- unique(data$id)
  dataset_out <- list()

  for (item in items) {
    item_df <- data |>
      select(id, value = all_of(item))

    # Coerce to numeric; warn if values are lost
    if (!is.numeric(item_df$value)) {
      coerced <- suppressWarnings(as.numeric(item_df$value))
      n_lost  <- sum(!is.na(item_df$value) & is.na(coerced))
      if (n_lost > 0L)
        message("  ! ", item, ": ", n_lost,
                " non-NA non-numeric value(s) coerced to NA (e.g. '",
                item_df$value[!is.na(item_df$value) & is.na(coerced)][1L], "')")
      item_df$value <- coerced
    }

    # per-participant filtering: too_few_obs takes priority over insufficient_variance
    ps <- item_df |>
      group_by(id) |>
      summarise(
        n_obs  = sum(!is.na(value)),
        sd_val = sd(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        filter_reason = case_when(
          n_obs < MIN_OBS              ~ "too_few_obs",
          is.na(sd_val) | sd_val == 0 ~ "insufficient_variance",
          TRUE                         ~ NA_character_
        )
      )

    inc_ids <- ps$id[is.na(ps$filter_reason)]
    if (length(inc_ids) < MIN_PARTICIPANTS) next

    # floor/ceiling: empirical min/max across included participants
    inc_vals  <- item_df$value[item_df$id %in% inc_ids & !is.na(item_df$value)]
    floor_val <- min(inc_vals)
    ceil_val  <- max(inc_vals)

    icc_df  <- item_df[item_df$id %in% inc_ids & !is.na(item_df$value), ]
    icc_res <- compute_icc(icc_df)

    person_stats <- lapply(all_ids, function(pid) {
      row  <- ps[ps$id == pid, ]
      vals <- item_df$value[item_df$id == pid]
      build_person_row(pid, vals, row$n_obs, row$filter_reason,
                        floor_val, ceil_val)
    })

    dataset_out[[length(dataset_out) + 1L]] <- list(
      dataset_id     = dataset_id,
      item           = item,
      n_participants = as.integer(n_total),
      icc            = icc_res$icc,
      icc_singular   = icc_res$singular,
      person_stats   = person_stats
    )

    n_inc <- length(inc_ids)
    message("  + ", item, " (", n_inc, " included / ",
            n_total - n_inc, " excluded)")
  }

  if (length(dataset_out) == 0L) { message("  No items passed filters"); return(FALSE) }

  out_path <- here("descriptives", "output",
                   paste0(dataset_id, "_", author, ".json"))
  writeLines(
    jsonlite::toJSON(dataset_out, auto_unbox = TRUE, pretty = TRUE,
                     null = "null", na = "null"),
    out_path
  )
  message("  -> Written: ", basename(out_path))
  TRUE
}


# --------------------------------------------------------------------------- #
# Main: select datasets and run
# --------------------------------------------------------------------------- #

all_ts_files <- list.files(here("data", "clean"),
                            pattern = "_ts\\.tsv$", full.names = TRUE)
message("Found ", length(all_ts_files), " _ts.tsv files")

# Command-line args are treated as dataset IDs (e.g. "0001", "0002").
# No args = process all datasets.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0L) {
  file_ids    <- sub("_.*", "", basename(all_ts_files))
  ts_to_run   <- all_ts_files[file_ids %in% args]
  unknown     <- args[!args %in% file_ids]
  if (length(unknown) > 0L)
    warning("No _ts.tsv found for dataset ID(s): ", paste(unknown, collapse = ", "))
} else {
  ts_to_run <- all_ts_files
}

message("Processing ", length(ts_to_run), " dataset(s)\n")
for (ts_file in ts_to_run) compute_dataset(ts_file)

# Rebuild the flat index from ALL output files (not just this run's).
out_dir <- here("descriptives", "output")
out_files <- setdiff(
  list.files(out_dir, pattern = "\\.json$", full.names = TRUE),
  file.path(out_dir, "descriptives_index.json")
)
index_rows <- unlist(lapply(out_files, function(f) {
  d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  lapply(d, function(entry) list(dataset_id = entry$dataset_id, item = entry$item))
}), recursive = FALSE)

writeLines(
  jsonlite::toJSON(index_rows, auto_unbox = TRUE, pretty = TRUE),
  file.path(out_dir, "descriptives_index.json")
)
message("\nDone. Index rebuilt from ", length(out_files), " output file(s).")
