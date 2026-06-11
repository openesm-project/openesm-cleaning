# =============================================================================
# descriptives/compute_descriptives.R   —   Schema version: 1.0
#
# Computes fully precomputed descriptive statistics for each rating-scale ESM
# item and writes one JSON per dataset to descriptives/output/.
#
# Usage (from repo root):
#   Rscript descriptives/compute_descriptives.R            # all datasets
#   Rscript descriptives/compute_descriptives.R 0001 0042  # specific datasets
#
# Statistical contract (shared with openesm-project/openesm frontend):
#   Skewness    : e1071::skewness(type = 2) — bias-corrected (Excel-style SKEW)
#   Exclusion   : n_obs < 5  → "too_few_obs"
#                 sd == 0    → "insufficient_variance"  (first match wins)
#   ICC         : one-way ANOVA formulation, algebraically identical to the
#                 lme4 unconditional means model but faster and always converges.
#                 icc_singular is always false on success, null on model failure.
#   n_modes     : local maxima in the frequency table — NOT kernel density, which
#                 blurs discrete Likert peaks with wide bandwidth. Capped at "4+".
#   KDE bw      : density() default ("nrd0", Silverman's rule of thumb)
#   KDE x-range : [Q1 − 3·IQR, Q3 + 3·IQR] when IQR > 1e-6;
#                 [min − 2·bw, max + 2·bw] for near-flat distributions.
#                 prop_floor and prop_ceil ranges are additionally clipped to
#                 [0, 1] to prevent impossible density mass outside that interval.
#   prop_floor  : fraction of a participant's responses equal to the GLOBAL item
#                 minimum across all included participants for that item.
#   prop_ceil   : same, using global item maximum.
#   person_dots : raw per-participant values; only included when n_included ≤ 150.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(e1071)
  library(jsonlite)
  library(here)
})

# --- Paths (edit here if directory structure changes) ------------------------
CLEAN_DIR  <- here("data", "clean")
META_DIR   <- here("data", "metadata")
OUTPUT_DIR <- here("descriptives", "output")

# --- Constants ---------------------------------------------------------------
MIN_OBS          <- 5L    # minimum non-missing obs per participant to be included
MIN_PARTICIPANTS <- 3L    # minimum included participants to emit an item
PERSON_DOTS_MAX  <- 150L  # n_included threshold below which person_dots is added
KDE_N            <- 100L  # number of points in each KDE grid

FALLBACK_EXCL <- c(       # dropped when no metadata JSON exists (heuristic)
  "id", "day", "beep", "scheduled", "issued", "response",
  "duration", "time", "date", "time_ms", "sent", "expired"
)

# variable_type values that represent substantive observations (not admin/timestamps).
# Includes passive sensing (numeric) and binary yes/no items.
# Excludes "other" (mostly day/beep/duration — always non-NA) and PosixCt/Date.
# NOTE: datasets 0022/0070/0066 have a few ESM items mis-typed as "other"; these
# will be missed in the landing stats count — acceptable imprecision for a headline figure.
LANDING_TYPES <- c("rating_scale", "numeric", "numerical",
                   "binary", "categorical", "freetext")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# Helper functions
# =============================================================================

# Count local maxima in the frequency table of x.
# Returns 1 for flat or single-valued distributions.
# Frequency-table approach is used (not kernel density) because KDE bandwidth
# is typically too wide for discrete Likert scales and can blur true bimodality.
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

# Aggregate per-participant mode counts into the mode_counts object.
# Buckets: "1", "2", "3", "4+". Only non-zero buckets are returned.
make_mode_counts <- function(modes_vec) {
  modes_vec <- as.integer(modes_vec[!is.na(modes_vec)])
  if (length(modes_vec) == 0L) return(list())
  bucketed <- ifelse(modes_vec >= 4L, "4+", as.character(modes_vec))
  tbl      <- table(bucketed)
  ordered  <- c("1", "2", "3", "4+")
  present  <- ordered[ordered %in% names(tbl)]
  result   <- as.list(as.integer(tbl[present]))
  names(result) <- present
  result
}

# Compute a KDE on an IQR-based x-grid.
# x_min/x_max clip the grid for bounded statistics (e.g. prop_floor/ceil in [0,1]).
# Returns a list of KDE_N {x, y} pairs, or NULL if too few finite values.
compute_kde <- function(values, x_min = -Inf, x_max = Inf) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(NULL)

  q1  <- as.numeric(quantile(values, 0.25))
  q3  <- as.numeric(quantile(values, 0.75))
  iqr <- q3 - q1

  if (iqr > 1e-6) {
    lo <- q1 - 3 * iqr
    hi <- q3 + 3 * iqr
  } else {
    bw <- tryCatch(bw.nrd0(values), error = function(e) diff(range(values)) / 4)
    bw <- max(bw, 1e-6)
    lo <- min(values) - 2 * bw
    hi <- max(values) + 2 * bw
  }

  lo <- max(lo, x_min)
  hi <- min(hi, x_max)
  if (lo >= hi) return(NULL)  # degenerate range after clipping

  dens <- tryCatch(
    density(values, from = lo, to = hi, n = KDE_N),
    error = function(e) NULL
  )
  if (is.null(dens)) return(NULL)

  lapply(seq_len(KDE_N), function(i) {
    list(x = round(dens$x[i], 4), y = round(dens$y[i], 4))
  })
}

# Build a stat block: median, IQR, and KDE for a vector of per-person values.
compute_stat_block <- function(values, x_min = -Inf, x_max = Inf) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(NULL)
  list(
    median = round(median(values), 4),
    iqr    = round(IQR(values), 4),
    kde    = compute_kde(values, x_min = x_min, x_max = x_max)
  )
}

# ICC(1) via one-way ANOVA (faster than lme4; algebraically equivalent for
# balanced and unbalanced designs via the harmonic-mean group-size correction).
# df: data frame with columns id (any type) and value (numeric, NA-free).
compute_icc <- function(df) {
  tryCatch({
    fit  <- aov(value ~ factor(id), data = df)
    ms   <- summary(fit)[[1L]][["Mean Sq"]]
    ms_b <- ms[1L]; ms_w <- ms[2L]
    n_k  <- length(unique(df$id))
    n_hm <- (nrow(df) - sum(tabulate(factor(df$id))^2) / nrow(df)) / (n_k - 1L)
    icc  <- max(0, (ms_b - ms_w) / (ms_b + (n_hm - 1) * ms_w))
    list(icc = round(icc, 4), singular = FALSE)
  }, error = function(e) list(icc = NA_real_, singular = NA))
}

# Resolve rating-scale item column names from the metadata JSON.
# Falls back to excluding known administrative columns if no metadata exists.
get_items <- function(dataset_id, author, data_cols) {
  meta_path <- file.path(META_DIR,
                         paste0(dataset_id, "_", author, "_metadata.json"))
  if (file.exists(meta_path)) {
    meta  <- jsonlite::read_json(meta_path)
    items <- vapply(meta$features, function(f) {
      if (identical(f$variable_type, "rating_scale")) f$name else NA_character_
    }, character(1L))
    intersect(items[!is.na(items)], data_cols)
  } else {
    message("  No metadata — heuristic column exclusion")
    excl_rx <- paste0("^(", paste(FALLBACK_EXCL, collapse = "|"), ")$")
    data_cols[!grepl(excl_rx, data_cols)]
  }
}


# Resolve EMA variable names for landing stats (broader than rating_scale only).
# Uses LANDING_TYPES to include passive sensing, binary, categorical, etc.
# Falls back to FALLBACK_EXCL heuristic when no metadata exists.
get_ema_vars <- function(dataset_id, author, data_cols) {
  meta_path <- file.path(META_DIR,
                         paste0(dataset_id, "_", author, "_metadata.json"))
  if (file.exists(meta_path)) {
    meta <- jsonlite::read_json(meta_path)
    vars <- vapply(meta$features, function(f) {
      if (f$variable_type %in% LANDING_TYPES) f$name else NA_character_
    }, character(1L))
    intersect(vars[!is.na(vars)], data_cols)
  } else {
    excl_rx <- paste0("^(", paste(FALLBACK_EXCL, collapse = "|"), ")$")
    data_cols[!grepl(excl_rx, data_cols)]
  }
}

# Compute landing page aggregate stats across ALL cleaned datasets.
# A beep is "valid" if at least one EMA variable is non-NA — mirrors the
# calc_observations() logic from openesm-paper/scripts/00_functions.R.
# Always scans all _ts.tsv files so the totals are never partial.
compute_landing_stats <- function() {
  all_ts <- list.files(CLEAN_DIR, pattern = "_ts\\.tsv$", full.names = TRUE)

  n_datasets     <- 0L
  n_participants <- 0L
  n_timepoints   <- 0L

  for (ts_file in all_ts) {
    stem       <- sub("_ts$", "", tools::file_path_sans_ext(basename(ts_file)))
    parts      <- strsplit(stem, "_")[[1L]]
    dataset_id <- parts[1L]
    author     <- parts[2L]

    data <- suppressMessages(readr::read_tsv(ts_file, show_col_types = FALSE))
    if (!"id" %in% colnames(data)) next
    data <- data[!is.na(data$id), ]

    ema_vars <- get_ema_vars(dataset_id, author, colnames(data))
    ema_vars <- intersect(ema_vars, colnames(data))
    if (length(ema_vars) == 0L) next

    n_datasets     <- n_datasets + 1L
    n_participants <- n_participants + length(unique(data$id))
    # valid beep = at least one EMA column is non-NA
    n_timepoints   <- n_timepoints +
      sum(rowSums(!is.na(data[, ema_vars, drop = FALSE])) > 0L)
  }

  list(
    n_datasets     = as.integer(n_datasets),
    n_participants = as.integer(n_participants),
    n_timepoints   = as.integer(n_timepoints),
    generated      = format(Sys.Date(), "%Y-%m-%d")
  )
}


# =============================================================================
# Per-dataset processing
# =============================================================================

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
  if (length(items) == 0L) { message("  Skipped: no items"); return(FALSE) }

  n_written <- 0L
  n_skipped <- 0L
  dataset_out <- list()

  for (item in items) {
    item_df <- data |> select(id, value = all_of(item))

    if (!is.numeric(item_df$value)) {
      coerced <- suppressWarnings(as.numeric(item_df$value))
      n_lost  <- sum(!is.na(item_df$value) & is.na(coerced))
      if (n_lost > 0L)
        message("  ! ", item, ": ", n_lost, " non-numeric value(s) coerced to NA")
      item_df$value <- coerced
    }

    # Per-participant filter criteria
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
          TRUE                          ~ NA_character_
        )
      )

    inc_ids    <- ps$id[is.na(ps$filter_reason)]
    n_included <- length(inc_ids)
    n_excluded <- nrow(ps) - n_included

    if (n_included < MIN_PARTICIPANTS) { n_skipped <- n_skipped + 1L; next }

    # Exclusion reason counts
    exc_tbl <- table(ps$filter_reason[!is.na(ps$filter_reason)])
    excluded_reasons <- if (length(exc_tbl) > 0L) {
      r <- as.list(as.integer(exc_tbl)); names(r) <- names(exc_tbl); r
    } else list()

    # Global item floor/ceiling (across included participants only)
    inc_vals  <- item_df$value[item_df$id %in% inc_ids & !is.na(item_df$value)]
    floor_val <- min(inc_vals)
    ceil_val  <- max(inc_vals)

    # ICC
    icc_df  <- item_df[item_df$id %in% inc_ids & !is.na(item_df$value), ]
    icc_res <- compute_icc(icc_df)

    # Per-participant statistics (included participants only)
    per_person <- item_df |>
      filter(id %in% inc_ids) |>
      group_by(id) |>
      summarise(
        mean_v  = mean(value, na.rm = TRUE),
        sd_v    = sd(value, na.rm = TRUE),
        skew_v  = if (sum(!is.na(value)) >= 3L)
                    e1071::skewness(value[!is.na(value)], type = 2L)
                  else NA_real_,
        pf_v    = mean(value == floor_val, na.rm = TRUE),
        pc_v    = mean(value == ceil_val,  na.rm = TRUE),
        n_modes = count_modes(value),
        .groups = "drop"
      )

    # Stat blocks: KDE + median + IQR per statistic
    stats_block <- list(
      mean       = compute_stat_block(per_person$mean_v),
      sd         = compute_stat_block(per_person$sd_v),
      skewness   = compute_stat_block(per_person$skew_v),
      prop_floor = compute_stat_block(per_person$pf_v, x_min = 0, x_max = 1),
      prop_ceil  = compute_stat_block(per_person$pc_v, x_min = 0, x_max = 1)
    )

    # person_dots: raw values for small datasets only (individual-dot overlay)
    person_dots <- if (n_included <= PERSON_DOTS_MAX) {
      list(
        mean       = round(per_person$mean_v,  4),
        sd         = round(per_person$sd_v,    4),
        skewness   = round(per_person$skew_v,  4),
        prop_floor = round(per_person$pf_v,    4),
        prop_ceil  = round(per_person$pc_v,    4)
      )
    } else NULL

    dataset_out[[length(dataset_out) + 1L]] <- list(
      dataset_id            = dataset_id,
      item                  = item,
      n_included            = as.integer(n_included),
      n_excluded            = as.integer(n_excluded),
      excluded_reasons      = excluded_reasons,
      icc                   = icc_res$icc,
      icc_singular          = icc_res$singular,
      empirical_scale_range = list(floor_val, ceil_val),
      mode_counts           = make_mode_counts(per_person$n_modes),
      stats                 = stats_block,
      person_dots           = person_dots
    )

    n_written <- n_written + 1L
    message("  + ", item, " (n_inc=", n_included, ", n_exc=", n_excluded, ")")
  }

  if (length(dataset_out) == 0L) { message("  No items passed filters"); return(FALSE) }

  out_path <- file.path(OUTPUT_DIR, paste0(dataset_id, "_", author, ".json"))
  writeLines(
    jsonlite::toJSON(dataset_out, auto_unbox = TRUE, pretty = FALSE,
                     null = "null", na = "null"),
    out_path
  )
  message("  -> Written: ", basename(out_path),
          " (", n_written, " items, ", n_skipped, " skipped)")
  TRUE
}


# =============================================================================
# Main
# =============================================================================

all_ts_files <- list.files(CLEAN_DIR, pattern = "_ts\\.tsv$", full.names = TRUE)
message("Found ", length(all_ts_files), " _ts.tsv files")

# Command-line args are treated as dataset IDs (e.g. "0001", "0042").
# No args = process all datasets.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0L) {
  file_ids  <- sub("_.*", "", basename(all_ts_files))
  ts_to_run <- all_ts_files[file_ids %in% args]
  unknown   <- args[!args %in% file_ids]
  if (length(unknown) > 0L)
    warning("No _ts.tsv found for: ", paste(unknown, collapse = ", "))
} else {
  ts_to_run <- all_ts_files
}

message("Processing ", length(ts_to_run), " dataset(s)\n")
for (ts_file in ts_to_run) compute_dataset(ts_file)

# Rebuild the flat index from ALL output files (not just this run's).
out_files <- setdiff(
  list.files(OUTPUT_DIR, pattern = "\\.json$", full.names = TRUE),
  file.path(OUTPUT_DIR, "descriptives_index.json")
)
index_rows <- unlist(lapply(out_files, function(f) {
  d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  lapply(d, function(entry) list(dataset_id = entry$dataset_id, item = entry$item))
}), recursive = FALSE)

writeLines(
  jsonlite::toJSON(index_rows, auto_unbox = TRUE, pretty = FALSE),
  file.path(OUTPUT_DIR, "descriptives_index.json")
)
message("\nDone. Index rebuilt from ", length(out_files), " file(s).")

# Landing page aggregate stats (always recomputed from all datasets)
message("\nComputing landing page stats...")
landing <- compute_landing_stats()
writeLines(
  jsonlite::toJSON(landing, auto_unbox = TRUE, pretty = FALSE),
  file.path(OUTPUT_DIR, "landing_stats.json")
)
message("landing_stats.json: ",
        landing$n_datasets, " datasets | ",
        landing$n_participants, " participants | ",
        landing$n_timepoints, " timepoints")
