# Construct suggestion from semantic similarity neighbors
#
# For each rating_scale item in a target dataset, looks up the constructs of
# its top-N semantic neighbors and outputs a suggestion table.
#
# Usage:
#   Rscript scripts/suggest_constructs.R <dataset_id> [top_n]
#
# Examples:
#   Rscript scripts/suggest_constructs.R 0076
#   Rscript scripts/suggest_constructs.R 0076 3

library(jsonlite)
library(here)

# Helper: replace NULL, zero-length, or NA with default ------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript suggest_constructs.R <dataset_id> [top_n]")
target_id <- args[1]

top_n <- if (length(args) >= 2) suppressWarnings(as.integer(args[2])) else 5L
if (is.na(top_n) || top_n < 1L) stop("top_n must be a positive integer (e.g. 3 or 5)")

# Load similarity neighbors -----------------------------------------------
similarity_path <- here("data", "similarity", "similar_items.json")
if (!file.exists(similarity_path)) {
  stop("similar_items.json not found. Run compute_similarity.py first.")
}
sim_data <- read_json(similarity_path)

# Load all metadata JSONs into a construct lookup -------------------------
# (dataset_id:variable_name) -> construct string
meta_dir   <- here("data", "metadata")
meta_files <- list.files(meta_dir, pattern = "_metadata\\.json$", full.names = TRUE)

construct_lookup <- list()
for (f in meta_files) {
  j   <- read_json(f)
  did <- j$dataset_id %||% ""
  for (feat in j$features) {
    key <- paste0(did, ":", feat$name %||% "")
    construct_lookup[[key]] <- feat$construct %||% ""
  }
}

# Filter items for target dataset -----------------------------------------
target_keys <- names(sim_data)[startsWith(names(sim_data), paste0(target_id, "_"))]
if (length(target_keys) == 0) {
  stop("No items found for dataset ", target_id,
       ". Is this dataset in similar_items.json?")
}

# Build suggestion table --------------------------------------------------
rows <- lapply(target_keys, function(key) {
  item      <- sim_data[[key]]
  neighbors <- item$neighbors

  if (length(neighbors) == 0) return(NULL)
  neighbors <- neighbors[seq_len(min(top_n, length(neighbors)))]

  neighbor_rows <- lapply(seq_along(neighbors), function(i) {
    nb  <- neighbors[[i]]
    con <- construct_lookup[[paste0(nb$dataset_id %||% "", ":", nb$variable_name %||% "")]] %||% ""
    data.frame(
      variable_name        = item$variable_name %||% "",
      description          = item$description   %||% "",
      neighbor_rank        = i,
      neighbor_dataset     = nb$dataset_id      %||% "",
      neighbor_variable    = nb$variable_name   %||% "",
      neighbor_description = nb$description     %||% "",
      neighbor_construct   = con,
      similarity           = round(nb$similarity %||% NA_real_, 3),
      stringsAsFactors     = FALSE
    )
  })
  do.call(rbind, neighbor_rows)
})

rows    <- Filter(Negate(is.null), rows)
results <- do.call(rbind, rows)

# Look up current construct for target items ------------------------------
results$current_construct <- vapply(results$variable_name, function(vn) {
  construct_lookup[[paste0(target_id, ":", vn)]] %||% ""
}, character(1))

# Reorder columns
results <- results[, c("variable_name", "description", "current_construct",
                        "neighbor_rank", "neighbor_dataset", "neighbor_variable",
                        "neighbor_description", "neighbor_construct", "similarity")]

# Print summary to console ------------------------------------------------
cat(sprintf("\nConstruct suggestions for dataset %s (top %d neighbors)\n", target_id, top_n))
cat(strrep("-", 72), "\n")

for (vn in unique(results$variable_name)) {
  item_rows <- results[results$variable_name == vn, ]
  current   <- item_rows$current_construct[1]
  desc      <- item_rows$description[1]

  cat(sprintf("\n%s  \"%s\"\n", vn, desc))
  if (nzchar(current)) cat(sprintf("  current: %s\n", current))

  for (i in seq_len(nrow(item_rows))) {
    r       <- item_rows[i, ]
    con_str <- if (nzchar(r$neighbor_construct)) r$neighbor_construct else "(no construct)"
    cat(sprintf("  [%.3f] %s:%s — %s\n",
                r$similarity, r$neighbor_dataset, r$neighbor_variable, con_str))
  }
}

# Write CSV ---------------------------------------------------------------
out_path <- here("data", sprintf("construct_suggestions_%s.csv", target_id))
write.csv(results, out_path, row.names = FALSE)
cat(sprintf("\n\nFull table written to: %s\n", out_path))
