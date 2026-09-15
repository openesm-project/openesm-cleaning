# diagnose_check_data.R
#
# Runs check_data(df, dataset_info, variable_data, verbose = TRUE) on every
# existing *_ts.tsv clean file, without re-running any cleaning script.
# Captures all warnings and the answer_categories summary, then writes two
# CSV reports:
#   data/check_data_warnings.csv          -- warnings and hard errors per dataset
#   data/check_data_answer_categories.csv -- rating_scale range/count check for all items
#


# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
source(here("scripts", "functions_data.R"))


# Load metadata -----------------------------------------------------------
cat("Reading master metadata sheet (this may take a moment)...\n")
meta_data <- read_sheet(METADATA_URL)

# Only process datasets that have both a clean _ts.tsv file and a metadata row
ts_files     <- list.files(here("data", "clean"), pattern = "_ts\\.tsv$",
                           full.names = TRUE)
ts_ids       <- substr(basename(ts_files), 1, 4)
ids_to_check <- intersect(meta_data$dataset_id, ts_ids)

cat(sprintf("Found %d datasets to check.\n", length(ids_to_check)))


# Diagnostic loop ---------------------------------------------------------
warnings_list <- list()
summary_list  <- list()

for (did in ids_to_check) {

  cat(sprintf("\n[%s] ", did))

  # Metadata row for this dataset
  dataset_info <- filter(meta_data, dataset_id == did)

  # Load coding sheet if URL is available
  coding_url    <- dataset_info[["Coding File URL"]]
  variable_data <- if (!is.null(coding_url) && !is.na(coding_url) && nzchar(coding_url)) {
    tryCatch(
      read_sheet(coding_url),
      error = function(e) {
        cat(sprintf("[could not read coding sheet: %s] ", conditionMessage(e)))
        NULL
      }
    )
  } else {
    cat("[no coding URL] ")
    NULL
  }

  # Load clean TSV
  tsv_path <- ts_files[startsWith(basename(ts_files), did)]
  df       <- read_tsv(tsv_path, show_col_types = FALSE)

  # Run check_data(), capturing warnings without stopping execution
  caught_warnings <- character(0)
  result <- tryCatch(
    withCallingHandlers(
      check_data(df, dataset_info, variable_data, verbose = TRUE),
      warning = function(w) {
        caught_warnings <<- c(caught_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) paste0("ERROR: ", conditionMessage(e))
  )

  # Store warnings (includes hard errors caught above)
  all_issues <- caught_warnings
  if (is.character(result) && startsWith(result, "ERROR:")) {
    all_issues <- c(all_issues, result)
  }

  if (length(all_issues) > 0) {
    warnings_list[[did]] <- tibble(dataset_id = did, issue = all_issues)
    cat(sprintf("%d issue(s)", length(all_issues)))
  } else {
    cat("OK")
  }

  # Store full answer_categories summary (all items, not just flagged ones)
  if (is.data.frame(result) && nrow(result) > 0) {
    n_flagged <- sum(result$status != "ok")
    summary_list[[did]] <- mutate(result, dataset_id = did, .before = 1)
    if (n_flagged > 0) cat(sprintf(", %d answer_category mismatch(es)", n_flagged))
  }
}

cat("\n\n")


# Report ------------------------------------------------------------------
warnings_df <- bind_rows(warnings_list)
summary_df  <- bind_rows(summary_list)

n_flagged_cats <- if (nrow(summary_df) > 0) sum(summary_df$status != "ok") else 0L

cat(sprintf(
  "=== SUMMARY ===\n%d datasets checked\n%d warning(s)/error(s) across %d datasets\n%d answer_category mismatch(es)\n",
  length(ids_to_check),
  nrow(warnings_df),
  dplyr::n_distinct(warnings_df$dataset_id),
  n_flagged_cats
))

if (nrow(warnings_df) > 0) {
  cat("\n--- Warnings / Errors ---\n")
  print(warnings_df, n = Inf)
}

if (n_flagged_cats > 0) {
  cat("\n--- Answer category mismatches ---\n")
  print(filter(summary_df, status != "ok"), n = Inf)
}


# Preserve any existing resolution/script_upgraded/notes annotations
warnings_out_path <- here("data", "check_data_warnings.csv")
if (file.exists(warnings_out_path)) {
  existing <- read_csv(warnings_out_path, show_col_types = FALSE)
  annotation_cols <- c("resolution", "script_upgraded", "notes")
  existing_annots <- existing |>
    select(dataset_id, issue, any_of(annotation_cols)) |>
    filter(if_any(any_of(annotation_cols), ~ !is.na(.)))
  if (nrow(existing_annots) > 0) {
    warnings_df <- warnings_df |>
      left_join(existing_annots, by = c("dataset_id", "issue"))
  }
}

write_csv(warnings_df, warnings_out_path)
write_csv(summary_df,  here("data", "check_data_answer_categories.csv"))

