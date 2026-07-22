# URL of the master metadata Google Sheet
METADATA_URL <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"

# Validate a cleaned data frame.
# Hard requirements (structure) abort with an error; everything else is a
# warning so a legitimate discrepancy does not block saving. Pass dataset_info
# (the metadata row) and variable_data (the coding sheet) to enable the
# data-vs-metadata cross-checks.
# verbose = TRUE returns a data frame summarising the answer_categories check
# for all rating_scale items (columns: item, n_cats_meta, n_distinct,
# range_min, range_max, status).
check_data <- function(data, dataset_info = NULL, variable_data = NULL,
                       verbose = FALSE){

  # --- hard requirements (error) ---
  if(!is.data.frame(data)){
    stop("Data is not a dataframe")
  }
  for (col in c("id", "day", "beep")) {
    if (!col %in% colnames(data)) {
      stop("Data does not have a '", col, "' column")
    }
  }
  if(!all(colnames(janitor::clean_names(data)) == colnames(data))){
    stop("Column names are not in snake case. Use janitor::clean_names().")
  }

  # --- soft checks (warning) ---
  # string sentinels that should have been converted to real NA
  sentinels <- c("NA", "NaN", "NULL", "", ".", "-99", "-999")
  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  flagged <- char_cols[vapply(char_cols, function(col)
    any(data[[col]] %in% sentinels, na.rm = TRUE), logical(1))]
  if (length(flagged) > 0) {
    warning("Possible unconverted missing values in: ",
            paste(flagged, collapse = ", "),
            ". Run recode_missing().", call. = FALSE)
  }

  # columns that are entirely NA (often a botched rename or select)
  all_na <- names(data)[vapply(data, function(x) all(is.na(x)), logical(1))]
  if (length(all_na) > 0) {
    warning("Columns that are entirely NA: ",
            paste(all_na, collapse = ", "), call. = FALSE)
  }

  # duplicate id-day-beep keys (only checked where day and beep are present)
  keys <- data[!is.na(data$day) & !is.na(data$beep), c("id", "day", "beep")]
  if (nrow(keys) > 0 && anyDuplicated(keys) > 0) {
    warning(sum(duplicated(keys)),
            " duplicated id-day-beep combination(s)", call. = FALSE)
  }

  # --- metadata cross-checks (warning) ---
  if (!is.null(dataset_info)) {
    n_data <- dplyr::n_distinct(data$id)
    n_meta <- suppressWarnings(as.numeric(dataset_info$`N Participants`))
    if (!is.na(n_meta) && n_data != n_meta) {
      warning("Participant count mismatch: ", n_data, " unique ids in data vs ",
              n_meta, " in metadata", call. = FALSE)
    }
  }

  if (!is.null(variable_data) && "name" %in% names(variable_data)) {
    meta_vars <- variable_data$name[!is.na(variable_data$name)]
    structural <- c("id", "day", "beep", "counter")
    only_data <- setdiff(setdiff(colnames(data), meta_vars), structural)
    only_meta <- setdiff(meta_vars, colnames(data))
    if (length(only_data) > 0) {
      warning("Columns in data but not annotated in metadata: ",
              paste(only_data, collapse = ", "), call. = FALSE)
    }
    if (length(only_meta) > 0) {
      warning("Variables in metadata but not in data: ",
              paste(only_meta, collapse = ", "), call. = FALSE)
    }

    # check answer_categories against empirical data for rating_scale items
    if (all(c("variable_type", "answer_categories") %in% names(variable_data))) {
      rating_scale_types <- c("rating_scale", "Likert", "ordinal", "VAS")
      rs_rows <- variable_data[
        !is.na(variable_data$variable_type) &
        variable_data$variable_type %in% rating_scale_types &
        !is.na(variable_data$answer_categories) &
        nzchar(trimws(as.character(variable_data$answer_categories))),
      ]
      check_summary <- vector("list", nrow(rs_rows))
      for (i in seq_len(nrow(rs_rows))) {
        col_name <- rs_rows$name[i]
        n_cats_meta <- suppressWarnings(as.numeric(rs_rows$answer_categories[i]))
        if (is.na(n_cats_meta) || !col_name %in% colnames(data)) next
        col_vals <- na.omit(data[[col_name]])
        if (length(col_vals) == 0) next

        n_unique <- dplyr::n_distinct(col_vals)
        col_num <- if (is.numeric(col_vals)) col_vals else
          suppressWarnings(as.numeric(col_vals))
        is_integer_like <- !anyNA(col_num) && all(col_num == floor(col_num))
        range_min <- if (is_integer_like) min(col_num) else NA_real_
        range_max <- if (is_integer_like) max(col_num) else NA_real_
        emp_range  <- if (is_integer_like) range_max - range_min + 1 else NA_real_

        status <- if (n_unique > n_cats_meta) {
          warning("Column '", col_name, "': ", n_unique,
                  " distinct values in data but answer_categories = ", n_cats_meta,
                  " in metadata", call. = FALSE)
          "n_distinct_mismatch"
        } else if (is_integer_like && emp_range > n_cats_meta) {
          warning("Column '", col_name, "': empirical range ",
                  range_min, "-", range_max,
                  " (", emp_range, " levels) exceeds answer_categories = ",
                  n_cats_meta, " in metadata", call. = FALSE)
          "range_mismatch"
        } else {
          "ok"
        }

        if (verbose) {
          check_summary[[i]] <- data.frame(
            item         = col_name,
            n_cats_meta  = n_cats_meta,
            n_distinct   = n_unique,
            range_min    = range_min,
            range_max    = range_max,
            status       = status,
            stringsAsFactors = FALSE
          )
        }
      }

      if (verbose) {
        return(do.call(rbind, Filter(Negate(is.null), check_summary)))
      }
    }
  }

  return("Data are clean.")

}

# Convert common string sentinels and numeric NaN to real NA.
recode_missing <- function(data) {
  data |>
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ dplyr::na_if(., "NA"))) |>
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ dplyr::na_if(., "NaN"))) |>
    dplyr::mutate(dplyr::across(where(is.character),
                                ~ dplyr::na_if(., ""))) |>
    dplyr::mutate(dplyr::across(where(is.numeric),
                                ~ ifelse(is.nan(.), NA, .)))
}

# Download a file from a direct URL unless it already exists.
download_if_missing <- function(url, dest) {
  if (!file.exists(dest)) {
    utils::download.file(url, dest, mode = "wb")
  }
  invisible(dest)
}

# Retrieve a file from OSF unless it already exists, renaming it to dest.
osf_download_if_missing <- function(osf_url, dest) {
  if (!file.exists(dest)) {
    f <- osfr::osf_retrieve_file(osf_url)
    osfr::osf_download(f, path = dirname(dest))
    file.rename(file.path(dirname(dest), f$name), dest)
  }
  invisible(dest)
}

# Read the coding sheet, build the metadata JSON and write it to data/metadata.
# Reuses meta_data / variable_data if already loaded to avoid re-reading sheets.
write_metadata <- function(dataset_id, author = NULL,
                           meta_data = NULL, variable_data = NULL) {
  did <- dataset_id
  if (is.null(meta_data)) meta_data <- googlesheets4::read_sheet(METADATA_URL)
  dataset_info <- dplyr::filter(meta_data, dataset_id == did)
  if (is.null(variable_data)) {
    variable_data <- googlesheets4::read_sheet(
      dplyr::pull(dataset_info, "Coding File URL"))
  }
  # create_metadata_json() reads these from the global environment
  meta_data <<- meta_data
  variable_data <<- variable_data
  if (is.null(author)) author <- tolower(dataset_info$Author)

  meta_json <- create_metadata_json(did) |>
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
  path <- here::here("data", "metadata",
                     paste0(did, "_", author, "_metadata.json"))
  write(meta_json, path)
  invisible(path)
}

# create metadata json file
create_metadata_json <- function(dataset_id_char,
                                 recode_variable_type = TRUE) {

  # Extract dataset-level info
  dataset_info <- meta_data |>
    dplyr::filter(dataset_id == dataset_id_char)

  # Extract feature-level info
  dataset_features <- variable_data |>
    dplyr::filter(dataset_id == dataset_id_char) |>
    dplyr::mutate_all(~ ifelse(is.na(.), "", .)) |>
    dplyr::select(!dataset_id)

  # recode variable type to simplify
  if(recode_variable_type) {
    dataset_features <- dataset_features |>
      dplyr::mutate(
        variable_type = dplyr::case_when(
          variable_type %in% c("Likert", "ordinal", "VAS") ~ "rating_scale",
          TRUE ~ variable_type
        )
      )
  }

  # preserve existing changelog if JSON already exists
  existing_json_path <- here::here(
    "data", "metadata",
    paste0(dataset_id_char, "_", tolower(dataset_info$Author), "_metadata.json")
  )

  existing <- if (file.exists(existing_json_path)) {
    jsonlite::read_json(existing_json_path)
  } else {
    NULL
  }
  existing_changelog <- if (is.null(existing$changelog)) list() else existing$changelog
  dataset_version <- if (!is.null(dataset_info$dataset_version) &&
                         !is.na(dataset_info$dataset_version) &&
                         nzchar(dataset_info$dataset_version)) {
    dataset_info$dataset_version
  } else if (!is.null(existing$dataset_version)) {
    existing$dataset_version
  } else {
    "1.0.0"
  }


  # Construct JSON structure
  list(
    first_author = dataset_info$Author,
    dataset_id = dataset_info$dataset_id,
    year = dataset_info$Year,
    reference_a = dataset_info$`Reference A`,
    reference_b = dataset_info$`Reference B`,
    paper_doi = dataset_info$`Paper DOI`,
    zenodo_doi = dataset_info$`Zenodo DOI`,
    dataset_version = dataset_version,
    changelog = existing$changelog,
    link_to_data = dataset_info$`Link to data`,
    license = dataset_info$`License`,
    link_to_codebook = dataset_info$`Optional: Link to Codebook`,
    link_to_code = dataset_info$`Optional: Link to Code`,
    n_participants = dataset_info$`N Participants`,
    n_time_points = dataset_info$`N Time Points`,
    n_days = dataset_info$`N Days`,
    n_beeps_per_day = dataset_info$`N Beeps/Day`,
    passive_data_available = dataset_info$`Passive data available?`,
    cross_sectional_available = dataset_info$`Cross sectional available?`,
    topics = dataset_info$Topics,
    implicit_missingness = dataset_info$`Implicit Missingness?`,
    raw_time_stamp = dataset_info$`Raw Time Stamp available?`,
    sampling_scheme = dataset_info$`Sampling Scheme`,
    participants = dataset_info$Participants,
    coding_file = dataset_info$`Coding File`,
    additional_comments = dataset_info$`additional comments`,
    coder_data = dataset_info$`Coder Data`,
    coder_metadata = dataset_info$`Coder Metadata`,
    features = dataset_features
  )
}


# Metadata analysis -------------------------------------------------------
# List all variable-construct pairs to understand construct annotation
# list variable-construct pairs (optionally include variables without constructs)
list_variable_construct_pairs <- function(folder_path, include_all = FALSE) {
  # list all metadata json files
  json_files <- list.files(folder_path, pattern = "\\.json$", full.names = TRUE)

  if (length(json_files) == 0) {
    stop("No JSON files found in the specified folder.")
  }

  # initialize storage tibble
  variable_construct_pairs <- dplyr::tibble(
    dataset_id = character(),
    variable_name = character(),
    construct = character()
  )

  for (file_path in json_files) {
    metadata <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    dataset_id <- metadata$dataset_id

    # iterate features
    if (!is.null(metadata$features) && length(metadata$features) > 0) {
      for (feature in metadata$features) {
        variable_name <- feature$name
        construct <- if (!is.null(feature$construct) && feature$construct != "") {
          feature$construct
        } else {
          NA_character_
        }

        # add depending on option
        if (include_all || !is.na(construct)) {
          variable_construct_pairs <- variable_construct_pairs |>
            dplyr::add_row(
              dataset_id = dataset_id,
              variable_name = variable_name,
              construct = construct
            )
        }
      }
    }
  }

  return(variable_construct_pairs)
}



# Construct Annotation ----------------------------------------------------
annotate_constructs <- function(to_be_named,
                                meta_data,
                                dry_run = FALSE,
                                overwrite = FALSE) {
  # initialize summary tracking
  updates_summary <- data.frame(
    dataset_id = character(),
    variables_updated = integer(),
    variables_overwritten = integer(),
    variables_skipped = integer(),
    status = character(),
    stringsAsFactors = FALSE
  )

  # get unique datasets that need updates
  datasets_to_update <- to_be_named |>
    dplyr::filter(!is.na(construct_recode)) |>
    dplyr::distinct(dataset_id) |>
    dplyr::pull(dataset_id)

  cat("Found", length(datasets_to_update), "datasets to update\n")
  cat("Overwrite mode:", ifelse(overwrite, "ON", "OFF"), "\n\n")

  # process each dataset
  for (dataset in datasets_to_update) {
    cat("Processing dataset:", dataset, "\n")

    # get the google sheet url
    sheet_url <- meta_data |>
      dplyr::filter(dataset_id == dataset) |>
      dplyr::pull("Coding File URL")

    if (length(sheet_url) == 0 || is.na(sheet_url)) {
      cat("  No Google Sheet URL found for dataset", dataset, "\n")
      next
    }

    # get variables to update for this dataset
    dataset_updates <- to_be_named |>
      dplyr::filter(dataset_id == dataset, !is.na(construct_recode))

    if (nrow(dataset_updates) == 0) {
      cat("  No variables to update for dataset", dataset, "\n")
      next
    }

    tryCatch({
      # read the current google sheet
      current_sheet <- googlesheets4::read_sheet(sheet_url)

      # check if construct column exists
      if (!"construct" %in% names(current_sheet)) {
        cat(
          "  Column 'construct' not found in sheet. Available columns: ",
          paste(names(current_sheet), collapse = ", "),
          "\n"
        )
        next
      }

      # prepare updates
      updated_sheet <- current_sheet
      variables_updated <- 0
      variables_overwritten <- 0
      variables_skipped <- 0

      # apply updates row by row
      for (i in 1:nrow(dataset_updates)) {
        var_name <- dataset_updates$variable_name[i]
        new_construct <- dataset_updates$construct_recode[i]

        # find matching rows
        # careful: in the metadata, this is only called "name"
        matching_rows <- which(current_sheet$name == var_name)

        if (length(matching_rows) == 0) {
          cat("    Variable '", var_name, "' not found in sheet\n")
          next
        }

        # update each matching row
        for (row_idx in matching_rows) {
          old_value <- current_sheet$construct[row_idx]

          # check if value already exists and handle based on overwrite setting
          if (!is.na(old_value) && old_value != "") {
            if (overwrite) {
              cat("    OVERWRITING: Variable '",
                  var_name,
                  "' (row ",
                  row_idx,
                  ")\n")
              cat("        Old: ", old_value, "\n")
              cat("        New: ", new_construct, "\n")
              updated_sheet$construct[row_idx] <- new_construct
              variables_overwritten <- variables_overwritten + 1
              variables_updated <- variables_updated + 1
            } else {
              cat(
                "    SKIPPING: Variable '",
                var_name,
                "' (row ",
                row_idx,
                ") - already has value: ",
                old_value,
                "\n"
              )
              variables_skipped <- variables_skipped + 1
            }
          } else {
            cat("    Updating: Variable '",
                var_name,
                "' (row ",
                row_idx,
                ")\n")
            updated_sheet$construct[row_idx] <- new_construct
            variables_updated <- variables_updated + 1
          }
        }
      }

      # write back to google sheet (unless dry run)
      if (!dry_run && variables_updated > 0) {
        googlesheets4::write_sheet(updated_sheet, ss = sheet_url, sheet = 1)
        status <- "SUCCESS"
        cat("  Successfully updated",
            variables_updated,
            "variables in Google Sheet\n")
      } else if (dry_run) {
        status <- "DRY_RUN"
        cat("  DRY RUN: Would update",
            variables_updated,
            "variables\n")
      } else {
        status <- "NO_UPDATES"
        cat("  No updates needed\n")
      }

      if (variables_skipped > 0) {
        cat("  Skipped",
            variables_skipped,
            "variables (already had values)\n")
      }

      # record summary
      updates_summary <- rbind(
        updates_summary,
        data.frame(
          dataset_id = dataset,
          variables_updated = variables_updated,
          variables_overwritten = variables_overwritten,
          variables_skipped = variables_skipped,
          status = status
        )
      )

    }, error = function(e) {
      cat("  Error processing dataset", dataset, ":", e$message, "\n")
      updates_summary <<- rbind(
        updates_summary,
        data.frame(
          dataset_id = dataset,
          variables_updated = 0,
          variables_overwritten = 0,
          variables_skipped = 0,
          status = paste("ERROR:", e$message)
        )
      )
    })

    cat("\n")
  }

  # print final summary
  cat("=== FINAL SUMMARY ===\n")
  print(updates_summary)

  cat("\nTotal variables updated:",
      sum(updates_summary$variables_updated),
      "\n")
  cat("Total variables overwritten:",
      sum(updates_summary$variables_overwritten),
      "\n")
  cat("Total variables skipped:",
      sum(updates_summary$variables_skipped),
      "\n")

  return(updates_summary)
}


# Construct Vocabulary ----------------------------------------------------
# Parse all construct fields across metadata JSONs and return a frequency table.
# Constructs are comma-separated strings; each term is counted separately.
extract_construct_vocabulary <- function(folder_path) {
  json_files <- list.files(folder_path, pattern = "\\.json$",
                           full.names = TRUE, recursive = FALSE)

  # exclude schema or other non-metadata files
  json_files <- json_files[!grepl("schema", basename(json_files))]

  if (length(json_files) == 0) {
    stop("No metadata JSON files found in: ", folder_path)
  }

  constructs <- character(0)

  for (file_path in json_files) {
    metadata <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    if (!is.null(metadata$features) && length(metadata$features) > 0) {
      for (feature in metadata$features) {
        raw <- feature$construct
        if (!is.null(raw) && nzchar(trimws(raw))) {
          terms <- stringr::str_split(raw, ",")[[1]] |>
            stringr::str_trim() |>
            tolower()
          terms <- terms[nzchar(terms)]
          constructs <- c(constructs, terms)
        }
      }
    }
  }

  tibble::tibble(construct = constructs) |>
    dplyr::count(construct, sort = TRUE, name = "n_occurrences")
}


# Metadata Validation -----------------------------------------------------
# Validate a single metadata JSON against the openESM schema.
# Returns TRUE invisibly on success; emits a warning listing issues on failure.
validate_metadata_json <- function(path) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(invisible(FALSE))
  }

  metadata <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  issues <- character(0)

  # --- dataset-level required fields ---
  required_fields <- c("first_author", "dataset_id", "year",
                       "paper_doi", "n_participants", "n_time_points",
                       "features")
  missing <- required_fields[!required_fields %in% names(metadata)]
  if (length(missing) > 0) {
    issues <- c(issues,
                paste("Missing required fields:", paste(missing, collapse = ", ")))
  }

  # --- dataset_id format ---
  if (!is.null(metadata$dataset_id) &&
      !grepl("^\\d{4}$", metadata$dataset_id)) {
    issues <- c(issues,
                paste0("dataset_id '", metadata$dataset_id,
                       "' is not a four-digit string"))
  }

  # --- features ---
  valid_variable_types <- c("rating_scale", "numeric", "categorical",
                             "PosixCt", "character", "logical",
                             "date", "time", "")
  valid_assessment_types <- c("ESM", "baseline", "follow-up", "")

  if (!is.null(metadata$features) && length(metadata$features) > 0) {
    for (i in seq_along(metadata$features)) {
      feat <- metadata$features[[i]]
      feat_label <- if (!is.null(feat$name) && nzchar(feat$name)) {
        paste0("'", feat$name, "'")
      } else {
        paste0("[feature ", i, "]")
      }

      if (is.null(feat$name) || !nzchar(feat$name)) {
        issues <- c(issues, paste("Feature", i, "has an empty 'name'"))
      }

      if (!is.null(feat$variable_type) &&
          !feat$variable_type %in% valid_variable_types) {
        issues <- c(issues,
                    paste0("Variable ", feat_label,
                           ": unknown variable_type '", feat$variable_type, "'"))
      }

      if (!is.null(feat$assessment_type) &&
          !feat$assessment_type %in% valid_assessment_types) {
        issues <- c(issues,
                    paste0("Variable ", feat_label,
                           ": unknown assessment_type '", feat$assessment_type, "'"))
      }
    }
  }

  if (length(issues) == 0) {
    message("Validation passed: ", basename(path))
    return(invisible(TRUE))
  } else {
    warning(
      "Validation issues in ", basename(path), ":\n",
      paste(" -", issues, collapse = "\n"),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
}
