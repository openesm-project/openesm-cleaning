# Package conflict resolution ------------------------------------------------
# conflicted turns ambiguous function calls into errors rather than silently
# using whichever package happened to load last.
library(conflicted)
conflict_prefer("filter",    "dplyr", quiet = TRUE)
conflict_prefer("select",    "dplyr", quiet = TRUE)
conflict_prefer("arrange",   "dplyr", quiet = TRUE)
conflict_prefer("mutate",    "dplyr", quiet = TRUE)
conflict_prefer("summarise", "dplyr", quiet = TRUE)
conflict_prefer("rename",    "dplyr", quiet = TRUE)
conflict_prefer("count",     "dplyr", quiet = TRUE)

# Validate a cleaned data frame.
# Hard requirements (structure) abort with an error; everything else is a
# warning so a legitimate discrepancy does not block saving. Pass metadata
# (from read_metadata()) to enable the data-vs-metadata cross-checks.
# verbose = TRUE returns a data frame summarising the answer_categories check
# for all rating_scale items (columns: item, n_cats_meta, n_distinct,
# range_min, range_max, status).
check_data <- function(data, metadata = NULL, verbose = FALSE){

  variable_data <- if (!is.null(metadata)) features_table(metadata$features)

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
  if (!is.null(metadata)) {
    n_data <- dplyr::n_distinct(data$id)
    n_meta <- suppressWarnings(as.numeric(metadata$n_participants %||% NA))
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

# Normalize an author name for use in filenames.
# Lowercases, converts German umlauts to digraphs, strips remaining diacritics,
# and removes spaces.
normalize_author_name <- function(author) {
  author <- tolower(author)
  author <- gsub("\u00e4", "ae", author)
  author <- gsub("\u00f6", "oe", author)
  author <- gsub("\u00fc", "ue", author)
  author <- stringi::stri_trans_general(author, "Latin-ASCII")
  author <- gsub(" ", "", author)
  author
}

# Metadata JSON -------------------------------------------------------------
# data/metadata/*.json is the source of truth: edit the files directly.
# Code reads them but never regenerates them.

# Read the metadata JSON of one dataset.
read_metadata <- function(dataset_id, dir = here::here("data", "metadata")) {
  path <- list.files(dir, full.names = TRUE,
                     pattern = paste0("^", dataset_id, "_.+_metadata\\.json$"))
  if (length(path) != 1) {
    stop("Expected one metadata JSON for dataset ", dataset_id, " in ", dir,
         ", found ", length(path), call. = FALSE)
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

# Flatten the features array into a data frame with the columns check_data()
# needs. Empty strings and nulls become NA.
features_table <- function(features) {
  field <- function(key) vapply(features, function(f) {
    value <- f[[key]]
    if (is.null(value) || length(value) == 0) return(NA_character_)
    value <- as.character(value[[1]])
    if (nzchar(value)) value else NA_character_
  }, character(1))
  data.frame(name = field("name"),
             variable_type = field("variable_type"),
             answer_categories = field("answer_categories"),
             stringsAsFactors = FALSE)
}

# Write a skeleton metadata JSON for a new dataset: one feature per column of
# data, all other fields empty. Never overwrites an existing file.
new_metadata_json <- function(dataset_id, author, data,
                              dir = here::here("data", "metadata")) {
  existing <- list.files(dir, pattern = paste0("^", dataset_id, "_.+_metadata\\.json$"))
  if (length(existing) > 0) {
    stop("Metadata JSON already exists for dataset ", dataset_id, ": ",
         existing[1], call. = FALSE)
  }
  path <- file.path(dir, paste0(dataset_id, "_",
                                normalize_author_name(author),
                                "_metadata.json"))

  dataset_keys <- c(
    "first_author", "dataset_id", "year", "reference_a", "reference_b",
    "paper_doi", "zenodo_doi", "dataset_version", "changelog",
    "link_to_data", "license", "link_to_codebook", "link_to_code",
    "n_participants", "n_time_points", "n_days", "n_beeps_per_day",
    "passive_data_available", "cross_sectional_available", "topics",
    "implicit_missingness", "raw_time_stamp", "sampling_scheme",
    "participants", "additional_comments", "coder_data", "coder_metadata"
  )
  feature_keys <- c(
    "name", "description", "variable_type", "answer_categories", "details",
    "labels", "transformation", "source", "assessment_type", "construct",
    "comments"
  )

  # dataset-level placeholders are null; `year` stays invalid until filled
  metadata <- setNames(vector("list", length(dataset_keys)), dataset_keys)
  metadata$first_author <- author
  metadata$dataset_id <- dataset_id
  metadata$dataset_version <- "1.0.0"
  metadata$changelog <- list()
  metadata$features <- lapply(colnames(data), function(col) {
    feature <- setNames(as.list(rep("", length(feature_keys))), feature_keys)
    feature$name <- col
    feature
  })

  jsonlite::write_json(metadata, path, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")
  invisible(path)
}


# Legacy: Google Sheets ---------------------------------------------------------
# DEPRECATED. The sheets are frozen and the JSON files are the source of truth.

# URL of the (frozen) master metadata Google Sheet
METADATA_URL <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"

# DEPRECATED: refuses to overwrite an existing JSON. Use new_metadata_json()
# for new datasets.
write_metadata <- function(dataset_id, author = NULL,
                           meta_data = NULL, variable_data = NULL) {
  .Deprecated("new_metadata_json")
  existing <- list.files(here::here("data", "metadata"),
                         pattern = paste0("^", dataset_id, "_.+_metadata\\.json$"))
  if (length(existing) > 0) {
    stop("Refusing to overwrite ", existing[1], ": edit the JSON directly.",
         call. = FALSE)
  }
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
  if (is.null(author)) {
    author <- normalize_author_name(dataset_info$Author)
  }

  path <- here::here("data", "metadata",
                     paste0(did, "_", author, "_metadata.json"))
  meta_json <- create_metadata_json(did, existing_path = path) |>
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
  write(meta_json, path)
  invisible(path)
}

# DEPRECATED. Create metadata JSON structure from Google Sheets data.
# existing_path: path to existing JSON for changelog/version preservation.
#   If NULL, computed via normalize_author_name().
create_metadata_json <- function(dataset_id_char,
                                 recode_variable_type = TRUE,
                                 existing_path = NULL) {

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

  # Preserve existing changelog/version if JSON already exists
  if (is.null(existing_path)) {
    existing_path <- here::here(
      "data", "metadata",
      paste0(dataset_id_char, "_",
             normalize_author_name(dataset_info$Author),
             "_metadata.json")
    )
  }

  existing <- if (file.exists(existing_path)) {
    jsonlite::read_json(existing_path)
  } else {
    NULL
  }

  # Normalize changelog to a plain list (handles NULL, {}, and absent)
  existing_changelog <- existing$changelog
  if (is.null(existing_changelog) ||
      (is.list(existing_changelog) && length(existing_changelog) == 0)) {
    existing_changelog <- list()
  }

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
    changelog = existing_changelog,
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
# Validate a metadata JSON against the schema in schema/v1/ (the single source
# of truth for allowed keys, types and values).
# Returns TRUE invisibly on success; emits a warning listing issues on failure.
validate_metadata_json <- function(path,
                                   schema = here::here("schema", "v1",
                                                       "metadata_schema.json")) {
  valid <- jsonvalidate::json_validate(path, schema, verbose = TRUE,
                                       engine = "ajv")
  if (valid) {
    message("Validation passed: ", basename(path))
    return(invisible(TRUE))
  }
  errors <- attr(valid, "errors")
  warning(
    "Validation issues in ", basename(path), ":\n",
    paste0(" - ", errors$instancePath, " ", errors$message, collapse = "\n"),
    call. = FALSE
  )
  invisible(FALSE)
}
