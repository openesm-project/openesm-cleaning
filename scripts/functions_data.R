check_data <- function(data){

  # Check if the data is a dataframe
  if(!is.data.frame(data)){
    stop("Data is not a dataframe")
  }

  # Check if the data has an "id" column
  if(!"id" %in% colnames(data)){
    stop("Data does not have an 'id' column")
  }

  # Check if the data has a "day" column
  if(!"day" %in% colnames(data)){
    stop("Data does not have a 'day' column")
  }

  # Check if the data has a "beep" column
  if(!"beep" %in% colnames(data)){
    stop("Data does not have a 'beep' column")
  }

  # Check if the column names are in snake case
  if(!all(colnames(janitor::clean_names(data)) == colnames(data))){
    stop("Column names are not in snake case. Use janitor::clean_names().")
  }

  return("Data are clean.")

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

  # Construct JSON structure
  list(
    first_author = dataset_info$Author,
    dataset_id = dataset_info$dataset_id,
    year = dataset_info$Year,
    reference_a = dataset_info$`Reference A`,
    reference_b = dataset_info$`Reference B`,
    paper_doi = dataset_info$`Paper DOI`,
    zenodo_doi = dataset_info$`Zenodo DOI`,
    link_to_data = dataset_info$`Link to data`,
    license = dataset_info$`License`,
    link_to_codebook = dataset_info$`Optional: Link to Codebook`,
    link_to_code = dataset_info$`Optional: Link to Code`,
    n_participants = dataset_info$`N Participants`,
    n_time_points = dataset_info$`N Time Points`,
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
    dplyr::filter(!is.na(construct_name_recode)) |>
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
      dplyr::filter(dataset_id == dataset, !is.na(construct_name_recode))

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
        new_construct <- dataset_updates$construct_name_recode[i]

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
