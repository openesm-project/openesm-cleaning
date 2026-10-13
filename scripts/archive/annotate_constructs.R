# Archived from scripts/functions_data.R. Writes construct labels to the
# (now frozen) Google Sheets coding files, not to the metadata JSONs.

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
