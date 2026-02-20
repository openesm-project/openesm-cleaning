# Helper: extract text from codebook file (PDF or XLSX)
read_codebook_text <- function(codebook_path) {
  if (is.null(codebook_path)) return(NULL)
  
  ext <- tolower(tools::file_ext(codebook_path))
  
  if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) 
      stop("Package 'pdftools' required for PDF codebooks.")
    text <- pdftools::pdf_text(codebook_path)
    return(paste(text, collapse = "\n"))
    
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' required for XLSX codebooks.")
    sheets <- readxl::excel_sheets(codebook_path)
    all_text <- lapply(sheets, function(s) {
      df <- readxl::read_excel(codebook_path, sheet = s)
      paste(capture.output(print(df)), collapse = "\n")
    })
    return(paste(all_text, collapse = "\n\n"))
    
  } else {
    stop("Unsupported codebook format. Provide a PDF or XLSX file.")
  }
}


# Generate annotation prompt for LLM-assisted variable coding
# This function compiles all relevant information about a dataset and its cleaning process
# into a structured prompt that can be fed to a language model for annotation helping.
generate_annotation_prompt <- function(
  df,
  dataset_id,
  codebook_path = NULL,
  script_path = NULL,
  vocab_path = here::here("data", "metadata")
) {
  
  # 1. Column names + types + basic stats
  col_info <- lapply(names(df), function(col) {
    x <- df[[col]]
    type <- class(x)[1]
    if (is.numeric(x)) {
      stats <- sprintf("min=%.2f, max=%.2f, n_unique=%d, n_na=%d",
                       min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                       n_distinct(x), sum(is.na(x)))
    } else {
      vals <- paste(head(unique(na.omit(as.character(x))), 6), collapse = ", ")
      stats <- sprintf("n_unique=%d, n_na=%d, example values: %s",
                       n_distinct(x), sum(is.na(x)), vals)
    }
    sprintf("  - %s [%s]: %s", col, type, stats)
  })
  col_block <- paste(col_info, collapse = "\n")
  
  # 2. Construct vocabulary (all distinct constructs, sorted by frequency)
  vocab <- tryCatch(
    extract_construct_vocabulary(vocab_path) |> pull(construct),
    error = function(e) character(0)
  )
  vocab_block <- if (length(vocab) > 0) {
    paste(vocab, collapse = ", ")
  } else {
    "(not available)"
  }
  
  # 3. Codebook text (truncated to ~3000 chars to keep prompt manageable)
  codebook_block <- if (!is.null(codebook_path)) {
    text <- read_codebook_text(codebook_path)
    if (nchar(text) > 3000) {
      paste0(substr(text, 1, 3000), "\n... [truncated]")
    } else {
      text
    }
  } else {
    "(not provided)"
  }
  
  # 4. Relevant lines from cleaning script (renames + recodes)
  script_block <- if (!is.null(script_path)) {
    lines <- readLines(script_path, warn = FALSE)
    relevant <- lines[grepl("rename|mutate|recode|reverse|select|8\\s*-", lines,
                            ignore.case = TRUE)]
    if (length(relevant) > 0) {
      paste(relevant, collapse = "\n")
    } else {
      "(no relevant lines found)"
    }
  } else {
    "(not provided)"
  }
  
  # 5. Output column spec
  columns <- c("dataset_id", "name", "description", "variable_type", "coding",
               "answer_categories", "details", "labels", "transformation",
               "source", "assessment_type", "construct", "comments")
  column_spec <- paste(columns, collapse = ", ")
  
  # Assemble prompt
  prompt <- glue::glue(
    "You are helping annotate variables in an Experience Sampling Method (ESM) dataset.",
    "Your task is to fill in metadata for each variable listed below.",
    "",
    "## Dataset ID",
    "{dataset_id}",
    "",
    "## Variables (name [R type]: stats)",
    "{col_block}",
    "",
    "## Known construct vocabulary (use these terms where appropriate)",
    "{vocab_block}",
    "",
    "## Codebook",
    "{codebook_block}",
    "",
    "## Cleaning script (renames and recodes)",
    "{script_block}",
    "",
    "## Instructions",
    "Return a JSON array with one object per variable.",
    "Each object must have exactly these fields (in this order):",
    "{column_spec}",
    "",
    "Rules:",
    "- dataset_id: always '{dataset_id}'",
    "- name: the variable name as given",
    "- variable_type: one of 'rating_scale', 'categorical', 'numeric', 'text', 'datetime', 'other'",
    "- assessment_type: usually 'ESM' unless clearly otherwise",
    "- construct: pick from the vocabulary list above if possible; leave empty if unsure",
    "- coding: always leave empty (\"\")",
    "- transformation: only fill if the cleaning script clearly shows a transformation",
    "- Leave any field empty (\"\") if you are not confident.",
    "- Return only the JSON array, no prose, no markdown code fences.",
    .sep = "\n"
  )
  
  cat(prompt)
  invisible(prompt)
}


# Convert LLM JSON response to xlsx coding sheet
annotation_json_to_xlsx <- function(
  json_path,
  output_path = here::here("data", "coding_sheets")
) {
  expected_cols <- c("dataset_id", "name", "description", "variable_type", "coding",
                     "answer_categories", "details", "labels", "transformation",
                     "source", "assessment_type", "construct", "comments")
  
  raw <- jsonlite::read_json(json_path, simplifyVector = TRUE)
  df <- as_tibble(raw)
  
  # ensure all expected columns exist, in order
  for (col in expected_cols) {
    if (!col %in% names(df)) df[[col]] <- ""
  }
  df <- df[, expected_cols]
  
  # derive filename from dataset_id
  dataset_id <- unique(df$dataset_id)[1]
  filename <- paste0(dataset_id, "_variables.xlsx")
  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  writexl::write_xlsx(df, path = file.path(output_path, filename))
  
  message("Saved: ", file.path(output_path, filename))
  invisible(df)
}
