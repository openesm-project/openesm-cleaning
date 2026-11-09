# Spreadsheet round-trip for metadata JSONs
#
# The JSON in data/metadata/ stays the source of truth; the xlsx is only an
# editing copy (gitignored, in metadata_edit/). Export, edit in Excel, import:
#
#   source(here::here("scripts", "metadata", "metadata_xlsx.R"))
#   metadata_to_xlsx("0074")   # writes metadata_edit/0074_schmitz_metadata.xlsx
#   xlsx_to_metadata("0074")   # writes the edits back into the JSON
#
# Importing can be repeated while you keep editing the same xlsx. It refuses if
# the JSON was changed in the meantime by anything else (by hand, by an LLM),
# so such changes are never silently overwritten.
#
# Variables cannot be added, removed or renamed in the spreadsheet (names must
# match the cleaned data); do that in the JSON. The changelog is edited in the
# JSON as well.

source(here::here("scripts", "functions_data.R"))

SCHEMA_PATH <- here::here("schema", "v1", "metadata_schema.json")
EDIT_DIR <- here::here("metadata_edit")

# dataset-level keys that are not shown in the spreadsheet
FIXED_KEYS <- c("dataset_id", "changelog", "features")


# Export ------------------------------------------------------------------

metadata_to_xlsx <- function(dataset_id, overwrite = FALSE) {
  json_path <- metadata_json_path(dataset_id)
  xlsx_path <- edit_path(json_path, ".xlsx")
  if (file.exists(xlsx_path) && !overwrite) {
    stop("Spreadsheet already exists: ", xlsx_path,
         "\nImport it first, or use overwrite = TRUE to discard its edits.",
         call. = FALSE)
  }

  meta <- jsonlite::read_json(json_path, simplifyVector = FALSE)
  schema <- jsonlite::read_json(SCHEMA_PATH, simplifyVector = FALSE)
  feature_props <- schema$definitions$feature$properties
  dataset_keys <- setdiff(names(schema$properties), FIXED_KEYS)

  features <- as.data.frame(
    lapply(setNames(names(feature_props), names(feature_props)), function(key)
      vapply(meta$features, function(f) as.character(f[[key]] %||% ""),
             character(1))),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  dataset <- data.frame(
    key = dataset_keys,
    value = vapply(dataset_keys, function(key)
      as.character(meta[[key]] %||% ""), character(1)),
    description = vapply(dataset_keys, function(key)
      schema$properties[[key]]$description %||% "", character(1)),
    stringsAsFactors = FALSE
  )

  # allowed values (from the schema) for dropdowns
  enum_values <- function(prop) {
    values <- unlist(Filter(Negate(is.null), prop$enum))
    values[nzchar(values)]
  }
  enums <- list(variable_type = enum_values(feature_props$variable_type),
                assessment_type = enum_values(feature_props$assessment_type))
  for (key in dataset_keys) {
    if (!is.null(schema$properties[[key]]$enum)) {
      enums[[key]] <- enum_values(schema$properties[[key]])
    }
  }

  wb <- openxlsx::createWorkbook()
  # text format also for empty cells, so Excel keeps "1-7" or "7" as typed
  text_style <- openxlsx::createStyle(numFmt = "TEXT", wrapText = TRUE,
                                      valign = "top")
  header_style <- openxlsx::createStyle(textDecoration = "bold")

  add_sheet <- function(name, df, widths) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, headerStyle = header_style)
    openxlsx::addStyle(wb, name, text_style, rows = 2:(nrow(df) + 1),
                       cols = seq_along(df), gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = widths)
  }
  add_sheet("features", features,
            widths = ifelse(names(features) %in% c("description", "details",
                                                   "labels", "coding"), 45, 18))
  add_sheet("dataset", dataset, widths = c(25, 60, 60))

  # dropdown lists live on a hidden sheet
  openxlsx::addWorksheet(wb, "lists", visible = FALSE)
  for (j in seq_along(enums)) {
    openxlsx::writeData(wb, "lists", enums[[j]], startCol = j)
  }
  add_dropdown <- function(sheet, col, rows, enum) {
    j <- match(enum, names(enums))
    letter <- openxlsx::int2col(j)
    openxlsx::dataValidation(
      wb, sheet, cols = col, rows = rows, type = "list",
      value = sprintf("'lists'!$%s$1:$%s$%d", letter, letter,
                      length(enums[[j]]))
    )
  }
  for (enum in c("variable_type", "assessment_type")) {
    add_dropdown("features", match(enum, names(features)),
                 2:(nrow(features) + 1), enum)
  }
  for (i in which(dataset$key %in% names(enums))) {
    add_dropdown("dataset", 2, i + 1, dataset$key[i])
  }

  dir.create(EDIT_DIR, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  write_json_hash(json_path)
  message("Wrote ", xlsx_path)
  invisible(xlsx_path)
}


# Import ------------------------------------------------------------------

xlsx_to_metadata <- function(dataset_id, force = FALSE) {
  json_path <- metadata_json_path(dataset_id)
  xlsx_path <- edit_path(json_path, ".xlsx")
  if (!file.exists(xlsx_path)) {
    stop("No spreadsheet found: ", xlsx_path, call. = FALSE)
  }

  # refuse if the JSON changed since the export / last import
  hash_path <- edit_path(json_path, ".md5")
  if (!force && (!file.exists(hash_path) ||
                 readLines(hash_path) != json_hash(json_path))) {
    stop(basename(json_path), " changed after the spreadsheet was exported ",
         "(edited by hand or by an LLM?). Importing would overwrite those ",
         "changes.\nRe-export with metadata_to_xlsx(\"", dataset_id,
         "\", overwrite = TRUE), or use force = TRUE to overwrite them.",
         call. = FALSE)
  }

  meta <- jsonlite::read_json(json_path, simplifyVector = FALSE)
  schema <- jsonlite::read_json(SCHEMA_PATH, simplifyVector = FALSE)
  feature_keys <- names(schema$definitions$feature$properties)
  dataset_keys <- setdiff(names(schema$properties), FIXED_KEYS)

  # features: same variables as the JSON, values from the spreadsheet
  features <- read_edit_sheet(xlsx_path, "features")
  unknown <- setdiff(names(features), feature_keys)
  if (length(unknown) > 0) {
    stop("Unknown columns in features sheet: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(features$name)) {
    stop("Duplicated variable names: ",
         paste(unique(features$name[duplicated(features$name)]),
               collapse = ", "), call. = FALSE)
  }
  json_names <- vapply(meta$features, function(f) f$name, character(1))
  added <- setdiff(features$name, json_names)
  removed <- setdiff(json_names, features$name)
  if (length(added) > 0 || length(removed) > 0) {
    stop("Variable names differ from the JSON (add, remove or rename ",
         "variables in the JSON instead).\n",
         "  Only in spreadsheet: ", paste(added, collapse = ", "), "\n",
         "  Only in JSON: ", paste(removed, collapse = ", "), call. = FALSE)
  }

  meta$features <- lapply(meta$features, function(old) {
    row <- features[features$name == old$name, , drop = FALSE]
    new <- list()
    for (key in feature_keys) {
      value <- if (key %in% names(row)) row[[key]] else old[[key]] %||% ""
      # keep the key set of the JSON; only add keys that get a value
      if (key %in% names(old) || nzchar(value)) new[[key]] <- value
    }
    new
  })

  # dataset-level fields
  dataset <- read_edit_sheet(xlsx_path, "dataset")
  unknown <- setdiff(dataset$key, dataset_keys)
  if (length(unknown) > 0) {
    stop("Unknown keys in dataset sheet: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  for (i in seq_len(nrow(dataset))) {
    key <- dataset$key[i]
    old <- meta[[key]]
    if (!nzchar(dataset$value[i]) && !key %in% names(meta)) next
    meta[key] <- list(parse_value(dataset$value[i], old,
                                  schema$properties[[key]]$type))
  }
  meta <- meta[c(setdiff(names(meta), "features"), "features")]

  jsonlite::write_json(meta, json_path, pretty = TRUE, auto_unbox = TRUE,
                       null = "null", digits = NA)
  status <- system2("python", c(here::here("scripts", "metadata",
                                           "format_metadata.py"),
                                shQuote(json_path)), stdout = FALSE)
  if (status != 0) {
    warning("Formatter failed; run scripts/metadata/format_metadata.py",
            call. = FALSE)
  }
  write_json_hash(json_path)
  validate_metadata_json(json_path)
  invisible(json_path)
}


# Helpers -----------------------------------------------------------------

edit_path <- function(json_path, ext) {
  file.path(EDIT_DIR, sub("\\.json$", ext, basename(json_path)))
}

json_hash <- function(json_path) unname(tools::md5sum(json_path))

write_json_hash <- function(json_path) {
  writeLines(json_hash(json_path), edit_path(json_path, ".md5"))
}

# read a sheet as text; empty cells become "", fully empty rows are dropped
read_edit_sheet <- function(xlsx_path, sheet) {
  df <- readxl::read_excel(xlsx_path, sheet = sheet, col_types = "text",
                           trim_ws = FALSE, .name_repair = "minimal")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(x) gsub("\r\n", "\n", ifelse(is.na(x), "", x)))
  df[rowSums(df != "") > 0, , drop = FALSE]
}

# convert a dataset-level cell to its JSON value; empty cells become null
# (or stay "" where the JSON had ""), whole numbers become integers where the
# schema allows it, unless the JSON already stored the value as a string
parse_value <- function(value, old, types) {
  if (!nzchar(value)) return(if (identical(old, "")) "" else NULL)
  if ("integer" %in% unlist(types) && grepl("^-?[0-9]+$", value) &&
      !(is.character(old) && "string" %in% unlist(types))) {
    return(as.integer(value))
  }
  value
}
