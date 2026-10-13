library(here)
library(tidyverse)
library(jsonlite)

source(here::here("scripts", "functions_data.R"))

# run all R scripts that start with "clean" in "/scripts"
scripts <- list.files(path = here::here("scripts"), pattern = "^clean_.*\\.R$", full.names = TRUE)

# only keep scripts of datasets that have a metadata JSON
included_ids <- substr(list.files(here::here("data", "metadata"),
                                  pattern = "_metadata\\.json$"), 1, 4)

# check with grepl whether script matches included id
included_scripts <- scripts[grepl(paste(included_ids, collapse = "|"), scripts)]

# run each script and validate the resulting metadata JSON
for (script in included_scripts[c(31:length(included_scripts))]) {
  message("Running script: ", script)
  source(script)

  # validate the metadata JSON produced by this script
  dataset_id_str <- stringr::str_extract(basename(script), "\\d{4}")
  json_candidates <- list.files(
    here::here("data", "metadata"),
    pattern = paste0("^", dataset_id_str, "_.*\\.json$"),
    full.names = TRUE
  )
  if (length(json_candidates) > 0) {
    validate_metadata_json(json_candidates[[1]])
  }
}


