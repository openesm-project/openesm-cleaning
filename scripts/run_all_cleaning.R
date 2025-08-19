library(here)
library(googlesheets4)
library(tidyverse)

# initialize google sheets once
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

# run all R scripts that start with "clean" in "/scripts"
scripts <- list.files(path = here::here("scripts"), pattern = "^clean_.*\\.R$", full.names = TRUE)

# only keep scripts that are "Done" in metadata
included_ids <- meta_data |>
  janitor::clean_names() |>
  filter(done == "1") |>
  pull(dataset_id)

# check with grepl wheter script matches included id
included_scripts <- scripts[grepl(paste(included_ids, collapse = "|"), scripts)]
rm(meta_data)
# run each script
for (script in included_scripts[-c(1:57)]) {
  message("Running script: ", script)
  source(script)
}


