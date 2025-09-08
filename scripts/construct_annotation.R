# Construct annotation ----------------------------------------------------
# In this script, we automatically retrieve and annotate construct metadata

# Prep
library(tidyverse)
library(jsonlite)
library(here)
library(writexl)
library(googledrive)

source(here::here("scripts", "functions_data.R"))

# list all variable-construct pairs
all_variables <- list_variable_construct_pairs(here::here("data", "metadata"),
                                               include_all = TRUE)

# Retrieve annotations ----------------------------------------------------
# ids that were expert-annotated
annotated_ids <- c("0001", "0002", "0003", "0004", "0005", "0006", "0008",
                   "0010", "0011", "0012", "0013", "0014")

# filter expert-coded variabes
annotated_pairs <- all_variables |>
  filter(dataset_id %in% annotated_ids) |>
  filter(!is.na(construct_name)) |>
  distinct(variable_name, construct_name)

# count number of distinct rows per variable name
nondistinct_variables <- annotated_pairs |>
  group_by(variable_name) |>
  summarise(n = n_distinct(construct_name)) |>
  arrange(desc(n)) |>
  filter(n > 1)

# show these nondistinct ones
all_variables |>
  filter(dataset_id %in% annotated_ids) |>
  filter(variable_name %in% nondistinct_variables$variable_name) |>
  arrange(variable_name) |>
  View()


# search the other datasets for these variable names
to_be_named <- all_variables |>
  filter(!dataset_id %in% annotated_ids) |>
  filter(variable_name %in% annotated_pairs$variable_name) |>
  left_join(annotated_pairs, by = "variable_name",
            suffix = c("_original", "_recode"))
to_be_named |>
  slice_sample(n = 6)



# check how many variables do not have a coding yet versus how many have one
# to get a progress percentage
all_variables |>
  filter(!variable_name %in% c("id", "beep", "day", "date")) |>
  mutate(annotated = ifelse(variable_name %in% annotated_pairs$variable_name, 1, 0)) |>
  group_by(annotated) |>
  summarise(n = n_distinct(variable_name)) |>
  mutate(perc = n / sum(n) * 100)


# Automatic annotation ----------------------------------------------------
# We now create a function that takes all expert-annotated variables and inserts
# the construct name into all other datasets that have the same variable name
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

# run for all datasets
test_results <- annotate_constructs(
  to_be_named = to_be_named,
  meta_data = meta_data,
  dry_run = FALSE,
  overwrite = TRUE
)



