# Construct annotation ----------------------------------------------------
# In this script, we automatically retrieve and annotate construct metadata

# Prep
library(tidyverse)
library(jsonlite)
library(here)
library(writexl)
library(googlesheets4)
library(googledrive)

source(here::here("scripts", "functions_data.R"))

# list all variable-construct pairs
all_variables <- list_variable_construct_pairs(here::here("data", "metadata"),
                                               include_all = TRUE)

# Retrieve annotations ----------------------------------------------------
# ids that were expert-annotated
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

annotated_ids <- meta_data |>
  filter(!is.na(`New Coding Scheme`)) |>
  pull(dataset_id)


# filter expert-coded variabes
annotated_pairs <- all_variables |>
  filter(dataset_id %in% annotated_ids) |>
  filter(!is.na(construct)) |>
  distinct(variable_name, construct)

# count number of distinct rows per variable name
nondistinct_variables <- annotated_pairs |>
  group_by(variable_name) |>
  summarise(n = n_distinct(construct)) |>
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
  dry_run = TRUE,
  overwrite = FALSE
)


# Double check affect items -----------------------------------------------
# we check if items labelled as "positive affect" or "negative affect"
# are also included in our example analysis

# first, obtain list of all variables labelled as positive or negative affect
# in our example analysis columns
evaluate_vars_string <- function(var_string) {
  if (is.na(var_string) || var_string == "") {
    return(character(0))
  }

  # replace smart quotes with double quotes
  cleaned_string <- str_replace_all(var_string, c("“" = "\"", "”" = "\""))

  # convert the string representation into a character vector
  return(eval(parse(text = cleaned_string)))
}

affect_example <- meta_data |>
  filter(`Example analysis` == "yes") |>
  mutate(
    positive_affect = map(positive_affect, evaluate_vars_string),
    negative_affect = map(negative_affect, evaluate_vars_string)
  ) |>
  select(dataset_id, positive_affect, negative_affect) |>
  mutate(
    positive_affect = sapply(positive_affect, \(x) paste(x, collapse = ", ")),
    negative_affect = sapply(negative_affect, \(x) paste(x, collapse = ", "))
  )

# then, filter construct column for all variables filtered as positive or negative affect
affect_constructs <- all_variables |>
  filter(grepl("positive affect|negative affect", construct, ignore.case = TRUE))

all_affect <- affect_example |>
  left_join(affect_constructs, by = c("dataset_id"))

# check for overlap between variable constructs and example analysis lists
affect_check <- all_affect |>
  mutate(
    in_pos_list = str_detect(positive_affect, fixed(variable_name)),
    in_neg_list = str_detect(negative_affect, fixed(variable_name)),
    has_pos_construct = str_detect(str_to_lower(construct), "positive affect"),
    has_neg_construct = str_detect(str_to_lower(construct), "negative affect"),
    pos_aligned = !in_pos_list | has_pos_construct,
    neg_aligned = !in_neg_list | has_neg_construct
  ) |>
  filter(in_pos_list | in_neg_list) |>
  select(dataset_id, variable_name, in_pos_list, in_neg_list,
         has_pos_construct, has_neg_construct, pos_aligned, neg_aligned)

misaligned <- affect_check |>
  filter(!pos_aligned | !neg_aligned)

print(affect_check)
print(misaligned)


